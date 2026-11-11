# The lazy plumbing every DuckDB reader shares: the session-wide connection
# (opened once, spilling to disk past `memory_limit`), the lazy source (the
# projection registered as a view, its columns and types), the SQL fragments
# for a normalised phone, a text column, the per-row label letter and the base
# row set (phone scope, sample fraction, pre-filter), the sequence window and
# the scope filters. Used by disposition_steps(), disposition_paths(), the DuckDB
# path of disposition_funnel() and the completion model
# (R/completion_model.R). Behaviour and measurements are documented on the
# readers (R/disposition_steps.R header).
# The session-wide DuckDB connection behind every lazy table (see
# .disposition_lazy_con). `n` numbers the registered lookup tables.
.disposition_lazy_env <- new.env(parent = emptyenv())

# Which of the packages the lazy readers need are missing (a function so a test
# can stub it).
.disposition_lazy_missing <- function() {
  pkgs <- c("duckdb", "DBI", "dbplyr")
  pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
}

.disposition_lazy_check <- function(fn) {
  absent <- .disposition_lazy_missing()
  if (length(absent) == 0L) return(invisible())
  # An interactive session is offered the install, as the duckdb-backed screen
  # does (.disposition_offer_duckdb); after it, re-check before giving up.
  if (interactive()) {
    rlang::check_installed(absent, reason = paste(
      "to query disposition sequences inside DuckDB (the full projection does",
      "not fit in memory otherwise)."))
    absent <- .disposition_lazy_missing()
    if (length(absent) == 0L) return(invisible())
  }
  stop_s160(sprintf(paste0(
    "needs %s installed: install.packages(c(%s)). It returns a lazy table that ",
    "DuckDB computes, so the full projection never has to fit in memory."),
    paste(absent, collapse = ", "),
    paste0("\"", absent, "\"", collapse = ", ")), fn = fn)
}

# A single non-empty string option, or its default.
.disposition_lazy_option <- function(name, default) {
  value <- getOption(name, default)
  if (!is.character(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    stop_s160(sprintf("option `%s` must be a single non-empty string%s.",
                      name, .not_got(value)))
  }
  value
}

# The session-wide connection: opened once and reused, so a lazy table stays valid
# for the whole session (a table dies with its connection) and several tables
# share one memory budget instead of stacking one per call. The resource options
# are re-applied on every call, so a change takes effect on the next table:
#   survey160r.duckdb_memory    DuckDB memory_limit (default "4GB" -- fits the
#                               16 GB laptop target; past it DuckDB spills)
#   survey160r.duckdb_temp_dir  spill ROOT (default under the package cache, on
#                               real disk -- never the session tempdir, which may
#                               be a small tmpfs). Each connection spills into its
#                               own subdirectory of it: DuckDB names spill files
#                               without any instance id and deletes them on
#                               shutdown, so two R sessions sharing one directory
#                               would overwrite and delete each other's buffers.
# Insertion order is not preserved: every query here orders by its own window,
# and dropping it lets DuckDB spill more freely.
.disposition_lazy_con <- function() {
  con <- .disposition_lazy_env$con
  if (is.null(con) || !DBI::dbIsValid(con)) {
    # shared_home (newer duckdb) only silences a note about the extension
    # directory; older versions lack the argument, and the subset is then an
    # empty list (do.call() needs a list either way).
    drv_args <- list(shared_home = FALSE)["shared_home" %in% names(formals(duckdb::duckdb))]
    drv <- do.call(duckdb::duckdb, drv_args)
    con <- DBI::dbConnect(drv)
    .disposition_lazy_env$con <- con
    .disposition_lazy_env$temp_root <- NULL
    if (is.null(.disposition_lazy_env$finalizer)) {
      reg.finalizer(.disposition_lazy_env, .disposition_lazy_close, onexit = TRUE)
      .disposition_lazy_env$finalizer <- TRUE
    }
  }
  memory <- .disposition_lazy_option("survey160r.duckdb_memory", "4GB")
  temp_root <- .disposition_lazy_option(
    "survey160r.duckdb_temp_dir",
    file.path(tools::R_user_dir("survey160r", "cache"), "duckdb_tmp"))
  tryCatch({
    DBI::dbExecute(con, paste("SET memory_limit =", DBI::dbQuoteString(con, memory)))
    # DuckDB refuses to switch the spill directory once it has been used (even to
    # the same path), so it is set only when the root changes: on a fresh
    # connection, or when the option names a new root before any spill. DuckDB
    # creates only the last path level itself, so the parents are made here.
    if (!identical(.disposition_lazy_env$temp_root, temp_root)) {
      temp_dir <- file.path(temp_root, sprintf("session-%d-%s", Sys.getpid(),
                                               basename(tempfile(""))))
      dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
      .disposition_prune_spill(temp_root)
      DBI::dbExecute(con, paste("SET temp_directory =", DBI::dbQuoteString(con, temp_dir)))
      .disposition_lazy_close_spill()
      .disposition_lazy_env$temp_root <- temp_root
      .disposition_lazy_env$temp_dir <- temp_dir
    }
    DBI::dbExecute(con, "SET preserve_insertion_order = false")
  }, error = function(e) {
    stop_s160(sprintf(paste0(
      "DuckDB rejected the connection settings (memory %s, spill directory %s): %s. ",
      "Check options `survey160r.duckdb_memory` (e.g. \"4GB\") and ",
      "`survey160r.duckdb_temp_dir` (a new spill directory applies only before the ",
      "session's first spill; restart R to move it later)."), memory, temp_root,
      conditionMessage(e)))
  })
  con
}

# Close the session connection (the finalizer at session exit; also callable).
.disposition_lazy_close <- function(env = .disposition_lazy_env) {
  con <- env$con
  if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con, shutdown = TRUE)
  env$con <- NULL
  env$registered <- NULL
  env$temp_root <- NULL
  .disposition_lazy_close_spill(env)
  invisible()
}

# Remove other sessions' spill subdirectories untouched for `days`: a session
# that crashed (or was killed) never ran its finalizer, so its directory -- and
# any spill files in it -- would otherwise stay under the root forever. A live
# session that has not spilled for two days loses only an empty directory, which
# DuckDB recreates on its next spill.
.disposition_prune_spill <- function(root, days = 2) {
  dirs <- list.files(root, pattern = "^session-", full.names = TRUE)
  stale <- dirs[file.mtime(dirs) < Sys.time() - days * 86400]
  unlink(stale, recursive = TRUE)
  invisible(stale)
}

# Remove this connection's spill subdirectory (DuckDB empties it on shutdown).
.disposition_lazy_close_spill <- function(env = .disposition_lazy_env) {
  if (!is.null(env$temp_dir)) unlink(env$temp_dir, recursive = TRUE)
  env$temp_dir <- NULL
  invisible()
}

# Register a lookup frame (requested phones, campaign ids) for a lazy table. Each
# distinct frame gets its own name -- tables share the connection, so reusing one
# name for different content would silently change an earlier table's rows --
# and stays registered for the session, because a lazy table (and every dplyr
# verb built on it) reads it again on each collect(). Identical frames reuse one
# registration, so screening the same sample repeatedly does not pile up copies.
.disposition_lazy_register <- function(con, prefix, df) {
  key <- paste(prefix, digest::digest(df, algo = "xxhash64"))
  registered <- .disposition_lazy_env$registered %||% list()
  name <- registered[[key]]
  if (!is.null(name) && name %in% DBI::dbListTables(con)) return(name)
  n <- (.disposition_lazy_env$n %||% 0L) + 1L
  .disposition_lazy_env$n <- n
  name <- sprintf("s160_%s_%d", prefix, n)
  duckdb::duckdb_register(con, name, df)
  registered[[key]] <- name
  .disposition_lazy_env$registered <- registered
  name
}

# Validate `dataset` and describe it: the quoted read_parquet() source (absolute
# path, so the lazy table survives a later setwd()) and its stored column names.
.disposition_lazy_source <- function(con, dataset, fn) {
  # disposition_summary() / disposition_funnel() take an in-memory frame too, so
  # passing one here is a natural mistake: say why it cannot work and the fix.
  if (is.data.frame(dataset)) {
    stop_s160(sprintf(paste0(
      "`dataset` must be the disposition Parquet path (from disposition_pull()), ",
      "not a data frame%s. The sequence readers run inside DuckDB over the file; ",
      "to use an in-memory frame, write it first with nanoparquet::write_parquet()."),
      sub("^, not a data frame", "", .not_got(dataset))), fn = fn)
  }
  .disposition_check_path(dataset)
  # Checked on the RESOLVED path: a glob character in the working directory or a
  # symlink target ("/data/run[1]/x.parquet") would expand just the same.
  resolved <- normalizePath(dataset, mustWork = TRUE)
  if (grepl("[*?[]", resolved)) {
    stop_s160(sprintf(paste0(
      "`dataset` must resolve to a literal file path; DuckDB would expand the glob ",
      "character(s) in %s and read other files too. Rename or copy the file."),
      resolved), fn = fn)
  }
  path <- DBI::dbQuoteString(con, resolved)
  schema <- DBI::dbGetQuery(con, paste0("DESCRIBE SELECT * FROM read_parquet(", path, ")"))
  # The file row number is the last tie-break of the sequence order (two stored
  # rows of one phone, date and campaign -- e.g. "12015550101" and "2015550101"
  # normalizing together). It sorts DESCENDING: disposition_summary() takes the
  # FIRST stored row of a full tie as the latest, so that row must be the last
  # step here.
  src <- paste0("read_parquet(", path, ", file_row_number = true)")
  cols <- schema$column_name
  missing_cols <- setdiff(.DISPOSITION_SEQUENCE_REQUIRED, cols)
  if (length(missing_cols) > 0L) {
    stop_s160(sprintf("`dataset` is missing required column(s): %s (it has %d: %s).",
                      paste(missing_cols, collapse = ", "), length(cols),
                      .preview_values(cols)), fn = fn)
  }
  # DuckDB identifiers are case-insensitive: a stored S160_CODE would shadow the
  # generated s160_code, so the guard compares lower-cased names.
  lower <- tolower(cols)
  clash <- cols[lower %in% c(.DISPOSITION_SEQUENCE_GENERATED, "file_row_number") |
                  startsWith(lower, "s160_")]
  if (length(clash) > 0L) {
    stop_s160(sprintf(paste0("`dataset` has column(s) named like the sequence columns these ",
                             "readers add: %s. Rename them in a copy of the file."),
                      paste(clash, collapse = ", ")), fn = fn)
  }
  types <- schema$column_type
  names(types) <- cols
  list(src = src, cols = cols, types = types)
}

# A stored id-like column (phone, error code) as text, the way R's as.character()
# prints it: a numeric column -- a fixture or a hand-built extract -- is cast
# through an integer first, so 2015550101 reads "2015550101", not "2015550101.0".
.disposition_text_sql <- function(col, type) {
  if (type %in% c("VARCHAR", "BLOB")) return(col)
  sprintf("CAST(TRY_CAST(%s AS HUGEINT) AS VARCHAR)", col)
}

# Columns the sequence readers generate; a stored column of the same name would
# be ambiguous (so is any `s160_*` name, the internal prefix).
.DISPOSITION_SEQUENCE_GENERATED <- c(
  "step", "n_campaigns", "label", "completion_known", "prev_label", "next_label",
  "next_date", "gap_prev_days", "gap_next_days", "completes_so_far",
  "non_responses_so_far", "replies_so_far", "run_length", "path_so_far",
  "prior_completes", "prior_replies", "prior_unanswered", "history_segment",
  "steps_since_reply", "steps_since_complete", "steps_since_undeliverable",
  "days_since_reply", "days_since_complete", "days_since_undeliverable",
  "history_matched", "context", "path", "shape", "first_date", "last_date",
  "n_unknown_completion", "matched", "match_start", "match_end",
  "match_start_date", "match_end_date")

# The SQL twin of .normalize_phone(): strip non-digits, blank -> NULL, an
# 11-digit number with a leading 1 drops it. A clean ten-digit phone (all of
# prod) short-circuits the regex, so normalizing costs ~0.5 s on the projection.
.disposition_phone_sql <- function(col) {
  digits <- sprintf("regexp_replace(%s, '[^0-9]', '', 'g')", col)
  sprintf(paste0(
    "CASE WHEN length(%1$s) = 10 AND NOT regexp_matches(%1$s, '[^0-9]') THEN %1$s ",
    "WHEN %2$s = '' THEN NULL ",
    "WHEN length(%2$s) = 11 AND starts_with(%2$s, '1') THEN substr(%2$s, 2) ",
    "ELSE %2$s END"), col, digits)
}

# The per-row label letter as a SQL CASE, built from the columns the file has:
# a projection without refused/ineligible/error simply never yields R/I/X (as the
# summary defaults them to 0). Precedence, later wins in the summary's terms:
# web_complete > completed > refused > ineligible > terminated > opted_in >
# engaged > undeliverable > non_response. `terminal = "engaged"` folds an
# unsplit terminated row (neither refused nor ineligible) back to the step it
# reached -- opted_in or engaged -- for analyses where the producer's unsplit
# terminal is noise.
.disposition_label_sql <- function(con, source, terminal, undeliverable_codes) {
  q <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  cols <- source$cols
  has <- function(x) x %in% cols
  when <- function(cond, code) sprintf("WHEN %s THEN '%s'", cond, code)
  flag <- function(x) paste(q(x), "= 1")
  codes <- paste(DBI::dbQuoteString(con, undeliverable_codes), collapse = ", ")
  rules <- c(
    if (has("web_complete")) when(flag("web_complete"), "W"),
    when(flag("completed"), "C"),
    if (has("refused")) when(flag("refused"), "R"),
    if (has("ineligible")) when(flag("ineligible"), "I"),
    if (has("terminated") && terminal == "engaged") {
      c(when(paste(flag("terminated"), "AND", flag("opted_in")), "O"),
        when(flag("terminated"), "E"))
    },
    if (has("terminated")) when(flag("terminated"), "T"),
    when(flag("opted_in"), "O"),
    when(flag("engaged"), "E"),
    if (has("error") && length(undeliverable_codes) > 0L) {
      when(sprintf("%s IN (%s)", .disposition_text_sql(q("error"), source$types[["error"]]), codes),
           "X")
    })
  paste("CASE", paste(rules, collapse = " "), "ELSE 'N' END")
}

# Letter -> label name, as SQL (NULL stays NULL, e.g. lead() past the last step).
.disposition_label_name_sql <- function(expr) {
  whens <- sprintf("WHEN '%s' THEN '%s'", .DISPOSITION_PATH_CODES,
                   names(.DISPOSITION_PATH_CODES))
  paste("CASE", expr, paste(whens, collapse = " "), "END")
}

.disposition_check_terminal_codes <- function(undeliverable_codes, fn) {
  if (is.null(undeliverable_codes)) return(character(0))
  if (!(is.character(undeliverable_codes) || is.numeric(undeliverable_codes)) ||
        anyNA(undeliverable_codes)) {
    stop_s160(sprintf(paste0("`undeliverable_codes` must be a character vector of ",
                             "delivery-error codes (or NULL for none)%s."),
                      .not_got(undeliverable_codes)), fn = fn)
  }
  unique(trimws(as.character(undeliverable_codes)))
}

.disposition_check_sample_frac <- function(sample_frac, fn) {
  if (is.null(sample_frac)) return(NULL)
  if (!is.numeric(sample_frac) || length(sample_frac) != 1L || is.na(sample_frac) ||
        sample_frac <= 0 || sample_frac > 1) {
    stop_s160(sprintf("`sample_frac` must be a single number in (0, 1]%s.",
                      .not_got(sample_frac)), fn = fn)
  }
  sample_frac
}

# The scoped base rows, one per stored (phone, campaign): every stored column,
# `phone` digit-normalized, `disposition_date` as a DATE, plus the label letter
# (`s160_code`) and whether the row's completion is known (`completion_known`:
# FALSE where `completed` is NA -- a t2w_external send, whose completes are
# never reported). Scoping happens HERE, before the history windows:
# `phones` and `sample_frac` keep or drop all of a number's campaigns, and `pre`
# adds the campaign / date scope (.disposition_scope_filters()), which removes
# campaigns from the history itself.
.disposition_base_sql <- function(con, source, phones, sample_frac, label_sql, pre = NULL) {
  q <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  others <- setdiff(source$cols, c("phone", "disposition_date"))
  phone <- .disposition_text_sql(q("phone"), source$types[["phone"]])
  select <- c(paste(.disposition_phone_sql(phone), "AS phone"),
              paste0("CAST(", q("disposition_date"), " AS DATE) AS disposition_date"),
              q(others),
              paste(label_sql, "AS s160_code"),
              "file_row_number AS s160_row",
              paste(q("completed"), "IS NOT NULL AS completion_known"))
  where <- c("phone IS NOT NULL", pre)
  inner_where <- NULL
  if (!is.null(phones)) {
    if (length(phones) == 0L) {
      where <- c(where, "FALSE")
    } else {
      req <- .disposition_lazy_register(con, "phones",
                                        data.frame(phone = phones, stringsAsFactors = FALSE))
      if (all(nchar(phones, type = "bytes") == 10L)) {
        # Every requested phone is ten digits (the usual case), so a stored
        # value of ten characters can only match as itself: match it as stored
        # and normalize only the other lengths. Applied on the scan, before
        # the normalized projection, it is a plain hash semi-join on the
        # stored column for all of prod -- ~2x faster than filtering the
        # normalized phone, with the same rows (see .disposition_read_duckdb).
        inner_where <- sprintf("(CASE WHEN length(%1$s) = 10 THEN %1$s ELSE %2$s END) IN (SELECT phone FROM %3$s)",
                               phone, .disposition_phone_sql(phone), req)
      } else {
        where <- c(where, sprintf("phone IN (SELECT phone FROM %s)", req))
      }
    }
  }
  if (!is.null(sample_frac) && sample_frac < 1) {
    # md5, not hash(): the same phones are sampled on every DuckDB version, so a
    # sample is reproducible across machines and upgrades.
    where <- c(where, sprintf("md5_number_lower(phone) %% 1000000 < %d",
                              as.integer(round(sample_frac * 1e6))))
  }
  paste0("SELECT * FROM (SELECT ", paste(select, collapse = ", "), " FROM ",
         source$src, if (!is.null(inner_where)) paste(" WHERE", inner_where),
         ") WHERE ", paste(where, collapse = " AND "))
}

# The ordered window every history column shares (one sort per query). It is the
# order disposition_summary() reads "latest" from (.disposition_collapse): by
# date, then by campaign id compared as a NUMBER (a text id "10" follows "9"),
# with a missing date or id counted as the OLDEST -- so a phone's last step here
# is the summary's latest_disposition.
# An integer id (prod) sorts as itself; only a text or other id pays for the
# numeric cast (~3 s on the full projection).
# Whether a DuckDB column type is numeric (ids stored as numbers), DECIMAL(p,s)
# included.
.disposition_numeric_type <- function(type) {
  type %in% c("TINYINT", "SMALLINT", "INTEGER", "BIGINT", "HUGEINT", "UTINYINT", "USMALLINT",
              "UINTEGER", "UBIGINT", "FLOAT", "DOUBLE") || startsWith(type, "DECIMAL")
}

.disposition_sequence_window <- function(source) {
  id_order <- if (.disposition_numeric_type(source$types[["campaign_id"]])) {
    "campaign_id ASC NULLS FIRST"
  } else {
    # numeric value only, like the summary's as.numeric(): ids that tie as
    # numbers ("01", "1") or are not numbers fall to the file-order tie-break
    "TRY_CAST(campaign_id AS DOUBLE) ASC NULLS FIRST"
  }
  order_by <- paste("PARTITION BY phone ORDER BY disposition_date ASC NULLS FIRST,", id_order,
                    ", s160_row DESC ROWS BETWEEN UNBOUNDED PRECEDING AND")
  # w: the history up to and including this step; w_prior: the steps before it
  paste0("WINDOW w AS (", order_by, " CURRENT ROW), w_prior AS (", order_by, " 1 PRECEDING)")
}

# The campaign / date scope as row predicates, applied to the base rows BEFORE
# the history windows -- the meaning date_from / date_to / campaign_ids have in
# every reader (disposition_summary(), disposition_screen(), ...): only campaigns
# in scope exist, so a step's history, a path and its anchors are the scoped
# ones. A caller who wants some rows with their full history filters the result.
.disposition_scope_filters <- function(con, source, campaign_ids, date_from, date_to, fn) {
  where <- character(0)
  if (!is.null(campaign_ids)) {
    if (!(is.character(campaign_ids) || is.numeric(campaign_ids)) || anyNA(campaign_ids)) {
      stop_s160(sprintf("`campaign_ids` must be a vector of campaign ids with no NA%s.",
                        .not_got(campaign_ids)), fn = fn)
    }
    # The other readers keep a row when as.character(its id) is one of
    # as.character(campaign_ids) (.disposition_filter). A text id column is
    # matched as that text. A numeric one is matched as numbers -- SQL casts a
    # DOUBLE 1 to "1.0", not R's "1" -- keeping only the values R spells the same
    # way, so "01" and "1.0" match nothing here either.
    txt <- as.character(campaign_ids)
    numeric_id <- .disposition_numeric_type(source$types[["campaign_id"]])
    id <- txt
    if (numeric_id) {
      id <- suppressWarnings(as.numeric(txt))
      # Values that are all non-numbers ("2026-02-01" passed by position, a
      # typo) would only return an empty table: say so. A mixed list keeps the
      # ids that match, as in the other readers.
      if (length(id) > 0L && all(is.na(id))) {
        stop_s160(sprintf("`campaign_ids` must be campaign numbers (the dataset's ids are numeric): %s.",
                          .preview_values(campaign_ids)), fn = fn)
      }
      id <- id[!is.na(id) & as.character(id) == txt]
    }
    ids <- .disposition_lazy_register(con, "campaigns", data.frame(id = unique(id), stringsAsFactors = FALSE))
    where <- c(where, sprintf("%s IN (SELECT id FROM %s)",
                              if (numeric_id) "CAST(campaign_id AS DOUBLE)" else "CAST(campaign_id AS VARCHAR)",
                              ids))
  }
  from <- .disposition_date_bound(date_from, "date_from", fn)
  to <- .disposition_date_bound(date_to, "date_to", fn)
  .check_day_range(from, to, fn)
  date_bound <- function(d, op) {
    if (!is.null(d)) sprintf("disposition_date %s DATE '%s'", op, format(d))
  }
  c(where, date_bound(from, ">="), date_bound(to, "<="))
}
