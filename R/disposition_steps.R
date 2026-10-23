# Sequence readers over the disposition dataset: disposition_steps() (one row per
# (phone, campaign), with the phone's history up to that campaign) and
# disposition_paths() (one row per phone, its campaigns as a letter path; see
# R/disposition_paths.R). Both answer "what happened to a number over time" --
# no reply first, then engaged, then completed -- which the per-phone rollup
# (disposition_summary) cannot, because it collapses the order away.
#
# Both return a LAZY dbplyr table on one session-wide DuckDB connection instead of
# a data frame. The history columns are window functions over every campaign a
# phone has, so on the production projection (~165M rows, ~85M phones) they only
# fit when DuckDB computes them -- spilling to disk past `memory_limit` -- and R
# receives just what the caller collect()s. Measured on prod (4 GB limit): a
# full-scan steps aggregate in ~10 s, paths in ~8 s, <= 5.5 GB RSS and up to
# ~20 GB of transient spill; a phone-scoped table (up to millions of phones) in
# 1-5 s. A whole-file read into R (nanoparquet) peaks past 20 GB, so there is no
# fallback engine: these two need duckdb + DBI + dbplyr (all Suggests).
#
# Ordering and labels are shared by the two readers:
#   * A phone's campaigns are ordered by disposition_date (its last-send date in
#     that campaign), then campaign_id; a missing date sorts last.
#   * Each (phone, campaign) gets ONE label, the furthest step reached, by the
#     same precedence as the summary's latest/best disposition
#     (.disposition_derive_category), plus `undeliverable`: no reply and a
#     delivery-error code in `undeliverable_codes`. A hard carrier rejection is
#     not a non-response -- on prod the next campaign gets a reply 0.7% of the
#     time after one of those codes vs 5.9% after a plain non-response, while
#     softer codes (4740, 4405, 4780) reply at or above the plain rate, so the
#     default list holds only the hard ones.
#   * The path alphabet is one letter per label (.DISPOSITION_PATH_CODES).

# One letter per label, in funnel order. `undeliverable` sits beside
# `non_response`: both are "no reply", split by whether the send was rejected.
.DISPOSITION_PATH_CODES <- c(non_response = "N", undeliverable = "X", engaged = "E",
                             opted_in = "O", terminated = "T", ineligible = "I",
                             refused = "R", completed = "C", web_complete = "W")

# Carrier delivery-error codes that mean the number could not be reached (no
# reply follows: 0.7-1.7% next-campaign reply on prod vs 5.9% for no error).
.DISPOSITION_UNDELIVERABLE_CODES <- c("4700", "4701", "4702", "4720", "4750", "4753")

# Stored columns the sequence readers cannot do without; the rest of the funnel
# flags (web_complete, terminated, refused, ineligible) and `error` are optional,
# as on a projection published before they existed.
.DISPOSITION_SEQUENCE_REQUIRED <- c("phone", "campaign_id", "disposition_date",
                                    "engaged", "opted_in", "completed")

# disposition_steps() column groups (`columns =`), each a few window columns.
.DISPOSITION_STEP_GROUPS <- c("neighbors", "so_far", "run_length", "path_so_far")

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
  "history_matched", "path", "shape", "first_date", "last_date",
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
# never reported). Scoping happens HERE, before the history windows, only by
# whole phones -- `phones` and `sample_frac` keep or drop all of a number's
# campaigns -- so a step's history is never cut short. `pre` adds row predicates
# (disposition_paths()' date window, which deliberately shapes the path).
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
  paste("WINDOW w AS (PARTITION BY phone ORDER BY disposition_date ASC NULLS FIRST,", id_order,
        ", s160_row DESC ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW)")
}

# Row filters applied AFTER the history windows (disposition_steps): keeping a
# campaign or a date range must not reset the history the kept rows carry, so a
# 2026 row still counts the phone's 2025 non-responses.
.disposition_after_filters <- function(con, source, campaign_ids, date_from, date_to, fn) {
  where <- character(0)
  if (!is.null(campaign_ids)) {
    if (!(is.character(campaign_ids) || is.numeric(campaign_ids)) || anyNA(campaign_ids)) {
      stop_s160(sprintf("`campaign_ids` must be a vector of campaign ids with no NA%s.",
                        .not_got(campaign_ids)), fn = fn)
    }
    # A numeric id column (a DOUBLE extract stores 1 as 1.0, which casts to the
    # text "1.0") is matched as numbers; a text id column as text, like
    # disposition_records()' as.character() match.
    numeric_id <- .disposition_numeric_type(source$types[["campaign_id"]])
    ids <- .disposition_lazy_register(con, "campaigns", data.frame(
      id = unique(if (numeric_id) suppressWarnings(as.numeric(campaign_ids)) else as.character(campaign_ids)),
      stringsAsFactors = FALSE))
    where <- c(where, sprintf("%s IN (SELECT id FROM %s)",
                              if (numeric_id) "CAST(campaign_id AS DOUBLE)" else "CAST(campaign_id AS VARCHAR)",
                              ids))
  }
  date_bound <- function(x, name, op) {
    d <- .disposition_date_bound(x, name)
    if (!is.null(d)) sprintf("disposition_date %s DATE '%s'", op, format(d))
  }
  c(where, date_bound(date_from, "date_from", ">="), date_bound(date_to, "date_to", "<="))
}

# The window columns for each requested group, over the base rows' letters. The
# history path is also built when only run_length or a pattern needs it.
.disposition_step_windows <- function(groups, need_path) {
  in_codes <- function(codes) {
    sprintf("CAST(sum(CAST(s160_code IN (%s) AS INTEGER)) OVER w AS INTEGER)",
            paste0("'", codes, "'", collapse = ", "))
  }
  c("CAST(row_number() OVER w AS INTEGER) AS step",
    "CAST(count(*) OVER (PARTITION BY phone) AS INTEGER) AS n_campaigns",
    if ("neighbors" %in% groups) {
      c("lag(s160_code) OVER w AS s160_prev", "lead(s160_code) OVER w AS s160_next",
        "lag(disposition_date) OVER w AS s160_prev_date",
        "lead(disposition_date) OVER w AS next_date")
    },
    if ("so_far" %in% groups) {
      c(paste(in_codes(c("C", "W")), "AS completes_so_far"),
        paste(in_codes(c("N", "X")), "AS non_responses_so_far"),
        paste(in_codes(c("E", "O", "T", "I", "R", "C", "W")), "AS replies_so_far"))
    },
    if (need_path) "string_agg(s160_code, '') OVER w AS path_so_far")
}

# The output projection: identity, label, history, then the stored columns. A
# run length is read off the history path -- its trailing run of the current
# letter -- so it shares the one window sort (a separate (phone, label)
# partition, the classic gaps-and-islands form, cost ~4x more on prod).
.disposition_step_select <- function(con, cols, groups, pattern_re) {
  q <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  stored <- setdiff(cols, c("phone", "campaign_id", "disposition_date"))
  c("phone", "campaign_id", "disposition_date", "step", "n_campaigns",
    paste(.disposition_label_name_sql("s160_code"), "AS label"),
    "completion_known",
    if ("neighbors" %in% groups) {
      c(paste(.disposition_label_name_sql("s160_prev"), "AS prev_label"),
        paste(.disposition_label_name_sql("s160_next"), "AS next_label"),
        "next_date",
        "CAST(disposition_date - s160_prev_date AS INTEGER) AS gap_prev_days",
        "CAST(next_date - disposition_date AS INTEGER) AS gap_next_days")
    },
    if ("so_far" %in% groups) c("completes_so_far", "non_responses_so_far", "replies_so_far"),
    if ("run_length" %in% groups) {
      "CAST(length(path_so_far) - length(rtrim(path_so_far, s160_code)) AS INTEGER) AS run_length"
    },
    if ("path_so_far" %in% groups) "path_so_far",
    if (!is.null(pattern_re)) {
      paste0("regexp_matches(path_so_far, ", DBI::dbQuoteString(con, pattern_re),
             ") AS history_matched")
    },
    q(stored))
}

#' Read the disposition dataset as a sequence: one row per campaign, with history
#'
#' Orders each phone's campaigns in time and returns one row per
#' \code{(phone, campaign_id)} carrying the phone's \emph{history up to that
#' campaign}: its position (\code{step} of \code{n_campaigns}), a single
#' disposition \code{label}, the previous and next labels and the gaps between
#' them, running counts, the length of the current run (e.g. the third
#' non-response in a row), and the history itself as a letter path. Filter, group
#' and summarise it with \pkg{dplyr} verbs to answer questions that depend on
#' what came before -- "after three non-responses in a row, how often does the
#' next campaign get a reply?", "once a number has completed twice, how often
#' does it complete again?". For one row per phone and sequence patterns
#' (non-response, then engaged, then completed) see
#' \code{\link{disposition_paths}}.
#'
#' \strong{A lazy table.} The result is a \pkg{dbplyr} table that DuckDB
#' computes when you \code{collect()} (or print) it, not a data frame: the history
#' columns are window functions over every campaign a phone has, which on the
#' production projection (~165M rows) only fit inside DuckDB. Aggregate before
#' you \code{collect()} -- the full table is about 16 GB in R -- or scope it with
#' \code{phones} / \code{sample_frac}. Rows come back unordered; \code{arrange()}
#' when order matters. All tables share one session-wide DuckDB connection (see
#' Resources).
#'
#' \strong{Order and labels.} A phone's campaigns are ordered by
#' \code{disposition_date} (its last send in that campaign), then
#' \code{campaign_id} compared as a number; a missing date or id counts as the
#' oldest, as in \code{\link{disposition_summary}}, so a phone's last step is its
#' \code{latest_disposition}. Campaigns that overlapped in time
#' can come out in either order (0.5\% of consecutive pairs on prod, all under 30
#' days apart). Each row gets the furthest step it reached, by the precedence of
#' \code{\link{disposition_summary}}'s \code{latest_disposition}:
#' \code{web_complete}, \code{completed}, \code{refused}, \code{ineligible},
#' \code{terminated}, \code{opted_in}, \code{engaged}, then \code{undeliverable}
#' (no reply and a delivery-error code in \code{undeliverable_codes}) and
#' \code{non_response}. Path letters: N non_response, X undeliverable, E
#' engaged, O opted_in, T terminated, I ineligible, R refused, C completed, W
#' web_complete.
#'
#' \strong{Filters.} \code{phones} and \code{sample_frac} keep or drop whole
#' phones \emph{before} the history is computed. \code{campaign_ids},
#' \code{date_from} and \code{date_to} drop rows \emph{after} it, so a kept row
#' still carries its full history (a 2026 row counts the phone's 2025
#' non-responses).
#'
#' \strong{Resources.} The connection uses a 4 GB DuckDB \code{memory_limit} and
#' spills to disk past it (a full-projection query can spill ~20 GB, freed when it
#' finishes); set \code{options(survey160r.duckdb_memory = "8GB")} or
#' \code{options(survey160r.duckdb_temp_dir = "/path")} to change either (a new
#' spill directory applies only before the session's first spill; each session
#' spills into its own subdirectory, removed when the session ends). Measured on
#' the production projection (~165M rows): a full-scan aggregate in ~16 s at
#' ~5.5 GB of process memory; a phone-scoped table (up to millions of phones) in
#' 1-5 s. Each distinct \code{phones} vector stays registered on the connection
#' for the session, so later operations on the table keep working.
#'
#' @param dataset Path to the disposition Parquet file, e.g. from
#'   \code{\link{disposition_pull}}.
#' @param phones Optional character vector of phone numbers; keeps only these
#'   phones (all of their campaigns). Matched digit-normalized, as the other
#'   readers do. \code{character(0)} returns no rows.
#' @param sample_frac Optional number in (0, 1]: keep a deterministic sample of
#'   that fraction of phones (by an md5 of the phone, so the same phones on every
#'   machine and DuckDB version). \code{0.1} makes full-projection exploration
#'   ~10x cheaper; the 10\% sample matches the full data to within 0.02 points
#'   on prod.
#' @param campaign_ids Optional vector; keep rows of these campaigns (history
#'   still covers all campaigns).
#' @param date_from,date_to Optional \code{Date}/date-string bounds on
#'   \code{disposition_date} (inclusive); keep rows in range (history still covers
#'   all campaigns).
#' @param columns Which history column groups to compute: any of
#'   \code{"neighbors"} (\code{prev_label}, \code{next_label}, \code{next_date},
#'   \code{gap_prev_days}, \code{gap_next_days}), \code{"so_far"}
#'   (\code{completes_so_far}, \code{non_responses_so_far}, \code{replies_so_far},
#'   counting this step), \code{"run_length"} (how many steps in a row, ending
#'   here, have this label) and \code{"path_so_far"} (the history as letters,
#'   ending with this step). All by default; each costs a few seconds on the full
#'   projection.
#' @param pattern Optional sequence pattern (see \code{\link{disposition_paths}}
#'   for the syntax); adds \code{history_matched}: whether the history up to and
#'   including this step matches. Use a \code{$} anchor for "history ends with",
#'   e.g. \code{"non_response|undeliverable{3,}$"}.
#' @param terminal How to label an unsplit \code{terminated} row (terminated but
#'   neither refused nor ineligible): \code{"raw"} (default) keeps
#'   \code{terminated}, matching \code{\link{disposition_summary}};
#'   \code{"engaged"} labels it by the step it reached (\code{opted_in} or
#'   \code{engaged}). On prod most of these rows come from a producer change in
#'   August 2026 rather than respondent behaviour.
#' @param undeliverable_codes Delivery-error codes that make a no-reply row
#'   \code{undeliverable} rather than \code{non_response}. Defaults to the hard
#'   carrier rejections (4700, 4701, 4702, 4720, 4750, 4753); \code{NULL} labels
#'   every no-reply row \code{non_response}, as the summary does.
#' @return A lazy \pkg{dbplyr} table, one row per \code{(phone, campaign_id)}:
#'   \code{phone}, \code{campaign_id}, \code{disposition_date}, \code{step},
#'   \code{n_campaigns}, \code{label}, \code{completion_known} (\code{FALSE} where
#'   \code{completed} is \code{NA}, i.e. text-to-web external, where a complete is
#'   never reported), the requested history columns, \code{history_matched} (with
#'   \code{pattern}), then every other stored column (funnel flags,
#'   \code{survey_mode}, \code{carrier}, \code{error}, \code{tracker_*}, ...).
#' @seealso \code{\link{disposition_paths}} (one row per phone),
#'   \code{\link{disposition_summary}} (per-phone counts),
#'   \code{\link{disposition_records}} (the stored rows)
#' @examples
#' \dontrun{
#' library(dplyr)
#' dataset <- disposition_pull()
#' steps <- disposition_steps(dataset, sample_frac = 0.1)
#'
#' # Reply rate on the next campaign after k non-responses in a row
#' steps |>
#'   filter(label == "non_response", !is.na(next_label)) |>
#'   group_by(run = pmin(run_length, 5L)) |>
#'   summarise(n = n(), reply_next = mean(as.integer(
#'     !next_label %in% c("non_response", "undeliverable")), na.rm = TRUE)) |>
#'   arrange(run) |>
#'   collect()
#' }
#' @export
disposition_steps <- function(dataset, phones = NULL, sample_frac = NULL,
                              campaign_ids = NULL, date_from = NULL, date_to = NULL,
                              columns = c("neighbors", "so_far", "run_length", "path_so_far"),
                              pattern = NULL,
                              terminal = c("raw", "engaged"),
                              undeliverable_codes = .DISPOSITION_UNDELIVERABLE_CODES) {
  fn <- "disposition_steps"
  terminal <- match.arg(terminal)
  .disposition_lazy_check(fn)
  if (!is.null(columns) &&
        (!is.character(columns) || anyNA(columns) || !all(columns %in% .DISPOSITION_STEP_GROUPS))) {
    stop_s160(sprintf("`columns` must be NULL or any of %s%s.",
                      paste(.DISPOSITION_STEP_GROUPS, collapse = ", "), .not_got(columns)),
              fn = fn)
  }
  pattern_re <- if (!is.null(pattern)) .disposition_pattern_compile(pattern, fn)
  codes <- .disposition_check_terminal_codes(undeliverable_codes, fn)
  sample_frac <- .disposition_check_sample_frac(sample_frac, fn)
  phones <- .disposition_request_phones(phones)

  con <- .disposition_lazy_con()
  source <- .disposition_lazy_source(con, dataset, fn)
  after <- .disposition_after_filters(con, source, campaign_ids, date_from, date_to, fn)
  base <- .disposition_base_sql(con, source, phones, sample_frac,
                                .disposition_label_sql(con, source, terminal, codes))
  need_path <- any(c("run_length", "path_so_far") %in% columns) || !is.null(pattern_re)
  windowed <- paste0("SELECT *, ", paste(.disposition_step_windows(columns, need_path), collapse = ", "),
                     " FROM (", base, ") ", .disposition_sequence_window(source))
  sql <- paste0("SELECT ", paste(.disposition_step_select(con, source$cols, columns, pattern_re),
                                 collapse = ", "),
                " FROM (", windowed, ")",
                if (length(after) > 0L) paste(" WHERE", paste(after, collapse = " AND ")))
  dplyr::tbl(con, dbplyr::sql(sql))
}
