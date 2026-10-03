# Disposition readers -- the R-only consumers of the disposition dataset.
#
# The disposition dataset is one row per (phone, campaign_id), contacted-only,
# produced upstream (per-campaign Parquet + a phone-sorted read projection). The
# caller screens a fresh sample against it -- "which of these numbers
# have been contacted / completed / refused before?" -- to clean the list. So a
# summary returns ONE ROW PER PHONE: the number's cross-campaign screening flags +
# its latest disposition.
#
# The readers stay bare and in the disposition family (not s160_-prefixed): their
# IO is confined to the private .disposition_read_parquet, and grouping the
# feature beats tagging IO.
#   disposition_summary(x, ...)         summarize per phone; x = a Parquet path OR an in-memory frame
#   disposition_records(dataset, ...)   read the Parquet, raw per-(phone, campaign) rows
#   disposition_screen(sample, ...)     annotate a caller's sample in place
#   disposition_pull(env, ...)          fetch the projection Parquet from GCS
# The pure per-phone rollup core is the private .disposition_rollup(). The Parquet
# read uses duckdb when installed (Suggests) -- a phone-scoped read pushes the
# match into the scan, so a screen loads only the sample's rows -- and nanoparquet
# (zero-dependency, but whole-file) otherwise; see .disposition_engine().

# Columns the summary reads (the Parquet read is projected to just these).
# `refused`/`ineligible` (survey160r 0.51.0) are optional -- a projection produced
# before the split lacks them, and the rollup defaults them to 0 (see below), so an
# old projection reads as all-`terminated` with no refused/ineligible detail.
.DISPOSITION_READ_COLS <- c("phone", "campaign_id", "engaged", "opted_in", "completed",
                   "web_complete", "refused", "ineligible", "terminated", "error",
                   "disposition_date")

# The derived disposition categories, in funnel order (least -> most advanced).
# `never_contacted` is only produced for screened phones absent from the data.
# The terminal band splits `terminated` into `ineligible` (screened out) and
# `refused` (declined), name-derived by survey160r; `terminated` is KEPT for the
# unsplit residual (the DB producer's SQL `status='terminated'` is a superset that
# also covers in-survey screeners whose step name carries no terminal signal, so a
# row can be terminated with neither refused nor ineligible set). Rank within the
# band: terminated (least specific) < ineligible < refused.
.DISPOSITION_CATEGORIES <- c("never_contacted", "non_response", "engaged", "opted_in",
                    "terminated", "ineligible", "refused", "completed", "web_complete")

# Columns of the per-phone summary (also the block appended by _screen()), in
# output order: identity, scope (n_campaigns + the id list), the cumulative status
# COUNTS (n_*: how many of the phone's campaigns set each flag) + n_error, the
# latest (most-recent) and best (furthest-reached) disposition each with its
# campaign, then the first/last disposition_date span. A never-contacted phone is
# marked by n_campaigns == 0 (was ever_contacted = FALSE, removed).
.DISPOSITION_SUMMARY_COLS <- c("phone", "n_campaigns", "campaigns",
                      "n_engaged", "n_opted_in", "n_completed", "n_web_complete",
                      "n_terminated", "n_refused", "n_ineligible", "n_error",
                      "latest_disposition", "latest_campaign_id",
                      "best_disposition", "best_campaign_id",
                      "first_disposition_date", "last_disposition_date")

# The stored disposition schema, in canonical order -- what
# disposition_records() returns. `sent`/`survey_mode`/`error`/`carrier` come from
# disposition_run() (`survey_mode` is the DETECTED mode, matching the latency
# parquet's `survey_mode`; renamed from `mode` in 0.65.0);
# the full `tracker_*` dimension set (loi, topic, mode, registration_id, client,
# project, brand, state, vendor, fielding_location, pricing_structure,
# voter_file_source, n_questions) and `disposition_date` are added by downstream
# enrichment, so an un-enriched projection lacks those and
# records() returns just the subset present. `tracker_registration_id` (added 0.54.0;
# carried the tracker_ prefix since 0.65.0) is a per-campaign Project-Tracker id
# carried through unchanged; `carrier` (0.60.0) is
# the recipient's mobile carrier from the export's optional misc column. Either is
# simply absent from a projection produced before it was added, and records() omits
# a column it does not carry.
.DISPOSITION_RECORD_COLS <- c("phone", "campaign_id", "sent", "engaged",
                      "opted_in", "completed", "web_complete", "refused",
                      "ineligible", "terminated", "error", "carrier", "survey_mode",
                      "tracker_loi", "tracker_topic", "tracker_mode",
                      "tracker_registration_id", "tracker_client", "tracker_project",
                      "tracker_brand", "tracker_state", "tracker_vendor",
                      "tracker_fielding_location", "tracker_pricing_structure",
                      "tracker_voter_file_source", "tracker_n_questions",
                      "disposition_date")

# Phone matching uses the shared .normalize_phone (aaa_utils.R) so a sample
# matches the disposition and opt-out datasets identically.

# Derive one disposition category per row from the 0/1 funnel flags, by funnel
# precedence (later assignment wins). A t2w_external row has completed = NA, so
# it falls through to the last known in-channel step -- never a false completed.
.disposition_derive_category <- function(d) {
  is1 <- function(v) which(v == 1L)     # NA compares drop out: not set
  out <- rep("non_response", nrow(d))   # data is contacted-only (sent == 1)
  out[is1(d$engaged)] <- "engaged"
  out[is1(d$opted_in)] <- "opted_in"
  # Terminal band, in precedence order (later wins, matching .DISPOSITION_CATEGORIES
  # rank): `terminated` is the unsplit residual, then the name-split `ineligible`
  # (screened out) and `refused` (declined) refine it where routing identified the
  # terminal. A row absent `refused`/`ineligible` (old projection -- the rollup
  # defaults them to 0) stays `terminated`. `refused`/`ineligible` can also be set
  # without `terminated` (routing saw the terminal step but the SQL status lagged),
  # so they are assigned independently, not gated on `terminated`.
  out[is1(d$terminated)] <- "terminated"
  out[is1(d$ineligible)] <- "ineligible"
  out[is1(d$refused)] <- "refused"
  out[is1(d$completed)] <- "completed"
  out[is1(d$web_complete)] <- "web_complete"
  out
}

# One all-zero/NA summary row per never-contacted phone (screened but absent).
.disposition_never_contacted <- function(phones) {
  n <- length(phones)
  data.frame(
    phone = phones,
    n_campaigns = rep(0L, n),
    campaigns = rep(NA_character_, n),
    n_engaged = rep(0L, n), n_opted_in = rep(0L, n), n_completed = rep(0L, n),
    n_web_complete = rep(0L, n), n_terminated = rep(0L, n), n_refused = rep(0L, n),
    n_ineligible = rep(0L, n), n_error = rep(0L, n),
    latest_disposition = rep("never_contacted", n),
    latest_campaign_id = rep(NA_character_, n),
    best_disposition = rep("never_contacted", n),
    best_campaign_id = rep(NA_character_, n),
    first_disposition_date = rep(as.Date(NA), n),
    last_disposition_date = rep(as.Date(NA), n),
    stringsAsFactors = FALSE
  )
}

# Coerce one optional date bound to a single Date, rejecting a multi-value or
# unparseable bound. A length > 1 bound would silently recycle in the >=/<=
# comparison below and mis-select rows; NULL passes through untouched.
.disposition_date_bound <- function(x, name) {
  if (is.null(x)) return(NULL)
  d <- tryCatch(as.Date(x), error = function(e) NA)
  if (length(d) != 1L || is.na(d)) {
    stop(sprintf("`%s` must be a single valid date.", name), call. = FALSE)
  }
  d
}

# Normalize a requested phone vector to the deduped, non-NA digit set used to
# scope a read: NULL passes through as "no phone filter"; an all-blank/unparseable
# request collapses to character(0) (matches nothing). Shared by the readers.
.disposition_request_phones <- function(phones) {
  if (is.null(phones)) {
    return(NULL)
  }
  req <- unique(.normalize_phone(phones))
  req[!is.na(req)]
}

# Normalize phone and apply the row-scope filters (requested phones, campaigns,
# disposition_date range). Pure; `data` already has .DISPOSITION_READ_COLS, and
# `date_from`/`date_to` are already coerced to Date (or NULL) by the caller.
# One combined keep-mask, subset once -- avoids the intermediate frame copies a
# filter-per-predicate chain allocates.
.disposition_filter <- function(data, keep_phones, campaign_ids, date_from, date_to) {
  data$phone <- .normalize_phone(data$phone)
  keep <- !is.na(data$phone)
  if (!is.null(keep_phones)) {
    keep <- keep & data$phone %in% keep_phones
  }
  if (!is.null(campaign_ids)) {
    keep <- keep & as.character(data$campaign_id) %in% as.character(campaign_ids)
  }
  # A date bound against an all-NA disposition_date (an un-enriched frame, or one
  # whose dates are all missing) silently drops every row -- warn, don't return empty.
  # A phone-scoped read (.disposition_read_scoped) carries the whole-dataset
  # answer as `all_dates_na`, since its rows are only the matched subset.
  all_dates_na <- function() {
    attr(data, "all_dates_na") %||%
      (nrow(data) > 0L && all(is.na(data$disposition_date)))
  }
  if ((!is.null(date_from) || !is.null(date_to)) && all_dates_na()) {
    warning("`date_from`/`date_to` filter on `disposition_date`, which is NA for ",
            "every row here; the filter returns no rows.", call. = FALSE)
  }
  if (!is.null(date_from)) {
    keep <- keep & !is.na(data$disposition_date) & data$disposition_date >= date_from
  }
  if (!is.null(date_to)) {
    keep <- keep & !is.na(data$disposition_date) & data$disposition_date <= date_to
  }
  # A scoped read usually keeps every row; skip the (multi-second at millions of
  # rows) data-frame copy then.
  if (all(keep)) {
    attr(data, "all_dates_na") <- NULL
    return(data)
  }
  data[keep, , drop = FALSE]
}

# Collapse the (phone, campaign) rows to one row per phone, phones in sorted
# order. Vectorized over one integer group index (no per-phone R calls -- the
# tapply() version re-factored the phone column once per output column and took
# ~11 s on a 200k-phone screen; this takes well under 1 s). `d$phone` is
# digit-normalized (.disposition_filter), so a radix (byte) sort orders it exactly
# as a locale sort would.
.disposition_collapse <- function(d) {
  category <- .disposition_derive_category(d)
  up <- sort(unique(d$phone), method = "radix")
  g <- data.table::chmatch(d$phone, up)
  n_groups <- length(up)
  # Every ordering below sorts by `g` first, so a group's rows are one run and its
  # first/last row is a neighbor comparison (cheaper than duplicated()'s hashing).
  run_start <- function(x) x != c(-1L, x[-length(x)])
  run_end <- function(x) x != c(x[-1L], -1L)
  first_of <- function(o) o[run_start(g[o])]   # first row per group, groups ascending
  # Latest campaign: max disposition_date (NA last), tie -> max campaign_id; any
  # remaining tie keeps input order (radix order is stable).
  dd_num <- as.numeric(d$disposition_date)
  dk <- dd_num
  dk[is.na(dk)] <- -Inf
  cid_num <- as.numeric(d$campaign_id)
  latest <- first_of(order(g, -dk, -cid_num, method = "radix"))
  # Best (furthest-reached) disposition across the phone's campaigns: the highest
  # funnel category any of them hit, ranked by the SAME precedence latest uses
  # (.DISPOSITION_CATEGORIES: non_response < engaged < opted_in < terminated <
  # ineligible < refused < completed < web_complete), tie -> latest date, then
  # max id, matching latest_disposition's tie-break.
  rk <- match(category, .DISPOSITION_CATEGORIES)
  best <- first_of(order(g, -rk, -dk, -cid_num, method = "radix"))
  # Cumulative status counts: how many of the phone's campaigns set each flag
  # (0/1/NA; NA counts as not-set). Overlapping -- a completed campaign is also
  # engaged -- so these are "reached status X", not a partition of n_campaigns.
  # (which() drops the NA comparisons, so NA counts as not-set.)
  count1 <- function(x) tabulate(g[which(x == 1L)], nbins = n_groups)
  # A campaign carries a delivery error when `error` holds a non-blank code.
  has_error <- !is.na(d$error) & nzchar(trimws(as.character(d$error)))
  # Distinct campaigns per phone (an NA id counts once, as unique() does) and
  # their sorted, comma-joined ids (NA dropped, as sort() does). The default
  # order() keeps sort()'s collation for a character id.
  oc <- order(g, d$campaign_id)
  gc <- g[oc]
  cc <- d$campaign_id[oc]
  nxt <- cc[-1L]
  prv <- cc[-length(cc)]
  eq <- nxt == prv
  same <- c(FALSE, gc[-1L] == gc[-length(gc)] &
              ((!is.na(eq) & eq) | (is.na(nxt) & is.na(prv))))
  gc <- gc[!same]
  cc <- cc[!same]
  # Join rank by rank -- one vectorized paste0() per position within a phone's
  # list (at most a few dozen), not one paste() call per phone.
  ids <- !is.na(cc)
  gj <- gc[ids]
  cj <- as.character(cc[ids])
  pos <- seq_along(gj)
  rank <- pos - cummax(pos * run_start(gj)) + 1L
  campaigns <- character(n_groups)
  by_rank <- split(pos, rank)
  for (r in seq_along(by_rank)) {
    at <- by_rank[[r]]
    campaigns[gj[at]] <- if (r == 1L) cj[at] else paste0(campaigns[gj[at]], ",", cj[at])
  }
  # Per-phone min/max disposition_date, NA when the phone has no dated campaign
  # (an un-enriched projection, or every date missing).
  dated <- which(!is.na(dd_num))
  od <- dated[order(g[dated], dd_num[dated], method = "radix")]
  span <- function(pick) {
    v <- rep(NA_real_, n_groups)
    v[g[pick]] <- dd_num[pick]
    as.Date(v, origin = "1970-01-01")
  }
  data.frame(
    phone = up,
    n_campaigns = tabulate(gc, nbins = n_groups),
    campaigns = campaigns,
    n_engaged = count1(d$engaged),
    n_opted_in = count1(d$opted_in),
    n_completed = count1(d$completed),
    n_web_complete = count1(d$web_complete),
    n_terminated = count1(d$terminated),
    n_refused = count1(d$refused),
    n_ineligible = count1(d$ineligible),
    n_error = tabulate(g[has_error], nbins = n_groups),
    latest_disposition = category[latest],
    latest_campaign_id = as.character(d$campaign_id[latest]),
    best_disposition = category[best],
    best_campaign_id = as.character(d$campaign_id[best]),
    first_disposition_date = span(od[run_start(g[od])]),
    last_disposition_date = span(od[run_end(g[od])]),
    stringsAsFactors = FALSE
  )
}

# 1-based page slice over the (phone-ordered) result. NULL page/size -> no-op.
.disposition_paginate <- function(summ, page, page_size) {
  if (is.null(page) && is.null(page_size)) return(summ)
  ps <- if (is.null(page_size)) max(1L, nrow(summ)) else page_size
  pg <- if (is.null(page)) 1L else page
  ok <- function(x) {
    # is.finite() rejects NA/NaN/Inf in one check (an Inf page slipped through the
    # old `x %% 1 == 0`, since `Inf %% 1` is NaN, and errored cryptically).
    is.numeric(x) && length(x) == 1L && is.finite(x) && x >= 1L && x %% 1 == 0
  }
  if (!ok(ps) || !ok(pg)) {
    stop("`page` and `page_size` must be positive integers.", call. = FALSE)
  }
  from <- (pg - 1L) * ps + 1L
  if (from > nrow(summ)) return(summ[0L, , drop = FALSE])
  summ[seq.int(from, min(pg * ps, nrow(summ))), , drop = FALSE]
}

# Validate a projection path (shared by every reader's I/O helper).
.disposition_check_path <- function(dataset) {
  if (!is.character(dataset) || length(dataset) != 1L || !nzchar(dataset)) {
    stop("`dataset` must be a single Parquet path.", call. = FALSE)
  }
  if (!file.exists(dataset)) {
    stop_not_found("disposition dataset", dataset)
  }
}

# Column names + whether DuckDB wrote the file, from ONE footer parse. nanoparquet
# 0.5.x's metadata readers cost time and memory in proportion to the row count
# (~9 s / ~19 GB peak each on the ~140M-row production projection), so the
# readers take both facts from a single read_parquet_metadata() call instead of
# read_parquet_info() + read_parquet_schema().
.disposition_parquet_meta <- function(dataset) {
  m <- nanoparquet::read_parquet_metadata(dataset)
  cb <- m$file_meta_data$created_by
  list(names = m$schema$name,
       duckdb = length(cb) == 1L && !is.na(cb) && grepl("duckdb", cb, ignore.case = TRUE))
}

# I/O: validate the path, read the projection, then (when `columns` is given)
# subset to those columns. `columns` = the summary read set by default; `NULL`
# (disposition_records()) keeps every stored column.
#
# Column-project via nanoparquet's `col_select` ONLY for a writer whose null
# encoding nanoparquet 0.5.1 decodes correctly under `col_select` -- verified for
# DuckDB, which writes the production projection (`disposition_all.parquet`). For
# any other writer read in full and subset in R: nanoparquet 0.5.1 MISREADS NA
# integers under `col_select` on its OWN writes -- returning uninitialized memory
# (0 / 1 / garbage, nondeterministic) instead of NA, which silently corrupts a
# projected read of e.g. `completed` (NA on t2w_external rows). `col_select` is a
# real memory win on the ~38M-row projection (~3.3 vs ~5.5 GB); the full read is
# the correctness fallback for fixtures / unknown writers. The intersect keeps a
# column-short/legacy projection returning only what is present, so the rollup's
# own missing-column guards still fire. Drop the branch once nanoparquet fixes the
# NA decode.
.disposition_read_parquet <- function(dataset, columns = .DISPOSITION_READ_COLS) {
  .disposition_check_path(dataset)
  if (.disposition_use_duckdb(dataset)) {
    d <- .disposition_read_duckdb(dataset, columns = columns)
    if (!is.null(d)) return(d)
  }
  meta <- .disposition_parquet_meta(dataset)
  if (meta$duckdb && !is.null(columns)) {
    cols <- intersect(columns, meta$names)
    return(as.data.frame(nanoparquet::read_parquet(dataset, col_select = cols)))
  }
  d <- as.data.frame(nanoparquet::read_parquet(dataset))
  if (!is.null(columns)) {
    d <- d[, intersect(columns, names(d)), drop = FALSE]
  }
  d
}

# Which engine the disposition reads use. "duckdb" pushes a phone match into the
# Parquet scan, so only the matching rows ever reach R: on the production
# projection (~140M rows, ~77M distinct phones) a screen drops from ~26 GB peak /
# minutes to well under 1 GB / seconds -- the full phone column is never
# materialized as R strings, and nanoparquet (whose every read of that file peaks
# near 20 GB, see .disposition_parquet_meta) is not touched at all. It is used
# whenever duckdb is installed (Suggests); "nanoparquet" is the zero-dependency
# fallback. Override with
# options(survey160r.disposition_engine = "auto" | "duckdb" | "nanoparquet").
.disposition_engine <- function() {
  engine <- getOption("survey160r.disposition_engine", "auto")
  choices <- c("auto", "duckdb", "nanoparquet")
  if (!is.character(engine) || length(engine) != 1L || !engine %in% choices) {
    stop_s160(sprintf("option `survey160r.disposition_engine` must be one of: %s",
                      paste(choices, collapse = ", ")))
  }
  if (engine == "nanoparquet") return(engine)
  has_duckdb <- requireNamespace("duckdb", quietly = TRUE) &&
    requireNamespace("DBI", quietly = TRUE)
  if (has_duckdb) return("duckdb")
  if (engine == "duckdb") {
    stop_s160(paste("option `survey160r.disposition_engine` is \"duckdb\" but the",
                    "duckdb package is not installed: install.packages(\"duckdb\")"))
  }
  "nanoparquet"
}

# Whether to read `dataset` with duckdb: the engine says so and the path has no
# glob metacharacter. DuckDB's read_parquet() expands `*`, `?`, and `[...]`, so a
# literal file named e.g. "b*.parquet" would silently pull in its neighbours too;
# such a path takes the literal-path nanoparquet read.
.disposition_use_duckdb <- function(dataset) {
  .disposition_engine() == "duckdb" && !grepl("[*?[]", dataset)
}

# I/O: read only the rows whose digit-normalized phone is in `phones` (already
# normalized + deduped by .disposition_request_phones). Returns the projected
# columns (`columns` intersected with the file's schema, in `columns` order) with
# `phone` digit-normalized -- the rows .disposition_read_parquet() followed by
# .disposition_filter()'s phone match would keep, without ever holding the whole
# projection in memory. A file with no `phone` column yields a zero-row frame of
# the columns it has, so the caller's missing-column check reports it.
#
# The date filters' all-NA warning (.disposition_filter) is about the WHOLE
# dataset, which this frame no longer is; with `check_dates` the whole-file answer
# rides along as the `all_dates_na` attribute so the warning fires exactly as on a
# full read.
.disposition_read_scoped <- function(dataset, phones,
                                     columns = .DISPOSITION_READ_COLS,
                                     check_dates = FALSE) {
  .disposition_check_path(dataset)
  d <- if (.disposition_use_duckdb(dataset)) {
    .disposition_read_duckdb(dataset, columns, phones, check_dates)
  }
  d %||% .disposition_read_nanoparquet(dataset, phones, columns, check_dates)
}

# nanoparquet fallback for .disposition_read_scoped(): the plain projected read,
# then the phone match in R. (A two-phase read -- `phone` alone, then the other
# columns -- does not lower the peak: nanoparquet's own per-read overhead on the
# production projection, ~20 GB, dominates either way.) On a large file it points
# the caller at duckdb, once per session, since only that engine is low-memory.
.disposition_read_nanoparquet <- function(dataset, phones, columns, check_dates) {
  if (file.size(dataset) > 1e8 && !requireNamespace("duckdb", quietly = TRUE)) {
    rlang::inform(
      c(paste("Reading the disposition projection without duckdb loads all of it",
              "into memory (tens of GB for the full production projection)."),
        i = "install.packages(\"duckdb\") for a low-memory, much faster screen."),
      .frequency = "once", .frequency_id = "survey160r_disposition_duckdb")
  }
  d <- .disposition_read_parquet(dataset, columns = columns)
  if (!"phone" %in% names(d)) return(d[0L, , drop = FALSE])
  all_na <- check_dates && "disposition_date" %in% names(d) && nrow(d) > 0L &&
    all(is.na(d$disposition_date))
  d$phone <- .normalize_phone(d$phone)
  d <- d[!is.na(d$phone) & d$phone %in% phones, , drop = FALSE]
  rownames(d) <- NULL
  .disposition_tag_dates(d, check_dates, all_na)
}

# DuckDB engine, for both reads. The schema comes from DuckDB's own footer read
# (no nanoparquet call) and column types come back as nanoparquet would give them
# (a DATE as an integer-backed Date). With `phones` NULL it is the plain projected
# read behind .disposition_read_parquet() (`columns` NULL = every column) --
# about half nanoparquet's peak memory and several times faster on the
# production projection, and immune to nanoparquet's col_select NA bug. With
# `phones` it is .disposition_read_scoped(): the SQL normalization mirrors
# .normalize_phone exactly (strip non-digits; blank -> NULL; an 11-digit number
# with a leading 1 drops it) and the match is a semi-join against the registered
# request vector, so DuckDB streams the scan and hands R only the matched rows. A
# scoped read returns NULL for a non-string stored phone (a numeric fixture): R's
# as.character() formatting, which .normalize_phone relies on, has no exact SQL
# twin, so the caller falls back to the in-R match. Rows come back in file order:
# a plain scan keeps it (DuckDB's default preserve_insertion_order -- do not turn
# it off) and the scoped query sorts by file row number.
.disposition_read_duckdb <- function(dataset, columns = NULL, phones = NULL,
                                     check_dates = FALSE) {
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  src <- paste0("read_parquet(", DBI::dbQuoteString(con, dataset), ")")
  schema <- DBI::dbGetQuery(con, paste("DESCRIBE SELECT * FROM", src))
  cols <- intersect(columns %||% schema$column_name, schema$column_name)
  if (length(cols) == 0L) {
    # None of the wanted columns: a zero-column frame (SQL has no empty SELECT
    # list), so the caller's missing-column check reports it as on nanoparquet.
    n <- DBI::dbGetQuery(con, paste("SELECT count(*) AS n FROM", src))$n
    return(data.frame(row.names = seq_len(n)))
  }
  # DuckDB hands these column types to R exactly as nanoparquet does (DATE after
  # the integer re-backing below). Anything else -- a TIMESTAMP (tzone differs),
  # or a file whose ARROW:schema metadata lets nanoparquet restore richer R types
  # (a factor) -- returns NULL so the caller reads with nanoparquet instead. The
  # production projection (DuckDB-written, plain types) always stays here.
  plain <- c("VARCHAR", "INTEGER", "BIGINT", "DOUBLE", "FLOAT", "BOOLEAN", "DATE")
  if (!all(schema$column_type[match(cols, schema$column_name)] %in% plain) ||
        .disposition_arrow_annotated(con, dataset)) {
    return(NULL)
  }
  quoted <- as.character(DBI::dbQuoteIdentifier(con, cols))
  select <- function(sql) {
    d <- DBI::dbGetQuery(con, sql)
    for (col in names(d)) {
      if (inherits(d[[col]], "Date")) {
        d[[col]] <- structure(as.integer(unclass(d[[col]])), class = "Date")
      }
    }
    d
  }
  if (is.null(phones)) {
    return(select(paste("SELECT", paste(quoted, collapse = ", "), "FROM", src)))
  }
  if (!"phone" %in% cols) {
    return(select(paste("SELECT", paste(quoted, collapse = ", "), "FROM", src, "LIMIT 0")))
  }
  if (!identical(schema$column_type[match("phone", schema$column_name)], "VARCHAR")) {
    return(NULL)
  }
  duckdb::duckdb_register(con, "s160_req",
                          data.frame(phone = phones, stringsAsFactors = FALSE))
  # Only the wanted columns are selected at every level (never `*`), so a file
  # column that happens to share the helper name s160_digits cannot shadow it.
  inner <- paste(c(quoted, "regexp_replace(phone, '[^0-9]', '', 'g') AS s160_digits",
                   "file_row_number AS s160_row"), collapse = ", ")
  outer <- quoted
  outer[cols == "phone"] <- paste(
    "CASE WHEN s160_digits = '' THEN NULL",
    "WHEN length(s160_digits) = 11 AND starts_with(s160_digits, '1')",
    "THEN substr(s160_digits, 2) ELSE s160_digits END AS phone")
  # A parallel semi-join does NOT keep file order (unlike a plain scan), so sort
  # the matched rows back into it by file row number: the rollup breaks a full
  # tie (same phone, date, and campaign id) by input order, as on nanoparquet.
  numbered <- paste0("read_parquet(", DBI::dbQuoteString(con, dataset),
                     ", file_row_number = true)")
  d <- select(paste0(
    "SELECT * EXCLUDE (s160_row) FROM (SELECT ", paste(c(outer, "s160_row"), collapse = ", "),
    " FROM (SELECT ", inner, " FROM ", numbered, ")) ",
    "WHERE phone IN (SELECT phone FROM s160_req) ORDER BY s160_row"))
  all_na <- check_dates && "disposition_date" %in% cols &&
    DBI::dbGetQuery(con, paste0("SELECT count(*) > 0 AND count(disposition_date) = 0",
                                " AS all_na FROM ", src))$all_na
  .disposition_tag_dates(d, check_dates, all_na)
}

# Whether the file carries ARROW:schema metadata (nanoparquet and arrow write
# it). nanoparquet restores R types from it -- a factor, a time with a tzone --
# that DuckDB does not, so such a file reads with nanoparquet to keep the exact
# types. DuckDB-written files (production) carry none: one cheap footer query.
.disposition_arrow_annotated <- function(con, dataset) {
  keys <- DBI::dbGetQuery(con, paste0(
    "SELECT decode(key) AS key FROM parquet_kv_metadata(",
    DBI::dbQuoteString(con, dataset), ")"))$key
  "ARROW:schema" %in% keys
}

# Attach the whole-dataset all-NA-date answer for .disposition_filter's warning.
.disposition_tag_dates <- function(d, check_dates, all_na) {
  if (check_dates) attr(d, "all_dates_na") <- isTRUE(all_na)
  d
}

# Pure per-phone rollup core, shared by disposition_summary() (public; path or
# frame) and disposition_screen(). Takes an in-memory disposition frame (one row
# per (phone, campaign_id)) and returns one row per phone. `fn` names the public
# caller so a validation error points at it. Callers always pass a data frame, so
# there is no is.data.frame() guard here.
.disposition_rollup <- function(data, phones = NULL, campaign_ids = NULL,
                                statuses = NULL, date_from = NULL, date_to = NULL,
                                page = NULL, page_size = NULL, fn) {
  # disposition_date is optional -- it only orders each phone's latest campaign and
  # backs the date filters -- so an un-enriched disposition_records() frame that
  # omits it still summarizes (mirroring disposition_records(), which tolerates
  # its absence too). The funnel-flag columns are always required.
  missing_cols <- setdiff(setdiff(.DISPOSITION_READ_COLS,
                                  c("disposition_date", "error",
                                    "refused", "ineligible")),
                          names(data))
  if (length(missing_cols) > 0L) {
    stop_s160(sprintf("input is missing required column(s): %s",
                      paste(missing_cols, collapse = ", ")),
              fn = fn)
  }
  if (!"disposition_date" %in% names(data)) {
    if (!is.null(date_from) || !is.null(date_to)) {
      stop_s160("input has no `disposition_date` column to filter on.", fn = fn)
    }
    data$disposition_date <- rep(as.Date(NA), nrow(data))
  }
  # `error` is optional too (an un-enriched frame lacks it) -> n_error is 0.
  if (!"error" %in% names(data)) {
    data$error <- rep(NA_character_, nrow(data))
  }
  # `refused`/`ineligible` (the terminal split) are optional: a projection produced
  # before survey160r 0.51.0 lacks them -> default 0, so those rows summarize as
  # plain `terminated` (n_refused / n_ineligible are 0 and the category never
  # refines past the residual). New projections carry them and the split appears.
  if (!"refused" %in% names(data)) {
    data$refused <- rep(0L, nrow(data))
  }
  if (!"ineligible" %in% names(data)) {
    data$ineligible <- rep(0L, nrow(data))
  }
  if (!is.null(statuses)) {
    bad <- setdiff(as.character(statuses), .DISPOSITION_CATEGORIES)
    if (length(bad) > 0L) {
      stop_s160(sprintf("unknown status(es): %s",
                        paste(bad, collapse = ", ")),
                fn = fn)
    }
  }
  date_from <- .disposition_date_bound(date_from, "date_from")
  date_to <- .disposition_date_bound(date_to, "date_to")
  req <- .disposition_request_phones(phones)

  d <- .disposition_filter(data, req, campaign_ids, date_from, date_to)
  summ <- if (nrow(d) == 0L) .disposition_never_contacted(character(0)) else .disposition_collapse(d)

  # Screened phones absent from the (filtered) data come back as never-contacted.
  if (!is.null(req)) {
    missing <- setdiff(req, summ$phone)
    if (length(missing) > 0L) summ <- rbind(summ, .disposition_never_contacted(missing))
  }
  if (!is.null(statuses)) {
    summ <- summ[summ$latest_disposition %in% as.character(statuses), ,
                 drop = FALSE]
  }
  # Phones are digit strings here, so a radix (byte) sort orders them exactly as
  # the locale sort would, in a fraction of the time on a large sample.
  summ <- summ[order(summ$phone, method = "radix"), , drop = FALSE]
  summ <- .disposition_paginate(summ, page, page_size)
  rownames(summ) <- NULL
  summ
}

#' Summarize the disposition dataset for a phone list (one row per phone)
#'
#' Rolls the disposition data up to \strong{one row per phone} -- each number's
#' cross-campaign status counts, date span, and latest/best disposition. Pass either the
#' projection \strong{path} (read it, then summarize) or an \strong{in-memory
#' frame} already read with \code{\link{disposition_records}} (summarize it
#' directly, no I/O), which lets you read once and summarize several phone
#' lists. For cleaning a sample file in place, use
#' \code{\link{disposition_screen}}; for the raw rows behind the rollup (one per
#' \code{(phone, campaign_id)}), use \code{\link{disposition_records}}.
#'
#' @param x Either a path to a disposition Parquet file (the phone-sorted read
#'   projection, e.g. from \code{\link{disposition_pull}}) or an in-memory
#'   disposition data frame (from \code{\link{disposition_records}}). A path is
#'   read projected to the summary columns (only the \code{phones} rows when
#'   given and \pkg{duckdb} is installed; see \code{\link{disposition_screen}},
#'   Memory); a frame must
#'   carry \code{phone}, \code{campaign_id}, \code{engaged}, \code{opted_in},
#'   \code{completed}, \code{web_complete}, and \code{terminated}.
#'   \code{refused} and \code{ineligible} (the terminal split) are optional --
#'   absent (a pre-0.51.0 projection) they default to 0, so terminals summarize as
#'   plain \code{terminated}; present, they split it (see \code{statuses} / Value).
#'   \code{disposition_date} is optional -- it orders each phone's latest campaign
#'   and backs the \code{date_from}/\code{date_to} filters; an un-enriched
#'   \code{\link{disposition_records}} frame that omits it still summarizes
#'   (disposition dates treated as unknown), but a date bound then errors.
#' @param phones Optional character vector of phone numbers to screen. When
#'   supplied, \strong{every} input number is returned -- never-contacted ones
#'   with \code{n_campaigns = 0} and
#'   \code{latest_disposition = "never_contacted"}. \code{NULL} (default)
#'   summarizes every phone present. Matched digit-normalized (a leading US
#'   \code{1} is dropped so 11-digit numbers match 10-digit ones).
#' @param campaign_ids Optional vector; restrict the underlying rows to these
#'   campaigns before summarizing.
#' @param statuses Optional subset of the derived disposition categories
#'   (\code{never_contacted}, \code{non_response}, \code{engaged},
#'   \code{opted_in}, \code{terminated}, \code{ineligible}, \code{refused},
#'   \code{completed}, \code{web_complete}); keep only phones whose
#'   \code{latest_disposition} is one of them. \code{ineligible} (screened out) and
#'   \code{refused} (declined) are the split of \code{terminated}; \code{terminated}
#'   now denotes only the unsplit residual (a terminal the routing name-match could
#'   not classify), so screen on all three to catch every hard stop.
#' @param date_from,date_to Optional \code{Date}/date-string bounds on
#'   \code{disposition_date}. A row whose \code{disposition_date} is \code{NA} is
#'   dropped by any bound (an all-\code{NA} column drops every row, with a
#'   warning). A projection with \strong{no} \code{disposition_date} column is a
#'   different case: setting a bound then \strong{errors} (see \code{x}); it is
#'   not a silent drop.
#' @param page,page_size Optional 1-based pagination over the per-phone result.
#' @return A data frame, one row per phone, columns in this order:
#'   \code{phone}; \code{n_campaigns} and \code{campaigns} (how many campaigns,
#'   and the comma-separated id list); the cumulative status counts
#'   \code{n_engaged}, \code{n_opted_in}, \code{n_completed},
#'   \code{n_web_complete}, \code{n_terminated} (campaigns the producer flagged as
#'   a hard stop), then \code{n_refused} and \code{n_ineligible} (the
#'   name-classified terminals; each \code{0} on a
#'   pre-0.51.0 projection; they need not sum to \code{n_terminated}, since a
#'   downstream projection can set \code{terminated} independently of the split --
#'   less when a terminal was unsplit, more when the split flagged a terminal the
#'   status had not) -- each \code{0} when the phone
#'   never reached that status and \code{> 0} the number of the phone's campaigns
#'   that did (they overlap: a completed campaign is also engaged) -- plus
#'   \code{n_error} (how many campaigns carried a carrier delivery-error code);
#'   \code{latest_disposition} + \code{latest_campaign_id} (the category of the
#'   phone's most-recent campaign and that campaign's id -- now one of
#'   \code{refused} / \code{ineligible} / \code{terminated} for a hard stop);
#'   \code{best_disposition}
#'   + \code{best_campaign_id} (the furthest-reached category across all the
#'   phone's campaigns -- ranked by the same funnel precedence, so \code{completed}
#'   / \code{web_complete} rank highest and the terminal band
#'   (\code{terminated} < \code{ineligible} < \code{refused}) above
#'   \code{opted_in} -- and the campaign that reached it); and
#'   \code{first_disposition_date} / \code{last_disposition_date} (earliest and
#'   latest \code{disposition_date} across the phone's campaigns, \code{NA} when
#'   none is dated). A never-contacted phone has \code{n_campaigns = 0} and
#'   \code{campaigns = NA}; a contacted phone whose campaign ids are all
#'   \code{NA} has \code{campaigns = ""}. Campaign ids are returned as character.
#'
#'   The counts assume the dataset's grain -- one row per \code{(phone,
#'   campaign_id)} -- and count rows: if two stored rows normalize to the same
#'   phone in the same campaign (e.g. \code{"5551234567"} and
#'   \code{"1 555 123 4567"}), each counts, so an \code{n_*} count can exceed
#'   \code{n_campaigns}. The "max campaign id" tie-break expects numeric ids (as
#'   the projection stores them); non-numeric ids tie-break by row order.
#' @seealso \code{\link{disposition_screen}}, \code{\link{disposition_records}},
#'   \code{\link{disposition_pull}}
#' @examples
#' # On an in-memory frame (no I/O), so this runs:
#' records <- data.frame(
#'   phone = c("5551234567", "5551234567", "5559876543"),
#'   campaign_id = c(101L, 102L, 101L),
#'   engaged = c(1L, 1L, 0L),
#'   opted_in = c(1L, 0L, 0L),
#'   completed = c(1L, 0L, 0L),
#'   web_complete = c(0L, 0L, 0L),
#'   terminated = c(0L, 1L, 0L),
#'   disposition_date = as.Date(c("2026-01-10", "2026-01-20", "2026-01-15")),
#'   stringsAsFactors = FALSE
#' )
#' disposition_summary(records, phones = c("5551234567", "5550000000"))
#' \dontrun{
#' # Or straight from the projection on disk:
#' dataset <- disposition_pull()
#' disposition_summary(dataset, phones = my_sample$phone)
#' }
#' @export
disposition_summary <- function(x, phones = NULL, campaign_ids = NULL,
                                statuses = NULL, date_from = NULL,
                                date_to = NULL, page = NULL, page_size = NULL) {
  data <- if (is.data.frame(x)) {
    x
  } else if (is.character(x) && length(x) == 1L && nzchar(x)) {
    # A phone list scopes the read itself, so only those phones' rows load.
    if (is.null(phones)) {
      .disposition_read_parquet(x)
    } else {
      .disposition_read_scoped(x, .disposition_request_phones(phones),
                               check_dates = !is.null(date_from) || !is.null(date_to))
    }
  } else {
    stop_s160(paste("`x` must be a disposition Parquet path (a single string)",
                    "or an in-memory disposition data frame."),
              fn = "disposition_summary")
  }
  .disposition_rollup(data, phones = phones, campaign_ids = campaign_ids,
                      statuses = statuses, date_from = date_from,
                      date_to = date_to, page = page, page_size = page_size,
                      fn = "disposition_summary")
}

#' Read the raw disposition records (one row per phone + campaign)
#'
#' Reads the disposition Parquet projection and returns its rows \strong{as
#' stored} -- one row per \code{(phone, campaign_id)}, carrying the full
#' disposition schema: \code{phone}, \code{campaign_id}, \code{sent},
#' \code{engaged}, \code{opted_in}, \code{completed}, \code{web_complete},
#' \code{refused}, \code{ineligible}, \code{terminated}, \code{error},
#' \code{carrier}, \code{survey_mode}, \code{tracker_loi}, \code{tracker_topic},
#' \code{tracker_mode}, \code{tracker_registration_id}, \code{tracker_client},
#' \code{tracker_project}, \code{tracker_brand}, \code{tracker_state},
#' \code{tracker_vendor}, \code{tracker_fielding_location},
#' \code{tracker_pricing_structure}, \code{tracker_voter_file_source},
#' \code{tracker_n_questions}, \code{disposition_date}.
#' This is the level directly beneath
#' \code{\link{disposition_summary}}: where \code{summary} rolls every phone up to a
#' single screening row, \code{records} hands back the raw per-campaign rows --
#' for inspection, export, or a custom rollup.
#'
#' Only the canonical columns \emph{present in the file} are returned, in the
#' order above -- a legacy or minimal projection that lacks a column (e.g.
#' \code{error}, \code{tracker_loi}, \code{tracker_topic}, or \code{disposition_date}) omits it,
#' rather than filling an all-\code{NA} column. A projection written straight from
#' \code{\link{disposition_run}} carries the funnel flags (including the
#' \code{refused} / \code{ineligible} terminal split as of 0.51.0; a pre-0.51.0
#' projection omits those two) plus \code{survey_mode} (the DETECTED mode),
#' \code{error} (the carrier delivery-error code), \code{carrier} (the recipient's
#' mobile carrier, from the export's optional \code{misc} column), and
#' \code{disposition_date}
#' (\code{max(scriptDate)}); the \code{tracker_*} dimensions (loi, topic, mode --
#' the DECLARED Tracker mode -- registration_id, client, project, brand, state,
#' vendor, fielding_location, pricing_structure, voter_file_source, n_questions)
#' are added by the tracker enrichment, so only the enriched projection carries
#' all 27.
#' \code{disposition_date} is \code{NA} for a row with no send; \code{error} is
#' \code{NA} when the export carries no usable code (a clean send, or an export
#' lacking the column). The
#' \code{phone} is digit-normalized for matching, and a stored row whose phone is
#' blank or unparseable is dropped. With \pkg{duckdb} installed and \code{phones}
#' given, only those phones' rows are read (see \code{\link{disposition_screen}},
#' Memory); otherwise the whole projection is read into memory and filtered in R.
#'
#' Two differences from the per-phone rollup follow from the raw
#' grain: a screened phone that was never contacted has \strong{no} row here
#' (there is no stored record to return, unlike the \code{never_contacted} row
#' \code{summary} synthesises), and there is no \code{statuses} argument -- that
#' selects a per-phone \code{latest_disposition}, which exists only after the
#' rollup.
#'
#' @param dataset Path to a disposition Parquet file (the read projection), e.g.
#'   from \code{\link{disposition_pull}}.
#' @param phones Optional character vector of phone numbers to keep. Matched
#'   digit-normalized (a leading US \code{1} is dropped so 11-digit numbers match
#'   10-digit ones). \code{NULL} (default) returns every row.
#' @param campaign_ids Optional vector; keep only rows for these campaigns.
#' @param date_from,date_to Optional \code{Date}/date-string bounds on
#'   \code{disposition_date}. A row with an \code{NA} disposition date is dropped
#'   by any bound. Supplying a bound when the projection has no
#'   \code{disposition_date} column at all is an error.
#' @param page,page_size Optional 1-based pagination over the
#'   \code{(phone, campaign_id)}-ordered rows.
#' @return A data frame, one row per \code{(phone, campaign_id)}, with the
#'   canonical disposition columns present in the file (see Details), ordered by
#'   \code{phone} then \code{campaign_id}.
#' @seealso \code{\link{disposition_summary}} (the per-phone rollup),
#'   \code{\link{disposition_screen}}, \code{\link{disposition_pull}}
#' @examples
#' \dontrun{
#' dataset <- disposition_pull()
#' disposition_records(dataset, phones = my_sample$phone)
#' }
#' @export
disposition_records <- function(dataset, phones = NULL, campaign_ids = NULL,
                                date_from = NULL, date_to = NULL,
                                page = NULL, page_size = NULL) {
  date_from <- .disposition_date_bound(date_from, "date_from")
  date_to <- .disposition_date_bound(date_to, "date_to")
  req <- .disposition_request_phones(phones)

  raw <- if (is.null(req)) {
    .disposition_read_parquet(dataset, columns = .DISPOSITION_RECORD_COLS)
  } else {
    .disposition_read_scoped(dataset, req, columns = .DISPOSITION_RECORD_COLS,
                             check_dates = !is.null(date_from) || !is.null(date_to))
  }
  missing_cols <- setdiff(c("phone", "campaign_id"), names(raw))
  if (length(missing_cols) > 0L) {
    stop_s160(sprintf("`dataset` is missing required column(s): %s",
                      paste(missing_cols, collapse = ", ")),
              fn = "disposition_records")
  }
  if ((!is.null(date_from) || !is.null(date_to)) &&
        !"disposition_date" %in% names(raw)) {
    stop_s160("`dataset` has no `disposition_date` column to filter on.",
              fn = "disposition_records")
  }

  d <- .disposition_filter(raw, req, campaign_ids, date_from, date_to)
  cols <- intersect(.DISPOSITION_RECORD_COLS, names(d))
  # radix keeps the order locale-independent (byte order on the digit strings).
  d <- d[order(d$phone, as.numeric(d$campaign_id), method = "radix"), cols, drop = FALSE]
  d <- .disposition_paginate(d, page, page_size)
  rownames(d) <- NULL
  d
}

#' Screen a sample against the disposition dataset (annotate in place)
#'
#' Takes a sample data frame (a phone column plus
#' whatever else -- strata, quota cells, ...) and returns it \strong{unchanged
#' with the disposition summary columns appended}, 1:1 with the input rows and
#' preserving the original phone formatting. The caller then filters and writes
#' it (dropping the numbers already completed or refused). Mirrors the
#' "append columns to my uploaded list" screening workflow.
#'
#' @param sample A data frame with a phone-number column.
#' @param dataset Path to a disposition Parquet file (the read projection).
#' @param phone_col Name of the phone column in \code{sample}
#'   (default \code{"phone"}).
#' @param campaign_ids,date_from,date_to Optional scoping of the disposition
#'   rows considered (see \code{\link{disposition_summary}}). No \code{statuses}
#'   or pagination here -- every sample row is returned.
#' @return \code{sample} with the \code{\link{disposition_summary}} columns
#'   appended (see there for their meaning and order): \code{n_campaigns},
#'   \code{campaigns}, \code{n_engaged}, \code{n_opted_in}, \code{n_completed},
#'   \code{n_web_complete}, \code{n_terminated}, \code{n_refused},
#'   \code{n_ineligible}, \code{n_error},
#'   \code{latest_disposition}, \code{latest_campaign_id}, \code{best_disposition},
#'   \code{best_campaign_id}, \code{first_disposition_date},
#'   \code{last_disposition_date}. A valid phone that is absent from the rows
#'   selected by \code{campaign_ids}, \code{date_from}, and \code{date_to} (the
#'   whole dataset when those are unset) gets a \code{never_contacted} row
#'   (\code{n_campaigns = 0}, the \code{n_*} counts \code{0}, the dates \code{NA},
#'   \code{latest_disposition = "never_contacted"}, \code{campaigns = NA}); only a
#'   phone that digit-normalizes to nothing (blank/unparseable) gets an
#'   all-\code{NA} block.
#' @section Memory:
#' With the \pkg{duckdb} package installed (Suggests; used automatically), the
#' sample's phone match runs inside the Parquet scan, so only the sample's rows
#' are read into R: screening the full production projection (over a hundred
#' million rows) takes seconds and well under 1 GB of RAM. Without it, the
#' projection is read whole with \pkg{nanoparquet}, which needs tens of GB.
#' \code{options(survey160r.disposition_engine = "nanoparquet")} forces the
#' fallback (\code{"duckdb"} requires it; the default \code{"auto"} picks duckdb
#' when installed). The result is identical either way.
#' @seealso \code{\link{disposition_summary}}, \code{\link{disposition_records}},
#'   \code{\link{opt_out_screen}}
#' @examples
#' \dontrun{
#' dataset <- disposition_pull()
#' cleaned <- disposition_screen(my_sample, dataset, phone_col = "phone")
#' # drop finished (completed/web) and every hard stop (refused / ineligible /
#' # terminated); blank-phone rows come back all-NA and are kept
#' subset(cleaned, !((n_completed > 0 | n_web_complete > 0) %in% TRUE |
#'   (n_terminated > 0 | n_refused > 0 | n_ineligible > 0) %in% TRUE))
#' }
#' @export
disposition_screen <- function(sample, dataset, phone_col = "phone",
                                    campaign_ids = NULL, date_from = NULL,
                                    date_to = NULL) {
  check_data_frame(sample, "sample", fn = "disposition_screen")
  if (!is.character(phone_col) || length(phone_col) != 1L ||
        !phone_col %in% names(sample)) {
    stop_s160(sprintf("phone column %s not found in `sample`.",
                      deparse(phone_col)), fn = "disposition_screen")
  }
  disposition_cols <- setdiff(.DISPOSITION_SUMMARY_COLS, "phone")
  clash <- intersect(disposition_cols, names(sample))
  if (length(clash) > 0L) {
    stop_s160(sprintf(paste0("`sample` already has ",
                             "disposition column(s) [%s]; rename them first."),
                      paste(clash, collapse = ", ")),
              fn = "disposition_screen")
  }
  # Normalize the sample once: the deduped set scopes the read (only the sample's
  # rows ever load) and the rollup, and the full vector maps rows back 1:1.
  norm <- .normalize_phone(sample[[phone_col]])
  req <- unique(norm[!is.na(norm)])
  data <- .disposition_read_scoped(dataset, req,
                                   check_dates = !is.null(date_from) || !is.null(date_to))
  summ <- .disposition_rollup(data, phones = req, campaign_ids = campaign_ids,
                              date_from = date_from, date_to = date_to,
                              fn = "disposition_screen")
  idx <- match(norm, summ$phone)
  for (col in disposition_cols) sample[[col]] <- summ[[col]][idx]
  sample
}

#' Download the disposition projection from GCS
#'
#' Pulls the phone-sorted disposition projection
#' (\code{disposition_all.parquet}) from the environment's
#' disposition bucket to a local file and returns the path -- ready to hand to
#' \code{\link{disposition_summary}} / \code{\link{disposition_screen}}. Downloaded
#' once and reused from the local cache on later calls (pass \code{refresh = TRUE}
#' to force a fresh download). This is the one \code{disposition_*} function that
#' reaches GCS: authenticate first with \code{\link{s160_gcs_init}}()
#' so the session's GCS credentials are set. A download without an initialized
#' session errors with \dQuote{GCS not initialized. Run s160_gcs_init() first.}
#' (a cache hit is served without needing auth).
#'
#' @param env Environment: \code{"prod"} (default) or \code{"dev"}. There is no
#'   staging disposition tier; passing \code{env = "staging"} errors clearly.
#' @param dest Where to save. \code{NULL} (default) caches under
#'   \code{tools::R_user_dir("survey160r", "cache")}. A directory saves the
#'   default filename (\code{<bucket>.parquet}) inside it; any other single
#'   string is treated as the exact output path (its parent is created).
#' @param bucket \strong{Deprecated.} Select data with \code{env =} instead; a
#'   supplied bucket is honored with a warning for back-compat.
#' @param refresh When \code{FALSE} (default), reuse an existing local copy;
#'   \code{TRUE} always re-downloads (the projection is rebuilt each pipeline
#'   pass, so refresh to pick up a newer one).
#' @param progress Show a download progress bar. Defaults to
#'   \code{interactive()}: a live bar in an interactive session, silent in batch
#'   or scheduled runs. The projection is a few hundred MB, so an interactive
#'   pull otherwise looks stalled while it transfers.
#' @return The local path to the downloaded Parquet (a single string).
#' @seealso \code{\link{disposition_summary}}, \code{\link{disposition_screen}},
#'   \code{\link{opt_out_pull}}, \code{\link{s160_gcs_init}}
#' @examples
#' \dontrun{
#' s160_gcs_init()   # one-time browser OAuth
#' dataset <- disposition_pull()                      # download (cached)
#' disposition_screen(my_sample, dataset)
#' }
#' @export
disposition_pull <- function(env = .ENV_CHOICES, dest = NULL,
                             bucket = NULL, refresh = FALSE,
                             progress = interactive()) {
  env <- match.arg(env)
  loc <- .locate("disposition", env, bucket, "disposition_pull")
  .gcs_pull_cached(
    fn = "disposition_pull", dest = dest, bucket = loc$bucket,
    refresh = refresh, progress = progress,
    object_name = loc$object,
    cache_suffix = ".parquet", noun = "disposition projection")
}
