# Disposition readers -- the R-only consumers of the disposition dataset.
#
# The disposition dataset is one row per (phone, campaign_id), contacted-only,
# produced upstream (per-campaign Parquet + a phone-sorted read projection). The
# caller screens a fresh sample against it -- "which of these numbers
# have been contacted / completed / refused before?" -- to clean the list. So a
# summary returns ONE ROW PER PHONE: the number's cross-campaign screening flags +
# its latest disposition.
#
# Split into a pure core plus thin IO readers. The readers stay bare and in
# the disposition family (not s160_-prefixed): their IO is confined to the
# private .disposition_read_parquet, and grouping the feature beats tagging IO.
#   disposition_rollup(data, ...)      PURE  -- roll an in-memory frame up
#   disposition_summary(dataset, ...)     IO    -- read the Parquet, summarize (per phone)
#   disposition_records(dataset, ...)   IO    -- read the Parquet, raw per-(phone, campaign) rows
#   disposition_screen(sample, ...)     IO    -- annotate a caller's sample
#   disposition_pull(env, ...)          IO    -- fetch the projection Parquet from GCS
# The Parquet read uses nanoparquet (tiny, zero-dependency); the projection is a
# single consolidated file, so a full read + in-R filter is sub-second.

# Columns the summary reads (the Parquet read is projected to just these).
.DISPOSITION_READ_COLS <- c("phone", "campaign_id", "engaged", "opt_in", "complete",
                   "web_complete", "terminated", "date_closed_on")

# The derived disposition categories, in funnel order (least -> most advanced).
# `never_contacted` is only produced for screened phones absent from the data.
.DISPOSITION_CATEGORIES <- c("never_contacted", "non_response", "engaged", "opt_in",
                    "terminated", "complete", "web_complete")

# Columns of the per-phone summary (also the block appended by _screen()).
.DISPOSITION_SUMMARY_COLS <- c("phone", "ever_contacted", "n_campaigns", "ever_engaged",
                      "ever_opted_in", "ever_complete", "ever_terminated",
                      "latest_disposition", "campaigns")

# The stored disposition schema (SUR-1518), in canonical order -- what
# disposition_records() returns. `started`/`mode` come from disposition_run();
# `error`/`loi`/`topic`/`date_closed_on` are added by downstream enrichment, so
# an un-enriched projection lacks them and records() returns just the subset present.
.DISPOSITION_RECORD_COLS <- c("phone", "campaign_id", "started", "engaged",
                      "opt_in", "complete", "web_complete", "terminated",
                      "error", "loi", "topic", "mode", "date_closed_on")

# Digit-normalize a phone for matching: strip non-digits, then drop a leading US
# country code so an 11-digit "1NXXNXXXXXX" matches a stored 10-digit number.
# Blank/NA -> NA.
.disposition_normalize_phone <- function(x) {
  x <- gsub("[^0-9]", "", as.character(x))
  x[!nzchar(x)] <- NA_character_
  eleven <- !is.na(x) & nchar(x) == 11L & startsWith(x, "1")
  x[eleven] <- substr(x[eleven], 2L, 11L)
  x
}

# Derive one disposition category per row from the 0/1 funnel flags, by funnel
# precedence (later assignment wins). A t2w_external row has complete = NA, so
# it falls through to the last known in-channel step -- never a false complete.
.disposition_derive_category <- function(d) {
  is1 <- function(v) !is.na(v) & v == 1L
  out <- rep("non_response", nrow(d))   # data is contacted-only (started == 1)
  out[is1(d$engaged)] <- "engaged"
  out[is1(d$opt_in)] <- "opt_in"
  out[is1(d$terminated)] <- "terminated"
  out[is1(d$complete)] <- "complete"
  out[is1(d$web_complete)] <- "web_complete"
  out
}

# One all-NA/FALSE summary row per never-contacted phone (screened but absent).
.disposition_never_contacted <- function(phones) {
  n <- length(phones)
  data.frame(
    phone = phones, ever_contacted = rep(FALSE, n), n_campaigns = rep(0L, n),
    ever_engaged = rep(FALSE, n), ever_opted_in = rep(FALSE, n),
    ever_complete = rep(FALSE, n), ever_terminated = rep(FALSE, n),
    latest_disposition = rep("never_contacted", n),
    campaigns = rep(NA_character_, n), stringsAsFactors = FALSE
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
  req <- unique(.disposition_normalize_phone(phones))
  req[!is.na(req)]
}

# Normalize phone and apply the row-scope filters (requested phones, campaigns,
# date_closed_on range). Pure; `data` already has .DISPOSITION_READ_COLS, and
# `date_from`/`date_to` are already coerced to Date (or NULL) by the caller.
# One combined keep-mask, subset once -- avoids the intermediate frame copies a
# filter-per-predicate chain allocates.
.disposition_filter <- function(data, keep_phones, campaign_ids, date_from, date_to) {
  data$phone <- .disposition_normalize_phone(data$phone)
  keep <- !is.na(data$phone)
  if (!is.null(keep_phones)) {
    keep <- keep & data$phone %in% keep_phones
  }
  if (!is.null(campaign_ids)) {
    keep <- keep & as.character(data$campaign_id) %in% as.character(campaign_ids)
  }
  if (!is.null(date_from)) {
    keep <- keep & !is.na(data$date_closed_on) & data$date_closed_on >= date_from
  }
  if (!is.null(date_to)) {
    keep <- keep & !is.na(data$date_closed_on) & data$date_closed_on <= date_to
  }
  data[keep, , drop = FALSE]
}

# Collapse the (phone, campaign) rows to one row per phone. Rows are ordered so
# the latest campaign (max date_closed_on, NA last; tie -> max campaign_id) is
# first per phone, so latest_disposition is a plain first-of-group pick.
.disposition_collapse <- function(d) {
  d$.category <- .disposition_derive_category(d)
  date_key <- as.numeric(d$date_closed_on)
  date_key[is.na(date_key)] <- -Inf
  d <- d[order(d$phone, -date_key, -as.numeric(d$campaign_id)), , drop = FALSE]
  first <- !duplicated(d$phone)
  ph <- d$phone[first]
  # Group a per-row vector by phone, apply `f`, and index the result back to the
  # first-of-group phone order (`ph`) so every column lines up row-for-row.
  by_phone <- function(x, f) tapply(x, d$phone, f)[ph]
  any_true <- function(x) any(x, na.rm = TRUE)
  data.frame(
    phone = ph,
    ever_contacted = TRUE,
    n_campaigns = as.integer(by_phone(d$campaign_id, function(x) length(unique(x)))),
    ever_engaged = as.logical(by_phone(d$engaged == 1L, any_true)),
    ever_opted_in = as.logical(by_phone(d$opt_in == 1L, any_true)),
    ever_complete = as.logical(
      by_phone((d$complete == 1L) | (d$web_complete == 1L), any_true)),
    ever_terminated = as.logical(by_phone(d$terminated == 1L, any_true)),
    latest_disposition = d$.category[first],
    campaigns = as.character(
      by_phone(d$campaign_id, function(x) paste(sort(unique(x)), collapse = ","))),
    stringsAsFactors = FALSE
  )
}

# 1-based page slice over the (phone-ordered) result. NULL page/size -> no-op.
.disposition_paginate <- function(summ, page, page_size) {
  if (is.null(page) && is.null(page_size)) return(summ)
  ps <- if (is.null(page_size)) max(1L, nrow(summ)) else page_size
  pg <- if (is.null(page)) 1L else page
  ok <- function(x) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && x >= 1L && x %% 1 == 0
  }
  if (!ok(ps) || !ok(pg)) {
    stop("`page` and `page_size` must be positive integers.", call. = FALSE)
  }
  from <- (pg - 1L) * ps + 1L
  if (from > nrow(summ)) return(summ[0L, , drop = FALSE])
  summ[seq.int(from, min(pg * ps, nrow(summ))), , drop = FALSE]
}

# I/O: validate the path and read the projection. `columns` picks what to read:
# the default reads just the summary columns; `NULL` reads every column
# (disposition_records() uses this for the full stored schema). A `NULL` full read
# is also the safe choice on nanoparquet-*written* multi-row files, which can crash
# on a `col_select` subset -- col_select is fine on the real arrow/duckdb-written
# projection (see the test-file note).
.disposition_read_parquet <- function(dataset, columns = .DISPOSITION_READ_COLS) {
  if (!is.character(dataset) || length(dataset) != 1L || !nzchar(dataset)) {
    stop("`dataset` must be a single Parquet path.", call. = FALSE)
  }
  if (!file.exists(dataset)) {
    stop_not_found("disposition dataset", dataset)
  }
  as.data.frame(nanoparquet::read_parquet(dataset, col_select = columns))
}

#' Roll disposition rows up to one row per phone
#'
#' The pure rollup core: takes an in-memory disposition frame
#' (one row per \code{(phone, campaign_id)}) and returns \strong{one row per
#' phone} with its cross-campaign screening flags and latest disposition. No
#' I/O -- \code{\link{disposition_summary}} reads a Parquet file and calls
#' this. Use it directly to read a projection once and screen several samples
#' against the in-memory frame.
#'
#' @param data A data frame with columns \code{phone}, \code{campaign_id},
#'   \code{engaged}, \code{opt_in}, \code{complete}, \code{web_complete},
#'   \code{terminated}, \code{date_closed_on}.
#' @param phones Optional character vector of phone numbers to screen. When
#'   supplied, \strong{every} input number is returned -- never-contacted ones
#'   with \code{ever_contacted = FALSE} and
#'   \code{latest_disposition = "never_contacted"}. \code{NULL} summarizes every
#'   phone in \code{data}. Matched digit-normalized (a leading US \code{1} is
#'   dropped so 11-digit numbers match 10-digit ones).
#' @param campaign_ids Optional vector; restrict the underlying rows to these
#'   campaigns before summarizing.
#' @param statuses Optional subset of the derived disposition categories
#'   (\code{never_contacted}, \code{non_response}, \code{engaged},
#'   \code{opt_in}, \code{terminated}, \code{complete}, \code{web_complete});
#'   keep only phones whose \code{latest_disposition} is one of them.
#' @param date_from,date_to Optional \code{Date}/date-string bounds on
#'   \code{date_closed_on}. In the beta \code{date_closed_on} is \code{NA}, so a
#'   date bound drops rows with an unknown close date.
#' @param page,page_size Optional 1-based pagination over the per-phone result.
#' @return A data frame, one row per phone: \code{phone}, \code{ever_contacted},
#'   \code{n_campaigns}, \code{ever_engaged}, \code{ever_opted_in},
#'   \code{ever_complete}, \code{ever_terminated}, \code{latest_disposition},
#'   \code{campaigns} (comma-separated campaign ids).
#' @seealso \code{\link{disposition_summary}},
#'   \code{\link{disposition_screen}}
#' @examples
#' records <- data.frame(
#'   phone = c("5551234567", "5551234567", "5559876543"),
#'   campaign_id = c(101L, 102L, 101L),
#'   engaged = c(1L, 1L, 0L),
#'   opt_in = c(1L, 0L, 0L),
#'   complete = c(1L, 0L, 0L),
#'   web_complete = c(0L, 0L, 0L),
#'   terminated = c(0L, 1L, 0L),
#'   date_closed_on = as.Date(c("2026-01-10", "2026-01-20", "2026-01-15")),
#'   stringsAsFactors = FALSE
#' )
#' disposition_rollup(records)
#' disposition_rollup(records, phones = c("5551234567", "5550000000"))
#' @export
disposition_rollup <- function(data, phones = NULL, campaign_ids = NULL,
                                statuses = NULL, date_from = NULL,
                                date_to = NULL, page = NULL, page_size = NULL) {
  check_data_frame(data, "data", fn = "disposition_rollup")
  missing_cols <- setdiff(.DISPOSITION_READ_COLS, names(data))
  if (length(missing_cols) > 0L) {
    stop_s160(sprintf("`data` is missing column(s): %s",
                      paste(missing_cols, collapse = ", ")),
              fn = "disposition_rollup")
  }
  if (!is.null(statuses)) {
    bad <- setdiff(as.character(statuses), .DISPOSITION_CATEGORIES)
    if (length(bad) > 0L) {
      stop_s160(sprintf("unknown status(es): %s",
                        paste(bad, collapse = ", ")),
                fn = "disposition_rollup")
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
  summ <- summ[order(summ$phone), , drop = FALSE]
  summ <- .disposition_paginate(summ, page, page_size)
  rownames(summ) <- NULL
  summ
}

#' Summarize the disposition dataset for a phone list (one row per phone)
#'
#' Reads the disposition Parquet projection and returns \strong{one row per
#' phone} (see \code{\link{disposition_rollup}}) -- the engine
#' for ad-hoc queries. For cleaning a sample file in place, use
#' \code{\link{disposition_screen}}; for the underlying rows \emph{before} the
#' per-phone rollup (one per \code{(phone, campaign_id)}), use
#' \code{\link{disposition_records}}.
#'
#' @param dataset Path to a disposition Parquet file (the phone-sorted read
#'   projection). Read with \pkg{nanoparquet}, projected to the summary columns.
#' @param phones Optional character vector of phone numbers to screen; every
#'   input number is returned (never-contacted ones flagged
#'   \code{ever_contacted = FALSE}). \code{NULL} summarizes every phone in
#'   \code{dataset}. See \code{\link{disposition_rollup}} for details.
#' @inheritParams disposition_rollup
#' @return A per-phone summary data frame (see \code{\link{disposition_rollup}}).
#' @seealso \code{\link{disposition_rollup}}, \code{\link{disposition_screen}},
#'   \code{\link{disposition_records}}
#' @examples
#' \dontrun{
#' dataset <- disposition_pull()
#' disposition_summary(dataset, phones = my_sample$phone)
#' }
#' @export
disposition_summary <- function(dataset, phones = NULL, campaign_ids = NULL,
                                   statuses = NULL, date_from = NULL,
                                   date_to = NULL, page = NULL,
                                   page_size = NULL) {
  disposition_rollup(.disposition_read_parquet(dataset), phones = phones,
                      campaign_ids = campaign_ids, statuses = statuses,
                      date_from = date_from, date_to = date_to,
                      page = page, page_size = page_size)
}

#' Read the raw disposition records (one row per phone + campaign)
#'
#' Reads the disposition Parquet projection and returns its rows \strong{as
#' stored} -- one row per \code{(phone, campaign_id)}, carrying the full
#' disposition schema: \code{phone}, \code{campaign_id}, \code{started},
#' \code{engaged}, \code{opt_in}, \code{complete}, \code{web_complete},
#' \code{terminated}, \code{error}, \code{loi}, \code{topic}, \code{mode},
#' \code{date_closed_on}. This is the level directly beneath
#' \code{\link{disposition_summary}}: where \code{summary} rolls every phone up to a
#' single screening row, \code{records} hands back the raw per-campaign rows --
#' for inspection, export, or a custom rollup.
#'
#' Only the canonical columns \emph{present in the file} are returned, in the
#' order above. A projection written straight from \code{\link{disposition_run}}
#' carries just the nine computed columns (no \code{error} / \code{loi} /
#' \code{topic} / \code{date_closed_on}); the enriched projection carries all
#' thirteen. In the current beta \code{error} and \code{date_closed_on} are
#' \code{NA} for every row. The whole projection is read into memory and filtered
#' in R (nanoparquet has no predicate pushdown, like \code{\link{disposition_summary}});
#' \code{phone} is digit-normalized for matching, and a stored row whose phone is
#' blank or unparseable is dropped.
#'
#' Two differences from \code{\link{disposition_rollup}} follow from the raw
#' grain: a screened phone that was never contacted has \strong{no} row here
#' (there is no stored record to return, unlike the \code{never_contacted} row
#' \code{summary} synthesises), and there is no \code{statuses} argument -- that
#' selects a per-phone \code{latest_disposition}, which exists only after the
#' rollup.
#'
#' @param dataset Path to a disposition Parquet file (the read projection), e.g.
#'   from \code{\link{disposition_pull}}. Read in full with \pkg{nanoparquet}.
#' @param phones Optional character vector of phone numbers to keep. Matched
#'   digit-normalized (a leading US \code{1} is dropped so 11-digit numbers match
#'   10-digit ones). \code{NULL} (default) returns every row.
#' @param campaign_ids Optional vector; keep only rows for these campaigns.
#' @param date_from,date_to Optional \code{Date}/date-string bounds on
#'   \code{date_closed_on}. A row with an \code{NA} close date is dropped by any
#'   bound -- and in the current beta \code{date_closed_on} is \code{NA} for every
#'   row, so any bound returns no rows. Supplying a bound when the projection has
#'   no \code{date_closed_on} column at all is an error.
#' @param page,page_size Optional 1-based pagination over the
#'   \code{(phone, campaign_id)}-ordered rows.
#' @return A data frame, one row per \code{(phone, campaign_id)}, with the
#'   canonical disposition columns present in the file (see Details), ordered by
#'   \code{phone} then \code{campaign_id}.
#' @seealso \code{\link{disposition_summary}} / \code{\link{disposition_rollup}}
#'   (the per-phone rollup), \code{\link{disposition_screen}},
#'   \code{\link{disposition_pull}}
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

  raw <- .disposition_read_parquet(dataset, columns = NULL)
  missing_cols <- setdiff(c("phone", "campaign_id"), names(raw))
  if (length(missing_cols) > 0L) {
    stop_s160(sprintf("`dataset` is missing required column(s): %s",
                      paste(missing_cols, collapse = ", ")),
              fn = "disposition_records")
  }
  if ((!is.null(date_from) || !is.null(date_to)) &&
        !"date_closed_on" %in% names(raw)) {
    stop_s160("`dataset` has no `date_closed_on` column to filter on.",
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
#'   rows considered (see \code{\link{disposition_rollup}}). No \code{statuses}
#'   or pagination here -- every sample row is returned.
#' @return \code{sample} with the columns \code{ever_contacted},
#'   \code{n_campaigns}, \code{ever_engaged}, \code{ever_opted_in},
#'   \code{ever_complete}, \code{ever_terminated}, \code{latest_disposition},
#'   \code{campaigns} appended. A valid phone that is absent from the rows
#'   selected by \code{campaign_ids}, \code{date_from}, and \code{date_to} (the
#'   whole dataset when those are unset) gets a \code{never_contacted} row
#'   (\code{ever_contacted = FALSE}, \code{n_campaigns = 0}, the other
#'   \code{ever_*} flags \code{FALSE}, \code{latest_disposition =
#'   "never_contacted"}, \code{campaigns = NA}); only a phone that
#'   digit-normalizes to nothing (blank/unparseable) gets an all-\code{NA} block.
#' @seealso \code{\link{disposition_summary}}, \code{\link{disposition_rollup}}
#' @examples
#' \dontrun{
#' dataset <- disposition_pull()
#' cleaned <- disposition_screen(my_sample, dataset, phone_col = "phone")
#' # drop finished/terminated; blank-phone rows come back all-NA and are kept
#' subset(cleaned, !(ever_complete %in% TRUE | ever_terminated %in% TRUE))
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
  summ <- disposition_rollup(.disposition_read_parquet(dataset),
                              phones = sample[[phone_col]],
                              campaign_ids = campaign_ids,
                              date_from = date_from, date_to = date_to)
  idx <- match(.disposition_normalize_phone(sample[[phone_col]]), summ$phone)
  for (col in disposition_cols) sample[[col]] <- summ[[col]][idx]
  sample
}

#' Download the disposition projection from GCS
#'
#' Pulls the phone-sorted disposition projection
#' (\code{disposition_by_phone/disposition_all.parquet}) from the environment's
#' disposition bucket to a local file and returns the path -- ready to hand to
#' \code{\link{disposition_summary}} / \code{\link{disposition_screen}}. Downloaded
#' once and reused from the local cache on later calls (pass \code{refresh = TRUE}
#' to force a fresh download). This is the one \code{disposition_*} function that
#' reaches GCS: authenticate first with \code{\link{s160_gcs_init}} (any bucket)
#' so the session's GCS credentials are set. A download without an initialized
#' session errors with \dQuote{GCS not initialized. Run s160_gcs_init() first.}
#' (a cache hit is served without needing auth).
#'
#' @param env Environment for the disposition bucket: \code{"prod"} (default) or
#'   \code{"dev"} (the \code{s160_disposition_<env>} buckets). There is no staging
#'   disposition bucket, so the values differ from \code{\link{s160_api_auth}}'s
#'   \code{prod}/\code{staging} by design -- each names the environments its own
#'   subsystem actually has.
#' @param dest Where to save. \code{NULL} (default) caches under
#'   \code{tools::R_user_dir("survey160r", "cache")}. A directory saves
#'   \code{disposition_all_<env>.parquet} inside it; any other single string is
#'   treated as the exact output path (its parent is created).
#' @param bucket Source GCS bucket. \code{NULL} (default) derives it from
#'   \code{env}; pass a bucket name to override.
#' @param refresh When \code{FALSE} (default), reuse an existing local copy;
#'   \code{TRUE} always re-downloads (the projection is rebuilt each pipeline
#'   pass, so refresh to pick up a newer one).
#' @return The local path to the downloaded Parquet (a single string).
#' @seealso \code{\link{disposition_summary}}, \code{\link{disposition_screen}},
#'   \code{\link{s160_gcs_init}}
#' @examples
#' \dontrun{
#' s160_gcs_init(bucket = "s160_disposition_prod")   # one-time browser OAuth
#' dataset <- disposition_pull()                      # download (cached)
#' disposition_screen(my_sample, dataset)
#' }
#' @export
disposition_pull <- function(env = c("prod", "dev"), dest = NULL,
                             bucket = NULL, refresh = FALSE) {
  env <- match.arg(env)
  if (!is.logical(refresh) || length(refresh) != 1L || is.na(refresh)) {
    stop_s160("`refresh` must be a single TRUE or FALSE.",
              fn = "disposition_pull")
  }
  if (is.null(bucket)) bucket <- sprintf("s160_disposition_%s", env)
  bucket <- resolve_bucket(bucket)
  object_name <- "disposition_by_phone/disposition_all.parquet"
  # Key the default cache on the resolved bucket, not just env: two `bucket=`
  # overrides with `dest = NULL` must not share (and silently reuse) one file.
  default_name <- sprintf("%s.parquet", bucket)

  if (is.null(dest)) {
    cache_dir <- tools::R_user_dir("survey160r", "cache")
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    local_path <- file.path(cache_dir, default_name)
  } else if (!is.character(dest) || length(dest) != 1L || !nzchar(trimws(dest))) {
    stop_s160("`dest` must be a single non-empty path or directory.",
              fn = "disposition_pull")
  } else if (dir.exists(dest)) {
    local_path <- file.path(dest, default_name)
  } else {
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    local_path <- dest
  }

  gcs_path <- sprintf("gs://%s/%s", bucket, object_name)
  if (!refresh && file.exists(local_path)) {
    message(sprintf("Using cached disposition projection: %s", local_path))
    return(local_path)
  }

  # A download needs an authenticated GCS session. `disposition_pull()` always
  # resolves a concrete bucket (so resolve_bucket() never reaches this check),
  # yet the download still fails without s160_gcs_init(); check explicitly here
  # so an un-initialized session gets the standard clear message rather than a
  # raw googleCloudStorageR error wrapped as "Failed to download". Placed after
  # the cache-hit return: reusing a local copy needs no auth.
  check_gcs_ready()

  message(sprintf("Downloading %s", gcs_path))
  # Download to a temp file in the destination dir, then atomically move it into
  # place on success -- a failed or partial download never poisons the cache,
  # and any existing good copy survives.
  tmp <- tempfile(tmpdir = dirname(local_path), fileext = ".part")
  on.exit(unlink(tmp), add = TRUE)
  tryCatch(
    download_with_verify(object_name = object_name, local_path = tmp,
                         bucket = bucket),
    s160_not_found = function(e) {
      stop_not_found("disposition projection", gcs_path, fn = "disposition_pull")
    },
    error = function(e) {
      stop_failed(sprintf("download %s", gcs_path), conditionMessage(e),
                  fn = "disposition_pull")
    }
  )
  if (!file.rename(tmp, local_path) &&
        !file.copy(tmp, local_path, overwrite = TRUE)) {
    stop_failed("move the downloaded file into place", local_path,
                fn = "disposition_pull")
  }
  local_path
}
