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
# The pure per-phone rollup core is the private .disposition_rollup(); the Parquet
# read uses nanoparquet (tiny, zero-dependency) and, since the projection is one
# consolidated file, a full read + in-R filter is sub-second.

# Columns the summary reads (the Parquet read is projected to just these).
.DISPOSITION_READ_COLS <- c("phone", "campaign_id", "engaged", "opted_in", "completed",
                   "web_complete", "terminated", "error", "disposition_date")

# The derived disposition categories, in funnel order (least -> most advanced).
# `never_contacted` is only produced for screened phones absent from the data.
.DISPOSITION_CATEGORIES <- c("never_contacted", "non_response", "engaged", "opted_in",
                    "terminated", "completed", "web_complete")

# Columns of the per-phone summary (also the block appended by _screen()), in
# output order: identity, scope (n_campaigns + the id list), the cumulative status
# COUNTS (n_*: how many of the phone's campaigns set each flag) + n_error, the
# latest (most-recent) and best (furthest-reached) disposition each with its
# campaign, then the first/last disposition_date span. A never-contacted phone is
# marked by n_campaigns == 0 (was ever_contacted = FALSE, removed).
.DISPOSITION_SUMMARY_COLS <- c("phone", "n_campaigns", "campaigns",
                      "n_engaged", "n_opted_in", "n_completed", "n_web_complete",
                      "n_terminated", "n_error", "latest_disposition",
                      "latest_campaign_id", "best_disposition", "best_campaign_id",
                      "first_disposition_date", "last_disposition_date")

# The stored disposition schema, in canonical order -- what
# disposition_records() returns. `sent`/`mode`/`error` come from disposition_run();
# `loi`/`topic`/`disposition_date` are added by downstream enrichment, so an
# un-enriched projection lacks those three and records() returns just the subset present.
.DISPOSITION_RECORD_COLS <- c("phone", "campaign_id", "sent", "engaged",
                      "opted_in", "completed", "web_complete", "terminated",
                      "error", "loi", "topic", "mode", "disposition_date")

# Phone matching uses the shared .normalize_phone (aaa_utils.R) so a sample
# matches the disposition and opt-out datasets identically.

# Derive one disposition category per row from the 0/1 funnel flags, by funnel
# precedence (later assignment wins). A t2w_external row has completed = NA, so
# it falls through to the last known in-channel step -- never a false completed.
.disposition_derive_category <- function(d) {
  is1 <- function(v) !is.na(v) & v == 1L
  out <- rep("non_response", nrow(d))   # data is contacted-only (sent == 1)
  out[is1(d$engaged)] <- "engaged"
  out[is1(d$opted_in)] <- "opted_in"
  out[is1(d$terminated)] <- "terminated"
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
    n_web_complete = rep(0L, n), n_terminated = rep(0L, n), n_error = rep(0L, n),
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
  if ((!is.null(date_from) || !is.null(date_to)) &&
        nrow(data) > 0L && all(is.na(data$disposition_date))) {
    warning("`date_from`/`date_to` filter on `disposition_date`, which is NA for ",
            "every row here; the filter returns no rows.", call. = FALSE)
  }
  if (!is.null(date_from)) {
    keep <- keep & !is.na(data$disposition_date) & data$disposition_date >= date_from
  }
  if (!is.null(date_to)) {
    keep <- keep & !is.na(data$disposition_date) & data$disposition_date <= date_to
  }
  data[keep, , drop = FALSE]
}

# Collapse the (phone, campaign) rows to one row per phone. Rows are ordered so
# the latest campaign (max disposition_date, NA last; tie -> max campaign_id) is
# first per phone, so latest_disposition is a plain first-of-group pick.
.disposition_collapse <- function(d) {
  d$.category <- .disposition_derive_category(d)
  date_key <- as.numeric(d$disposition_date)
  date_key[is.na(date_key)] <- -Inf
  d <- d[order(d$phone, -date_key, -as.numeric(d$campaign_id)), , drop = FALSE]
  first <- !duplicated(d$phone)
  ph <- d$phone[first]
  # Group a per-row vector by phone, apply `f`, and index the result back to the
  # first-of-group phone order (`ph`) so every column lines up row-for-row.
  by_phone <- function(x, f) tapply(x, d$phone, f)[ph]
  # Cumulative status counts: how many of the phone's campaigns set each flag
  # (0/1/NA; NA counts as not-set). Overlapping -- a completed campaign is also
  # engaged -- so these are "reached status X", not a partition of n_campaigns.
  count1 <- function(x) sum(x == 1L, na.rm = TRUE)
  # A campaign carries a delivery error when `error` holds a non-blank code.
  has_error <- !is.na(d$error) & nzchar(trimws(as.character(d$error)))
  # Per-phone min/max disposition_date, NA when the phone has no dated campaign
  # (an un-enriched projection, or every date missing). tapply on the numeric
  # day-count keeps the Date class off the grouping; restore it after.
  dd_num <- as.numeric(d$disposition_date)
  span <- function(reduce) {
    v <- tapply(dd_num, d$phone, function(z) {
      z <- z[!is.na(z)]
      if (length(z)) reduce(z) else NA_real_
    })[ph]
    as.Date(unname(v), origin = "1970-01-01")
  }
  # Best (furthest-reached) disposition across the phone's campaigns: the highest
  # funnel category any of them hit, ranked by the SAME precedence latest uses
  # (.DISPOSITION_CATEGORIES: non_response < engaged < opted_in < terminated <
  # completed < web_complete). Re-rank the rows highest-category first (tie ->
  # latest date, then max id, matching latest_disposition's tie-break), take the
  # first per phone, and align back to the latest-order phone vector `ph`.
  rk <- match(d$.category, .DISPOSITION_CATEGORIES)
  # `date_key` above is in the PRE-reorder order; rebuild the key aligned with the
  # now-reordered `d` (NA dates sort last, as in the latest ordering).
  dk <- dd_num
  dk[is.na(dk)] <- -Inf
  ob <- order(d$phone, -rk, -dk, -as.numeric(d$campaign_id))
  db <- d[ob, , drop = FALSE]
  best <- !duplicated(db$phone)
  b <- match(ph, db$phone[best])
  data.frame(
    phone = ph,
    n_campaigns = as.integer(by_phone(d$campaign_id, function(x) length(unique(x)))),
    campaigns = as.character(
      by_phone(d$campaign_id, function(x) paste(sort(unique(x)), collapse = ","))),
    n_engaged = as.integer(by_phone(d$engaged, count1)),
    n_opted_in = as.integer(by_phone(d$opted_in, count1)),
    n_completed = as.integer(by_phone(d$completed, count1)),
    n_web_complete = as.integer(by_phone(d$web_complete, count1)),
    n_terminated = as.integer(by_phone(d$terminated, count1)),
    n_error = as.integer(by_phone(has_error, function(x) sum(x, na.rm = TRUE))),
    latest_disposition = d$.category[first],
    latest_campaign_id = as.character(d$campaign_id[first]),
    best_disposition = db$.category[best][b],
    best_campaign_id = as.character(db$campaign_id[best][b]),
    first_disposition_date = span(min),
    last_disposition_date = span(max),
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
  if (!is.character(dataset) || length(dataset) != 1L || !nzchar(dataset)) {
    stop("`dataset` must be a single Parquet path.", call. = FALSE)
  }
  if (!file.exists(dataset)) {
    stop_not_found("disposition dataset", dataset)
  }
  cb <- nanoparquet::read_parquet_info(dataset)$created_by
  duckdb <- length(cb) == 1L && !is.na(cb) && grepl("duckdb", cb, ignore.case = TRUE)
  if (duckdb && !is.null(columns)) {
    cols <- intersect(columns, nanoparquet::read_parquet_schema(dataset)$name)
    return(as.data.frame(nanoparquet::read_parquet(dataset, col_select = cols)))
  }
  d <- as.data.frame(nanoparquet::read_parquet(dataset))
  if (!is.null(columns)) {
    d <- d[, intersect(columns, names(d)), drop = FALSE]
  }
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
                                  c("disposition_date", "error")),
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
  summ <- summ[order(summ$phone), , drop = FALSE]
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
#'   read with \pkg{nanoparquet}, projected to the summary columns; a frame must
#'   carry \code{phone}, \code{campaign_id}, \code{engaged}, \code{opted_in},
#'   \code{completed}, \code{web_complete}, and \code{terminated}.
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
#'   \code{opted_in}, \code{terminated}, \code{completed}, \code{web_complete});
#'   keep only phones whose \code{latest_disposition} is one of them.
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
#'   \code{n_web_complete}, \code{n_terminated} -- each \code{0} when the phone
#'   never reached that status and \code{> 0} the number of the phone's campaigns
#'   that did (they overlap: a completed campaign is also engaged) -- plus
#'   \code{n_error} (how many campaigns carried a carrier delivery-error code);
#'   \code{latest_disposition} + \code{latest_campaign_id} (the category of the
#'   phone's most-recent campaign and that campaign's id); \code{best_disposition}
#'   + \code{best_campaign_id} (the furthest-reached category across all the
#'   phone's campaigns -- ranked by the same funnel precedence, so \code{completed}
#'   / \code{web_complete} rank highest and \code{terminated} above
#'   \code{opted_in} -- and the campaign that reached it); and
#'   \code{first_disposition_date} / \code{last_disposition_date} (earliest and
#'   latest \code{disposition_date} across the phone's campaigns, \code{NA} when
#'   none is dated). A never-contacted phone has \code{n_campaigns = 0}. Campaign
#'   ids are returned as character.
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
    .disposition_read_parquet(x)
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
#' \code{terminated}, \code{error}, \code{loi}, \code{topic}, \code{mode},
#' \code{disposition_date}. This is the level directly beneath
#' \code{\link{disposition_summary}}: where \code{summary} rolls every phone up to a
#' single screening row, \code{records} hands back the raw per-campaign rows --
#' for inspection, export, or a custom rollup.
#'
#' Only the canonical columns \emph{present in the file} are returned, in the
#' order above -- a legacy or minimal projection that lacks a column (e.g.
#' \code{error}, \code{loi}, \code{topic}, or \code{disposition_date}) omits it,
#' rather than filling an all-\code{NA} column. A projection written straight from
#' \code{\link{disposition_run}} carries the funnel flags plus \code{mode},
#' \code{error} (the carrier delivery-error code), and \code{disposition_date}
#' (\code{max(scriptDate)}); \code{loi} / \code{topic} are added by the tracker
#' enrichment, so only the enriched projection carries all thirteen.
#' \code{disposition_date} is \code{NA} for a row with no send; \code{error} is
#' \code{NA} when the export carries no usable code (a clean send, or an export
#' lacking the column). The
#' whole projection is read into memory and filtered
#' in R (nanoparquet has no predicate pushdown, like \code{\link{disposition_summary}});
#' \code{phone} is digit-normalized for matching, and a stored row whose phone is
#' blank or unparseable is dropped.
#'
#' Two differences from the per-phone rollup follow from the raw
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

  raw <- .disposition_read_parquet(dataset, columns = NULL)
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
#'   \code{n_web_complete}, \code{n_terminated}, \code{n_error},
#'   \code{latest_disposition}, \code{latest_campaign_id}, \code{best_disposition},
#'   \code{best_campaign_id}, \code{first_disposition_date},
#'   \code{last_disposition_date}. A valid phone that is absent from the rows
#'   selected by \code{campaign_ids}, \code{date_from}, and \code{date_to} (the
#'   whole dataset when those are unset) gets a \code{never_contacted} row
#'   (\code{n_campaigns = 0}, the \code{n_*} counts \code{0}, the dates \code{NA},
#'   \code{latest_disposition = "never_contacted"}, \code{campaigns = NA}); only a
#'   phone that digit-normalizes to nothing (blank/unparseable) gets an
#'   all-\code{NA} block.
#' @seealso \code{\link{disposition_summary}}, \code{\link{disposition_records}},
#'   \code{\link{opt_out_screen}}
#' @examples
#' \dontrun{
#' dataset <- disposition_pull()
#' cleaned <- disposition_screen(my_sample, dataset, phone_col = "phone")
#' # drop finished/terminated; blank-phone rows come back all-NA and are kept
#' subset(cleaned, !((n_completed > 0 | n_web_complete > 0) %in% TRUE |
#'                     (n_terminated > 0) %in% TRUE))
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
  summ <- .disposition_rollup(.disposition_read_parquet(dataset),
                              phones = sample[[phone_col]],
                              campaign_ids = campaign_ids,
                              date_from = date_from, date_to = date_to,
                              fn = "disposition_screen")
  idx <- match(.normalize_phone(sample[[phone_col]]), summ$phone)
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
#'   or scheduled runs. The projection is around 150 MB, so an interactive pull
#'   otherwise looks stalled while it transfers.
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
