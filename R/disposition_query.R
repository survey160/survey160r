# Disposition query -- the R-only consumer of the disposition dataset.
#
# The disposition dataset is one row per (phone, campaign_id), contacted-only,
# produced upstream (per-campaign Parquet + a phone-sorted read projection). A
# Survey Manager screens a fresh sample against it -- "which of these numbers
# have been contacted / completed / refused before?" -- to clean the list. So a
# query returns ONE ROW PER PHONE: the number's cross-campaign screening flags +
# its latest disposition.
#
# Split, per the package's pure-core / IO-edge convention:
#   disposition_summary(data, ...)          PURE   -- roll an in-memory frame up
#   s160_disposition_query(dataset, ...)    I/O    -- read the Parquet, summarize
#   s160_disposition_screen(sample, ...)    I/O    -- annotate a caller's sample
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
    stop(sprintf("disposition_summary: `%s` must be a single valid date.", name),
         call. = FALSE)
  }
  d
}

# Normalize phone and apply the row-scope filters (requested phones, campaigns,
# date_closed_on range). Pure; `data` already has .DISPOSITION_READ_COLS.
.disposition_filter <- function(data, keep_phones, campaign_ids, date_from, date_to) {
  data$phone <- .disposition_normalize_phone(data$phone)
  data <- data[!is.na(data$phone), , drop = FALSE]
  if (!is.null(keep_phones)) {
    data <- data[data$phone %in% keep_phones, , drop = FALSE]
  }
  if (!is.null(campaign_ids)) {
    data <- data[as.character(data$campaign_id) %in%
                   as.character(campaign_ids), , drop = FALSE]
  }
  if (!is.null(date_from)) {
    data <- data[!is.na(data$date_closed_on) &
                   data$date_closed_on >= as.Date(date_from), , drop = FALSE]
  }
  if (!is.null(date_to)) {
    data <- data[!is.na(data$date_closed_on) &
                   data$date_closed_on <= as.Date(date_to), , drop = FALSE]
  }
  data
}

# Roll the (phone, campaign) rows up to one row per phone. Rows are ordered so
# the latest campaign (max date_closed_on, NA last; tie -> max campaign_id) is
# first per phone, so latest_disposition is a plain first-of-group pick.
.disposition_rollup <- function(d) {
  d$.category <- .disposition_derive_category(d)
  date_key <- as.numeric(d$date_closed_on)
  date_key[is.na(date_key)] <- -Inf
  d <- d[order(d$phone, -date_key, -as.numeric(d$campaign_id)), , drop = FALSE]
  first <- !duplicated(d$phone)
  ph <- d$phone[first]
  data.frame(
    phone = ph,
    ever_contacted = TRUE,
    n_campaigns = as.integer(
      tapply(d$campaign_id, d$phone, function(x) length(unique(x)))[ph]),
    ever_engaged = as.logical(
      tapply(d$engaged == 1L, d$phone, any, na.rm = TRUE)[ph]),
    ever_opted_in = as.logical(
      tapply(d$opt_in == 1L, d$phone, any, na.rm = TRUE)[ph]),
    ever_complete = as.logical(
      tapply((d$complete == 1L) | (d$web_complete == 1L), d$phone, any,
             na.rm = TRUE)[ph]),
    ever_terminated = as.logical(
      tapply(d$terminated == 1L, d$phone, any, na.rm = TRUE)[ph]),
    latest_disposition = d$.category[first],
    campaigns = as.character(
      tapply(d$campaign_id, d$phone,
             function(x) paste(sort(unique(x)), collapse = ","))[ph]),
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

# I/O: validate the path and read the projection, projected to the query columns.
.disposition_read_parquet <- function(dataset) {
  if (!is.character(dataset) || length(dataset) != 1L || !nzchar(dataset)) {
    stop("dataset must be a single Parquet path.", call. = FALSE)
  }
  if (!file.exists(dataset)) {
    stop(sprintf("disposition dataset not found: %s", dataset), call. = FALSE)
  }
  as.data.frame(nanoparquet::read_parquet(dataset, col_select = .DISPOSITION_READ_COLS))
}

#' Summarize disposition rows to one row per phone
#'
#' The pure core of the disposition query: takes an in-memory disposition frame
#' (one row per \code{(phone, campaign_id)}) and returns \strong{one row per
#' phone} with its cross-campaign screening flags and latest disposition. No
#' I/O -- \code{\link{s160_disposition_query}} reads a Parquet file and calls
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
#' @seealso \code{\link{s160_disposition_query}},
#'   \code{\link{s160_disposition_screen}}
#' @export
disposition_summary <- function(data, phones = NULL, campaign_ids = NULL,
                                statuses = NULL, date_from = NULL,
                                date_to = NULL, page = NULL, page_size = NULL) {
  if (!is.data.frame(data)) {
    stop("disposition_summary: `data` must be a data frame.", call. = FALSE)
  }
  missing_cols <- setdiff(.DISPOSITION_READ_COLS, names(data))
  if (length(missing_cols) > 0L) {
    stop(sprintf("disposition_summary: `data` is missing column(s): %s.",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
  if (!is.null(statuses)) {
    bad <- setdiff(as.character(statuses), .DISPOSITION_CATEGORIES)
    if (length(bad) > 0L) {
      stop(sprintf("disposition_summary: unknown status(es): %s.",
                   paste(bad, collapse = ", ")), call. = FALSE)
    }
  }
  date_from <- .disposition_date_bound(date_from, "date_from")
  date_to <- .disposition_date_bound(date_to, "date_to")
  req <- NULL
  if (!is.null(phones)) {
    req <- unique(.disposition_normalize_phone(phones))
    req <- req[!is.na(req)]
  }

  d <- .disposition_filter(data, req, campaign_ids, date_from, date_to)
  summ <- if (nrow(d) == 0L) .disposition_never_contacted(character(0)) else .disposition_rollup(d)

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

#' Query the disposition dataset for a phone list (sample screening)
#'
#' Reads the disposition Parquet projection and returns \strong{one row per
#' phone} (see \code{\link{disposition_summary}}) -- the analyst-facing engine
#' for ad-hoc queries. For cleaning a Survey Manager's sample file in place, use
#' \code{\link{s160_disposition_screen}}.
#'
#' @param dataset Path to a disposition Parquet file (the phone-sorted read
#'   projection). Read with \pkg{nanoparquet}, projected to the query columns.
#' @inheritParams disposition_summary
#' @return A per-phone summary data frame (see \code{\link{disposition_summary}}).
#' @seealso \code{\link{disposition_summary}}, \code{\link{s160_disposition_screen}}
#' @export
s160_disposition_query <- function(dataset, phones = NULL, campaign_ids = NULL,
                                   statuses = NULL, date_from = NULL,
                                   date_to = NULL, page = NULL,
                                   page_size = NULL) {
  disposition_summary(.disposition_read_parquet(dataset), phones = phones,
                      campaign_ids = campaign_ids, statuses = statuses,
                      date_from = date_from, date_to = date_to,
                      page = page, page_size = page_size)
}

#' Screen a sample against the disposition dataset (annotate in place)
#'
#' The Survey Manager surface: takes a sample data frame (a phone column plus
#' whatever else -- strata, quota cells, ...) and returns it \strong{unchanged
#' with the disposition summary columns appended}, 1:1 with the input rows and
#' preserving the original phone formatting. The Manager then filters and writes
#' it (e.g. \code{subset(out, !ever_complete & !ever_terminated)}). Mirrors the
#' "append columns to my uploaded list" screening workflow.
#'
#' @param sample A data frame with a phone-number column.
#' @param dataset Path to a disposition Parquet file (the read projection).
#' @param phone_col Name of the phone column in \code{sample}
#'   (default \code{"phone"}).
#' @param campaign_ids,date_from,date_to Optional scoping of the disposition
#'   rows considered (see \code{\link{disposition_summary}}). No \code{statuses}
#'   or pagination here -- every sample row is returned.
#' @return \code{sample} with the columns \code{ever_contacted},
#'   \code{n_campaigns}, \code{ever_engaged}, \code{ever_opted_in},
#'   \code{ever_complete}, \code{ever_terminated}, \code{latest_disposition},
#'   \code{campaigns} appended (a never-matched / unparseable phone gets an
#'   all-\code{NA} block).
#' @seealso \code{\link{s160_disposition_query}}, \code{\link{disposition_summary}}
#' @export
s160_disposition_screen <- function(sample, dataset, phone_col = "phone",
                                    campaign_ids = NULL, date_from = NULL,
                                    date_to = NULL) {
  if (!is.data.frame(sample)) {
    stop("s160_disposition_screen: `sample` must be a data frame.",
         call. = FALSE)
  }
  if (!is.character(phone_col) || length(phone_col) != 1L ||
        !phone_col %in% names(sample)) {
    stop(sprintf("s160_disposition_screen: phone column %s not found in `sample`.",
                 deparse(phone_col)), call. = FALSE)
  }
  disposition_cols <- setdiff(.DISPOSITION_SUMMARY_COLS, "phone")
  clash <- intersect(disposition_cols, names(sample))
  if (length(clash) > 0L) {
    stop(sprintf(paste0("s160_disposition_screen: `sample` already has ",
                        "disposition column(s) [%s]; rename them first."),
                 paste(clash, collapse = ", ")), call. = FALSE)
  }
  summ <- disposition_summary(.disposition_read_parquet(dataset),
                              phones = sample[[phone_col]],
                              campaign_ids = campaign_ids,
                              date_from = date_from, date_to = date_to)
  idx <- match(.disposition_normalize_phone(sample[[phone_col]]), summ$phone)
  for (col in disposition_cols) sample[[col]] <- summ[[col]][idx]
  sample
}
