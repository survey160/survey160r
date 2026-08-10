# Disposition query -- the R-only consumer of the disposition Parquet dataset.
#
# The disposition dataset is one row per (phone, campaign_id), contacted-only,
# produced upstream (per-campaign Parquet + a phone-sorted read projection). A
# Survey Manager screens a fresh sample against it -- "which of these numbers
# have been contacted / completed / refused before?" -- to clean the list. So
# the query returns ONE ROW PER PHONE (Shape 2): the number's cross-campaign
# screening flags + its latest disposition, 1:1 with the uploaded sample.
#
# Reads via nanoparquet (a tiny, zero-dependency Parquet reader) -- the dataset
# is a single consolidated projection file, so a full read + in-R filter is
# sub-second and needs no heavy dataset/predicate-pushdown engine.

# Columns the query reads from the projection (project the read to just these).
.DQ_READ_COLS <- c("phone", "campaign_id", "started", "engaged", "opt_in",
                   "complete", "web_complete", "terminated", "date_closed_on")

# The derived disposition categories, in funnel order (least -> most advanced).
# `never_contacted` is only produced for screened phones absent from the data.
.DQ_CATEGORIES <- c("never_contacted", "non_response", "engaged", "opt_in",
                    "terminated", "complete", "web_complete")

# Digit-normalize a phone for matching: strip non-digits, then drop a leading US
# country code so an 11-digit "1NXXNXXXXXX" matches a stored 10-digit number.
# Blank/NA -> NA.
.dq_normalize_phone <- function(x) {
  x <- gsub("[^0-9]", "", as.character(x))
  x[!nzchar(x)] <- NA_character_
  eleven <- !is.na(x) & nchar(x) == 11L & startsWith(x, "1")
  x[eleven] <- substr(x[eleven], 2L, 11L)
  x
}

# Derive one disposition category per row from the 0/1 funnel flags, by funnel
# precedence (later assignment wins). A t2w_external row has complete = NA, so
# it falls through to the last known in-channel step -- never a false complete.
.dq_derive_disposition <- function(d) {
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
.dq_never_contacted <- function(phones) {
  n <- length(phones)
  data.frame(
    phone = phones, ever_contacted = rep(FALSE, n), n_campaigns = rep(0L, n),
    ever_engaged = rep(FALSE, n), ever_opted_in = rep(FALSE, n),
    ever_complete = rep(FALSE, n), ever_terminated = rep(FALSE, n),
    latest_disposition = rep("never_contacted", n),
    campaigns = rep(NA_character_, n), stringsAsFactors = FALSE
  )
}

# Read the projection (projected to .DQ_READ_COLS), normalize phone, and apply
# the row-scope filters (campaign_ids, date_closed_on range, requested phones).
.dq_read <- function(dataset, campaign_ids, date_from, date_to, keep_phones) {
  if (!is.character(dataset) || length(dataset) != 1L || !nzchar(dataset)) {
    stop("s160_disposition_query: `dataset` must be a single Parquet path.",
         call. = FALSE)
  }
  if (!file.exists(dataset)) {
    stop(sprintf("s160_disposition_query: dataset not found: %s", dataset),
         call. = FALSE)
  }
  d <- as.data.frame(
    nanoparquet::read_parquet(dataset, col_select = .DQ_READ_COLS))
  d$phone <- .dq_normalize_phone(d$phone)
  d <- d[!is.na(d$phone), , drop = FALSE]
  if (!is.null(keep_phones)) {
    d <- d[d$phone %in% keep_phones, , drop = FALSE]
  }
  if (!is.null(campaign_ids)) {
    d <- d[as.character(d$campaign_id) %in% as.character(campaign_ids), ,
           drop = FALSE]
  }
  if (!is.null(date_from)) {
    d <- d[!is.na(d$date_closed_on) & d$date_closed_on >= as.Date(date_from), ,
           drop = FALSE]
  }
  if (!is.null(date_to)) {
    d <- d[!is.na(d$date_closed_on) & d$date_closed_on <= as.Date(date_to), ,
           drop = FALSE]
  }
  d
}

# Roll the (phone, campaign) rows up to one row per phone. Rows are ordered so
# the latest campaign (max date_closed_on, NA last; tie -> max campaign_id) is
# first per phone, so latest_disposition is a plain first-of-group pick.
.dq_rollup <- function(d) {
  d$.dispo <- .dq_derive_disposition(d)
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
    latest_disposition = d$.dispo[first],
    campaigns = as.character(
      tapply(d$campaign_id, d$phone,
             function(x) paste(sort(unique(x)), collapse = ","))[ph]),
    stringsAsFactors = FALSE
  )
}

# 1-based page slice over the (phone-ordered) result. NULL page/size -> no-op.
.dq_paginate <- function(summ, page, page_size) {
  if (is.null(page) && is.null(page_size)) return(summ)
  ps <- if (is.null(page_size)) nrow(summ) else page_size
  pg <- if (is.null(page)) 1L else page
  ok <- function(x) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && x >= 1L && x %% 1 == 0
  }
  if (!ok(ps) || !ok(pg)) {
    stop("s160_disposition_query: `page` and `page_size` must be positive integers.",
         call. = FALSE)
  }
  from <- (pg - 1L) * ps + 1L
  if (from > nrow(summ)) return(summ[0L, , drop = FALSE])
  summ[seq.int(from, min(pg * ps, nrow(summ))), , drop = FALSE]
}

#' Query the disposition dataset for a phone list (sample screening)
#'
#' Reads the disposition Parquet projection and returns \strong{one row per
#' phone} summarizing that number's disposition across every campaign it appears
#' in -- the shape a Survey Manager uses to clean a fresh sample (drop prior
#' completes / refusals, flag never-contacted numbers). The underlying dataset
#' is one row per \code{(phone, campaign_id)}, contacted-only.
#'
#' @param dataset Path to a disposition Parquet file (the phone-sorted read
#'   projection). Read with \pkg{nanoparquet}; the read is projected to the
#'   columns the query needs.
#' @param phones Optional character vector of phone numbers to screen. When
#'   supplied, \strong{every} input number is returned -- never-contacted ones
#'   included, with \code{ever_contacted = FALSE} and
#'   \code{latest_disposition = "never_contacted"}. \code{NULL} summarizes every
#'   phone present in the dataset. Numbers are matched digit-normalized (a
#'   leading US \code{1} is dropped so 11-digit numbers match 10-digit ones).
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
#' @export
s160_disposition_query <- function(dataset, phones = NULL, campaign_ids = NULL,
                                   statuses = NULL, date_from = NULL,
                                   date_to = NULL, page = NULL,
                                   page_size = NULL) {
  if (!is.null(statuses)) {
    bad <- setdiff(as.character(statuses), .DQ_CATEGORIES)
    if (length(bad) > 0L) {
      stop(sprintf("s160_disposition_query: unknown status(es): %s.",
                   paste(bad, collapse = ", ")), call. = FALSE)
    }
  }
  req <- NULL
  if (!is.null(phones)) {
    req <- unique(.dq_normalize_phone(phones))
    req <- req[!is.na(req)]
  }

  d <- .dq_read(dataset, campaign_ids, date_from, date_to, req)
  summ <- if (nrow(d) == 0L) {
    .dq_never_contacted(character(0))
  } else {
    .dq_rollup(d)
  }

  # Screened phones absent from the (filtered) data come back as never-contacted.
  if (!is.null(req)) {
    missing <- setdiff(req, summ$phone)
    if (length(missing) > 0L) {
      summ <- rbind(summ, .dq_never_contacted(missing))
    }
  }

  if (!is.null(statuses)) {
    summ <- summ[summ$latest_disposition %in% as.character(statuses), ,
                 drop = FALSE]
  }
  summ <- summ[order(summ$phone), , drop = FALSE]
  summ <- .dq_paginate(summ, page, page_size)
  rownames(summ) <- NULL
  summ
}
