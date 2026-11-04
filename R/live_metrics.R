# Live-metrics reader -- the hourly fleet-wide delivery counts.
#
# `live/metrics/dt=YYYY-MM-DD.parquet` is one file per ET day in the analytics
# bucket, rewritten hourly by the pipeline from the live phone lists: one row per
# (et_hour, carrier, brand, campaign_id, error_code) with n_sent / n_engaged.
# Recipients are bucketed by their FIRST send and carry their LATEST delivery
# code ("(none)" when there is none), so counts are per recipient, not per
# message or delivery attempt. Aggregate counts, no phone: not PII.
#
# live_metrics_pull() fetches the day files (a copy downloaded after the day's
# last rewrite is reused, any other is re-fetched), live_metrics_records()
# reads them into one frame, and live_metrics_error_rates() rolls that up to
# daily and cumulative error rates by carrier (or any other dimension). Bare
# names, like campaign_metrics_pull(): a pipeline-derived artifact, not a raw
# Survey160 source.

.LIVE_METRICS_COLS <- c("et_date", "et_hour", "carrier", "brand", "campaign_id",
                        "error_code", "n_sent", "n_engaged")
.LIVE_METRICS_DIMS <- c("carrier", "brand", "campaign_id", "error_code", "et_hour")
.LIVE_METRICS_NO_ERROR <- "(none)"
.LIVE_METRICS_TZ <- "America/New_York"

# Hours into the second ET day after a day by which the pipeline has written its
# last copy of that day (it rewrites today and yesterday every hour).
.LIVE_METRICS_FINAL_HOURS <- 2

# Today's date in ET, the day boundary the files use.
.live_metrics_today <- function() {
  as.Date(format(Sys.time(), tz = .LIVE_METRICS_TZ))
}

# Earliest day a bound may name: anything before is a mis-parsed date (e.g.
# "10/01/2026" read as year 10), which would otherwise loop over centuries.
.LIVE_METRICS_EARLIEST <- as.Date("2020-01-01")

# Coerce one optional day bound to a Date. A date-time is read in ET (the files'
# day boundary), a string only as YYYY-MM-DD; NULL passes through.
.live_metrics_day <- function(x, name, fn) {
  if (is.null(x)) return(NULL)
  d <- tryCatch(
    if (inherits(x, "POSIXt")) {
      as.Date(format(x, tz = .LIVE_METRICS_TZ))
    } else if (is.character(x)) {
      as.Date(x, tryFormats = "%Y-%m-%d")
    } else {
      as.Date(x)
    },
    error = function(e) NA)
  if (length(d) != 1L || is.na(d) || d < .LIVE_METRICS_EARLIEST) {
    stop_s160(sprintf("`%s` must be a single date (YYYY-MM-DD) on or after %s%s.",
                      name, .LIVE_METRICS_EARLIEST, .not_got(x)), fn = fn)
  }
  d
}

# Whether the cached copy of `day` at `local` is final: downloaded after the
# pipeline's last rewrite of that day. Keyed on when the copy was written, not
# on how old the day is, so a copy fetched while the day was still open is
# fetched again later instead of being kept as final.
.live_metrics_final <- function(local, day) {
  final_at <- as.POSIXct(format(day + 2), tz = .LIVE_METRICS_TZ) +
    .LIVE_METRICS_FINAL_HOURS * 3600
  file.exists(local) && file.mtime(local) >= final_at
}

#' Download the live delivery metrics for a range of days
#'
#' Pulls the daily live-metrics files -- the hourly, fleet-wide counts behind the
#' dashboard's Live tab -- from the environment's analytics bucket and returns
#' their local paths, ready for \code{\link{live_metrics_records}} or
#' \code{\link{live_metrics_error_rates}}. Each day is one file (ET calendar
#' day) covering every campaign in the live phone lists, with sends broken down
#' by hour, carrier, brand, campaign and delivery error code.
#'
#' Files are cached. The pipeline rewrites today's and yesterday's files every
#' hour, so a day is final at 02:00 ET two days after it (Nov 1 at 02:00 ET on
#' Nov 3). A cached copy downloaded after that is reused; any other copy is
#' fetched again on every pull. A day
#' with no file (no sends, or before the pipeline published it) is
#' skipped with a message. Authenticate first with \code{\link{s160_gcs_init}}()
#' (a fully cached range needs no auth).
#'
#' @param date_from,date_to First and last ET day to fetch, both included: a
#'   \code{Date}, a \code{"YYYY-MM-DD"} string, or a date-time (read as its ET
#'   day). \code{NULL} (default): \code{date_to} is today (ET) and
#'   \code{date_from} is 30 days before \code{date_to}.
#' @param env Environment: \code{"prod"} (default) or \code{"dev"}. There is no
#'   staging analytics tier.
#' @param dest Directory to save the files in. \code{NULL} (default) caches
#'   under \code{tools::R_user_dir("survey160r", "cache")}.
#' @param refresh \code{TRUE} re-downloads every day in the range, final or not.
#' @return A character vector of local Parquet paths, named by ET day
#'   (\code{"YYYY-MM-DD"}), for the days that have a file.
#' @seealso \code{\link{live_metrics_error_rates}},
#'   \code{\link{live_metrics_records}}, \code{\link{s160_gcs_init}}
#' @examples
#' \dontrun{
#' s160_gcs_init()
#' paths <- live_metrics_pull(date_from = "2026-10-01")
#' live_metrics_error_rates(paths)            # by carrier and day
#' }
#' @export
live_metrics_pull <- function(date_from = NULL, date_to = NULL,
                              env = .ENV_CHOICES, dest = NULL,
                              refresh = FALSE) {
  fn <- "live_metrics_pull"
  env <- match.arg(env)
  to <- .live_metrics_day(date_to, "date_to", fn) %||% .live_metrics_today()
  from <- .live_metrics_day(date_from, "date_from", fn) %||% (to - 30)
  if (from > to) {
    stop_s160(sprintf("`date_from` (%s) is after `date_to` (%s).", from, to),
              fn = fn)
  }
  .require_single_logical(refresh, "refresh", fn)
  loc <- resolve_dataset("live_metrics", env, fn = fn)
  prefix <- dataset_tiers("live_metrics")[[env]]$prefix

  if (is.null(dest)) dest <- tools::R_user_dir("survey160r", "cache")
  check_nonempty_string(dest, "dest", fn = fn)
  if (file.exists(dest) && !dir.exists(dest)) {
    stop_s160(sprintf("`dest` must be a directory; %s is a file.", dest), fn = fn)
  }
  dir <- file.path(dest, paste0(loc$bucket, ".live_metrics"))
  if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE)) {
    stop_failed("create the cache directory", dir, fn = fn)
  }

  days <- seq(from, to, by = "day")
  paths <- character(0)
  fetched <- 0L
  missing <- character(0)
  for (i in seq_along(days)) {
    day <- format(days[[i]])
    name <- sprintf("dt=%s.parquet", day)
    local <- file.path(dir, name)
    again <- refresh || !.live_metrics_final(local, days[[i]])
    got <- tryCatch(
      suppressMessages(.gcs_pull_cached(
        fn = fn, dest = local, bucket = loc$bucket, refresh = again,
        progress = FALSE, object_name = paste0(prefix, name),
        cache_suffix = ".parquet", noun = "live metrics day")),
      s160_not_found = function(e) NULL)
    if (is.null(got)) {
      missing <- c(missing, day)
      next
    }
    paths[[day]] <- got
    fetched <- fetched + as.integer(again)
  }
  message(sprintf("live metrics: %d day(s) (%d downloaded, %d cached)%s",
                  length(paths), fetched, length(paths) - fetched,
                  if (length(missing)) {
                    sprintf("; no file for %s", paste(missing, collapse = ", "))
                  } else {
                    ""
                  }))
  paths
}

#' Read live delivery metrics into one data frame
#'
#' Reads the day files from \code{\link{live_metrics_pull}} into one frame: one
#' row per ET day, hour, carrier, brand, campaign and delivery error code, with
#' the number of recipients sent and engaged.
#'
#' Counts are per recipient. A recipient is counted on the day and hour of
#' their \emph{first} send, under their \emph{latest} delivery error code
#' (\code{"(none)"} when there is none), so a rejection that a later retry
#' delivered no longer shows, and a recipient sent on several days counts once.
#'
#' @param x The paths returned by \code{\link{live_metrics_pull}}, or a data
#'   frame already read from them.
#' @param campaign_ids Optional campaign ids to keep; \code{NULL} keeps all.
#' @return A data frame with columns \code{et_date}, \code{et_hour},
#'   \code{carrier}, \code{brand}, \code{campaign_id}, \code{error_code},
#'   \code{n_sent} and \code{n_engaged}. A missing \code{carrier} or
#'   \code{brand} reads as \code{"(unknown)"}; a frame without
#'   \code{n_engaged} gets it as \code{NA} (no rate uses it).
#' @seealso \code{\link{live_metrics_error_rates}}
#' @examples
#' cells <- data.frame(
#'   et_date = as.Date("2026-11-01"), et_hour = 10L, carrier = "TMobile",
#'   brand = "B1", campaign_id = 1L, error_code = c("(none)", "4780"),
#'   n_sent = c(90L, 10L), n_engaged = c(20L, 0L))
#' live_metrics_records(cells)
#' @export
live_metrics_records <- function(x, campaign_ids = NULL) {
  fn <- "live_metrics_records"
  d <- if (is.data.frame(x)) {
    as.data.frame(x)
  } else {
    .live_metrics_read(x, fn)
  }
  # n_engaged is carried through but no rate needs it: optional, NA when absent.
  if (!"n_engaged" %in% names(d)) d$n_engaged <- rep(NA_integer_, nrow(d))
  miss <- setdiff(.LIVE_METRICS_COLS, names(d))
  if (length(miss)) {
    stop_s160(sprintf("`x` is missing column(s): %s (%s).",
                      paste(miss, collapse = ", "), .columns_hint(d, "x")),
              fn = fn)
  }
  d <- d[, .LIVE_METRICS_COLS, drop = FALSE]
  d$et_date <- as.Date(d$et_date)
  # Counts read back from a CSV may be factors or strings.
  for (col in c("n_sent", "n_engaged")) {
    if (!is.numeric(d[[col]])) d[[col]] <- as.numeric(as.character(d[[col]]))
  }
  for (col in c("carrier", "brand")) {
    v <- as.character(d[[col]])
    v[is.na(v) | !nzchar(v)] <- "(unknown)"
    d[[col]] <- v
  }
  d$error_code <- as.character(d$error_code)
  d$error_code[is.na(d$error_code)] <- .LIVE_METRICS_NO_ERROR
  if (!is.null(campaign_ids)) {
    d <- d[d$campaign_id %in% campaign_ids, , drop = FALSE]
  }
  rownames(d) <- NULL
  d
}

# Read and stack the day files; an empty vector is an empty frame.
.live_metrics_read <- function(paths, fn) {
  if (!is.character(paths)) {
    stop_s160(sprintf("`x` must be the paths from live_metrics_pull() or a data frame%s.",
                      .not_got(paths)), fn = fn)
  }
  gone <- paths[!file.exists(paths)]
  if (length(gone)) {
    stop_not_found("live metrics file", gone[[1L]], fn = fn, hint = paste0(
      "live_metrics_pull() downloads the day files and returns their paths. ",
      .path_hint(gone[[1L]]) %||% ""))
  }
  if (!length(paths)) {
    empty <- data.frame(et_date = as.Date(character(0)), et_hour = integer(0),
                        carrier = character(0), brand = character(0),
                        campaign_id = integer(0), error_code = character(0),
                        n_sent = integer(0), n_engaged = integer(0),
                        stringsAsFactors = FALSE)
    return(empty)
  }
  parts <- lapply(paths, function(p) {
    cols <- intersect(.LIVE_METRICS_COLS, nanoparquet::read_parquet_schema(p)$name)
    as.data.frame(nanoparquet::read_parquet(p, col_select = cols))
  })
  as.data.frame(dplyr::bind_rows(parts))
}

#' Daily and cumulative delivery error rates by carrier
#'
#' Rolls the live metrics up to one row per ET day and carrier (or any other
#' \code{by} dimension), with the day's error rate and the cumulative rate from
#' the group's first day in \code{x} through that day. This answers "how is each
#' carrier's error rate moving, across every ongoing survey?".
#'
#' A recipient counts as an error when their latest delivery code is anything
#' other than \code{"(none)"}. Rates are per recipient sent (see
#' \code{\link{live_metrics_records}} for what that means for retries). Each
#' code in \code{codes} also gets its own count and rates, so a single carrier
#' rejection such as T-Mobile's 4780 can be followed apart from the rest. A rate
#' with no sends is \code{NA}.
#'
#' @param x The paths from \code{\link{live_metrics_pull}}, or a frame from
#'   \code{\link{live_metrics_records}}.
#' @param by Dimension(s) to break the days down by: any of \code{"carrier"}
#'   (default), \code{"brand"}, \code{"campaign_id"}, \code{"error_code"},
#'   \code{"et_hour"}, or \code{NULL} for one fleet-wide row per day.
#' @param codes Delivery error codes to count separately (default
#'   \code{"4780"}); \code{character(0)} for none.
#' @param campaign_ids Optional campaign ids to keep; \code{NULL} keeps all.
#' @param percent \code{TRUE} scales the rates to 0-100.
#' @return A data frame sorted by \code{by} then \code{et_date}, with
#'   \code{et_date}, the \code{by} columns, \code{n_sent}, \code{n_error},
#'   \code{error_rate}, then \code{cum_sent}, \code{cum_error} and
#'   \code{cum_error_rate}; and for each code \code{c} in \code{codes},
#'   \code{n_c}, \code{rate_c} and \code{cum_rate_c}.
#' @seealso \code{\link{live_metrics_pull}}, \code{\link{live_metrics_records}},
#'   \code{vignette("delivery-errors")}
#' @examples
#' cells <- data.frame(
#'   et_date = as.Date(c("2026-11-01", "2026-11-01", "2026-11-02")),
#'   et_hour = 10L, carrier = "TMobile", brand = "B1", campaign_id = 1L,
#'   error_code = c("(none)", "4780", "(none)"),
#'   n_sent = c(90L, 10L, 100L), n_engaged = 0L)
#' live_metrics_error_rates(cells)
#' @export
live_metrics_error_rates <- function(x, by = "carrier", codes = "4780",
                                     campaign_ids = NULL, percent = FALSE) {
  fn <- "live_metrics_error_rates"
  d <- live_metrics_records(x, campaign_ids = campaign_ids)
  by <- unique(as.character(by))
  bad <- setdiff(by, .LIVE_METRICS_DIMS)
  if (length(bad)) {
    stop_s160(sprintf("`by` must be among %s; got %s.",
                      paste(.LIVE_METRICS_DIMS, collapse = ", "),
                      paste(bad, collapse = ", ")), fn = fn)
  }
  codes <- unique(as.character(codes))
  if (anyNA(codes) || !all(nzchar(codes))) {
    stop_s160(sprintf("`codes` must be non-empty error codes such as \"4780\"%s.",
                      .not_got(codes)), fn = fn)
  }
  .require_single_logical(percent, "percent", fn)
  mult <- if (percent) 100 else 1
  ratio <- function(num, den) ifelse(den > 0, mult * num / den, NA_real_)

  d$n_error <- ifelse(d$error_code == .LIVE_METRICS_NO_ERROR, 0L, d$n_sent)
  for (code in codes) {
    d[[paste0("n_", code)]] <- ifelse(d$error_code == code, d$n_sent, 0L)
  }
  # paste0("n_", character(0)) is "n_", not character(0): guard the no-codes case.
  counts <- c("n_sent", "n_error", if (length(codes)) paste0("n_", codes))
  g <- dplyr::summarise(
    dplyr::group_by(d, dplyr::across(dplyr::all_of(c(by, "et_date")))),
    dplyr::across(dplyr::all_of(counts), function(v) sum(v, na.rm = TRUE)),
    .groups = "drop")
  # Running totals per `by` group in date order. dplyr groups keep an NA key as
  # its own group (interaction() / ave() would reset it every row).
  g <- dplyr::arrange(g, dplyr::across(dplyr::all_of(c(by, "et_date"))))
  g <- dplyr::mutate(
    dplyr::group_by(g, dplyr::across(dplyr::all_of(by))),
    dplyr::across(dplyr::all_of(counts), cumsum, .names = "cum_{.col}"))
  g <- as.data.frame(dplyr::ungroup(g))
  names(g)[names(g) == "cum_n_error"] <- "cum_error"
  names(g)[names(g) == "cum_n_sent"] <- "cum_sent"

  g$error_rate <- ratio(g$n_error, g$n_sent)
  g$cum_error_rate <- ratio(g$cum_error, g$cum_sent)
  for (code in codes) {
    g[[paste0("rate_", code)]] <- ratio(g[[paste0("n_", code)]], g$n_sent)
    g[[paste0("cum_rate_", code)]] <- ratio(g[[paste0("cum_n_", code)]], g$cum_sent)
  }
  per_code <- unlist(lapply(codes, function(code) {
    paste0(c("n_", "rate_", "cum_rate_"), code)
  }))
  g[, c("et_date", by, "n_sent", "n_error", "error_rate", "cum_sent",
        "cum_error", "cum_error_rate", per_code), drop = FALSE]
}
