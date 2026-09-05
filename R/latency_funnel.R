# Safe funnel reduction over a latency_report()/latency_run() consolidated
# frame. The consolidated table is a wide, denormalised, multi-grain analytics
# table: the funnel anchors (n_sent / n_engaged / n_opted_in / n_completed) are
# computed once per (campaign_id, date, hour_local) bucket and then repeated
# across every (segment x threshold_min) row of that bucket, AND the frame
# carries two grains at once -- hour rows (hour_local 0-23) plus a day-rollup
# row per bucket (hour_local = NA). Summing an anchor column over the raw frame
# therefore multi-counts by (n_segments x n_thresholds x 2 grains). This
# accessor performs the one correct reduction so no consumer has to re-derive
# it (see R/latency_aggregate.R for how the fan-out is built).

#' Reduce a consolidated latency table to the funnel counts
#'
#' \code{latency_run()} / \code{latency_report()} return a \code{consolidated}
#' table whose funnel anchor columns (\code{n_sent}, \code{n_engaged},
#' \code{n_opted_in}, \code{n_completed}) are denormalised: one value per
#' \code{(campaign_id, date, hour_local)} bucket is repeated across every
#' \code{(segment, threshold_min)} row of that bucket, and the frame holds two
#' grains at once (per-hour rows plus a day-rollup row with
#' \code{hour_local = NA}). Summing an anchor column over the raw frame
#' multi-counts. This accessor collapses the \code{segment x threshold} fan-out
#' and selects a single grain, returning one row per bucket with the funnel
#' anchors carried through unchanged.
#'
#' Pass the whole thing across campaigns: the reduction keys on
#' \code{campaign_id}, so a fleet-wide \code{consolidated} (many campaigns)
#' reduces correctly. When a \code{consolidated} carries more than one fleet
#' pass (differing \code{run_at_utc}) per campaign, de-duplicate to the pass you
#' want before calling; this function does not pick a pass for you.
#'
#' @param consolidated The \code{consolidated} element of a
#'   \code{latency_run()} / \code{latency_report()} result (a data frame; a
#'   \code{data.table} or tibble is accepted and coerced).
#' @param grain One of \code{"day"} (default) or \code{"hour"}. \code{"day"}
#'   reads the day-rollup rows (\code{hour_local = NA}) and returns one row per
#'   \code{(campaign_id, date)}. \code{"hour"} reads the per-hour rows and
#'   returns one row per \code{(campaign_id, date, hour_local)} for a
#'   time-of-day view. The two grains are never mixed (that would double-count);
#'   summing an anchor across the \code{"hour"} result equals the matching
#'   \code{"day"} value.
#' @return A data frame with \code{campaign_id}, \code{date},
#'   \code{hour_local} (only when \code{grain = "hour"}), and the funnel anchors
#'   \code{n_sent}, \code{n_engaged}, \code{n_opted_in}, \code{n_completed},
#'   sorted by campaign then date (then hour). For a
#'   \code{"t2w_external"} campaign \code{n_completed} is \code{NA} (completion
#'   is off-platform), inherited from the source frame.
#' @examples
#' data <- data.frame(
#'   campaignid = c(1L, 1L),
#'   id.intro.finalText = c("Yes", "Yes"),
#'   id.intro.scriptDate = c("2026-01-26 21:00:00Z", "2026-01-26 22:05:00Z"),
#'   id.intro.batchDate  = c("2026-01-26 21:00:30Z", "2026-01-26 22:05:20Z"),
#'   id.q1.scriptDate    = c("2026-01-26 21:01:00Z", "2026-01-26 22:06:00Z"),
#'   id.q1.batchDate     = c("2026-01-26 21:01:20Z", "2026-01-26 22:06:15Z"),
#'   id.close.scriptDate = c("2026-01-26 21:02:00Z", "2026-01-26 22:07:00Z"),
#'   check.names = FALSE, stringsAsFactors = FALSE
#' )
#' config <- latency_build_config(1L, data, field_timezone = "America/New_York")
#' result <- latency_report(data, config, run_at = as.POSIXct("2026-01-01", tz = "UTC"))
#' latency_funnel(result$consolidated)               # one row per (campaign, date)
#' latency_funnel(result$consolidated, grain = "hour")  # one row per hour
#' @export
latency_funnel <- function(consolidated, grain = c("day", "hour")) {
  grain <- match.arg(grain)
  if (!is.data.frame(consolidated)) {
    stop_s160(paste0("`consolidated` must be a data frame -- pass the ",
                     "`consolidated` element of a latency_run() result."),
              fn = "latency_funnel")
  }
  consolidated <- as.data.frame(consolidated, stringsAsFactors = FALSE)

  required <- c("campaign_id", "date", "hour_local", "segment_index",
                "threshold_min", "n_sent", "n_engaged", "n_opted_in",
                "n_completed")
  missing_cols <- setdiff(required, names(consolidated))
  if (length(missing_cols) > 0L) {
    stop_s160(paste0("`consolidated` is missing column(s): ",
                     paste(missing_cols, collapse = ", "),
                     ". Pass the `consolidated` element of a latency_run() ",
                     "result."),
              fn = "latency_funnel")
  }

  anchors <- c("n_sent", "n_engaged", "n_opted_in", "n_completed")
  out_cols <- c("campaign_id", "date",
                if (grain == "hour") "hour_local", anchors)
  if (nrow(consolidated) == 0L) {
    return(empty_funnel(consolidated, out_cols))
  }

  # Pick one grain: the day-rollup rows (hour_local NA) or the per-hour rows.
  is_day <- is.na(consolidated$hour_local)
  rows <- consolidated[if (grain == "day") is_day else !is_day, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(empty_funnel(consolidated, out_cols))
  }

  # Collapse the (segment x threshold) fan-out to one row per bucket. The
  # anchors are identical across that fan-out, so de-duplicating on the bucket
  # key keeps a single representative -- without assuming which threshold or
  # segment values the frame carries (the anchors pass through un-summed).
  bucket_key <- c("campaign_id", "date", if (grain == "hour") "hour_local")
  rows <- rows[!duplicated(rows[, bucket_key, drop = FALSE]), , drop = FALSE]
  out <- rows[, out_cols, drop = FALSE]

  order_cols <- c("campaign_id", "date", if (grain == "hour") "hour_local")
  out <- out[do.call(order, out[order_cols]), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Zero-row funnel frame with the requested columns, typed from the source
# consolidated so the empty and non-empty returns share a schema.
empty_funnel <- function(consolidated, out_cols) {
  out <- consolidated[integer(0), out_cols, drop = FALSE]
  rownames(out) <- NULL
  out
}
