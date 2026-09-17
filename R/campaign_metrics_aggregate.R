# Campaign-metrics aggregation -- post-pull transforms for the consolidated
# per-campaign metrics projection (`campaign_all.parquet`). Parallels
# disposition_aggregate.R / disposition_read.R: campaign_metrics_pull() fetches
# the projection, campaign_metrics_records() reads it into analysis-ready cells,
# and campaign_metrics_summary() rolls those up to funnel counts + rates by any
# project dimension. funnel_rates() is the single definition of the funnel rates.

# tracker_* project dimensions, renamed to short names on the records frame.
.CAMPAIGN_METRICS_RENAME <- c(
  registration_id   = "tracker_registration_id",
  brand             = "tracker_brand",
  client            = "tracker_client",
  fielding_location = "tracker_fielding_location",
  mode              = "tracker_mode")

.CAMPAIGN_METRICS_DIMS   <- c("campaign_id", "date", "hour_local",
                              names(.CAMPAIGN_METRICS_RENAME))
.CAMPAIGN_METRICS_COUNTS <- c("n_sent", "n_engaged", "n_opted_in", "n_completed")

# Read the projection Parquet, projecting to the cell columns. Mirrors
# .disposition_read_parquet(): use nanoparquet col_select when the writer was
# duckdb (column pushdown), otherwise read and subset.
.campaign_metrics_read_parquet <- function(dataset) {
  if (!is.character(dataset) || length(dataset) != 1L || !nzchar(dataset)) {
    stop("`x` must be a single Parquet path or a data frame.", call. = FALSE)
  }
  if (!file.exists(dataset)) stop_not_found("campaign metrics projection", dataset)
  want <- c("campaign_id", "date", "hour_local",
            unname(.CAMPAIGN_METRICS_RENAME), .CAMPAIGN_METRICS_COUNTS)
  cb <- nanoparquet::read_parquet_info(dataset)$created_by
  duckdb <- length(cb) == 1L && !is.na(cb) && grepl("duckdb", cb, ignore.case = TRUE)
  if (duckdb) {
    cols <- intersect(want, nanoparquet::read_parquet_schema(dataset)$name)
    return(as.data.frame(nanoparquet::read_parquet(dataset, col_select = cols)))
  }
  d <- as.data.frame(nanoparquet::read_parquet(dataset))
  d[, intersect(want, names(d)), drop = FALSE]
}

# tracker_<x> -> <x>, only when the short name is not already present.
.campaign_metrics_rename <- function(d) {
  for (new in names(.CAMPAIGN_METRICS_RENAME)) {
    old <- .CAMPAIGN_METRICS_RENAME[[new]]
    if (old %in% names(d) && !(new %in% names(d))) names(d)[names(d) == old] <- new
  }
  d
}

#' Add the standard funnel rates to a counts frame
#'
#' The single definition of the Survey160 funnel rates, so every consumer
#' computes them the same way. Given a frame carrying the summed funnel counts,
#' appends three columns: \code{engagement_rate} (\code{engaged / sent}, a first
#' reply), \code{optin_engaged_rate} (\code{opted_in / engaged}, conversion among
#' the engaged), and \code{completion_rate} (\code{completed / sent}). A rate
#' with a zero denominator is \code{NA} (not \code{NaN}/\code{Inf}). Used by
#' \code{\link{campaign_metrics_summary}}, and usable on any counts frame.
#'
#' @param x A data frame with the four count columns named by the arguments
#'   below (defaults match \code{\link{campaign_metrics_records}}).
#' @param sent,engaged,opted_in,completed Column names of the funnel counts.
#' @param percent When \code{FALSE} (default) rates are proportions in
#'   \code{[0, 1]}; \code{TRUE} scales them to \code{0-100}.
#' @return \code{x} with \code{engagement_rate}, \code{optin_engaged_rate}, and
#'   \code{completion_rate} appended (same type as \code{x}).
#' @seealso \code{\link{campaign_metrics_summary}}
#' @examples
#' df <- data.frame(n_sent = c(1000, 500), n_engaged = c(80, 0),
#'                  n_opted_in = c(20, 0), n_completed = c(5, 0))
#' funnel_rates(df, percent = TRUE)
#' @export
funnel_rates <- function(x, sent = "n_sent", engaged = "n_engaged",
                         opted_in = "n_opted_in", completed = "n_completed",
                         percent = FALSE) {
  cols <- c(sent, engaged, opted_in, completed)
  miss <- setdiff(cols, names(x))
  if (length(miss)) {
    stop(sprintf("funnel_rates(): missing count columns: %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }
  mult  <- if (isTRUE(percent)) 100 else 1
  ratio <- function(num, den) ifelse(den > 0, num / den, NA_real_)
  x$engagement_rate    <- mult * ratio(x[[engaged]],   x[[sent]])
  x$optin_engaged_rate <- mult * ratio(x[[opted_in]],  x[[engaged]])
  x$completion_rate    <- mult * ratio(x[[completed]], x[[sent]])
  x
}

#' Read the campaign-metrics projection into analysis-ready cells
#'
#' Reads \code{campaign_all.parquet} (from \code{\link{campaign_metrics_pull}})
#' into a tidy frame: the \code{tracker_*} project dimensions renamed to short
#' names (\code{registration_id}, \code{brand}, \code{client},
#' \code{fielding_location}, \code{mode}), the funnel counts (\code{n_sent},
#' \code{n_engaged}, \code{n_opted_in}, \code{n_completed}), and the cell keys
#' (\code{campaign_id}, \code{date}, \code{hour_local}).
#'
#' The projection is stored at \code{(campaign_id, date, hour_local, segment,
#' threshold)} grain, and the funnel counts are \strong{replicated} across the
#' latency \code{threshold} (and segment) rows -- so summing the raw file
#' double-counts. With \code{dedup = TRUE} (default) the frame is collapsed to
#' \strong{one row per cell} (distinct rows over the returned columns, dropping
#' the replicating grain), which is the correct base for
#' \code{\link{campaign_metrics_summary}}. This is the step every consumer must
#' get right; do not sum the raw pull.
#'
#' @param x A path to a campaign-metrics Parquet (from
#'   \code{\link{campaign_metrics_pull}}), or an in-memory data frame already
#'   read (carrying either the \code{tracker_*} names or the short names). The
#'   four \code{n_*} count columns are required.
#' @param dedup Collapse the latency/threshold replication to one row per cell
#'   (default \code{TRUE}). \code{FALSE} returns the projected columns without
#'   collapsing (replicated rows remain).
#' @return A data frame of cells: \code{campaign_id}, \code{date},
#'   \code{hour_local}, the present project dimensions, and the four \code{n_*}
#'   counts.
#' @seealso \code{\link{campaign_metrics_pull}},
#'   \code{\link{campaign_metrics_summary}}, \code{\link{funnel_rates}}
#' @examples
#' \dontrun{
#' s160_gcs_init()
#' cells <- campaign_metrics_records(campaign_metrics_pull())
#' }
#' @export
campaign_metrics_records <- function(x, dedup = TRUE) {
  d <- if (is.data.frame(x)) as.data.frame(x) else .campaign_metrics_read_parquet(x)
  d <- .campaign_metrics_rename(d)
  miss <- setdiff(.CAMPAIGN_METRICS_COUNTS, names(d))
  if (length(miss)) {
    stop(sprintf("campaign metrics frame is missing funnel columns: %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }
  keep <- intersect(c(.CAMPAIGN_METRICS_DIMS, .CAMPAIGN_METRICS_COUNTS), names(d))
  d <- d[, keep, drop = FALSE]
  if (isTRUE(dedup)) d <- as.data.frame(dplyr::distinct(d))
  d
}

#' Summarize campaign metrics to funnel counts + rates by dimension
#'
#' Rolls the campaign-metrics cells up to summed funnel counts and the standard
#' funnel rates, grouped by any project dimension(s). Reads and de-duplicates
#' through \code{\link{campaign_metrics_records}} first, so it is safe to pass
#' either a projection \strong{path} or a frame -- the latency/threshold
#' replication is always collapsed before summing (no double counting).
#'
#' @param x A path to a campaign-metrics Parquet, or a data frame (raw or a
#'   \code{\link{campaign_metrics_records}} frame).
#' @param by Character vector of grouping columns, from the cell dimensions
#'   (\code{campaign_id} (default), \code{registration_id}, \code{brand},
#'   \code{client}, \code{fielding_location}, \code{mode}).
#' @param rates Append the funnel rates via \code{\link{funnel_rates}} (default
#'   \code{TRUE}).
#' @param percent Passed to \code{\link{funnel_rates}}: proportions (default) or
#'   \code{0-100}.
#' @return A data frame, one row per \code{by} group: the \code{by} columns,
#'   \code{campaigns} (distinct campaign count), the summed \code{n_sent},
#'   \code{n_engaged}, \code{n_opted_in}, \code{n_completed}, and (when
#'   \code{rates}) \code{engagement_rate}, \code{optin_engaged_rate},
#'   \code{completion_rate}.
#' @seealso \code{\link{campaign_metrics_records}}, \code{\link{funnel_rates}}
#' @examples
#' \dontrun{
#' s160_gcs_init()
#' path <- campaign_metrics_pull()
#' campaign_metrics_summary(path, by = "registration_id", percent = TRUE)
#' campaign_metrics_summary(path, by = c("brand", "mode"))
#' }
#' @export
campaign_metrics_summary <- function(x, by = "campaign_id", rates = TRUE,
                                     percent = FALSE) {
  d  <- campaign_metrics_records(x)
  by <- as.character(by)
  bad <- setdiff(by, names(d))
  if (length(bad)) {
    stop(sprintf("campaign_metrics_summary(): `by` column(s) not available: %s",
                 paste(bad, collapse = ", ")), call. = FALSE)
  }
  g <- dplyr::summarise(
    dplyr::group_by(d, dplyr::across(dplyr::all_of(by))),
    campaigns   = dplyr::n_distinct(.data$campaign_id),
    n_sent      = sum(.data$n_sent, na.rm = TRUE),
    n_engaged   = sum(.data$n_engaged, na.rm = TRUE),
    n_opted_in  = sum(.data$n_opted_in, na.rm = TRUE),
    n_completed = sum(.data$n_completed, na.rm = TRUE),
    .groups = "drop")
  g <- as.data.frame(g)
  if (isTRUE(rates)) g <- funnel_rates(g, percent = percent)
  g
}
