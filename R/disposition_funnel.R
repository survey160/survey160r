# Funnel-count rollup over the disposition dataset. The disposition data is one
# row per (phone, campaign_id), contacted-only, so a recipient is one row and the
# funnel flags (engaged / opted_in / completed / ineligible / refused) are 0/1 per
# recipient. This accessor rolls those per-recipient flags up to funnel COUNTS
# grouped by arbitrary dimension(s) -- carrier by default -- and (optionally) the
# recipient's latest disposition_date, so "how many sends / opt-ins / completes per
# carrier" is one call. It is the disposition sibling of latency_funnel(): there,
# the latency consolidated table carries a denormalised (segment x threshold) fan-out
# that must be collapsed before counting; here each recipient is already one row, so
# the reduction is a straight group-and-sum. data.table does the grouping because the
# read projection is ~140M rows, where a dplyr group_by does not scale (see the note
# in R/latency_aggregate.R).

# Resolve `x` (an in-memory frame or a Parquet path) to an in-memory data frame,
# projected to `want_cols` on the path read. Split out of disposition_funnel() to
# keep its cyclomatic complexity down.
.disposition_funnel_data <- function(x, want_cols) {
  if (is.data.frame(x)) {
    return(x)
  }
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    return(.disposition_read_parquet(x, columns = want_cols))
  }
  stop_s160(paste("`x` must be a disposition Parquet path (a single string)",
                  "or an in-memory disposition data frame."),
            fn = "disposition_funnel")
}

# Typed zero-row funnel frame: the group columns keep their input types, the count
# columns are integer, the rate columns double. Lets disposition_funnel() return
# early on empty input without running the data.table type-probe on empty groups.
.disposition_funnel_empty <- function(data, group_cols) {
  out <- data[integer(0), group_cols, drop = FALSE]
  for (col in c("n_sent", "n_engaged", "n_opted_in", "n_completed",
                "n_ineligible", "n_refused")) {
    out[[col]] <- integer(0)
  }
  for (col in c("pct_engaged", "pct_opted_in", "pct_completed")) {
    out[[col]] <- numeric(0)
  }
  rownames(out) <- NULL
  out
}

#' Roll the disposition dataset up to funnel counts by dimension
#'
#' The disposition dataset is one row per \code{(phone, campaign_id)},
#' contacted-only, with 0/1 funnel flags per recipient. \code{disposition_funnel()}
#' groups those rows by one or more dimensions (\code{by}, e.g. \code{carrier}) and
#' -- when \code{grain = "day"} -- by \code{disposition_date} (the recipient's
#' latest activity date), returning the funnel COUNTS per group: \code{n_sent},
#' \code{n_engaged}, \code{n_opted_in}, \code{n_completed}, \code{n_ineligible},
#' \code{n_refused}, plus the send-anchored rates \code{pct_engaged} /
#' \code{pct_opted_in} / \code{pct_completed}.
#'
#' Because the disposition grain is already one row per recipient, "how many per
#' carrier" is a straight group-and-sum -- there is no denormalised fan-out to
#' collapse (contrast \code{\link{latency_funnel}}, which first reduces the latency
#' consolidated table's segment x threshold repetition). \code{n_sent} is the count
#' of contacted recipients in the group. A \code{carrier} the uploaded list did not
#' supply is \code{NA}, kept as its own group (the "unknown carrier" bucket); this
#' function does not fold carrier aliases (e.g. Metro PCS into T-Mobile) -- do that
#' in the caller. Off-channel (\code{t2w_external}) completes are unknowable, so
#' \code{completed} is \code{NA} on those rows; a group whose completes are
#' \emph{all} \code{NA} returns \code{n_completed = NA} (not a false \code{0}),
#' matching the disposition readers.
#'
#' @param x Either a path to a disposition Parquet file (the read projection, e.g.
#'   from \code{\link{disposition_pull}}) or an in-memory disposition data frame
#'   (from \code{\link{disposition_records}}). A path is read with \pkg{nanoparquet},
#'   projected to the columns this rollup needs. Pre-filter (by campaign / date)
#'   with \code{\link{disposition_records}} first when you do not want the whole
#'   projection.
#' @param by Character vector of grouping column name(s). Default \code{"carrier"};
#'   any stored dimension works (\code{campaign_id}, \code{survey_mode},
#'   \code{tracker_client}, ...). Every name must be present in \code{x}.
#' @param grain One of \code{"day"} (default) or \code{"all"}. \code{"day"} adds
#'   \code{disposition_date} to the grouping (one row per \code{(by..., date)});
#'   \code{"all"} collapses the date axis (one row per \code{by...}).
#' @return A data frame, one row per group: the \code{by} column(s), then
#'   \code{disposition_date} (only when \code{grain = "day"}), then \code{n_sent},
#'   \code{n_engaged}, \code{n_opted_in}, \code{n_completed}, \code{n_ineligible},
#'   \code{n_refused}, and the rates \code{pct_engaged}, \code{pct_opted_in},
#'   \code{pct_completed} (each count / \code{n_sent} * 100, \code{NA} when
#'   \code{n_sent} is 0). Sorted by the grouping columns.
#' @seealso \code{\link{disposition_records}} (the raw rows this rolls up),
#'   \code{\link{disposition_summary}} (the per-phone rollup),
#'   \code{\link{latency_funnel}} (the latency-side sibling)
#' @examples
#' records <- data.frame(
#'   phone = c("5551112222", "5553334444", "5555556666", "5557778888"),
#'   campaign_id = c(101L, 101L, 101L, 101L),
#'   sent = c(1L, 1L, 1L, 1L),
#'   engaged = c(1L, 1L, 0L, 1L),
#'   opted_in = c(1L, 0L, 0L, 1L),
#'   completed = c(1L, 0L, 0L, 0L),
#'   ineligible = c(0L, 0L, 0L, 0L),
#'   refused = c(0L, 1L, 0L, 0L),
#'   carrier = c("AT&T", "AT&T", "Verizon", NA),
#'   disposition_date = as.Date(c("2026-01-10", "2026-01-10",
#'                                "2026-01-11", "2026-01-11")),
#'   stringsAsFactors = FALSE
#' )
#' disposition_funnel(records)                 # by carrier, per disposition_date
#' disposition_funnel(records, grain = "all")  # by carrier, collapsed
#' @export
disposition_funnel <- function(x, by = "carrier", grain = c("day", "all")) {
  grain <- match.arg(grain)
  if (!is.character(by) || length(by) < 1L || anyNA(by) || !all(nzchar(by))) {
    stop_s160("`by` must be a non-empty character vector of column name(s).",
              fn = "disposition_funnel")
  }
  group_cols <- unique(c(by, if (grain == "day") "disposition_date"))
  need_funnel <- c("sent", "engaged", "opted_in", "completed")
  want_cols <- unique(c(group_cols, need_funnel, "refused", "ineligible"))

  data <- .disposition_funnel_data(x, want_cols)

  missing_cols <- setdiff(c(group_cols, need_funnel), names(data))
  if (length(missing_cols) > 0L) {
    stop_s160(sprintf("input is missing required column(s): %s",
                      paste(missing_cols, collapse = ", ")),
              fn = "disposition_funnel")
  }
  if (nrow(data) == 0L) {
    return(.disposition_funnel_empty(data, group_cols))
  }
  # refused / ineligible (the terminal split) are optional: a projection produced
  # before survey160r 0.51.0 lacks them -> default 0 (matches the disposition rollup).
  if (!"refused" %in% names(data)) data$refused <- rep(0L, nrow(data))
  if (!"ineligible" %in% names(data)) data$ineligible <- rep(0L, nrow(data))

  # Bare column names below are data.table (j) references, not free variables;
  # NULL-bind them so R CMD check / lintr do not flag them.
  sent <- engaged <- opted_in <- completed <- ineligible <- refused <- NULL
  # Count that stays NA when EVERY value in the group is NA (an all-off-channel
  # group's completes) rather than collapsing to a false 0; a real 0 is a group
  # with some non-NA rows that sum to zero.
  csum <- function(v) {
    if (all(is.na(v))) NA_integer_ else as.integer(sum(v, na.rm = TRUE))
  }
  dt <- data.table::as.data.table(data)
  agg <- dt[, list(
    n_sent       = as.integer(sum(sent == 1L, na.rm = TRUE)),
    n_engaged    = csum(engaged),
    n_opted_in   = csum(opted_in),
    n_completed  = csum(completed),
    n_ineligible = csum(ineligible),
    n_refused    = csum(refused)
  ), by = group_cols]
  out <- as.data.frame(agg, stringsAsFactors = FALSE)

  out$pct_engaged <- safe_pct(out$n_engaged, out$n_sent)
  out$pct_opted_in <- safe_pct(out$n_opted_in, out$n_sent)
  out$pct_completed <- safe_pct(out$n_completed, out$n_sent)

  out <- out[do.call(order, out[group_cols]), , drop = FALSE]
  rownames(out) <- NULL
  out[, c(group_cols, "n_sent", "n_engaged", "n_opted_in", "n_completed",
          "n_ineligible", "n_refused", "pct_engaged", "pct_opted_in",
          "pct_completed"), drop = FALSE]
}
