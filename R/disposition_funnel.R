# Funnel-count rollup over the disposition dataset. The disposition data is one
# row per (phone, campaign_id), contacted-only, so a recipient is one row and the
# funnel flags (engaged / opted_in / completed / ineligible / refused) are 0/1 per
# recipient. This accessor rolls those per-recipient flags up to funnel COUNTS +
# the standard funnel rates, grouped by arbitrary dimension(s) -- carrier by
# default -- and (optionally) the recipient's latest disposition_date. It is the
# disposition analogue of campaign_metrics_summary(): same shape (by / rates /
# percent, a campaigns count, funnel_rates() for the rates), over the disposition
# projection instead of the latency one, plus the disposition-only terminal split
# (n_ineligible / n_refused). data.table does the grouping because the read
# projection is ~140M rows, where a dplyr group_by does not scale (see the note in
# R/latency_aggregate.R).

# Resolve `x` (an in-memory frame or a Parquet path) to a BASE data frame,
# projected to `want_cols` on the path read. Coercing to a base data.frame here
# normalises any data.frame subclass (a `data.table` or tibble) to base `[` / `$`
# semantics for every downstream step -- notably `.disposition_funnel_empty()`,
# whose `data[integer(0), group_cols]` would otherwise be a data.table `j`
# expression rather than a column select -- and keeps the return type a plain data
# frame regardless of input. Split out of disposition_funnel() to keep its
# cyclomatic complexity down.
.disposition_funnel_data <- function(x, want_cols) {
  if (is.data.frame(x)) {
    return(as.data.frame(x))
  }
  if (is.character(x) && length(x) == 1L && nzchar(x)) {
    return(.disposition_read_parquet(x, columns = want_cols))
  }
  stop_s160(paste("`x` must be a disposition Parquet path (a single string)",
                  "or an in-memory disposition data frame."),
            fn = "disposition_funnel")
}

# Typed zero-row counts frame (the group columns keep their input types, the count
# columns are integer). Lets disposition_funnel() return early on empty input
# without running the data.table type-probe on empty groups; the rate columns are
# appended by funnel_rates() on the same path as the non-empty case.
.disposition_funnel_empty <- function(data, group_cols) {
  out <- data[integer(0), group_cols, drop = FALSE]
  for (col in c("campaigns", "n_sent", "n_engaged", "n_opted_in", "n_completed",
                "n_ineligible", "n_refused")) {
    out[[col]] <- integer(0)
  }
  rownames(out) <- NULL
  out
}

# The grouped funnel counts, in a form data.table computes in C across all
# groups at once (GForce): every aggregate is a bare sum() / uniqueN() / .N on
# a column, never an expression. The aggregated columns are referenced under
# internal names (`.cid`, `.sent1`, `.engaged`, ...) so a column that also
# appears in `by` (a caller grouping by `opted_in`) is still read as the full
# group vector, not the length-1 group key. The "all NA -> NA" rule of
# .disposition_funnel_count() is applied afterwards from a per-group NA count,
# computed only for a flag column that has any NA (the off-channel
# `completed`), so the common all-present column costs nothing extra.
#
# The previous form read each column through `.SD$` inside `j`, which
# evaluated j in R per group and materialised .SD each time -- fine at
# thousands of rows, the whole cost at the 140M-row projection.
.disposition_funnel_counts <- function(data, group_cols) {
  .cid <- .sent1 <- .engaged <- .opted_in <- .completed <- .ineligible <-
    .refused <- .na <- .N <- NULL
  flags <- c("engaged", "opted_in", "completed", "ineligible", "refused")
  work <- data[group_cols]
  work[[".cid"]] <- data[["campaign_id"]]
  work[[".sent1"]] <- data[["sent"]] == 1L
  for (f in flags) work[[paste0(".", f)]] <- data[[f]]
  dt <- data.table::as.data.table(work)
  agg <- dt[, list(
    campaigns = data.table::uniqueN(.cid),
    n_sent = sum(.sent1, na.rm = TRUE),
    n_engaged = sum(.engaged, na.rm = TRUE),
    n_opted_in = sum(.opted_in, na.rm = TRUE),
    n_completed = sum(.completed, na.rm = TRUE),
    n_ineligible = sum(.ineligible, na.rm = TRUE),
    n_refused = sum(.refused, na.rm = TRUE),
    .n_rows = .N
  ), by = group_cols]
  for (f in flags) {
    if (!anyNA(data[[f]])) next
    data.table::set(dt, j = ".na", value = is.na(data[[f]]))
    n_na <- dt[, list(.n_na = sum(.na)), by = group_cols][[".n_na"]]
    col <- paste0("n_", f)
    data.table::set(agg, i = which(n_na == agg[[".n_rows"]]), j = col,
                    value = NA_integer_)
  }
  out <- as.data.frame(agg, stringsAsFactors = FALSE)
  out[[".n_rows"]] <- NULL
  for (col in c("campaigns", "n_sent", "n_engaged", "n_opted_in", "n_completed",
                "n_ineligible", "n_refused")) {
    out[[col]] <- as.integer(out[[col]])
  }
  out
}

# Sum a 0/1 flag column within a group, but return NA (not a false 0) when EVERY
# value is NA -- the all-off-channel case for `completed` on t2w_external rows. A
# group with at least one non-NA value sums to a real count (0 included). Used in
# the data.table `j` below.
.disposition_funnel_count <- function(v) {
  if (all(is.na(v))) NA_integer_ else as.integer(sum(v, na.rm = TRUE))
}

#' Roll the disposition dataset up to funnel counts + rates by dimension
#'
#' The disposition dataset is one row per \code{(phone, campaign_id)},
#' contacted-only, with 0/1 funnel flags per recipient. \code{disposition_funnel()}
#' groups those rows by one or more dimensions (\code{by}, e.g. \code{carrier}) and
#' -- when \code{grain = "day"} -- by \code{disposition_date} (the recipient's
#' latest activity date), returning the funnel COUNTS per group (\code{campaigns},
#' \code{n_sent}, \code{n_engaged}, \code{n_opted_in}, \code{n_completed},
#' \code{n_ineligible}, \code{n_refused}) and, when \code{rates = TRUE}, the
#' standard funnel rates via \code{\link{funnel_rates}}.
#'
#' It is the disposition analogue of \code{\link{campaign_metrics_summary}} (same
#' \code{by} / \code{rates} / \code{percent} interface, a \code{campaigns} count,
#' and the shared \code{\link{funnel_rates}} definition), over the disposition
#' projection rather than the latency one, plus the disposition-only terminal split
#' (\code{n_ineligible} / \code{n_refused}) and a \code{grain} switch for the
#' \code{disposition_date} axis (the latency sibling instead takes the date as a
#' \code{by} column). Because the disposition grain is already one row per recipient,
#' "how many per carrier" is a straight group-and-sum -- there is no denormalised
#' fan-out to collapse (contrast \code{\link{latency_funnel}}). The disposition
#' dataset is contacted-only (\code{sent == 1}), so \code{n_sent} is the group's row
#' count; \code{campaigns} is its distinct campaign count.
#'
#' A \code{carrier} the uploaded list did not supply is \code{NA}, kept as its own
#' group (the "unknown carrier" bucket); this function does not fold carrier aliases
#' (e.g. Metro PCS into T-Mobile) -- do that in the caller. Off-channel
#' (\code{t2w_external}) completes are unknowable, so \code{completed} is \code{NA}
#' on those rows; a group whose completes are \emph{all} \code{NA} returns
#' \code{n_completed = NA} (and a \code{NA} completion rate), not a false \code{0}.
#'
#' @param x Either a path to a disposition Parquet file (the read projection, e.g.
#'   from \code{\link{disposition_pull}}) or an in-memory disposition data frame
#'   (from \code{\link{disposition_records}}). A path is read projected to the
#'   columns this rollup needs (with \pkg{duckdb} when installed, else
#'   \pkg{nanoparquet}). Pre-filter (by campaign / date)
#'   with \code{\link{disposition_records}} first when you do not want the whole
#'   projection.
#' @param by Character vector of grouping column name(s). Default \code{"carrier"};
#'   any stored dimension works (\code{survey_mode}, \code{tracker_client},
#'   \code{tracker_topic}, ...). Every name must be present in \code{x}.
#' @param grain One of \code{"day"} (default) or \code{"all"}. \code{"day"} adds
#'   \code{disposition_date} to the grouping (one row per \code{(by..., date)});
#'   \code{"all"} collapses the date axis (one row per \code{by...}).
#' @param rates Append the funnel rates via \code{\link{funnel_rates}} (default
#'   \code{TRUE}): \code{engagement_rate}, \code{opted_in_rate},
#'   \code{opted_in_engaged_rate}, \code{completion_rate}.
#' @param percent Passed to \code{\link{funnel_rates}}: proportions (default) or
#'   \code{0-100}.
#' @return A data frame, one row per group: the \code{by} column(s), then
#'   \code{disposition_date} (only when \code{grain = "day"}), \code{campaigns}, the
#'   counts \code{n_sent}, \code{n_engaged}, \code{n_opted_in}, \code{n_completed},
#'   \code{n_ineligible}, \code{n_refused}, and (when \code{rates}) the four
#'   \code{\link{funnel_rates}} columns. Sorted by the grouping columns.
#' @seealso \code{\link{campaign_metrics_summary}} (the latency-side analogue),
#'   \code{\link{funnel_rates}}, \code{\link{disposition_records}},
#'   \code{\link{disposition_summary}} (the per-phone rollup)
#' @examples
#' records <- data.frame(
#'   phone = c("5551112222", "5553334444", "5555556666", "5557778888"),
#'   campaign_id = c(101L, 101L, 102L, 102L),
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
#' disposition_funnel(records, grain = "all", percent = TRUE)
#' @export
disposition_funnel <- function(x, by = "carrier", grain = c("day", "all"),
                               rates = TRUE, percent = FALSE) {
  grain <- match.arg(grain)
  if (!is.character(by) || length(by) < 1L || anyNA(by) || !all(nzchar(by))) {
    stop_s160("`by` must be a non-empty character vector of column name(s).",
              fn = "disposition_funnel")
  }
  group_cols <- unique(c(by, if (grain == "day") "disposition_date"))
  need_funnel <- c("sent", "engaged", "opted_in", "completed")
  want_cols <- unique(c(group_cols, "campaign_id", need_funnel,
                        "refused", "ineligible"))

  data <- .disposition_funnel_data(x, want_cols)

  missing_cols <- setdiff(c(group_cols, "campaign_id", need_funnel), names(data))
  if (length(missing_cols) > 0L) {
    stop_s160(sprintf("input is missing required column(s): %s",
                      paste(missing_cols, collapse = ", ")),
              fn = "disposition_funnel")
  }
  # refused / ineligible (the terminal split) are optional: a projection produced
  # before survey160r 0.51.0 lacks them -> default 0 (matches the disposition rollup).
  if (!"refused" %in% names(data)) data$refused <- rep(0L, nrow(data))
  if (!"ineligible" %in% names(data)) data$ineligible <- rep(0L, nrow(data))

  out <- if (nrow(data) == 0L) {
    .disposition_funnel_empty(data, group_cols)
  } else {
    .disposition_funnel_counts(data, group_cols)
  }

  if (isTRUE(rates)) out <- funnel_rates(out, percent = percent)

  # Columns are already in canonical order -- data.table (and the empty helper)
  # emit the group columns first, then the counts in `list()` order, and
  # funnel_rates() appends the rates last -- so no positional re-select is needed
  # (which also avoids a duplicate-name select if a group column shared an output
  # name). Sort rows by the group keys with method = "radix" so the order is
  # locale-independent, matching disposition_records().
  ord <- do.call(order, c(as.list(out[group_cols]), list(method = "radix")))
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL
  out
}
