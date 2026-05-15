# Aggregation of the per-respondent x per-segment frame to the consolidated
# Parquet table (spec §3.1). Cells are
# (campaign_id, date, hour_local, segment, threshold_min). For day buckets
# hour_local is NA on every row.
#
# Orchestrator: aggregate_consolidated()
# Pieces:
#   prepare_bucketed_frame()   -- alias date and apply day/hour bucketing
#   aggregate_totals()         -- per-bucket distinct respondent count
#   aggregate_worst_cascade()  -- per-threshold respondent worst-Δ cascade
#   aggregate_segment_cells()  -- per-(bucket, segment, threshold) cell rows
#   assemble_consolidated()    -- joins, schema-shaped data.frame, sort

# Grouping keys used by every aggregation in this file. Kept as a single
# vector so a future column addition (e.g. operator_id) only edits one place.
.bucket_keys <- c("campaign_id", "date", "hour_local")

# 100 * num / denom, or NA when denom is missing or non-positive. The pattern
# shows up everywhere a "percent of X" column is built; centralised so the
# zero-denominator NA convention can't drift between cells, cascade, and
# diagnostics.
safe_pct <- function(num, denom) {
  ifelse(!is.na(denom) & denom > 0, 100 * num / denom, NA_real_)
}

aggregate_consolidated <- function(frame, config, cfg_hash, run_at,
                                   src_csv_hash = NA_character_) {
  project_id <- as.integer(config$project_id)
  if (nrow(frame) == 0) {
    return(empty_consolidated(project_id, cfg_hash, run_at))
  }

  thresholds <- UNIVERSAL_THRESHOLDS_MIN
  bucketed <- prepare_bucketed_frame(frame, config$reports$time_bucket)

  totals <- aggregate_totals(bucketed)
  cascade <- aggregate_worst_cascade(bucketed, thresholds)
  cells <- aggregate_segment_cells(bucketed, thresholds)

  assemble_consolidated(cells, totals, cascade,
                        project_id = project_id,
                        cfg_hash = cfg_hash,
                        run_at = run_at,
                        src_csv_hash = src_csv_hash)
}

# Copy `frame` and rename/blank-out columns to match the bucketing the
# downstream summaries group on. Day buckets collapse hour_local to NA on
# every row so the dplyr group_by yields one row per (campaign, date, NA).
prepare_bucketed_frame <- function(frame, bucket) {
  out <- frame
  if (bucket == "day") {
    out$hour_local <- NA_integer_
  }
  out$date <- out$segment_date_local
  out
}

# Total respondents per bucket -- the denominator for pct_resp_hit_gt.
# Defined as distinct respondent_index appearing in *any* segment for the
# bucket key (matches "all in-wave respondents" per spec).
aggregate_totals <- function(bucketed) {
  dplyr::summarise(
    dplyr::group_by(bucketed, .data$campaign_id, .data$date, .data$hour_local),
    .total_resp = dplyr::n_distinct(.data$respondent_index),
    .groups = "drop"
  )
}

# Per-bucket respondent latency cascade: for each threshold, how many
# respondents had at least one valid Δ in the bucket and what fraction of
# those had a *worst* Δ exceeding the threshold. This is the wave-level
# view the historical reports show.
aggregate_worst_cascade <- function(bucketed, thresholds) {
  worst <- dplyr::summarise(
    dplyr::group_by(
      dplyr::filter(bucketed, !is.na(.data$delta_min)),
      .data$campaign_id, .data$date, .data$hour_local,
      .data$respondent_index
    ),
    worst_delta = suppressWarnings(max(.data$delta_min, na.rm = TRUE)),
    .groups = "drop"
  )
  # Drop respondents whose worst is non-finite. Shouldn't happen given the
  # filter above, but guards against dplyr edge cases that emit -Inf.
  worst <- worst[is.finite(worst$worst_delta), , drop = FALSE]

  chunks <- lapply(thresholds, function(t) cascade_chunk(worst, t))
  do.call(rbind, chunks)
}

# Single-threshold cascade row builder. Extracted so aggregate_worst_cascade
# is purely the lapply skeleton + rbind.
cascade_chunk <- function(worst, t) {
  chunk <- dplyr::summarise(
    dplyr::group_by(worst, .data$campaign_id, .data$date, .data$hour_local),
    n_respondents = dplyr::n(),
    n_worst_over = sum(.data$worst_delta > t),
    .groups = "drop"
  )
  chunk$threshold_min <- as.integer(t)
  chunk$pct_resp_worst_gt <- safe_pct(chunk$n_worst_over, chunk$n_respondents)
  chunk[, c(.bucket_keys, "threshold_min", "n_respondents",
            "pct_resp_worst_gt")]
}

# Per-(bucket, segment, threshold) cell rows. n is the in-window valid count;
# pct_le and n_resp_over are derived from the same set.
aggregate_segment_cells <- function(bucketed, thresholds) {
  rows <- lapply(thresholds, function(t) segment_cells_chunk(bucketed, t))
  do.call(rbind, rows)
}

segment_cells_chunk <- function(bucketed, t) {
  cells <- dplyr::summarise(
    dplyr::group_by(
      bucketed,
      .data$campaign_id, .data$date, .data$hour_local,
      .data$segment, .data$segment_index
    ),
    n = sum(!is.na(.data$delta_min) & .data$in_window == 1L),
    n_le = sum(!is.na(.data$delta_min) & .data$in_window == 1L &
                 .data$delta_min <= t),
    n_resp_over = dplyr::n_distinct(
      .data$respondent_index[!is.na(.data$delta_min) & .data$delta_min > t]
    ),
    .groups = "drop"
  )
  cells$threshold_min <- as.integer(t)
  cells$pct_le <- safe_pct(cells$n_le, cells$n)
  cells
}

# Join the three aggregations, stamp provenance, coerce to the final schema
# order, and sort. Returns the data.frame written to Parquet.
assemble_consolidated <- function(cells, totals, cascade,
                                  project_id, cfg_hash, run_at,
                                  src_csv_hash) {
  joined <- dplyr::left_join(cells, totals, by = .bucket_keys)
  # pct_resp_hit_gt is gated on `n > 0` (the cell has at least one valid
  # in-window Δ); without that gate, a cell with n=0 but a non-zero
  # n_resp_over from out-of-window segments would emit a misleading
  # percentage. safe_pct() handles the denominator side; we still need the
  # explicit `joined$n > 0` mask layered on top.
  joined$pct_resp_hit_gt <- ifelse(
    joined$n > 0,
    safe_pct(joined$n_resp_over, joined$.total_resp),
    NA_real_
  )
  # Cascade left-join can produce NA n_respondents / pct_resp_worst_gt for
  # a bucket where *every* segment was NA (parse failure, chain break, or
  # missing endpoint) so the worst-by-respondent step has no row for it.
  # The Arrow schema permits int32 NA so this round-trips cleanly;
  # downstream consumers should treat NA cascade columns as "no respondent
  # had any valid Δ in this bucket."
  joined <- dplyr::left_join(joined, cascade,
                             by = c(.bucket_keys, "threshold_min"))

  out <- data.frame(
    campaign_id = as.integer(joined$campaign_id),
    project_id = rep(project_id, nrow(joined)),
    date = joined$date,
    hour_local = joined$hour_local,
    segment = joined$segment,
    segment_index = as.integer(joined$segment_index),
    threshold_min = as.integer(joined$threshold_min),
    n = as.integer(joined$n),
    pct_le = joined$pct_le,
    pct_resp_hit_gt = joined$pct_resp_hit_gt,
    n_respondents = as.integer(joined$n_respondents),
    pct_resp_worst_gt = as.numeric(joined$pct_resp_worst_gt),
    algorithm_version = .algorithm_version,
    config_hash = cfg_hash,
    source_csv_hash = src_csv_hash %||% NA_character_,
    run_at_utc = run_at,
    run_by = NA_character_,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$campaign_id, out$date, out$hour_local,
                   out$segment_index, out$threshold_min), , drop = FALSE]
  rownames(out) <- NULL
  out
}

empty_consolidated <- function(project_id, cfg_hash, run_at) {
  data.frame(
    campaign_id = integer(0),
    project_id = integer(0),
    date = as.Date(character(0)),
    hour_local = integer(0),
    segment = character(0),
    segment_index = integer(0),
    threshold_min = integer(0),
    n = integer(0),
    pct_le = numeric(0),
    pct_resp_hit_gt = numeric(0),
    n_respondents = integer(0),
    pct_resp_worst_gt = numeric(0),
    algorithm_version = character(0),
    config_hash = character(0),
    source_csv_hash = character(0),
    run_at_utc = as.POSIXct(character(0), tz = "UTC"),
    run_by = character(0),
    stringsAsFactors = FALSE
  )
}
