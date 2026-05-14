# Aggregation of the per-respondent x per-segment frame to the consolidated
# Parquet table (spec §3.1). Cells are
# (campaign_id, date, hour_local, segment, threshold_min). For day buckets
# hour_local is NA on every row.

aggregate_consolidated <- function(frame, config, cfg_hash, run_at,
                                   src_csv_hash = NA_character_) {
  thresholds <- UNIVERSAL_THRESHOLDS_MIN
  bucket <- config$reports$time_bucket
  project_id <- as.integer(config$project_id)

  if (nrow(frame) == 0) {
    return(empty_consolidated(project_id, cfg_hash, run_at))
  }

  bucketed <- frame
  if (bucket == "day") {
    bucketed$hour_local <- NA_integer_
  }
  bucketed$date <- bucketed$segment_date_local

  # Total respondents per (campaign_id, date, hour_local) -- denominator for
  # pct_resp_hit_gt. Defined as distinct respondent_index appearing in any
  # segment for that bucket key (matches "all in-wave respondents" per spec).
  totals <- dplyr::summarise(
    dplyr::group_by(bucketed, .data$campaign_id, .data$date, .data$hour_local),
    .total_resp = dplyr::n_distinct(.data$respondent_index),
    .groups = "drop"
  )

  # Per-respondent worst Δ across all segments, per (campaign, date, hour).
  # Drives the n_respondents and pct_resp_worst_gt columns -- the wave-level
  # "respondent latency cascade" the historical reports show.
  worst_by_respondent <- dplyr::summarise(
    dplyr::group_by(
      dplyr::filter(bucketed, !is.na(.data$delta_min)),
      .data$campaign_id, .data$date, .data$hour_local,
      .data$respondent_index
    ),
    worst_delta = suppressWarnings(max(.data$delta_min, na.rm = TRUE)),
    .groups = "drop"
  )
  # Drop respondents whose worst is non-finite (no valid Δ at all in this
  # bucket -- shouldn't happen given the filter above, but guards against
  # edge cases where dplyr emits -Inf).
  worst_by_respondent <- worst_by_respondent[
    is.finite(worst_by_respondent$worst_delta), , drop = FALSE]
  cascade_rows <- list()
  for (t in thresholds) {
    chunk <- dplyr::summarise(
      dplyr::group_by(worst_by_respondent,
                      .data$campaign_id, .data$date, .data$hour_local),
      n_respondents = dplyr::n(),
      n_worst_over = sum(.data$worst_delta > t),
      .groups = "drop"
    )
    chunk$threshold_min <- as.integer(t)
    chunk$pct_resp_worst_gt <- ifelse(
      chunk$n_respondents > 0,
      100 * chunk$n_worst_over / chunk$n_respondents,
      NA_real_
    )
    cascade_rows[[length(cascade_rows) + 1L]] <-
      chunk[, c("campaign_id", "date", "hour_local", "threshold_min",
                "n_respondents", "pct_resp_worst_gt")]
  }
  cascade <- do.call(rbind, cascade_rows)

  rows <- list()
  for (t in thresholds) {
    cells <- dplyr::summarise(
      dplyr::group_by(
        bucketed,
        .data$campaign_id, .data$date, .data$hour_local,
        .data$segment, .data$segment_index
      ),
      n = sum(!is.na(.data$delta_min) & .data$in_window == 1L),
      pct_le = ifelse(
        sum(!is.na(.data$delta_min) & .data$in_window == 1L) > 0,
        100 * mean(.data$delta_min[!is.na(.data$delta_min) & .data$in_window == 1L] <= t),
        NA_real_
      ),
      n_resp_over = dplyr::n_distinct(
        .data$respondent_index[!is.na(.data$delta_min) & .data$delta_min > t]
      ),
      .groups = "drop"
    )
    cells$threshold_min <- as.integer(t)
    rows[[length(rows) + 1L]] <- cells
  }
  combined <- do.call(rbind, rows)
  joined <- dplyr::left_join(
    combined, totals,
    by = c("campaign_id", "date", "hour_local")
  )
  joined$pct_resp_hit_gt <- ifelse(
    !is.na(joined$.total_resp) & joined$.total_resp > 0 & joined$n > 0,
    100 * joined$n_resp_over / joined$.total_resp,
    NA_real_
  )
  # Cascade left-join can produce NA n_respondents / pct_resp_worst_gt for
  # a (campaign, date, hour_local) bucket where *every* segment was NA
  # (parse failure, chain break, or missing endpoint) so worst_by_respondent
  # has no row for it. The Arrow schema permits int32 NA so this round-trips
  # cleanly; downstream consumers should treat NA cascade columns as
  # "no respondent had any valid Δ in this bucket."
  joined <- dplyr::left_join(
    joined, cascade,
    by = c("campaign_id", "date", "hour_local", "threshold_min")
  )

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
