# Diagnostics assembly per spec §3.3. Pure functions over the
# already-built latency_frame.

# Build the diagnostics list per spec §3.3.
build_diagnostics <- function(frame, n_respondents_in, parse_failures,
                              config_hash) {
  n_clamped <- attr(frame, "n_clamped") %||% 0L
  if (nrow(frame) == 0) {
    return(.empty_diagnostics(n_respondents_in, n_clamped, parse_failures,
                              config_hash))
  }
  # Per-respondent worst (max) valid delta, one value per respondent that has
  # at least one valid segment -- a GForce max in data.table. The dplyr
  # group_by/summarise this replaces evaluated `max(na.rm = TRUE)` per
  # respondent group in R (~6 s on a 2M-row frame; this is ~0.1 s).
  worst <- .worst_delta_by_respondent(frame$respondent_index, frame$delta_min)
  used <- length(worst)
  # uniqueN() hashes the integer column once; length(unique()) was over half
  # of this function's time on a 4.4M-row frame.
  total_resp_observed <- data.table::uniqueN(frame$respondent_index)

  list(
    n_respondents_in = n_respondents_in,
    n_respondents_used = used,
    n_respondents_no_valid_segment = total_resp_observed - used,
    n_segments_total = nrow(frame),
    n_segments_na = sum(is.na(frame$delta_min)),
    n_segments_na_by_reason = .na_reason_counts(frame$na_reason),
    n_negative_latencies_clamped = n_clamped,
    parse_failures_per_column = parse_failures,
    config_hash = config_hash,
    algorithm_version = .algorithm_version,
    respondent_summary = .respondent_summary(worst)
  )
}

# Diagnostics when no respondent reached the latency frame (every row removed
# by dedupe / date_filter, or an empty input). Nobody was measured, so
# n_respondents_no_valid_segment is 0: the same base as the non-empty path,
# where it counts the frame's respondents without a valid segment. Rows the
# filters removed are n_respondents_in - (used + no_valid) on both paths.
# Shared by build_diagnostics() and the compact-path
# .build_diagnostics_streamed() so the two cannot drift.
.empty_diagnostics <- function(n_in, n_clamped, parse_failures, config_hash) {
  list(
    n_respondents_in = n_in,
    n_respondents_used = 0L,
    n_respondents_no_valid_segment = 0L,
    n_segments_total = 0L,
    n_segments_na = 0L,
    n_segments_na_by_reason = list(parse_failure = 0L,
                                   missing_endpoint = 0L,
                                   chain_break = 0L),
    n_negative_latencies_clamped = n_clamped,
    parse_failures_per_column = parse_failures,
    config_hash = config_hash,
    algorithm_version = .algorithm_version,
    respondent_summary = .respondent_summary(numeric(0))
  )
}

# respondent_summary from the per-respondent worst deltas. The cascade
# percentages are over the *measured* respondents (those with at least one
# valid delta), so n_respondents = length(worst) and the three buckets
# (<= 5, (5, 10], > 10 minutes) partition it. This matches the consolidated
# cascade (aggregate_worst_cascade: pct_resp_worst_gt over the bucket's
# measured respondents, `worst > t`) and the legacy-parity definition; dividing
# by every observed respondent instead deflates the buckets and makes
# n_respondents * pct / 100 stop recovering a count. With nobody measured the
# percentages are undefined -> NA. One copy for both latency paths: the
# denominator bug this replaces lived in duplicated inline blocks.
.respondent_summary <- function(worst) {
  used <- length(worst)
  if (used == 0L) {
    return(list(n_respondents = 0L, pct_clean_at_5min = NA_real_,
                pct_worst_in_5_to_10 = NA_real_, pct_worst_over_10 = NA_real_))
  }
  n_over_5 <- sum(worst > 5)
  n_over_10 <- sum(worst > 10)
  list(
    n_respondents = used,
    pct_clean_at_5min = 100 * ((used - n_over_5) / used),
    pct_worst_in_5_to_10 = 100 * ((n_over_5 - n_over_10) / used),
    pct_worst_over_10 = 100 * (n_over_10 / used)
  )
}

# The per-respondent worst (max) delta over the valid (non-NA) segments only:
# one unnamed numeric per respondent with >= 1 valid delta, in no particular
# order (every consumer reduces it with sum() / length()). Shared by
# build_diagnostics() and the compact-path .build_diagnostics_streamed().
.worst_delta_by_respondent <- function(respondent_index, delta_min) {
  r <- d <- NULL
  valid <- !is.na(delta_min)
  if (!any(valid)) return(numeric(0))
  per_resp <- data.table::data.table(r = respondent_index[valid],
                                     d = delta_min[valid])
  per_resp[, list(d = max(d)), by = r][["d"]]
}

# The three NA-reason counts of a frame's na_reason column as the named list the
# diagnostics carry: one match() over the column and a tabulate(), where three
# `== "<reason>"` string comparisons each scanned the whole long frame.
.na_reason_counts <- function(na_reason) {
  counts <- tabulate(match(na_reason, .na_reason_levels), nbins = 3L)
  list(parse_failure = counts[[1L]], missing_endpoint = counts[[2L]],
       chain_break = counts[[3L]])
}
