# Diagnostics assembly per spec §3.3. Pure functions over the
# already-built latency_frame.

# Build the diagnostics list per spec §3.3.
build_diagnostics <- function(frame, n_respondents_in, parse_failures,
                              config_hash) {
  n_clamped <- attr(frame, "n_clamped") %||% 0L
  if (nrow(frame) == 0) {
    return(list(
      n_respondents_in = n_respondents_in,
      n_respondents_used = 0L,
      n_respondents_no_valid_segment = n_respondents_in,
      n_segments_total = 0L,
      n_segments_na = 0L,
      n_segments_na_by_reason = list(parse_failure = 0L,
                                     missing_endpoint = 0L,
                                     chain_break = 0L),
      n_negative_latencies_clamped = n_clamped,
      parse_failures_per_column = parse_failures,
      config_hash = config_hash,
      algorithm_version = .algorithm_version,
      respondent_summary = list(
        n_respondents = 0L,
        pct_clean_at_5min = NA_real_,
        pct_worst_in_5_to_10 = NA_real_,
        pct_worst_over_10 = NA_real_
      )
    ))
  }
  valid <- !is.na(frame$delta_min)
  # Per-respondent worst (max) valid delta, one value per respondent that has
  # at least one valid segment -- a GForce max in data.table. The dplyr
  # group_by/summarise this replaces evaluated `max(na.rm = TRUE)` per
  # respondent group in R (~6 s on a 2M-row frame; this is ~0.1 s).
  worst <- .worst_delta_by_respondent(frame$respondent_index, frame$delta_min)
  used <- length(worst)
  total_resp_observed <- length(unique(frame$respondent_index))
  no_valid <- total_resp_observed - used
  total_segments <- nrow(frame)
  na_segments <- sum(!valid)

  # Cascade percentages are over the *measured* respondents (those with at
  # least one valid Delta), matching respondent_summary$n_respondents = used,
  # the consolidated cascade (aggregate_worst_cascade), and the legacy-parity
  # definition. Dividing by total_resp_observed instead would deflate the
  # buckets by the no-valid-segment fraction and make them sum to < 100%, so
  # that n_respondents * pct / 100 no longer recovers a respondent count.
  # When used == 0 the percentages are undefined -> NA (same as the empty-frame
  # path above).
  if (used > 0L) {
    pct_clean <- 100 * mean(worst <= 5)
    pct_5_10 <- 100 * mean(worst > 5 & worst <= 10)
    pct_over_10 <- 100 * mean(worst > 10)
  } else {
    pct_clean <- NA_real_
    pct_5_10 <- NA_real_
    pct_over_10 <- NA_real_
  }

  list(
    n_respondents_in = n_respondents_in,
    n_respondents_used = used,
    n_respondents_no_valid_segment = no_valid,
    n_segments_total = total_segments,
    n_segments_na = na_segments,
    n_segments_na_by_reason = list(
      parse_failure = sum(frame$na_reason == "parse_failure", na.rm = TRUE),
      missing_endpoint = sum(frame$na_reason == "missing_endpoint",
                             na.rm = TRUE),
      chain_break = sum(frame$na_reason == "chain_break", na.rm = TRUE)
    ),
    n_negative_latencies_clamped = n_clamped,
    parse_failures_per_column = parse_failures,
    config_hash = config_hash,
    algorithm_version = .algorithm_version,
    respondent_summary = list(
      n_respondents = used,
      pct_clean_at_5min = pct_clean,
      pct_worst_in_5_to_10 = pct_5_10,
      pct_worst_over_10 = pct_over_10
    )
  )
}

# The per-respondent worst (max) delta over the valid (non-NA) segments only:
# one unnamed numeric per respondent with >= 1 valid delta, in no particular
# order (every consumer reduces it with mean() / length()). Shared by
# build_diagnostics() and the compact-path .build_diagnostics_streamed().
.worst_delta_by_respondent <- function(respondent_index, delta_min) {
  r <- d <- NULL
  valid <- !is.na(delta_min)
  if (!any(valid)) return(numeric(0))
  per_resp <- data.table::data.table(r = respondent_index[valid],
                                     d = delta_min[valid])
  per_resp[, list(d = max(d)), by = r][["d"]]
}
