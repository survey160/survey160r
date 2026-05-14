# Diagnostics assembly per spec §3.3. Pure functions over the
# already-built latency_frame.

# Build the diagnostics list per spec §3.3.
build_diagnostics <- function(frame, n_respondents_in, parse_failures,
                              windows_df, field_tz, config_hash) {
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
      n_out_of_window_dropped = 0L,
      parse_failures_per_column = parse_failures,
      windows_normalized_utc = windows_to_utc(windows_df, field_tz),
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
  by_resp <- dplyr::summarise(
    dplyr::group_by(frame, .data$respondent_index),
    has_valid = any(!is.na(.data$delta_min)),
    max_delta = suppressWarnings(max(.data$delta_min, na.rm = TRUE)),
    .groups = "drop"
  )
  used <- sum(by_resp$has_valid)
  total_resp_observed <- nrow(by_resp)
  no_valid <- total_resp_observed - used
  total_segments <- nrow(frame)
  na_segments <- sum(is.na(frame$delta_min))
  out_of_window <- sum(!is.na(frame$delta_min) & frame$in_window == 0L)

  worst <- by_resp$max_delta
  worst[!is.finite(worst)] <- NA_real_
  pct_clean <- 100 * mean(!is.na(worst) & worst <= 5)
  pct_5_10 <- 100 * mean(!is.na(worst) & worst > 5 & worst <= 10)
  pct_over_10 <- 100 * mean(!is.na(worst) & worst > 10)

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
    n_out_of_window_dropped = out_of_window,
    parse_failures_per_column = parse_failures,
    windows_normalized_utc = windows_to_utc(windows_df, field_tz),
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

windows_to_utc <- function(windows_df, field_tz) {
  if (is.null(windows_df) || nrow(windows_df) == 0) {
    return(data.frame(start_utc = as.POSIXct(character(0), tz = "UTC"),
                      end_utc = as.POSIXct(character(0), tz = "UTC")))
  }
  midnights <- as.POSIXct(format(windows_df$date), tz = field_tz)
  start_local <- midnights + windows_df$start_hour * 3600
  end_local <- midnights + windows_df$end_hour * 3600
  attr(start_local, "tzone") <- "UTC"
  attr(end_local, "tzone") <- "UTC"
  data.frame(start_utc = start_local, end_utc = end_local)
}
