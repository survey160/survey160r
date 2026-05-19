# Legacy-parity gate. Locks the v2 algorithm to the four primitives that
# every legacy per-wave R script implemented inline (timestamp_diff,
# texting_hour_by_date, percent_below_thresholds_data, latency_indicator_vars)
# so a methodology drift from v2 to legacy can be caught in CI.
#
# The primitives below are reimplemented verbatim from the legacy scripts
# (modulo the load(.rda) / setwd boilerplate). Survey question names and
# project metadata are generic so this test file carries no client identity.
#
# The fixture is constructed to avoid the known divergence sources the v2
# algorithm intentionally fixes -- otherwise a parity test would have to
# encode "expected drift" tolerances and stop being a gate:
#   - no NA batchDates anywhere (so chain validity is a no-op)
#   - no negative segment deltas (so the v2 clamp is a no-op)
#   - every respondent has >=1 valid segment (so cascade denominators agree)

# --- Legacy primitives (verbatim) ------------------------------------------

.legacy_timestamp_diff <- function(df, start_var, end_var, unit = "mins") {
  start_time <- suppressWarnings(as.POSIXct(df[[start_var]],
                                            format = "%Y-%m-%d %H:%M:%S",
                                            tz = "GMT"))
  end_time <- suppressWarnings(as.POSIXct(df[[end_var]],
                                          format = "%Y-%m-%d %H:%M:%S",
                                          tz = "GMT"))
  as.numeric(difftime(end_time, start_time, units = unit))
}

.legacy_texting_hour_by_date <- function(time_gmt, date_windows) {
  time_gmt <- as.POSIXct(time_gmt, tz = "GMT")
  message_date <- as.Date(time_gmt)
  message_hour <- as.numeric(format(time_gmt, "%H"))
  result <- integer(length(time_gmt))
  for (i in seq_len(nrow(date_windows))) {
    date_i <- as.Date(date_windows$date[i])
    start_i <- date_windows$start_hour[i]
    end_i <- date_windows$end_hour[i]
    match_idx <- which(message_date == date_i &
                         message_hour >= start_i &
                         message_hour < end_i)
    result[match_idx] <- 1
  }
  result
}

.legacy_percent_below_thresholds <- function(time_var, flag_var, thresholds) {
  keep <- !is.na(time_var) & !is.na(flag_var) & flag_var == 1
  valid <- time_var[keep]
  vapply(thresholds, function(t) mean(valid <= t) * 100, numeric(1))
}

.legacy_latency_indicator_vars <- function(latency_vars, thresholds) {
  out <- sapply(thresholds, function(t) {
    as.integer(apply(latency_vars, 1,
                     function(row) any(row > t, na.rm = TRUE)))
  })
  colnames(out) <- paste0("over_", thresholds)
  as.data.frame(out)
}

# --- Synthetic fixture (generic) -------------------------------------------

# Generic 8-question flow. Mirrors the structural shape every legacy script
# operates on (>= 2 questions, one terminal close with no batchDate).
.parity_questions <- c("intro", "q1", "q2", "q3", "q4", "q5", "q6", "close")

# Fixture: tests/testthat/fixtures/synthetic_parity.csv. Six respondents,
# each with a per-segment latency vector landing in a specific cascade
# bucket (below 1 / 1-3 / 3-5 / 5-10 / over 10). See load_synthetic_parity().

.parity_config <- function() {
  list(
    project_id = 1L,
    campaign_id = 1L,
    # UTC mirrors the legacy script's tz="GMT" so window evaluation matches
    # cell-for-cell. (Production configs use the operator's local tz.)
    field_timezone = "UTC",
    flow = list(questions = .parity_questions),
    filters = list(
      population = 'id.intro.finalText == "Yes"',
      campaign_id_column = "campaignid",
      respondent_id_column = NULL,
      date_filter = "2026-01-10"
    )
  )
}

# --- Parity tests ----------------------------------------------------------

test_that("legacy parity: pct_le matches per segment per threshold", {
  data <- load_synthetic_parity()
  config <- .parity_config()
  thresholds <- c(1, 3, 5, 10)
  date_windows <- data.frame(date = "2026-01-10",
                             start_hour = 16, end_hour = 24)

  # Build legacy-style per-segment Δ and window-flag columns.
  qs <- .parity_questions
  legacy <- data
  diffs <- list()
  flags <- list()
  for (i in seq_len(length(qs) - 1)) {
    prior <- qs[i]
    nxt <- qs[i + 1]
    diffs[[paste0(prior, "->", nxt)]] <- .legacy_timestamp_diff(
      legacy,
      sprintf("id.%s.batchDate", prior),
      sprintf("id.%s.scriptDate", nxt)
    )
    flags[[prior]] <- .legacy_texting_hour_by_date(
      legacy[[sprintf("id.%s.batchDate", prior)]], date_windows
    )
  }
  legacy_pct <- lapply(seq_len(length(qs) - 1), function(i) {
    .legacy_percent_below_thresholds(
      diffs[[i]], flags[[qs[i]]], thresholds
    )
  })

  result <- latency_report(data, config)
  # Compare against the day rollup rows in the output (hour_local = NA),
  # which carry the legacy single-value-per-(segment, threshold) shape.
  day_rows <- result$consolidated[is.na(result$consolidated$hour_local), ]

  for (i in seq_len(length(qs) - 1)) {
    seg <- sprintf("%s→%s", qs[i], qs[i + 1])
    for (j in seq_along(thresholds)) {
      t <- thresholds[j]
      new_val <- day_rows$pct_le[day_rows$segment == seg &
                                   day_rows$threshold_min == t]
      expect_length(new_val, 1)
      expect_equal(new_val, unname(legacy_pct[[i]][j]), tolerance = 1e-9,
                   info = sprintf("segment=%s threshold=%d", seg, t))
    }
  }
})

test_that("legacy parity: respondent cascade matches over-threshold bucket pcts", {
  data <- load_synthetic_parity()
  config <- .parity_config()
  thresholds <- c(1, 3, 5, 10)

  qs <- .parity_questions
  diffs <- as.data.frame(lapply(seq_len(length(qs) - 1), function(i) {
    .legacy_timestamp_diff(
      data,
      sprintf("id.%s.batchDate", qs[i]),
      sprintf("id.%s.scriptDate", qs[i + 1])
    )
  }))
  indicators <- .legacy_latency_indicator_vars(diffs, thresholds)
  legacy_pct_worst_gt <- vapply(indicators, mean, numeric(1)) * 100

  result <- latency_report(data, config)
  # Cascade can't be rolled up from hour-grained consolidated rows (respondents
  # who appear in multiple hours are counted in each hour's denominator). Derive
  # the wave-level cascade from the per-respondent latency_frame instead --
  # this is the same recipe spec §2 specifies.
  frame <- result$latency_frame
  worst_by_resp <- dplyr::summarise(
    dplyr::group_by(frame, .data$respondent_index),
    worst = suppressWarnings(max(.data$delta_min, na.rm = TRUE)),
    .groups = "drop"
  )
  worst_by_resp <- worst_by_resp[is.finite(worst_by_resp$worst), , drop = FALSE]
  derived_pct_worst_gt <- vapply(thresholds, function(t) {
    100 * mean(worst_by_resp$worst > t)
  }, numeric(1))

  expect_equal(derived_pct_worst_gt,
               unname(legacy_pct_worst_gt),
               tolerance = 1e-9)
})
