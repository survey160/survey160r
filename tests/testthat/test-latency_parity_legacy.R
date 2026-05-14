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

# Build a (respondent x question) timestamp grid. Each respondent gets a per-
# segment latency vector designed to land in a specific bucket in the legacy
# cascade (below 1 / 1-3 / 3-5 / 5-10 / over 10).
.build_parity_data <- function() {
  base_day <- "2026-01-10"
  deltas <- list(
    r1 = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),   # all <= 1
    r2 = c(0.5, 2.0, 0.5, 0.5, 0.5, 0.5, 0.5),   # one in (1,3]
    r3 = c(0.5, 0.5, 4.0, 0.5, 0.5, 0.5, 0.5),   # one in (3,5]
    r4 = c(0.5, 0.5, 0.5, 7.0, 0.5, 0.5, 0.5),   # one in (5,10]
    r5 = c(0.5, 0.5, 0.5, 0.5, 12.0, 0.5, 0.5),  # one > 10
    r6 = c(2.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
  )
  questions <- .parity_questions
  rows <- list()
  for (rid in names(deltas)) {
    intro_hour <- 18 + (which(rid == names(deltas)) %% 4) # 18..21 UTC
    intro_script <- as.POSIXct(
      sprintf("%s %02d:00:00", base_day, intro_hour), tz = "UTC"
    )
    row <- list(
      campaignid = 1L,
      userid = rid,
      id.intro.finalText = "Yes",
      id.intro.scriptDate = intro_script
    )
    prev_batch <- intro_script + 30
    row[["id.intro.batchDate"]] <- prev_batch
    for (i in seq_along(questions[-1])) {
      q_next <- questions[i + 1]
      next_script <- prev_batch + deltas[[rid]][i] * 60
      row[[sprintf("id.%s.scriptDate", q_next)]] <- next_script
      if (q_next != "close") {
        # close has no batchDate (matches the legacy script convention).
        next_batch <- next_script + 30
        row[[sprintf("id.%s.batchDate", q_next)]] <- next_batch
        prev_batch <- next_batch
      }
    }
    rows[[rid]] <- row
  }
  to_iso <- function(x) {
    if (inherits(x, "POSIXct")) {
      paste0(format(x, "%Y-%m-%d %H:%M:%OS6", tz = "UTC"), "Z")
    } else {
      x
    }
  }
  all_cols <- unique(unlist(lapply(rows, names)))
  out <- lapply(all_cols, function(col) {
    vapply(rows, function(r) {
      if (is.null(r[[col]])) NA_character_ else as.character(to_iso(r[[col]]))
    }, character(1))
  })
  names(out) <- all_cols
  df <- as.data.frame(out, stringsAsFactors = FALSE)
  df$campaignid <- as.integer(df$campaignid)
  df
}

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
    ),
    texting_windows = list(
      list(date = "2026-01-10", start_hour = 16, end_hour = 24)
    ),
    reports = list(time_bucket = "day")
  )
}

# --- Parity tests ----------------------------------------------------------

test_that("legacy parity: pct_le matches per segment per threshold", {
  data <- .build_parity_data()
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
  cons <- result$consolidated

  for (i in seq_len(length(qs) - 1)) {
    seg <- sprintf("%s→%s", qs[i], qs[i + 1])
    for (j in seq_along(thresholds)) {
      t <- thresholds[j]
      new_val <- cons$pct_le[cons$segment == seg & cons$threshold_min == t]
      expect_length(new_val, 1)
      expect_equal(new_val, unname(legacy_pct[[i]][j]), tolerance = 1e-9,
                   info = sprintf("segment=%s threshold=%d", seg, t))
    }
  }
})

test_that("legacy parity: respondent cascade matches over-threshold bucket pcts", {
  data <- .build_parity_data()
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
  cons <- result$consolidated
  worst <- unique(cons[, c("threshold_min", "pct_resp_worst_gt")])
  worst <- worst[order(worst$threshold_min), ]

  expect_equal(worst$pct_resp_worst_gt,
               unname(legacy_pct_worst_gt),
               tolerance = 1e-9)
})
