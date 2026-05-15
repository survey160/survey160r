# Coverage for R/latency_report.R: the pure latency_report() function.
# Uses the synthetic fixture under tests/testthat/fixtures/.

.load_synthetic <- function() {
  csv_path <- test_path("fixtures/synthetic.csv")
  data <- read.csv(csv_path, stringsAsFactors = FALSE, na.strings = c(""))
  # read.csv translates NA to NA, but we want empty strings in the original
  # cells to test na_if_blank end-to-end. Reread without na.strings.
  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  list(data = data, config = synthetic_config())
}

test_that("latency_report honors an explicit run_at override", {
  fx <- .load_synthetic()
  fixed <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")
  out <- latency_report(fx$data, fx$config, run_at = fixed)
  expect_equal(out$meta$run_at_utc, fixed)
  expect_true(all(out$consolidated$run_at_utc == fixed))
})

test_that("latency_report is deterministic on identical inputs", {
  fx <- .load_synthetic()
  r1 <- latency_report(fx$data, fx$config)
  r2 <- latency_report(fx$data, fx$config)
  # Strip run_at_utc (clock-dependent) before comparison.
  r1$consolidated$run_at_utc <- NULL
  r2$consolidated$run_at_utc <- NULL
  r1$meta$run_at_utc <- NULL
  r2$meta$run_at_utc <- NULL
  expect_identical(r1$consolidated, r2$consolidated)
  expect_identical(r1$meta$config_hash, r2$meta$config_hash)
  expect_identical(r1$diagnostics$config_hash, r2$diagnostics$config_hash)
})

test_that("latency_report drops respondents excluded by population_filter", {
  fx <- .load_synthetic()
  result <- latency_report(fx$data, fx$config)
  # 4 rows in CSV; 1 has finalText="No". Should yield 3 respondents in.
  expect_equal(result$diagnostics$n_respondents_in, 3L)
})

test_that("latency_report consolidated has hour and day rollup rows", {
  fx <- .load_synthetic()
  result <- latency_report(fx$data, fx$config)
  cons <- result$consolidated

  # Fixture: 3 respondents (r1, r2, r3) with intro.batchDate at distinct
  # UTC hours that map to 3 distinct ET hours (the configured field_tz).
  # Hour-grain rows: 3 hours x 3 segments x 4 thresholds = 36.
  # Day-rollup rows: 1 date x 3 segments x 4 thresholds = 12.
  # Total = 48. r3 (UTC 20:00 -> ET 15:00) is out of texting window 16-24.
  expect_equal(nrow(cons), 36L + 12L)

  hour_rows <- cons[!is.na(cons$hour_local), ]
  day_rows  <- cons[is.na(cons$hour_local), ]
  expect_equal(nrow(hour_rows), 36L)
  expect_equal(nrow(day_rows), 12L)

  expect_setequal(unique(cons$segment),
                  c("intro→q1", "q1→q2", "q2→close"))
  expect_setequal(unique(cons$threshold_min), c(1L, 3L, 5L, 10L))
  expect_setequal(unique(hour_rows$hour_local), c(15L, 16L, 17L))
  expect_equal(unique(as.character(cons$date)), "2026-01-26")
  expect_equal(unique(cons$campaign_id), 1L)
  expect_equal(unique(cons$project_id), 1L)
  expect_equal(unique(cons$algorithm_version), "2.0.0")

  # In-window n per hour cell is 1 for hours 16 and 17, 0 for hour 15.
  in_window_cells <- hour_rows[hour_rows$hour_local %in% c(16L, 17L), ]
  expect_true(all(in_window_cells$n == 1L))
  expect_true(all(hour_rows$n[hour_rows$hour_local == 15L] == 0L))

  # pct_le per single-respondent hour cell.
  r2_q1q2_t1 <- hour_rows[hour_rows$segment == "q1→q2" &
                            hour_rows$threshold_min == 1L &
                            hour_rows$hour_local == 17L, ]
  expect_equal(r2_q1q2_t1$pct_le, 0)         # r2's q1->q2 is 4 min > 1 min
  r2_q1q2_t5 <- hour_rows[hour_rows$segment == "q1→q2" &
                            hour_rows$threshold_min == 5L &
                            hour_rows$hour_local == 17L, ]
  expect_equal(r2_q1q2_t5$pct_le, 100)       # 4 min <= 5 min

  # Day rollup row: n is sum of in-window hour cells (2 = r1 + r2). pct_le
  # for q1->q2 at threshold 1 is 50% (r1 fast, r2 slow).
  day_q1q2_t1 <- day_rows[day_rows$segment == "q1→q2" &
                            day_rows$threshold_min == 1L, ]
  expect_equal(day_q1q2_t1$n, 2L)
  expect_equal(day_q1q2_t1$pct_le, 50)
})

test_that("hour-rollup of pct_le matches the day rows in the same output", {
  fx <- .load_synthetic()
  result <- latency_report(fx$data, fx$config)
  cons <- result$consolidated
  hour_rows <- cons[!is.na(cons$hour_local), ]
  day_rows  <- cons[is.na(cons$hour_local), ]

  rolled <- dplyr::summarise(
    dplyr::group_by(hour_rows, .data$segment, .data$threshold_min),
    pct_le_rolled = ifelse(sum(.data$n) > 0,
                           sum(.data$pct_le * .data$n, na.rm = TRUE) /
                             sum(.data$n),
                           NA_real_),
    n_rolled = sum(.data$n),
    .groups = "drop"
  )
  merged <- merge(
    day_rows[, c("segment", "threshold_min", "n", "pct_le")],
    rolled,
    by = c("segment", "threshold_min")
  )
  expect_equal(merged$n, merged$n_rolled, tolerance = 0)
  expect_equal(merged$pct_le, merged$pct_le_rolled, tolerance = 1e-9)
})

test_that("cascade columns are present and per-hour-respondent", {
  fx <- .load_synthetic()
  result <- latency_report(fx$data, fx$config)
  cons <- result$consolidated
  hour_rows <- cons[!is.na(cons$hour_local), ]
  day_rows  <- cons[is.na(cons$hour_local), ]

  # Hour grain: each of the 3 in-fixture respondents occupies a distinct
  # hour, so each hour bucket has exactly one respondent.
  expect_equal(unique(hour_rows$n_respondents), 1L)
  # Day rollup carries the wave-level distinct count: all 3 respondents.
  expect_equal(unique(day_rows$n_respondents), 3L)

  # Hour 17 (r2, worst delta = 4 min): pct_resp_worst_gt > 0 for
  # thresholds 1 and 3; 0 for 5 and 10.
  h17 <- unique(hour_rows[hour_rows$hour_local == 17L,
                     c("threshold_min", "pct_resp_worst_gt")])
  h17 <- h17[order(h17$threshold_min), ]
  expect_equal(h17$pct_resp_worst_gt, c(100, 100, 0, 0))

  # Hour 15 (r3): r3 is out-of-window for in_window aggregation but still
  # contributes to the cascade (cascade ignores in_window). r3's worst delta
  # is 0.5 min, so pct_resp_worst_gt is 0 at every threshold.
  h15 <- unique(hour_rows[hour_rows$hour_local == 15L,
                     c("threshold_min", "pct_resp_worst_gt")])
  expect_true(all(h15$pct_resp_worst_gt == 0))
})

test_that("latency_report dedupes by respondent_id keeping earliest intro", {
  fx <- .load_synthetic()
  # Add a duplicate of r1 with a later intro.scriptDate (should be dropped).
  data <- fx$data
  dup <- data[data$userid == "r1", ]
  dup$id.intro.scriptDate <- "2026-01-26 23:00:00.000000Z"
  dup$id.intro.batchDate <- "2026-01-26 23:00:30.000000Z"
  dup$id.q1.scriptDate <- "2026-01-26 23:01:00.000000Z"
  data <- rbind(data, dup)
  result <- latency_report(data, fx$config)
  expect_equal(result$diagnostics$n_respondents_used, 3L)
})

test_that("latency_report emits an empty consolidated when no respondents pass filter", {
  fx <- .load_synthetic()
  data <- fx$data
  data$id.intro.finalText <- "No"
  result <- latency_report(data, fx$config)
  expect_equal(nrow(result$consolidated), 0L)
  expect_equal(result$diagnostics$n_respondents_in, 0L)
})

test_that("latency_report emits both hour rows (0-23) and a day rollup row (NA) per cell", {
  fx <- .load_synthetic()
  cons <- latency_report(fx$data, fx$config)$consolidated
  expect_true(any(!is.na(cons$hour_local)),
              info = "expect at least one hour row")
  expect_true(any(is.na(cons$hour_local)),
              info = "expect at least one day rollup row")
  # Hour rows carry integer 0-23; day rows carry NA.
  expect_true(all(cons$hour_local[!is.na(cons$hour_local)] %in% 0:23))
})

test_that("latency_report's date_filter restricts to listed dates", {
  fx <- .load_synthetic()
  cfg <- fx$config
  cfg$filters$date_filter <- as.Date("2099-01-01")
  result <- latency_report(fx$data, cfg)
  expect_equal(nrow(result$consolidated), 0L)
})

test_that("latency_report errors on invalid population_filter", {
  fx <- .load_synthetic()
  cfg <- fx$config
  cfg$filters$population <- "this is not valid R syntax !!!"
  expect_error(latency_report(fx$data, cfg), "filters.population")
})

test_that("latency_report threads source_csv_hash from input attribute to consolidated", {
  fx <- .load_synthetic()
  data <- fx$data
  attr(data, "source_csv_hash") <- "sha256:fixture-from-attr"
  result <- latency_report(data, fx$config)
  expect_equal(unique(result$consolidated$source_csv_hash),
               "sha256:fixture-from-attr")
})

test_that("latency_report leaves source_csv_hash NA when no input attribute", {
  fx <- .load_synthetic()
  result <- latency_report(fx$data, fx$config)
  expect_true(all(is.na(result$consolidated$source_csv_hash)))
})

test_that("diagnostics na_by_reason: parse_failure counted on garbage timestamps", {
  fx <- .load_synthetic()
  data <- fx$data
  # Corrupt one batchDate cell with non-blank garbage on r1's q1 segment.
  data$id.q1.batchDate[data$userid == "r1"] <- "not-a-date"
  result <- latency_report(data, fx$config)
  reasons <- result$diagnostics$n_segments_na_by_reason
  # The q1->q2 segment for r1 should be classified parse_failure.
  expect_gte(reasons$parse_failure, 1L)
  # Conservation: sums of all reasons == total NA segments.
  total <- reasons$parse_failure + reasons$missing_endpoint + reasons$chain_break
  expect_equal(total, result$diagnostics$n_segments_na)
})

test_that("diagnostics na_by_reason: missing_endpoint counted on blank batchDate", {
  fx <- .load_synthetic()
  data <- fx$data
  # Blank out r2's q1.batchDate. Both endpoint cells were originally valid, so
  # this is a legitimate "respondent didn't advance" miss, not a parse failure.
  data$id.q1.batchDate[data$userid == "r2"] <- ""
  result <- latency_report(data, fx$config)
  reasons <- result$diagnostics$n_segments_na_by_reason
  expect_gte(reasons$missing_endpoint, 1L)
  expect_equal(reasons$parse_failure, 0L)
  total <- reasons$parse_failure + reasons$missing_endpoint + reasons$chain_break
  expect_equal(total, result$diagnostics$n_segments_na)
})

test_that("diagnostics na_by_reason: chain_break counted when a prior batchDate is NA", {
  fx <- .load_synthetic()
  data <- fx$data
  # Blank intro.batchDate for r1. Subsequent segments q1->q2 and q2->close keep
  # both endpoints valid, but chain validity nukes them because a prior
  # batchDate in the chain is NA. The intro->q1 segment itself is
  # missing_endpoint (its batch_prior is intro.batchDate which is now NA).
  data$id.intro.batchDate[data$userid == "r1"] <- ""
  result <- latency_report(data, fx$config)
  reasons <- result$diagnostics$n_segments_na_by_reason
  expect_gte(reasons$chain_break, 1L)
  expect_gte(reasons$missing_endpoint, 1L)
  expect_equal(reasons$parse_failure, 0L)
  total <- reasons$parse_failure + reasons$missing_endpoint + reasons$chain_break
  expect_equal(total, result$diagnostics$n_segments_na)
})

test_that("latency_report negative-clamp counter is exposed in diagnostics", {
  fx <- .load_synthetic()
  data <- fx$data
  # Force a negative on r1's q1->q2: set q2.scriptDate before q1.batchDate.
  # Scale carefully so flow_order check still passes (we only need <10% bad).
  data <- rbind(data, data[1, ])
  data$userid[nrow(data)] <- "r5"
  data$id.q2.scriptDate[nrow(data)] <- "2026-01-26 21:00:50.000000Z"
  result <- latency_report(data, fx$config)
  expect_gte(result$diagnostics$n_negative_latencies_clamped, 1L)
})
