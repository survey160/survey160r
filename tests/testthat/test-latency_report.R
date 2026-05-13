# Coverage for R/latency_report.R: the pure latency_report() function.
# Uses the synthetic fixture under tests/testthat/fixtures/.

.load_synthetic <- function() {
  csv_path <- test_path("fixtures/synthetic.csv")
  cfg_path <- test_path("fixtures/synthetic_config.yaml")
  data <- read.csv(csv_path, stringsAsFactors = FALSE, na.strings = c(""))
  # read.csv translates NA to NA, but we want empty strings in the original
  # cells to test na_if_blank end-to-end. Reread without na.strings.
  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  list(data = data, config = read_config(cfg_path))
}

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

test_that("latency_report consolidated rows match hand-derived expectations", {
  fx <- .load_synthetic()
  result <- latency_report(fx$data, fx$config)
  cons <- result$consolidated

  expect_equal(nrow(cons), 3 * 4)  # 3 segments x 4 universal thresholds
  expect_setequal(unique(cons$segment),
                  c("intro→q1", "q1→q2", "q2→close"))
  expect_setequal(unique(cons$threshold_min), c(1L, 3L, 5L, 10L))
  expect_true(all(is.na(cons$hour_local)))  # day bucket
  expect_equal(unique(as.character(cons$date)), "2026-01-26")
  expect_equal(unique(cons$campaign_id), 1L)
  expect_equal(unique(cons$project_id), 1L)
  expect_equal(unique(cons$algorithm_version), "2.0.0")

  # In-window n is 2 for every segment (r1 and r2 in window; r3 out).
  expect_true(all(cons$n == 2L))

  # Pull specific cells.
  intro_q1_t1 <- cons[cons$segment == "intro→q1" & cons$threshold_min == 1L, ]
  expect_equal(intro_q1_t1$pct_le, 100)
  expect_equal(intro_q1_t1$pct_resp_hit_gt, 0)

  q1_q2_t1 <- cons[cons$segment == "q1→q2" & cons$threshold_min == 1L, ]
  expect_equal(q1_q2_t1$pct_le, 50)
  expect_equal(round(q1_q2_t1$pct_resp_hit_gt, 4), round(100 / 3, 4))

  q1_q2_t5 <- cons[cons$segment == "q1→q2" & cons$threshold_min == 5L, ]
  expect_equal(q1_q2_t5$pct_le, 100)
  expect_equal(q1_q2_t5$pct_resp_hit_gt, 0)
})

test_that("cascade columns are present and consistent", {
  fx <- .load_synthetic()
  result <- latency_report(fx$data, fx$config)
  cons <- result$consolidated

  # All three respondents have at least one valid Δ on this date.
  # n_respondents is constant across (campaign, date, hour_local).
  expect_equal(unique(cons$n_respondents), 3L)

  # The 5-bucket cascade derives from pct_resp_worst_gt by subtraction.
  worst <- unique(cons[, c("threshold_min", "pct_resp_worst_gt")])
  worst <- worst[order(worst$threshold_min), ]
  # r1 worst Δ = 0.5, r2 worst Δ = 4 (q1→q2 segment), r3 worst Δ = 0.5.
  # So pct with worst > 1 = 1/3 (r2), > 3 = 1/3, > 5 = 0, > 10 = 0.
  expect_equal(round(worst$pct_resp_worst_gt, 4),
               round(c(100 / 3, 100 / 3, 0, 0), 4))
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

test_that("latency_report supports hour-bucketed output", {
  fx <- .load_synthetic()
  cfg <- fx$config
  cfg$reports$time_bucket <- "hour"
  result <- latency_report(fx$data, cfg)
  expect_false(all(is.na(result$consolidated$hour_local)))
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
