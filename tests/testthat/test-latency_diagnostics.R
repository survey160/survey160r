# Coverage for R/latency_diagnostics.R respondent_summary percentages.
# The cascade percentages (pct_clean_at_5min / pct_worst_in_5_to_10 /
# pct_worst_over_10) must be relative to the *measured* respondents (those
# with at least one valid Delta), i.e. respondent_summary$n_respondents = used.
# This keeps them consistent with the consolidated cascade and legacy parity,
# and lets n_respondents * pct / 100 recover a respondent count.

# Build a one-segment (intro -> q1) campaign frame. `q1_minutes` is the
# per-respondent intro->q1 latency in minutes; NA means the q1 scriptDate is
# blank (a no-valid-segment respondent). All respondents consent.
.diag_data <- function(q1_minutes) {
  n <- length(q1_minutes)
  base <- "2026-01-26 17:00:00"
  q1_script <- vapply(q1_minutes, function(m) {
    if (is.na(m)) return("")
    format(as.POSIXct(base, tz = "UTC") + m * 60,
           "%Y-%m-%d %H:%M:%SZ", tz = "UTC")
  }, character(1))
  data.frame(
    campaignid = rep(1L, n),
    id.intro.finalText = rep("Yes", n),
    id.intro.batchDate = rep(paste0(base, "Z"), n),
    id.intro.scriptDate = rep(paste0(base, "Z"), n),
    id.q1.batchDate = rep(paste0(base, "Z"), n),
    id.q1.scriptDate = q1_script,
    stringsAsFactors = FALSE
  )
}

.diag_summary <- function(q1_minutes) {
  d <- .diag_data(q1_minutes)
  cfg <- latency_build_config(1, d, field_timezone = "UTC")
  latency_report(d, cfg)$diagnostics
}

test_that("respondent_summary percentages divide by measured respondents, not all observed", {
  # r1 clean (3 min), r2 has no valid segment (blank q1). used = 1.
  diag <- .diag_summary(c(3, NA))
  expect_equal(diag$n_respondents_used, 1L)
  expect_equal(diag$n_respondents_no_valid_segment, 1L)
  rs <- diag$respondent_summary
  expect_equal(rs$n_respondents, 1L)
  # The single measured respondent is clean: 100%, not 50%.
  expect_equal(rs$pct_clean_at_5min, 100)
  expect_equal(rs$pct_worst_in_5_to_10, 0)
  expect_equal(rs$pct_worst_over_10, 0)
})

test_that("respondent_summary buckets partition the measured respondents (sum to 100)", {
  # Three measured respondents, one per bucket: 2 min (<=5), 7 min (5-10),
  # 15 min (>10), plus one no-valid respondent that must not dilute the buckets.
  rs <- .diag_summary(c(2, 7, 15, NA))$respondent_summary
  expect_equal(rs$n_respondents, 3L)
  expect_equal(rs$pct_clean_at_5min, 100 / 3)
  expect_equal(rs$pct_worst_in_5_to_10, 100 / 3)
  expect_equal(rs$pct_worst_over_10, 100 / 3)
  expect_equal(
    rs$pct_clean_at_5min + rs$pct_worst_in_5_to_10 + rs$pct_worst_over_10,
    100
  )
})

test_that("respondent_summary bucket boundaries are inclusive at 5 and 10 (<=5, (5,10], >10)", {
  # Exactly 5 min -> clean bucket; exactly 10 min -> 5_to_10 bucket.
  rs <- .diag_summary(c(5, 10))$respondent_summary
  expect_equal(rs$n_respondents, 2L)
  expect_equal(rs$pct_clean_at_5min, 50)
  expect_equal(rs$pct_worst_in_5_to_10, 50)
  expect_equal(rs$pct_worst_over_10, 0)
})

test_that("respondent_summary percentages are NA when no respondent has a valid segment", {
  # Non-empty frame (segments exist) but every respondent's q1 is blank, so
  # used == 0. Percentages are undefined -> NA, matching the empty-frame path.
  diag <- .diag_summary(c(NA, NA))
  expect_equal(diag$n_respondents_used, 0L)
  rs <- diag$respondent_summary
  expect_equal(rs$n_respondents, 0L)
  expect_true(is.na(rs$pct_clean_at_5min))
  expect_true(is.na(rs$pct_worst_in_5_to_10))
  expect_true(is.na(rs$pct_worst_over_10))
})
