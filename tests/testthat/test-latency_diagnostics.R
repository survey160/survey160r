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

test_that("respondent_summary matches a direct count of each respondent's worst delta", {
  # Mixed latencies across all three buckets, including the 5 / 10 boundaries,
  # plus no-valid respondents (NA) that must sit outside the denominator.
  q1 <- c(0, 1.5, 5, 5.5, 9, 10, 10.5, 30, NA, 4, 12, NA, 7)
  rs <- .diag_summary(q1)$respondent_summary
  m <- q1[!is.na(q1)]
  expect_equal(rs$n_respondents, length(m))
  expect_equal(rs$pct_clean_at_5min, 100 * mean(m <= 5))
  expect_equal(rs$pct_worst_in_5_to_10, 100 * mean(m > 5 & m <= 10))
  expect_equal(rs$pct_worst_over_10, 100 * mean(m > 10))
  # n_respondents * pct / 100 recovers each bucket's respondent count.
  expect_equal(rs$n_respondents * rs$pct_clean_at_5min / 100, sum(m <= 5))
  expect_equal(rs$n_respondents * rs$pct_worst_over_10 / 100, sum(m > 10))
})

test_that("respondent_summary agrees with the consolidated day-grain cascade on a one-day campaign", {
  # One campaign, one day, one segment: the day row's cascade covers the same
  # measured respondents, so n_respondents matches and pct_resp_worst_gt at the
  # 5 and 10 thresholds is the complement of the clean bucket / the >10 bucket.
  d <- .diag_data(c(2, 4, 5, 6, 8, 10, 11, 25, NA))
  cfg <- latency_build_config(1, d, field_timezone = "UTC")
  for (compact in c(FALSE, TRUE)) {
    res <- latency_report(d, cfg, compact = compact)
    rs <- res$diagnostics$respondent_summary
    cons <- res$consolidated
    day <- cons[is.na(cons$hour_local) & !is.na(cons$date), , drop = FALSE]
    gt5 <- day[day$threshold_min == 5L, , drop = FALSE]
    gt10 <- day[day$threshold_min == 10L, , drop = FALSE]
    expect_equal(nrow(gt5), 1L)
    expect_equal(gt5$n_respondents, rs$n_respondents)
    expect_equal(gt5$pct_resp_worst_gt, 100 - rs$pct_clean_at_5min)
    expect_equal(gt10$pct_resp_worst_gt, rs$pct_worst_over_10)
    expect_equal(gt5$pct_resp_worst_gt - gt10$pct_resp_worst_gt,
                 rs$pct_worst_in_5_to_10)
  }
})

test_that("respondent_summary on a synthetic campaign matches a per-respondent recomputation", {
  export <- synthetic_export(n = 600L, n_questions = 6L)
  cfg <- latency_build_config(1980, export, field_timezone = "UTC")
  frame <- latency_report(export, cfg)$latency_frame
  valid <- !is.na(frame$delta_min)
  worst <- vapply(split(frame$delta_min[valid], frame$respondent_index[valid]),
                  max, numeric(1))
  for (compact in c(FALSE, TRUE)) {
    diag <- latency_report(export, cfg, compact = compact)$diagnostics
    rs <- diag$respondent_summary
    expect_equal(diag$n_respondents_used, length(worst))
    expect_equal(rs$n_respondents, length(worst))
    expect_equal(rs$pct_clean_at_5min, 100 * mean(worst <= 5))
    expect_equal(rs$pct_worst_in_5_to_10, 100 * mean(worst > 5 & worst <= 10))
    expect_equal(rs$pct_worst_over_10, 100 * mean(worst > 10))
    expect_equal(rs$pct_clean_at_5min + rs$pct_worst_in_5_to_10 +
                   rs$pct_worst_over_10, 100)
  }
})

test_that("respondent counts share one base whether or not any row survives the filters", {
  # used + no_valid counts the respondents that reached the latency frame
  # (after dedupe and date_filter); the rows the filters removed are the gap
  # to n_respondents_in. When the filters remove every row nobody reached the
  # frame, so no_valid is 0 -- not n_respondents_in, which counted the deduped
  # and date-filtered rows as "no valid segment" in that one case only.
  d <- load_synthetic_data()
  dup <- rbind(d, d[1, ])
  counts <- function(cfg, compact) {
    diag <- latency_report(dup, cfg, compact = compact)$diagnostics
    c(inn = diag$n_respondents_in, used = diag$n_respondents_used,
      none = diag$n_respondents_no_valid_segment)
  }
  keep <- latency_build_config(1, dup, field_timezone = "UTC",
                               respondent_id_column = "userid")
  drop <- latency_build_config(1, dup, field_timezone = "UTC",
                               respondent_id_column = "userid",
                               date_filter = "2030-01-01")
  for (compact in c(FALSE, TRUE)) {
    # 5 rows in, 1 duplicate removed: 3 measured + 1 unmeasured reach the frame.
    expect_equal(counts(keep, compact), c(inn = 5, used = 3, none = 1))
    # The date filter then removes every remaining row.
    expect_equal(counts(drop, compact), c(inn = 5, used = 0, none = 0))
  }
})

test_that(".respondent_summary handles the empty and single-bucket edges", {
  expect_identical(
    survey160r:::.respondent_summary(numeric(0)),
    list(n_respondents = 0L, pct_clean_at_5min = NA_real_,
         pct_worst_in_5_to_10 = NA_real_, pct_worst_over_10 = NA_real_)
  )
  all_over <- survey160r:::.respondent_summary(c(11, 60, 10.01))
  expect_identical(all_over$n_respondents, 3L)
  expect_identical(c(all_over$pct_clean_at_5min, all_over$pct_worst_in_5_to_10,
                     all_over$pct_worst_over_10), c(0, 0, 100))
})
