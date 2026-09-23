# Coverage for R/latency_stream.R: the compact (streaming) latency path.
# The contract is byte-identical consolidated + diagnostics vs the default
# full-frame path, so most tests assert that parity; the tail exercises the
# helper edge branches directly for full coverage.

.load_synthetic_stream <- function() {
  data <- read.csv(test_path("fixtures/synthetic.csv"), stringsAsFactors = FALSE)
  list(data = data, config = synthetic_config())
}

.expect_compact_parity <- function(data, config, run_at) {
  full <- latency_report(data, config, run_at = run_at, compact = FALSE)
  comp <- latency_report(data, config, run_at = run_at, compact = TRUE)
  a <- full$consolidated
  b <- comp$consolidated
  rownames(a) <- NULL
  rownames(b) <- NULL
  expect_equal(b, a)
  expect_equal(comp$diagnostics, full$diagnostics)
  invisible(comp)
}

test_that("compact path is byte-identical to the full path (synthetic fixture)", {
  fx <- .load_synthetic_stream()
  .expect_compact_parity(fx$data, fx$config, as.POSIXct("2026-01-01", tz = "UTC"))
})

test_that("compact parity holds with a heavy NA-date bucket (all drop at intro)", {
  # Five respondents, four questions; only the first has any real reply, so the
  # deep segments are entirely NA-date drop-off -- the case the compact path
  # optimises. Output must still match the full frame exactly.
  d <- data.frame(
    campaignid = rep(7L, 5),
    id.intro.finalText = rep("Yes", 5),
    id.intro.scriptDate = sprintf("2026-02-01 1%d:00:00Z", 0:4),
    id.intro.batchDate  = c("2026-02-01 10:00:30Z", NA, NA, NA, NA),
    id.q1.scriptDate    = c("2026-02-01 10:01:00Z", NA, NA, NA, NA),
    id.q1.batchDate     = rep(NA_character_, 5),
    id.q2.scriptDate    = rep(NA_character_, 5),
    id.q2.batchDate     = rep(NA_character_, 5),
    id.close.scriptDate = rep(NA_character_, 5),
    check.names = FALSE, stringsAsFactors = FALSE)
  cfg <- latency_build_config(7L, d, field_timezone = "America/New_York")
  .expect_compact_parity(d, cfg, as.POSIXct("2026-01-01", tz = "UTC"))
})

test_that("compact parity holds when every batchDate is NA (kept frame empty)", {
  fx <- .load_synthetic_stream()
  # Null every batchDate so every segment's prior endpoint is NA -> all rows are
  # NA-date, the kept frame is empty, and diagnostics take the used == 0 branch;
  # the full path lands on the same values.
  bd <- grep("batchDate$", names(fx$data), value = TRUE)
  fx$data[bd] <- NA_character_
  comp <- .expect_compact_parity(fx$data, fx$config,
                                 as.POSIXct("2026-01-01", tz = "UTC"))
  expect_equal(nrow(comp$latency_frame), 0L)
  expect_equal(comp$diagnostics$n_respondents_used, 0L)
})

test_that("compact parity holds with a parse failure at the intro batchDate", {
  fx <- .load_synthetic_stream()
  fx$data$id.intro.batchDate[1] <- "not-a-timestamp"
  .expect_compact_parity(fx$data, fx$config, as.POSIXct("2026-01-01", tz = "UTC"))
})

test_that("compact parity holds on empty input", {
  fx <- .load_synthetic_stream()
  .expect_compact_parity(fx$data[0, , drop = FALSE], fx$config,
                         as.POSIXct("2026-01-01", tz = "UTC"))
})

test_that("latency_run forwards compact to latency_report", {
  fx <- .load_synthetic_stream()
  run_at <- as.POSIXct("2026-01-01", tz = "UTC")
  full <- latency_run(1L, fx$data, config = fx$config, run_at = run_at)
  comp <- latency_run(1L, fx$data, config = fx$config, run_at = run_at,
                      compact = TRUE)
  a <- full$consolidated
  b <- comp$consolidated
  rownames(a) <- NULL
  rownames(b) <- NULL
  expect_equal(b, a)
})

test_that(".stream_latency_frame short-circuits on no segments / no rows", {
  fx <- .load_synthetic_stream()
  one_q <- fx$config
  one_q$flow$questions <- fx$config$flow$questions[1]
  s0 <- .stream_latency_frame(fx$data, one_q, NULL)
  expect_equal(nrow(s0$kept), 0L)
  expect_equal(nrow(s0$na_date), 0L)
  expect_equal(s0$n_clamped, 0L)

  s_empty <- .stream_latency_frame(fx$data[0, , drop = FALSE], fx$config, NULL)
  expect_equal(nrow(s_empty$kept), 0L)
  expect_equal(nrow(s_empty$na_date), 0L)
})

test_that(".na_date_day_rows returns a zero-row consolidated when no NA-date rows", {
  fx <- .load_synthetic_stream()
  out <- .na_date_day_rows(.empty_na_date(), fx$config, "h",
                           as.POSIXct("2026-01-01", tz = "UTC"),
                           NA_character_,
                           empty_summary_frame(), empty_ineligible_frame(), "sms")
  expect_equal(nrow(out), 0L)
  # Same schema as a normal consolidated frame.
  expect_true(all(c("campaign_id", "segment", "threshold_min", "n_na_missing")
                  %in% names(out)))
})

test_that(".build_diagnostics_streamed empty-frame branch matches build_diagnostics", {
  # n_seg == 0 (single-question) and n_frame == 0 both take the empty branch.
  pf <- c(id.intro.batchDate = 0L)
  d_empty <- .build_diagnostics_streamed(empty_latency_frame(), .empty_na_date(),
                                         n_in = 12L, n_frame = 0L, n_seg = 2L,
                                         parse_failures = pf, config_hash = "h")
  ref <- build_diagnostics(empty_latency_frame(), n_respondents_in = 12L,
                           parse_failures = pf, config_hash = "h")
  expect_equal(d_empty, ref)

  d_noseg <- .build_diagnostics_streamed(empty_latency_frame(), .empty_na_date(),
                                         n_in = 3L, n_frame = 3L, n_seg = 0L,
                                         parse_failures = pf, config_hash = "h")
  expect_equal(d_noseg$n_respondents_in, 3L)
  expect_equal(d_noseg$n_segments_total, 0L)
})
