# Coverage for the pure helpers in R/latency_primitives.R.

test_that("na_if_blank converts empty strings to NA on character columns", {
  df <- data.frame(
    a = c("x", "", "y"),
    b = c(1, 2, 3),
    c = c("foo", "bar", ""),
    stringsAsFactors = FALSE
  )
  out <- survey160r:::na_if_blank(df)
  expect_equal(out$a, c("x", NA, "y"))
  expect_equal(out$b, c(1, 2, 3))
  expect_equal(out$c, c("foo", "bar", NA))
})

test_that("parse_timestamps parses microsecond Z-suffixed UTC and counts failures", {
  df <- data.frame(
    ts = c("2026-01-26 21:00:30.000000Z", "garbage", NA, "", "2026-01-26 22:00:00.000000Z"),
    stringsAsFactors = FALSE
  )
  out <- survey160r:::parse_timestamps(df, "ts")
  expect_s3_class(out$data$ts, "POSIXct")
  expect_equal(format(out$data$ts[1], tz = "UTC"), "2026-01-26 21:00:30")
  expect_true(is.na(out$data$ts[2]))
  expect_true(is.na(out$data$ts[3]))
  expect_true(is.na(out$data$ts[4]))
  # Only "garbage" counts as a parse failure (non-blank, unparseable).
  expect_equal(out$parse_failures[["ts"]], 1L)
})

test_that("parse_timestamps errors when column missing", {
  df <- data.frame(a = "x", stringsAsFactors = FALSE)
  expect_error(survey160r:::parse_timestamps(df, "missing"),
               "Timestamp column not found")
})

test_that("parse_timestamps preserves already-POSIXct columns and normalizes tz", {
  ts <- as.POSIXct("2026-01-26 21:00:30", tz = "America/New_York")
  df <- data.frame(ts = ts)
  out <- survey160r:::parse_timestamps(df, "ts")
  expect_equal(attr(out$data$ts, "tzone"), "UTC")
})

test_that("compute_segment_delta clamps negatives and propagates NA", {
  bp <- as.POSIXct(c("2026-01-26 21:00:00", "2026-01-26 22:00:00", NA,
                     "2026-01-26 23:00:00"), tz = "UTC")
  sn <- as.POSIXct(c("2026-01-26 21:00:30", "2026-01-26 21:59:30",
                     "2026-01-26 22:00:00", NA), tz = "UTC")
  out <- survey160r:::compute_segment_delta(bp, sn)
  expect_equal(out$delta[1], 0.5)
  expect_equal(out$delta[2], 0)  # negative clamped
  expect_true(is.na(out$delta[3]))
  expect_true(is.na(out$delta[4]))
  expect_equal(out$n_clamped, 1L)
})

test_that("compute_segment_delta errors on length mismatch", {
  expect_error(
    survey160r:::compute_segment_delta(
      as.POSIXct("2026-01-26", tz = "UTC"),
      as.POSIXct(c("2026-01-26", "2026-01-27"), tz = "UTC")
    ),
    "same length"
  )
})

test_that("apply_chain_validity sets delta NA where any prior batchDate is NA", {
  delta <- c(1, 2, 3, 4)
  priors <- list(
    c(as.POSIXct("2026-01-26", tz = "UTC"), NA, as.POSIXct("2026-01-26", tz = "UTC"),
      as.POSIXct("2026-01-26", tz = "UTC"))
  )
  out <- survey160r:::apply_chain_validity(delta, priors)
  expect_equal(out, c(1, NA, 3, 4))
})

test_that("apply_chain_validity is identity when chain is empty", {
  expect_equal(survey160r:::apply_chain_validity(c(1, 2), list()), c(1, 2))
})

test_that("in_window_flag returns 1 for every row when windows are empty", {
  ts <- as.POSIXct(c("2026-01-26 18:00:00", "2026-01-26 02:00:00"), tz = "UTC")
  expect_equal(survey160r:::in_window_flag(ts, NULL, "America/New_York"),
               c(1L, 1L))
  empty <- data.frame(date = as.Date(character(0)),
                      start_hour = integer(0), end_hour = integer(0))
  expect_equal(survey160r:::in_window_flag(ts, empty, "America/New_York"),
               c(1L, 1L))
})

test_that("in_window_flag honors half-open [start, end) in field timezone", {
  windows <- data.frame(date = as.Date("2026-01-26"),
                        start_hour = 16, end_hour = 24,
                        stringsAsFactors = FALSE)
  # 16:00 EST = 21:00 UTC, 23:59 EST = 04:59 UTC next day, 15:59 EST = 20:59 UTC
  ts <- as.POSIXct(
    c("2026-01-26 21:00:00",  # exactly 16:00 EST -- start (inclusive)
      "2026-01-27 04:59:00",  # 23:59 EST -- inside
      "2026-01-27 05:00:00",  # 24:00 EST -- end (exclusive)
      "2026-01-26 20:59:00"), # 15:59 EST -- before start
    tz = "UTC"
  )
  flag <- survey160r:::in_window_flag(ts, windows, "America/New_York")
  expect_equal(flag, c(1L, 1L, 0L, 0L))
})

test_that("in_window_flag errors when batch_dates is not POSIXct", {
  expect_error(
    survey160r:::in_window_flag("2026-01-26", data.frame(), "UTC"),
    "POSIXct"
  )
})

test_that("in_window_flag errors when windows lacks required columns", {
  ts <- as.POSIXct("2026-01-26 21:00:00", tz = "UTC")
  bad <- data.frame(date = as.Date("2026-01-26"))
  expect_error(survey160r:::in_window_flag(ts, bad, "UTC"),
               "missing columns")
})

test_that("normalize_windows handles list-of-list input from YAML", {
  raw <- list(
    list(date = "2026-01-26", start_hour = 16, end_hour = 24),
    list(date = "2026-01-27", start_hour = 14, end_hour = 22)
  )
  out <- survey160r:::normalize_windows(raw)
  expect_equal(nrow(out), 2)
  expect_equal(out$date, as.Date(c("2026-01-26", "2026-01-27")))
  expect_equal(out$start_hour, c(16L, 14L))
  expect_equal(out$end_hour, c(24L, 22L))
})

test_that("normalize_windows returns empty data.frame for NULL or empty input", {
  out <- survey160r:::normalize_windows(NULL)
  expect_equal(nrow(out), 0)
  expect_equal(names(out), c("date", "start_hour", "end_hour"))
  expect_equal(nrow(survey160r:::normalize_windows(list())), 0)
})
