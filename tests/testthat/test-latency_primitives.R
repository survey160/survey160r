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
               "timestamp column not found")
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
