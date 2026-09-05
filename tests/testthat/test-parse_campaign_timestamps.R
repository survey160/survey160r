# Coverage for the exported parse_campaign_timestamps() (R/latency_primitives.R).

test_that("parses the export's Z-suffixed microsecond format to UTC", {
  x <- parse_campaign_timestamps(c(
    "2026-01-26 17:30:16.853688Z", "2026-01-26 17:30:16Z"
  ))
  expect_s3_class(x, "POSIXct")
  expect_equal(attr(x, "tzone"), "UTC")
  expect_false(anyNA(x))
})

test_that("blank, NA, and unparseable inputs return NA (no error/warning)", {
  expect_silent(
    x <- parse_campaign_timestamps(c("", NA, "not-a-timestamp"))
  )
  expect_true(all(is.na(x)))
})

test_that("length is preserved, including the empty case", {
  expect_length(parse_campaign_timestamps(character(0)), 0L)
  expect_length(parse_campaign_timestamps(c("2026-01-26 00:00:00Z", "")), 2L)
})

test_that("a non-UTC POSIXct input is returned in UTC, instant preserved", {
  inst <- as.POSIXct("2026-01-26 21:00:00", tz = "America/New_York")
  out <- parse_campaign_timestamps(inst)
  expect_s3_class(out, "POSIXct")
  expect_equal(attr(out, "tzone"), "UTC")
  expect_equal(as.numeric(out), as.numeric(inst))   # not shifted
})
