# Coverage for R/question.R: question_timestamps() and question_funnel().

.mk_export <- function() {
  data.frame(
    id.intro.scriptDate = c("2026-01-26 21:00:00Z", "2026-01-26 21:05:00Z",
                            "2026-01-26 21:10:00Z"),
    id.intro.batchDate  = c("2026-01-26 21:00:30Z", NA,
                            "2026-01-26 21:11:00Z"),
    id.q1.scriptDate    = c("2026-01-26 21:01:00Z", "2026-01-26 21:06:00Z", NA),
    id.close.scriptDate = c("2026-01-26 21:02:00Z", NA, NA),
    id.blank.scriptDate = c(NA, NA, NA),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

test_that("question_timestamps parses a question's scriptDate column", {
  ts <- question_timestamps(.mk_export(), "intro")
  expect_s3_class(ts, "POSIXct")
  expect_equal(length(ts), 3L)
  expect_true(all(!is.na(ts)))
  expect_equal(attr(ts, "tzone"), "UTC")
})

test_that("question_timestamps reads the batchDate field", {
  ts <- question_timestamps(.mk_export(), "intro", field = "batchDate")
  expect_equal(sum(!is.na(ts)), 2L)   # r2 has no reply
})

test_that("question_timestamps rejects a non-data-frame", {
  expect_error(question_timestamps(list(), "intro"), "must be a data frame")
})

test_that("question_timestamps validates question and field", {
  df <- .mk_export()
  expect_error(question_timestamps(df, ""), "non-empty string")
  expect_error(question_timestamps(df, 123), "non-empty string")
  expect_error(question_timestamps(df, "intro", field = ""), "non-empty string")
})

test_that("question_timestamps errors on a missing column", {
  expect_error(question_timestamps(.mk_export(), "nope"),
               "id\\.nope\\.scriptDate` not found")
})

test_that("question_timestamps passes a POSIXct column through in UTC (no shift)", {
  inst <- as.POSIXct("2026-01-26 21:00:00", tz = "America/New_York")
  d <- data.frame(id.x.scriptDate = inst, check.names = FALSE)
  ts <- question_timestamps(d, "x")
  expect_s3_class(ts, "POSIXct")
  expect_equal(attr(ts, "tzone"), "UTC")
  expect_equal(as.numeric(ts), as.numeric(inst))   # same instant, not re-parsed
})

test_that("question_funnel returns reached counts, index, and pct of head", {
  f <- question_funnel(.mk_export(), c("intro", "q1", "close"))
  expect_identical(names(f),
                   c("question", "question_index", "n_reached", "pct_reached"))
  expect_equal(f$question, c("intro", "q1", "close"))
  expect_equal(f$question_index, 1:3)
  expect_equal(f$n_reached, c(3L, 2L, 1L))
  expect_equal(f$pct_reached, c(100, 200 / 3, 100 / 3))
})

test_that("question_funnel head with zero reached yields NA pct", {
  f <- question_funnel(.mk_export(), c("blank", "intro"))
  expect_equal(f$n_reached, c(0L, 3L))
  expect_true(all(is.na(f$pct_reached)))
})

test_that("question_funnel rejects a non-data-frame", {
  expect_error(question_funnel(list(), "intro"), "must be a data frame")
})

test_that("question_funnel validates the questions vector", {
  df <- .mk_export()
  expect_error(question_funnel(df, character(0)), "non-empty character")
  expect_error(question_funnel(df, c("intro", NA)), "non-empty character")
  expect_error(question_funnel(df, c("intro", "")), "non-empty character")
  expect_error(question_funnel(df, 123), "non-empty character")
})
