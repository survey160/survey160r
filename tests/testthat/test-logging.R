# Coverage for R/logging.R -- the injected-sink structured logger. A configured
# options(survey160r.log_fn=) sink receives events verbatim; otherwise s160r_log
# falls back to a message() line on stderr, gated by S160_LOG_LEVEL.

test_that("delegates to an injected sink verbatim and returns invisibly", {
  captured <- new_capture()
  withr::local_options(survey160r.log_fn = function(event, level, ...) {
    captured$event <- event
    captured$level <- level
    captured$fields <- list(...)
    "sink-return-ignored"
  })
  res <- s160r_log("gcs.read.start", level = "warning", path = "gs://b/1/x.csv")
  expect_null(res)
  expect_equal(captured$event, "gcs.read.start")
  expect_equal(captured$level, "warning")
  expect_equal(captured$fields$path, "gs://b/1/x.csv")
})

test_that("fallback emits a human-readable line carrying its fields", {
  withr::local_options(survey160r.log_fn = NULL)
  withr::local_envvar(S160_LOG_LEVEL = "info")
  expect_message(s160r_log("gcs.read.start", path = "gs://b/1/x.csv"),
                 "\\[INFO\\] gcs.read.start path=gs://b/1/x.csv")
})

test_that("fallback with no fields emits just the level and event", {
  withr::local_options(survey160r.log_fn = NULL)
  withr::local_envvar(S160_LOG_LEVEL = "info")
  expect_message(s160r_log("gcs.campaigns.none"),
                 "^\\[INFO\\] gcs.campaigns.none\\s*$")
})

test_that("fallback renders a multi-value field comma-joined", {
  withr::local_options(survey160r.log_fn = NULL)
  withr::local_envvar(S160_LOG_LEVEL = "info")
  expect_message(s160r_log("evt", ids = c("1", "2")), "ids=1,2")
})

test_that("fallback renders a zero-length field as empty", {
  withr::local_options(survey160r.log_fn = NULL)
  withr::local_envvar(S160_LOG_LEVEL = "info")
  expect_message(s160r_log("evt", empty = character(0)), "empty=")
})

test_that("S160_LOG_LEVEL gates the fallback below its threshold", {
  withr::local_options(survey160r.log_fn = NULL)
  withr::local_envvar(S160_LOG_LEVEL = "warning")
  expect_message(s160r_log("evt", level = "info"), NA)          # suppressed
  expect_message(s160r_log("evt", level = "error"), "\\[ERROR\\] evt")
})

test_that("an unknown level or S160_LOG_LEVEL defaults to the info rank", {
  withr::local_options(survey160r.log_fn = NULL)
  # Unknown threshold -> defaults to info; an info event still emits.
  withr::local_envvar(S160_LOG_LEVEL = "bogus")
  expect_message(s160r_log("evt", level = "info"), "\\[INFO\\] evt")
  # Unknown level -> defaults to the info rank (>= info threshold) and emits.
  withr::local_envvar(S160_LOG_LEVEL = "info")
  expect_message(s160r_log("evt", level = "bogus"), "\\[BOGUS\\] evt")
})
