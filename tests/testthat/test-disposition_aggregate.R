# Coverage for R/disposition_aggregate.R.
# Disposition frames are constructed inline -- the shared fixtures
# (synthetic.csv) predate the disposition columns (phone, finalValue,
# web_complete, ineligible/refusal), so they cannot exercise these masks.

# Survey160 v2 timestamp literal; "" stands in for an absent event.
TS <- "2026-01-26 15:00:00.000000Z"

# Build a disposition input frame from named column vectors. `phone` is
# required; `campaignid` defaults to 1234 for every row. Column names carry
# dots on purpose (dot-form, post read.csv).
disp_frame <- function(phone, ...) {
  cols <- list(phone = phone, ...)
  if (is.null(cols$campaignid)) {
    cols$campaignid <- rep(1234L, length(phone))
  }
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

test_that("sms campaign: per-respondent flags and mode", {
  d <- disp_frame(
    phone = c("+15550101", "+15550102", "+15550103"),
    id.intro.batchDate  = c(TS, TS, ""),          # r3 never texted
    id.intro.finalText  = c("Yes", "No", "Yes"),  # r2 did not consent
    id.intro.finalValue = c("1", "2", ""),        # r3 never replied
    id.close.scriptDate = c(TS, "", TS)           # r1 reached close
  )
  res <- disposition_run(1234, d)

  expect_named(res, c("phone", "campaign_id", "started", "engaged", "opt_in",
                      "complete", "web_complete", "terminated", "mode"))
  expect_equal(res$phone, c("+15550101", "+15550102", "+15550103"))
  expect_true(is.integer(res$campaign_id) && all(res$campaign_id == 1234L))
  expect_equal(res$started,      c(1L, 1L, 0L))
  expect_equal(res$engaged,      c(1L, 1L, 0L))
  expect_equal(res$opt_in,       c(1L, 0L, 0L))  # r3 said Yes but never texted
  expect_equal(res$complete,     c(1L, 0L, 0L))  # r3 has close ts but started=0
  expect_equal(res$web_complete, c(0L, 0L, 0L))
  expect_equal(res$terminated,   c(0L, 0L, 0L))
  expect_true(all(res$mode == "sms"))
})

test_that("t2w campaign: complete comes from the web_complete callback", {
  d <- disp_frame(
    phone = c("+15550201", "+15550202", "+15550203"),
    id.intro.batchDate  = c(TS, TS, ""),      # r3 never texted
    id.intro.finalText  = c("Yes", "Yes", "Yes"),
    id.close.scriptDate = c(TS, TS, TS),      # ignored under t2w
    web_complete        = c("1", "0", "1")    # a 1 present -> mode t2w
  )
  res <- disposition_run(1234, d)

  expect_true(all(res$mode == "t2w"))
  expect_equal(res$web_complete, c(1L, 0L, 1L))
  # complete = web_complete==1 AND started; r3 has wc=1 but started=0.
  expect_equal(res$complete, c(1L, 0L, 0L))
})

test_that("t2w_external campaign: complete is NA for every row", {
  d <- disp_frame(
    phone = c("+15550301", "+15550302"),
    id.intro.batchDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes"),
    # Two distinct personalized close URLs, no web_complete -> t2w_external.
    id.close.scriptText = c("go https://s.example/a", "go https://s.example/b")
  )
  res <- disposition_run(1234, d)

  expect_true(all(res$mode == "t2w_external"))
  expect_true(all(is.na(res$complete)))
  expect_equal(res$started, c(1L, 1L))
  expect_equal(res$opt_in, c(1L, 1L))
})

test_that("terminated flags ineligible OR refusal", {
  d <- disp_frame(
    phone = c("+15550401", "+15550402", "+15550403", "+15550404"),
    id.intro.batchDate       = rep(TS, 4),
    id.intro.finalText       = rep("Yes", 4),
    id.ineligible.scriptDate = c(TS, "",  TS, ""),
    id.refusal.scriptDate    = c("",  TS, TS, "")
  )
  res <- disposition_run(1234, d)
  expect_equal(res$terminated, c(1L, 1L, 1L, 0L))
})

test_that("custom population expression drives opt_in", {
  d <- disp_frame(
    phone = c("+15550501", "+15550502"),
    id.intro.batchDate = c(TS, TS),
    id.intro.finalText = c("Maybe", "Yes")
  )
  res <- disposition_run(1234, d, population = "id.intro.finalText == \"Maybe\"")
  expect_equal(res$opt_in, c(1L, 0L))
})

test_that("optional columns absent: masks are null-safe (no error)", {
  # Only the minimum: phone + campaignid + intro batch/text. No finalValue,
  # web_complete, close, ineligible, or refusal columns at all.
  d <- disp_frame(
    phone = c("+15550601", "+15550602"),
    id.intro.batchDate = c(TS, ""),
    id.intro.finalText = c("Yes", "Yes")
  )
  res <- disposition_run(1234, d)
  expect_true(all(res$mode == "sms"))
  expect_equal(res$started,      c(1L, 0L))
  expect_equal(res$engaged,      c(0L, 0L))  # no finalValue column
  expect_equal(res$opt_in,       c(1L, 0L))
  expect_equal(res$complete,     c(0L, 0L))  # no close column
  expect_equal(res$web_complete, c(0L, 0L))
  expect_equal(res$terminated,   c(0L, 0L))
})

test_that("web_complete non-1 / non-numeric values do not count", {
  d <- disp_frame(
    phone = c("+15550701", "+15550702", "+15550703"),
    id.intro.batchDate = rep(TS, 3),
    id.intro.finalText = rep("Yes", 3),
    web_complete = c("1", "", "x")   # only the first is a real callback
  )
  res <- disposition_run(1234, d)
  expect_true(all(res$mode == "t2w"))
  expect_equal(res$web_complete, c(1L, 0L, 0L))
})

test_that("duplicate phone is rejected (grain guard)", {
  d <- disp_frame(
    phone = c("+15550801", "+15550801"),
    id.intro.batchDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes")
  )
  expect_error(disposition_run(1234, d), "duplicate phone")
})

test_that("missing phone column is rejected", {
  d <- data.frame(campaignid = 1234L, id.intro.finalText = "Yes",
                  stringsAsFactors = FALSE)
  expect_error(disposition_run(1234, d), "must contain a `phone` column")
})

test_that("non-data-frame input is rejected", {
  expect_error(disposition_run(1234, list(phone = "x")),
               "must be a data frame")
})

test_that("zero-row input returns the empty disposition frame", {
  d <- disp_frame(
    phone = character(0),
    id.intro.batchDate = character(0),
    id.intro.finalText = character(0)
  )
  res <- disposition_run(1234, d)
  expect_equal(nrow(res), 0L)
  expect_named(res, c("phone", "campaign_id", "started", "engaged", "opt_in",
                      "complete", "web_complete", "terminated", "mode"))
  expect_true(is.integer(res$started))
  expect_true(is.character(res$phone))
})

test_that("opt_in is null-safe when the population column is absent", {
  # No id.intro.finalText at all -> default population can't be evaluated;
  # opt_in degrades to 0 like the other masks rather than erroring.
  d <- disp_frame(
    phone = c("+15551001", "+15551002"),
    id.intro.batchDate = c(TS, TS)
  )
  res <- disposition_run(1234, d)
  expect_equal(res$opt_in, c(0L, 0L))
  expect_equal(res$started, c(1L, 1L))
})

test_that("an unparseable population expression still errors", {
  d <- disp_frame(
    phone = "+15551101",
    id.intro.batchDate = TS,
    id.intro.finalText = "Yes"
  )
  expect_error(disposition_run(1234, d, population = "finalText =="),
               "not valid R")
})

test_that("engaged ignores a whitespace-only finalValue", {
  d <- disp_frame(
    phone = c("+15551201", "+15551202"),
    id.intro.batchDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes"),
    id.intro.finalValue = c("1", "   ")   # r2 replied only whitespace
  )
  res <- disposition_run(1234, d)
  expect_equal(res$engaged, c(1L, 0L))
})

test_that("duplicate-phone error message does not leak the phone value (PII)", {
  d <- disp_frame(
    phone = c("+15551301", "+15551301"),
    id.intro.batchDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes")
  )
  err <- tryCatch(disposition_run(1234, d), error = function(e) conditionMessage(e))
  expect_no_match(err, "\\+1555", fixed = FALSE)
})

test_that("campaign_id is stamped from the argument, not a column", {
  # campaignid column (1234) differs from the argument (999) so the assertion
  # pins the source to the argument.
  d <- disp_frame(
    phone = c("+15551401", "+15551402"),
    campaignid = c(1234L, 1234L),
    id.intro.batchDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes")
  )
  res <- disposition_run(999, d)
  expect_true(all(res$campaign_id == 999L))
})
