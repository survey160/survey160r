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
  res <- disposition_run(1234, d, contacted_only = FALSE)

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
  res <- disposition_run(1234, d, contacted_only = FALSE)

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
  res <- disposition_run(1234, d, contacted_only = FALSE)
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

test_that("opt_in handles a base symbol in the population expression", {
  # `T` is a base constant, not a data column -- it must not be misread as an
  # absent column (which would wrongly zero opt_in for every row).
  d <- disp_frame(
    phone = c("+15551501", "+15551502"),
    id.intro.batchDate = c(TS, TS),
    id.intro.finalText = c("Yes", "No")
  )
  res <- disposition_run(1234, d, population = 'id.intro.finalText == "Yes" & T')
  expect_equal(res$opt_in, c(1L, 0L))
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

# --- contacted_only ---------------------------------------------------------

test_that("contacted_only default drops never-attempted rows", {
  d <- disp_frame(
    phone = c("+15552001", "+15552002", "+15552003"),
    id.intro.batchDate = c(TS, "", TS),          # r2 never texted
    id.intro.finalText = c("Yes", "Yes", "Yes")
  )
  res <- disposition_run(1234, d)                 # default contacted_only = TRUE
  expect_equal(nrow(res), 2L)
  expect_true(all(res$started == 1L))
  expect_equal(res$phone, c("+15552001", "+15552003"))
  expect_identical(rownames(res), c("1", "2"))    # rownames reset after the filter
})

test_that("contacted_only = FALSE emits one row per input respondent", {
  d <- disp_frame(
    phone = c("+15552101", "+15552102", "+15552103"),
    id.intro.batchDate = c(TS, "", TS),
    id.intro.finalText = c("Yes", "Yes", "Yes")
  )
  res <- disposition_run(1234, d, contacted_only = FALSE)
  expect_equal(nrow(res), 3L)
  expect_equal(res$started, c(1L, 0L, 1L))
})

test_that("contacted_only defaults to TRUE", {
  d <- disp_frame(
    phone = c("+15552201", "+15552202"),
    id.intro.batchDate = c(TS, ""),
    id.intro.finalText = c("Yes", "Yes")
  )
  expect_equal(disposition_run(1234, d),
               disposition_run(1234, d, contacted_only = TRUE))
})

test_that("contacted_only keeps non-responses (contacted but no reply)", {
  d <- disp_frame(
    phone = c("+15552301", "+15552302"),
    id.intro.batchDate = c(TS, TS),              # both texted
    id.intro.finalValue = c("1", ""),            # r2 never replied -> non-response
    id.intro.finalText = c("Yes", "Yes")
  )
  res <- disposition_run(1234, d)
  expect_equal(nrow(res), 2L)                    # non-responder retained
  expect_equal(res$engaged, c(1L, 0L))
})

test_that("contacted_only with no contacted rows yields a typed zero-row frame", {
  d <- disp_frame(
    phone = c("+15552401", "+15552402"),
    id.intro.batchDate = c("", ""),              # nobody texted
    id.intro.finalText = c("Yes", "Yes")
  )
  res <- disposition_run(1234, d)
  expect_equal(nrow(res), 0L)
  expect_named(res, c("phone", "campaign_id", "started", "engaged", "opt_in",
                      "complete", "web_complete", "terminated", "mode"))
  expect_true(is.integer(res$started))
  expect_true(is.character(res$phone))
})

test_that("contacted_only keeps t2w_external contacted rows (complete = NA)", {
  d <- disp_frame(
    phone = c("+15552501", "+15552502"),
    id.intro.batchDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes"),
    id.close.scriptText = c("go https://s.example/a", "go https://s.example/b")
  )
  res <- disposition_run(1234, d)                 # default TRUE
  expect_equal(nrow(res), 2L)
  expect_true(all(res$mode == "t2w_external"))
  expect_true(all(is.na(res$complete)))
})

test_that("contacted_only does not change mode (mode is computed on full data)", {
  # The only web_complete==1 sits on a never-texted row. Mode must still be t2w
  # even though that row is dropped -- proving mode is detected on the full data,
  # not the filtered output. Would read "sms" if the filter ran before detection.
  d <- disp_frame(
    phone = c("+15552601", "+15552602"),
    id.intro.batchDate = c(TS, ""),              # r2 never texted
    id.intro.finalText = c("Yes", "Yes"),
    web_complete = c("0", "1")                   # the sole web_complete==1 is r2
  )
  res <- disposition_run(1234, d)                 # default TRUE -> drops r2
  expect_equal(nrow(res), 1L)
  expect_true(all(res$mode == "t2w"))
})

test_that("duplicate phone is rejected even when a duplicate is never-attempted", {
  d <- disp_frame(
    phone = c("+15552701", "+15552701"),         # duplicate
    id.intro.batchDate = c(TS, ""),              # one contacted, one not
    id.intro.finalText = c("Yes", "Yes")
  )
  expect_error(disposition_run(1234, d), "duplicate phone")   # default TRUE
})

# --- required_disposition_columns -------------------------------------------

test_that("required_disposition_columns: default set is exactly the read columns", {
  cols <- required_disposition_columns()
  expect_setequal(cols, c("phone", "id.intro.batchDate", "id.intro.finalValue",
                          "web_complete", "id.close.scriptDate",
                          "id.ineligible.scriptDate", "id.refusal.scriptDate",
                          "id.intro.finalText"))
  expect_false("campaignid" %in% cols)           # stamped from the argument
})

test_that("required_disposition_columns: retains close-message Text cols from `available`", {
  header <- c("phone", "id.close.scriptText", "id.closeB.batchText",
              "id.intro.scriptText", "userid")
  cols <- required_disposition_columns(available = header)
  expect_true(all(c("id.close.scriptText", "id.closeB.batchText") %in% cols))
  expect_false("id.intro.scriptText" %in% cols)  # not a close-message Text col
  expect_false("userid" %in% cols)
})

test_that("required_disposition_columns: a custom population adds its columns", {
  cols <- required_disposition_columns(population = "some_flag == 1")
  expect_true("some_flag" %in% cols)
  expect_false("id.intro.finalText" %in% cols)   # default population not used
})

test_that("required_disposition_columns: projected read matches a full read", {
  # A rich frame: disposition columns + a t2w_external mode signal (2 distinct
  # close URLs) + columns disposition_run ignores (campaignid, userid, status).
  full <- disp_frame(
    phone = c("+15552801", "+15552802"),
    id.intro.batchDate = c(TS, TS),
    id.intro.finalValue = c("1", "2"),
    id.intro.finalText = c("Yes", "No"),
    id.close.scriptText = c("go https://a", "go https://b"),  # -> t2w_external
    id.ineligible.scriptDate = c("", TS),
    id.refusal.scriptDate = c("", ""),
    userid = c("agent-1", "agent-2"),            # ignored by disposition_run
    status = c("complete", "open")               # ignored by disposition_run
  )
  keep <- required_disposition_columns(available = names(full))
  projected <- full[, intersect(keep, names(full)), drop = FALSE]
  expect_equal(disposition_run(1234, projected, contacted_only = FALSE),
               disposition_run(1234, full, contacted_only = FALSE))
})
