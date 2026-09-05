# Coverage for R/disposition_aggregate.R.
# Disposition frames are constructed inline -- the shared fixtures
# (synthetic.csv) predate the disposition columns (phone, intro script/batch dates,
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
    id.intro.scriptDate  = c(TS, TS, ""),          # r3 never texted
    id.intro.finalText  = c("Yes", "No", "Yes"),  # r2 did not consent
    id.intro.batchDate  = c(TS, TS, ""),          # r3 never replied
    id.close.scriptDate = c(TS, "", TS)           # r1 reached close
  )
  res <- disposition_run(1234, d, contacted_only = FALSE)$consolidated

  expect_named(res, c("phone", "campaign_id", "sent", "engaged", "opted_in",
                      "completed", "web_complete", "terminated", "mode", "error"))
  expect_equal(res$phone, c("+15550101", "+15550102", "+15550103"))
  expect_true(is.integer(res$campaign_id))
  expect_equal(res$campaign_id, rep(1234L, 3L))
  expect_equal(res$sent,      c(1L, 1L, 0L))
  expect_equal(res$engaged,      c(1L, 1L, 0L))
  expect_equal(res$opted_in,       c(1L, 0L, 0L))  # r3 said Yes but never texted
  expect_equal(res$completed,     c(1L, 0L, 0L))  # r3 has close ts but sent=0
  expect_equal(res$web_complete, c(0L, 0L, 0L))
  expect_equal(res$terminated,   c(0L, 0L, 0L))
  expect_true(all(res$mode == "sms"))
  expect_true(all(is.na(res$error)))            # no error_code column -> all NA
})

test_that("error: raw carrier code passes through; blank/None/whitespace -> NA", {
  d <- disp_frame(
    phone = c("+15550301", "+15550302", "+15550303", "+15550304", "+15550305"),
    id.intro.scriptDate = rep(TS, 5),             # all contacted
    id.intro.finalText  = rep("Yes", 5),
    error_code          = c("4720", "", "None", "  4753  ", "9902")
  )
  res <- disposition_run(1234, d, contacted_only = FALSE)$consolidated
  expect_true(is.character(res$error))
  # string passthrough; whitespace trimmed; blank / "None" -> NA
  expect_equal(res$error, c("4720", NA, NA, "4753", "9902"))
})

test_that("error: reader-inferred integer / all-NA logical error_code coerce to strings", {
  # fread infers error_code's type per file: codes+blanks -> integer (blanks NA),
  # an all-blank column (no errors, the common case) -> logical all-NA. Both must
  # coerce to the character `error`, mirroring the real read path (not just the
  # character column the other test forces via "None").
  d_int <- disp_frame(
    phone = c("+15550401", "+15550402", "+15550403"),
    id.intro.scriptDate = rep(TS, 3),
    id.intro.finalText  = rep("Yes", 3),
    error_code          = c(4720L, NA_integer_, 4753L)     # integer column
  )
  expect_equal(disposition_run(1234, d_int, contacted_only = FALSE)$consolidated$error,
               c("4720", NA, "4753"))
  d_lgl <- disp_frame(
    phone = c("+15550404", "+15550405"),
    id.intro.scriptDate = rep(TS, 2),
    id.intro.finalText  = rep("Yes", 2),
    error_code          = c(NA, NA)                          # logical all-NA column
  )
  res <- disposition_run(1234, d_lgl, contacted_only = FALSE)$consolidated
  expect_true(is.character(res$error))
  expect_true(all(is.na(res$error)))
})

test_that("t2w campaign: completed comes from the web_complete callback", {
  d <- disp_frame(
    phone = c("+15550201", "+15550202", "+15550203"),
    id.intro.scriptDate  = c(TS, TS, ""),      # r3 never texted
    id.intro.finalText  = c("Yes", "Yes", "Yes"),
    id.close.scriptDate = c(TS, TS, TS),      # ignored under t2w
    web_complete        = c("1", "0", "1")    # a 1 present -> mode t2w
  )
  res <- disposition_run(1234, d, contacted_only = FALSE)$consolidated

  expect_true(all(res$mode == "t2w"))
  expect_equal(res$web_complete, c(1L, 0L, 1L))
  # completed = web_complete==1 AND sent; r3 has wc=1 but sent=0.
  expect_equal(res$completed, c(1L, 0L, 0L))
})

test_that("t2w_external campaign: completed is NA for every row", {
  d <- disp_frame(
    phone = c("+15550301", "+15550302"),
    id.intro.scriptDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes"),
    # Two distinct personalized close URLs, no web_complete -> t2w_external.
    id.close.scriptText = c("go https://s.example/a", "go https://s.example/b")
  )
  res <- disposition_run(1234, d)$consolidated

  expect_true(all(res$mode == "t2w_external"))
  expect_true(all(is.na(res$completed)))
  expect_equal(res$sent, c(1L, 1L))
  expect_equal(res$opted_in, c(1L, 1L))
})

test_that("terminated flags ineligible OR refusal", {
  d <- disp_frame(
    phone = c("+15550401", "+15550402", "+15550403", "+15550404"),
    id.intro.scriptDate       = rep(TS, 4),
    id.intro.finalText       = rep("Yes", 4),
    id.ineligible.scriptDate = c(TS, "",  TS, ""),
    id.refusal.scriptDate    = c("",  TS, TS, "")
  )
  res <- disposition_run(1234, d)$consolidated
  expect_equal(res$terminated, c(1L, 1L, 1L, 0L))
})

test_that("custom population expression drives opted_in", {
  d <- disp_frame(
    phone = c("+15550501", "+15550502"),
    id.intro.scriptDate = c(TS, TS),
    id.intro.finalText = c("Maybe", "Yes")
  )
  res <- disposition_run(1234, d, population = "id.intro.finalText == \"Maybe\"")$consolidated
  expect_equal(res$opted_in, c(1L, 0L))
})

test_that("optional columns absent: masks are null-safe (no error)", {
  # Only the minimum: phone + campaignid + intro script/text. No batchDate (reply),
  # web_complete, close, ineligible, or refusal columns at all.
  d <- disp_frame(
    phone = c("+15550601", "+15550602"),
    id.intro.scriptDate = c(TS, ""),
    id.intro.finalText = c("Yes", "Yes")
  )
  res <- disposition_run(1234, d, contacted_only = FALSE)$consolidated
  expect_true(all(res$mode == "sms"))
  expect_equal(res$sent,      c(1L, 0L))
  expect_equal(res$engaged,      c(0L, 0L))  # no batchDate (reply) column
  expect_equal(res$opted_in,       c(1L, 0L))
  expect_equal(res$completed,     c(0L, 0L))  # no close column
  expect_equal(res$web_complete, c(0L, 0L))
  expect_equal(res$terminated,   c(0L, 0L))
})

test_that("web_complete non-1 / non-numeric values do not count", {
  d <- disp_frame(
    phone = c("+15550701", "+15550702", "+15550703"),
    id.intro.scriptDate = rep(TS, 3),
    id.intro.finalText = rep("Yes", 3),
    web_complete = c("1", "", "x")   # only the first is a real callback
  )
  res <- disposition_run(1234, d)$consolidated
  expect_true(all(res$mode == "t2w"))
  expect_equal(res$web_complete, c(1L, 0L, 0L))
})

test_that("duplicate phone is rejected (grain guard)", {
  d <- disp_frame(
    phone = c("+15550801", "+15550801"),
    id.intro.scriptDate = c(TS, TS),
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
    id.intro.scriptDate = character(0),
    id.intro.finalText = character(0)
  )
  res <- disposition_run(1234, d)$consolidated
  expect_equal(nrow(res), 0L)
  expect_named(res, c("phone", "campaign_id", "sent", "engaged", "opted_in",
                      "completed", "web_complete", "terminated", "mode", "error"))
  expect_true(is.integer(res$sent))
  expect_true(is.character(res$phone))
  expect_true(is.character(res$error))   # empty-frame error type matches the live path
})

test_that("opted_in is null-safe when the population column is absent", {
  # No id.intro.finalText at all -> default population can't be evaluated;
  # opted_in degrades to 0 like the other masks rather than erroring.
  d <- disp_frame(
    phone = c("+15551001", "+15551002"),
    id.intro.scriptDate = c(TS, TS)
  )
  res <- disposition_run(1234, d)$consolidated
  expect_equal(res$opted_in, c(0L, 0L))
  expect_equal(res$sent, c(1L, 1L))
})

test_that("opted_in handles a base symbol in the population expression", {
  # `T` is a base constant, not a data column -- it must not be misread as an
  # absent column (which would wrongly zero opted_in for every row).
  d <- disp_frame(
    phone = c("+15551501", "+15551502"),
    id.intro.scriptDate = c(TS, TS),
    id.intro.finalText = c("Yes", "No")
  )
  res <- disposition_run(1234, d, population = 'id.intro.finalText == "Yes" & T')$consolidated
  expect_equal(res$opted_in, c(1L, 0L))
})

test_that("an unparseable population expression still errors", {
  d <- disp_frame(
    phone = "+15551101",
    id.intro.scriptDate = TS,
    id.intro.finalText = "Yes"
  )
  expect_error(disposition_run(1234, d, population = "finalText =="),
               "not valid R")
})

test_that("engaged is null-safe when the batchDate (reply) column is absent", {
  # engaged now keys on id.intro.batchDate (the inbound reply); with no batchDate
  # column at all, .column_timestamps returns all-NA -> nobody engaged.
  d <- disp_frame(
    phone = c("+15551201", "+15551202"),
    id.intro.scriptDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes")
  )
  res <- disposition_run(1234, d)$consolidated
  expect_equal(res$engaged, c(0L, 0L))
})

test_that("duplicate-phone error message does not leak the phone value (PII)", {
  d <- disp_frame(
    phone = c("+15551301", "+15551301"),
    id.intro.scriptDate = c(TS, TS),
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
    id.intro.scriptDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes")
  )
  res <- disposition_run(999, d)$consolidated
  expect_true(all(res$campaign_id == 999L))
})

test_that("campaign_id must be a single value", {
  # A vector id would recycle into the frame and multiply rows past the dedup
  # guard, silently breaking the (phone, campaign_id) grain -- reject it.
  d <- disp_frame(
    phone = c("+15553001", "+15553002"),
    id.intro.scriptDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes")
  )
  expect_error(disposition_run(c(1234, 5678), d), "must be a single value")
})

test_that("a factor campaign_id stamps its label, not its level code", {
  d <- disp_frame(
    phone = c("+15553101", "+15553102"),
    id.intro.scriptDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes")
  )
  res <- disposition_run(factor("1234"), d)$consolidated
  expect_true(all(res$campaign_id == 1234L))
})

test_that("contacted_only must be a single non-NA logical", {
  d <- disp_frame(
    phone = c("+15553201", "+15553202"),
    id.intro.scriptDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes")
  )
  expect_error(disposition_run(1234, d, contacted_only = NA), "single TRUE or FALSE")
  expect_error(disposition_run(1234, d, contacted_only = "yes"), "single TRUE or FALSE")
  expect_error(disposition_run(1234, d, contacted_only = c(TRUE, FALSE)),
               "single TRUE or FALSE")
})

# --- contacted_only ---------------------------------------------------------

test_that("contacted_only default drops never-attempted rows", {
  d <- disp_frame(
    phone = c("+15552001", "+15552002", "+15552003"),
    id.intro.scriptDate = c(TS, "", TS),          # r2 never texted
    id.intro.finalText = c("Yes", "Yes", "Yes")
  )
  res <- disposition_run(1234, d)$consolidated                 # default contacted_only = TRUE
  expect_equal(nrow(res), 2L)
  expect_true(all(res$sent == 1L))
  expect_equal(res$phone, c("+15552001", "+15552003"))
  expect_identical(rownames(res), c("1", "2"))    # rownames reset after the filter
})

test_that("contacted_only = FALSE emits one row per input respondent", {
  d <- disp_frame(
    phone = c("+15552101", "+15552102", "+15552103"),
    id.intro.scriptDate = c(TS, "", TS),
    id.intro.finalText = c("Yes", "Yes", "Yes")
  )
  res <- disposition_run(1234, d, contacted_only = FALSE)$consolidated
  expect_equal(nrow(res), 3L)
  expect_equal(res$sent, c(1L, 0L, 1L))
})

test_that("contacted_only defaults to TRUE", {
  d <- disp_frame(
    phone = c("+15552201", "+15552202"),
    id.intro.scriptDate = c(TS, ""),
    id.intro.finalText = c("Yes", "Yes")
  )
  expect_equal(disposition_run(1234, d),
               disposition_run(1234, d, contacted_only = TRUE))
})

test_that("contacted_only keeps non-responses (contacted but no reply)", {
  d <- disp_frame(
    phone = c("+15552301", "+15552302"),
    id.intro.scriptDate = c(TS, TS),              # both texted
    id.intro.batchDate = c(TS, ""),              # r2 never replied -> non-response
    id.intro.finalText = c("Yes", "Yes")
  )
  res <- disposition_run(1234, d)$consolidated
  expect_equal(nrow(res), 2L)                    # non-responder retained
  expect_equal(res$engaged, c(1L, 0L))
})

test_that("contacted_only with no contacted rows yields a typed zero-row frame", {
  d <- disp_frame(
    phone = c("+15552401", "+15552402"),
    id.intro.scriptDate = c("", ""),              # nobody texted
    id.intro.finalText = c("Yes", "Yes")
  )
  res <- disposition_run(1234, d)$consolidated
  expect_equal(nrow(res), 0L)
  expect_named(res, c("phone", "campaign_id", "sent", "engaged", "opted_in",
                      "completed", "web_complete", "terminated", "mode", "error"))
  expect_true(is.integer(res$sent))
  expect_true(is.character(res$phone))
  expect_true(is.character(res$error))   # empty-frame error type matches the live path
})

test_that("contacted_only keeps t2w_external contacted rows (completed = NA)", {
  d <- disp_frame(
    phone = c("+15552501", "+15552502"),
    id.intro.scriptDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes"),
    id.close.scriptText = c("go https://s.example/a", "go https://s.example/b")
  )
  res <- disposition_run(1234, d)$consolidated                 # default TRUE
  expect_equal(nrow(res), 2L)
  expect_true(all(res$mode == "t2w_external"))
  expect_true(all(is.na(res$completed)))
})

test_that("contacted_only does not change mode (mode is computed on full data)", {
  # The only web_complete==1 sits on a never-texted row. Mode must still be t2w
  # even though that row is dropped -- proving mode is detected on the full data,
  # not the filtered output. Would read "sms" if the filter ran before detection.
  d <- disp_frame(
    phone = c("+15552601", "+15552602"),
    id.intro.scriptDate = c(TS, ""),              # r2 never texted
    id.intro.finalText = c("Yes", "Yes"),
    web_complete = c("0", "1")                   # the sole web_complete==1 is r2
  )
  res <- disposition_run(1234, d)$consolidated                 # default TRUE -> drops r2
  expect_equal(nrow(res), 1L)
  expect_true(all(res$mode == "t2w"))
})

test_that("duplicate phone is rejected even when a duplicate is never-attempted", {
  d <- disp_frame(
    phone = c("+15552701", "+15552701"),         # duplicate
    id.intro.scriptDate = c(TS, ""),              # one contacted, one not
    id.intro.finalText = c("Yes", "Yes")
  )
  expect_error(disposition_run(1234, d), "duplicate phone")   # default TRUE
})

# --- disposition_input_columns -------------------------------------------

test_that("disposition_input_columns: default set is exactly the read columns", {
  cols <- disposition_input_columns()
  expect_setequal(cols, c("phone", "id.intro.scriptDate", "id.intro.batchDate",
                          "web_complete", "error_code", "id.close.scriptDate",
                          "id.ineligible.scriptDate", "id.refusal.scriptDate",
                          "id.intro.finalText"))
  expect_false("campaignid" %in% cols)           # stamped from the argument
})

test_that("disposition_input_columns: retains close-message Text cols from `available`", {
  header <- c("phone", "id.close.scriptText", "id.closeB.batchText",
              "id.intro.scriptText", "userid")
  cols <- disposition_input_columns(available = header)
  expect_true(all(c("id.close.scriptText", "id.closeB.batchText") %in% cols))
  expect_false("id.intro.scriptText" %in% cols)  # not a close-message Text col
  expect_false("userid" %in% cols)
})

test_that("disposition_input_columns: a custom population adds its columns", {
  cols <- disposition_input_columns(population = "some_flag == 1")
  expect_true("some_flag" %in% cols)
  expect_false("id.intro.finalText" %in% cols)   # default population not used
})

test_that("disposition_input_columns: projected read matches a full read", {
  # A rich frame: disposition columns + a t2w_external mode signal (2 distinct
  # close URLs) + columns disposition_run ignores (campaignid, userid, status).
  full <- disp_frame(
    phone = c("+15552801", "+15552802"),
    id.intro.scriptDate = c(TS, TS),
    id.intro.batchDate = c(TS, TS),
    id.intro.finalText = c("Yes", "No"),
    id.close.scriptText = c("go https://a", "go https://b"),  # -> t2w_external
    id.ineligible.scriptDate = c("", TS),
    id.refusal.scriptDate = c("", ""),
    error_code = c("4720", ""),                  # retained by the projection
    userid = c("agent-1", "agent-2"),            # ignored by disposition_run
    status = c("completed", "open")               # ignored by disposition_run
  )
  keep <- disposition_input_columns(available = names(full))
  expect_true("error_code" %in% intersect(keep, names(full)))  # projection keeps it
  projected <- full[, intersect(keep, names(full)), drop = FALSE]
  full_res <- disposition_run(1234, full, contacted_only = FALSE)
  expect_equal(disposition_run(1234, projected, contacted_only = FALSE), full_res)
  expect_equal(full_res$consolidated$error, c("4720", NA))     # survived projection
})

# --- result shape (list mirroring latency_run) ------------------------------

test_that("disposition_run returns a list of consolidated + meta", {
  d <- disp_frame(
    phone = c("+15554001", "+15554002"),
    id.intro.scriptDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes")
  )
  res <- disposition_run(1234, d)
  expect_named(res, c("consolidated", "meta"))
  expect_true(is.data.frame(res$consolidated))
  expect_named(res$meta, c("source_csv_hash", "source_csv_path"))
})

test_that("disposition_run meta carries source provenance from data attrs", {
  d <- disp_frame(
    phone = c("+15554101", "+15554102"),
    id.intro.scriptDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes")
  )
  attr(d, "source_csv_hash") <- "sha256:abc"
  attr(d, "source_csv_path") <- "gs://b/1234.csv"
  res <- disposition_run(1234, d)
  expect_equal(res$meta$source_csv_hash, "sha256:abc")
  expect_equal(res$meta$source_csv_path, "gs://b/1234.csv")
})

test_that("disposition_run meta is NA when data carries no provenance", {
  d <- disp_frame(
    phone = "+15554201",
    id.intro.scriptDate = TS,
    id.intro.finalText = "Yes"
  )
  res <- disposition_run(1234, d)
  expect_true(is.na(res$meta$source_csv_hash))
  expect_true(is.na(res$meta$source_csv_path))
})

# --- opening-question fallback (non-intro campaigns) ------------------------

test_that("non-intro opener (FIRSTNET) is measured, not silently dropped", {
  # A campaign whose first question is "FIRSTNET", not "intro". Before the
  # opening-question fallback every flag came up 0 and contacted_only dropped
  # the whole campaign (873-send campaign 2085 vanished in prod). r3 never texted.
  d <- disp_frame(
    phone = c("+15559001", "+15559002", "+15559003"),
    id.FIRSTNET.scriptDate = c(TS, TS, ""),
    id.FIRSTNET.batchDate  = c(TS, "", ""),      # only r1 replied
    id.FIRSTNET.finalText  = c("Yes", "No", "Yes"),
    id.close.scriptDate    = c(TS, "", "")
  )
  res <- disposition_run(1234, d, contacted_only = FALSE)$consolidated
  expect_true(all(res$mode == "sms"))
  expect_equal(res$sent,  c(1L, 1L, 0L))
  expect_equal(res$engaged,  c(1L, 0L, 0L))
  expect_equal(res$opted_in,   c(1L, 0L, 0L))     # r2 said No; r3 Yes but not texted
  expect_equal(res$completed, c(1L, 0L, 0L))     # r1 reached close & sent

  # contacted_only default now emits the contacted rows (was a zero-row frame)
  kept <- disposition_run(1234, d)$consolidated
  expect_equal(nrow(kept), 2L)
  expect_equal(kept$phone, c("+15559001", "+15559002"))
})

test_that("intro_latinos opener is detected (opener name varies)", {
  # Real prod case: campaign 2420's opener is "intro_latinos", not "intro".
  d <- disp_frame(
    phone = c("+15559101", "+15559102"),
    id.intro_latinos.scriptDate = c(TS, TS),
    id.intro_latinos.finalText  = c("Yes", "No")
  )
  res <- disposition_run(1234, d)$consolidated
  expect_equal(res$sent, c(1L, 1L))
  expect_equal(res$opted_in,  c(1L, 0L))
})

test_that("mixed campaign counts BOTH opener branches (intro + intro_sp)", {
  # Bilingual routing (prod: CA MIHA): r1 got the English intro, r2 the Spanish
  # intro_sp -- both are opening sends, so both are contacted, each replies on and
  # consents via its own branch. (Before this change only the intro branch counted.)
  d <- disp_frame(
    phone = c("+15559301", "+15559302"),
    id.intro.scriptDate    = c(TS, ""),
    id.intro.batchDate     = c(TS, ""),
    id.intro.finalText     = c("Yes", ""),
    id.intro_sp.scriptDate = c("", TS),
    id.intro_sp.batchDate  = c("", TS),
    id.intro_sp.finalText  = c("", "Yes")
  )
  res <- disposition_run(1234, d, contacted_only = FALSE)$consolidated
  expect_equal(res$sent, c(1L, 1L))     # both branches contacted
  expect_equal(res$engaged, c(1L, 1L))     # both replied
  expect_equal(res$opted_in,  c(1L, 1L))     # each said Yes on its own opener
})

test_that("3-way routed campaign counts every intro-family branch", {
  # prod: intro + intro_black + intro_hispanic.
  d <- disp_frame(
    phone = c("+15559401", "+15559402", "+15559403"),
    id.intro.scriptDate          = c(TS, "", ""),
    id.intro_black.scriptDate    = c("", TS, ""),
    id.intro_hispanic.scriptDate = c("", "", TS),
    id.intro.finalText           = c("Yes", "", ""),
    id.intro_black.finalText     = c("", "No", ""),
    id.intro_hispanic.finalText  = c("", "", "Yes")
  )
  res <- disposition_run(1234, d, contacted_only = FALSE)$consolidated
  expect_equal(res$sent, c(1L, 1L, 1L))
  expect_equal(res$opted_in,  c(1L, 0L, 1L))  # r2 answered No on intro_black
})

test_that("mixed opted_in ignores an absent opener finalText column (null-safe)", {
  # Only the intro branch has a finalText column; intro_sp recipients still count
  # as contacted but the default population uses only the present branch, without
  # erroring on the missing id.intro_sp.finalText.
  d <- disp_frame(
    phone = c("+15559501", "+15559502"),
    id.intro.scriptDate    = c(TS, ""),
    id.intro.finalText     = c("Yes", ""),
    id.intro_sp.scriptDate = c("", TS)        # sent, but no finalText column
  )
  res <- disposition_run(1234, d, contacted_only = FALSE)$consolidated
  expect_equal(res$sent, c(1L, 1L))    # both contacted
  expect_equal(res$opted_in,  c(1L, 0L))    # only the present-branch consent counts
})

test_that("disposition_input_columns discovers a non-intro opener from `available`", {
  header <- c("phone", "id.FIRSTNET.scriptDate", "id.FIRSTNET.batchDate",
              "id.FIRSTNET.finalText", "id.close.scriptDate", "userid")
  cols <- disposition_input_columns(available = header)
  expect_true(all(c("id.FIRSTNET.scriptDate", "id.FIRSTNET.batchDate",
                    "id.FIRSTNET.finalText") %in% cols))
  # The opener columns lead, so a reordered projection keeps the opener before a
  # later question (close) -- otherwise latency_discover_questions() would pick
  # close as the "first" question and the masks would key off the wrong column.
  expect_lt(match("id.FIRSTNET.scriptDate", cols),
            match("id.close.scriptDate", cols))
  expect_false("id.intro.scriptDate" %in% cols)   # no intro hardcoding remains
  expect_false("userid" %in% cols)
})

test_that("disposition_input_columns: projected read matches full for a non-intro opener", {
  # Guards the projection-order robustness: base `[` reorders columns to the
  # projected set's order, so the opener must lead or opener detection differs
  # between the projected and full frames (which would break this equivalence).
  full <- disp_frame(
    phone = c("+15559201", "+15559202"),
    id.FIRSTNET.scriptDate = c(TS, TS),
    id.FIRSTNET.batchDate = c(TS, TS),
    id.FIRSTNET.finalText = c("Yes", "No"),
    id.close.scriptDate = c(TS, ""),
    userid = c("a", "b")                          # ignored by disposition_run
  )
  keep <- disposition_input_columns(available = names(full))
  projected <- full[, intersect(keep, names(full)), drop = FALSE]
  expect_equal(disposition_run(1234, projected, contacted_only = FALSE),
               disposition_run(1234, full, contacted_only = FALSE))
})

test_that("engaged is gated on sent: a reply with no send is not engaged", {
  # Data anomaly -- a batchDate (reply) with no scriptDate (send). The latency
  # view gates n_engaged on `texted`; disposition now gates `engaged` on
  # `sent` too, so a reply-without-send is engaged in neither view.
  d <- disp_frame(
    phone = c("+15550201", "+15550202"),
    id.intro.scriptDate = c(TS, ""),   # r2: never sent
    id.intro.batchDate  = c(TS, TS)    # both carry a reply timestamp
  )
  res <- disposition_run(1234, d, contacted_only = FALSE)$consolidated
  expect_equal(res$sent, c(1L, 0L))
  expect_equal(res$engaged, c(1L, 0L))   # r2 gated (was c(1L, 1L) before the fix)
})

test_that("dedup guard is raw-phone: a mixed-format collision is not caught", {
  # Documented (finding #2): disposition_run() dedups on the RAW phone, but the
  # readers normalize (strip a leading US 1). Two formats of one number in one
  # campaign are distinct raw strings, so they pass the guard and emit two rows
  # -- which then collide to one (normalized phone, campaign_id) downstream,
  # making disposition_summary()'s latest_disposition order-dependent. Requires
  # mixed-format duplicates within a campaign (not seen in prod exports, which
  # were verified on raw values); pinned so the raw-only guard is a known limit.
  d <- disp_frame(
    phone = c("15551234567", "5551234567"),   # same number, two formats
    id.intro.scriptDate = c(TS, TS),
    id.intro.finalText = c("Yes", "Yes")
  )
  res <- disposition_run(2339, d)$consolidated
  expect_equal(nrow(res), 2L)                  # raw guard passed -> two rows
  expect_setequal(res$phone, c("15551234567", "5551234567"))
})
