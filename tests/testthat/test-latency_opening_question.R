# Opening-question resolution shared by the latency summary / config / funnel
# filter (R/latency_config.R helpers + R/summary_aggregate.R + R/latency_filter.R).
# Pure-intro campaigns are byte-identical to the pre-fix behaviour (guarded by
# test-summary_aggregate.R / test-latency_parity_legacy.R / test-latency_config.R);
# these cover the NON-intro (FIRSTNET) and bilingual (intro + intro_sp) paths.

TS <- "2026-01-26 15:00:00.000000Z"

# One-campaign frame from named column vectors (dot-form names preserved).
op_frame <- function(...) {
  cols <- list(...)
  n <- length(cols[[1L]])
  cols$campaignid <- rep(1L, n)
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

test_that(".opening_questions resolves the opener set", {
  expect_equal(.opening_questions(c("intro", "q1", "close")), "intro")
  expect_equal(.opening_questions(c("intro", "intro_sp", "q1")),
               c("intro", "intro_sp"))
  expect_equal(.opening_questions(c("FIRSTNET", "q1", "close")), "FIRSTNET")
  expect_equal(.opening_questions(character(0)), "intro")   # degenerate -> intro
})

test_that(".opening_questions honours the intro-family word boundary", {
  # The whole fix pivots on `^intro(_|$)`: only "intro" itself or an underscore
  # variant (intro_sp / intro_latinos) is intro-family. A longer word that merely
  # starts with the letters (e.g. "introduction") must NOT be folded in -- else a
  # campaign with both `intro` and a distinct `introduction` question would
  # inflate sent / opted_in. Guards against a weakening to `^intro`.
  expect_equal(.opening_questions(c("intro", "introduction", "close")), "intro")
  expect_equal(.opening_questions(c("introduction", "close")), "introduction")
  expect_equal(.opening_questions(c("intros", "close")), "intros")
  # Case-sensitive: an uppercase opener is not intro-family (v2 emits lowercase);
  # it falls through to the single-first-opener branch.
  expect_equal(.opening_questions(c("Intro", "close")), "Intro")
})

test_that(".opener_population builds a present-only disjunction (intro == default)", {
  expect_equal(.opener_population("intro", "id.intro.finalText"),
               .default_population)                          # pure intro == default
  expect_equal(
    .opener_population(c("intro", "intro_sp"),
                      c("id.intro.finalText", "id.intro_sp.finalText")),
    'id.intro.finalText == "Yes" | id.intro_sp.finalText == "Yes"'
  )
  # an absent branch is dropped; if none present, keep one (null-safe zero)
  expect_equal(.opener_population(c("intro", "intro_sp"), "id.intro.finalText"),
               'id.intro.finalText == "Yes"')
  expect_equal(.opener_population("FIRSTNET", character(0)),
               'id.FIRSTNET.finalText == "Yes"')
})

test_that(".question_timestamp coalesces across the set, null-safe on absent cols", {
  d <- op_frame(
    id.intro.scriptDate    = c(TS, ""),
    id.intro_sp.scriptDate = c("", TS)
  )
  ts <- .question_timestamp(d, c("intro", "intro_sp"), "scriptDate")
  expect_false(any(is.na(ts)))                 # each recipient's own branch send
  expect_equal(attr(ts, "tzone"), "UTC")
  ts2 <- .question_timestamp(d, c("intro", "intro_sp"), "batchDate")
  expect_true(all(is.na(ts2)))                 # no batchDate columns -> all NA
})

test_that("FIRSTNET campaign: config validates and summary counts (no crash)", {
  d <- op_frame(
    id.FIRSTNET.scriptDate = c(TS, TS, ""),
    id.FIRSTNET.batchDate  = c(TS, "", ""),
    id.FIRSTNET.finalText  = c("Yes", "No", "Yes"),
    id.close.scriptDate    = c(TS, "", "")
  )
  config <- latency_build_config(1L, d, field_timezone = "America/New_York")
  expect_equal(config$flow$questions[[1L]], "FIRSTNET")
  expect_equal(config$filters$population, 'id.FIRSTNET.finalText == "Yes"')
  expect_silent(latency_validate_config(config, d))     # previously hard-errored
  res <- build_summary_frame(d, config, survey_mode = "sms")
  expect_equal(sum(res$n_sent), 2L)
  expect_equal(sum(res$n_engaged), 1L)
  expect_equal(sum(res$n_opted_in), 1L)   # r2 "No"; r3 "Yes" but not texted
  expect_equal(sum(res$n_completed), 1L)
})

test_that("bilingual campaign: summary counts BOTH opener branches", {
  d <- op_frame(
    id.intro.scriptDate    = c(TS, ""),
    id.intro.batchDate     = c(TS, ""),
    id.intro.finalText     = c("Yes", ""),
    id.intro_sp.scriptDate = c("", TS),
    id.intro_sp.batchDate  = c("", TS),
    id.intro_sp.finalText  = c("", "Yes"),
    id.close.scriptDate    = c(TS, TS)
  )
  config <- latency_build_config(1L, d, field_timezone = "America/New_York")
  expect_equal(.opening_questions(config$flow$questions), c("intro", "intro_sp"))
  res <- build_summary_frame(d, config, survey_mode = "sms")
  expect_equal(sum(res$n_sent), 2L)      # both branches (was 1 pre-fix)
  expect_equal(sum(res$n_engaged), 2L)
  expect_equal(sum(res$n_opted_in), 2L)   # each said Yes on its own opener
})

test_that("a recipient on BOTH opener branches is counted once (OR, not sum)", {
  # Data anomaly: a single recipient has both id.intro.* and id.intro_sp.*
  # populated. The opener set ORs the branches (.question_timestamp coalesces,
  # the population disjunction is an OR), so the recipient must count once, not
  # twice -- no double-count from the set expansion.
  d <- op_frame(
    id.intro.scriptDate    = TS,
    id.intro.batchDate     = TS,
    id.intro.finalText     = "Yes",
    id.intro_sp.scriptDate = TS,
    id.intro_sp.batchDate  = TS,
    id.intro_sp.finalText  = "Yes",
    id.close.scriptDate    = TS
  )
  config <- latency_build_config(1L, d, field_timezone = "America/New_York")
  res <- build_summary_frame(d, config, survey_mode = "sms")
  expect_equal(sum(res$n_sent), 1L)
  expect_equal(sum(res$n_engaged), 1L)
  expect_equal(sum(res$n_opted_in), 1L)
})

test_that("latency_build_config accepts a character header (bilingual population)", {
  # latency_build_config, like latency_discover_questions, accepts a raw header
  # vector (names() is NULL there) -- the population must still cover every
  # present opener branch, not just the first.
  header <- c("campaignid", "id.intro.scriptDate", "id.intro.batchDate",
              "id.intro.finalText", "id.intro_sp.scriptDate",
              "id.intro_sp.batchDate", "id.intro_sp.finalText",
              "id.close.scriptDate")
  config <- latency_build_config(1L, header, field_timezone = "America/New_York")
  expect_equal(config$flow$questions, c("intro", "intro_sp", "close"))
  expect_equal(config$filters$population,
               'id.intro.finalText == "Yes" | id.intro_sp.finalText == "Yes"')
})

test_that("latency_build_config normalizes a raw bracket-form header", {
  # latency_discover_questions() also accepts raw on-disk headers
  # (id[<q>]field, before the readers make.names-munge them to dot-form). A raw
  # bilingual header must still cover BOTH opener branches -- previously it
  # collapsed to the first opener because .opener_population matched only
  # dot-form finalText names against the raw bracket-form header.
  header <- c("campaignid", "id[intro]scriptDate", "id[intro]batchDate",
              "id[intro]finalText", "id[intro_sp]scriptDate",
              "id[intro_sp]batchDate", "id[intro_sp]finalText",
              "id[close]scriptDate")
  config <- latency_build_config(1L, header, field_timezone = "America/New_York")
  expect_equal(config$flow$questions, c("intro", "intro_sp", "close"))
  expect_equal(config$filters$population,
               'id.intro.finalText == "Yes" | id.intro_sp.finalText == "Yes"')
})

test_that("build_ineligible_frame anchors on the opener set (bilingual)", {
  d <- op_frame(
    id.intro.scriptDate      = c(TS, ""),
    id.intro.batchDate       = c(TS, ""),
    id.intro_sp.scriptDate   = c("", TS),
    id.intro_sp.batchDate    = c("", TS),
    id.q1.scriptDate         = c(TS, TS),
    id.ineligible.scriptDate = c(TS, TS)     # both screened out at segment 1
  )
  config <- latency_build_config(1L, d, field_timezone = "America/New_York")
  res <- build_ineligible_frame(d, config)
  expect_equal(sum(res$n_ineligible), 2L)    # intro_sp respondent not dropped
})

test_that("latency funnel filters key on the opener set", {
  d <- op_frame(
    userid                 = c("a", "b"),
    id.intro.scriptDate    = c(TS, ""),
    id.intro_sp.scriptDate = c("", TS)
  )
  # date filter must keep the intro_sp send (both are on 2026-01-26)
  expect_equal(date_filter_keep_rows(d, as.Date("2026-01-26"), "UTC"), c(1L, 2L))
  # dedupe keeps both distinct respondents, ordered by their own opener send
  expect_equal(dedupe_keep_rows(d, "userid"), c(1L, 2L))
})

test_that("funnel filters keep every row when no opener send column exists", {
  d <- op_frame(userid = c("a", "b"), status = c("open", "open"))
  expect_equal(date_filter_keep_rows(d, as.Date("2026-01-26"), "UTC"), c(1L, 2L))
  expect_equal(dedupe_keep_rows(d, "userid"), c(1L, 2L))
})
