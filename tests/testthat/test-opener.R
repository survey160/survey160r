# R/opener.R is the single source of truth both the latency summary and the
# disposition transform resolve the opener set from. These tests lock the shared
# helpers and, crucially, the latency<->disposition alignment: if the two views
# ever resolved a different opener set or population, they would report different
# funnels. (The core .opening_questions / .opener_population / .question_timestamp /
# .dot_form_headers behaviour is covered by test-latency_opening_question.R.)

# One-row frame with the named columns present (blank values); presence is all
# the opener/population resolution looks at. check.names=FALSE keeps dot/bracket.
hdr_frame <- function(...) {
  cols <- c(...)
  d <- as.data.frame(as.list(stats::setNames(rep("", length(cols)), cols)),
                     stringsAsFactors = FALSE, check.names = FALSE)
  d$campaignid <- 1L
  d
}

test_that(".discover_openers resolves the opener set from a frame or raw header", {
  d <- hdr_frame("id.intro.scriptDate", "id.intro_latinos.scriptDate",
                 "id.close.scriptDate")
  expect_equal(.discover_openers(d), c("intro", "intro_latinos"))
  expect_equal(.discover_openers("id.FIRSTNET.scriptDate"), "FIRSTNET")
  expect_equal(.discover_openers(character(0)), "intro")            # minimal export
  # raw bracket-form header (as it arrives on disk) resolves identically
  expect_equal(.discover_openers(c("id[intro]scriptDate", "id[intro_sp]scriptDate")),
               c("intro", "intro_sp"))
})

test_that(".question_events is the send/reply disjunction over the opener set", {
  ts <- "2026-01-26 15:00:00.000000Z"
  d <- data.frame(
    id.intro.scriptDate    = c(ts, ""),
    id.intro_sp.scriptDate = c("", ts),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  ev <- .question_events(d, c("intro", "intro_sp"), "scriptDate")
  expect_equal(ev, c(TRUE, TRUE))                     # each recipient's own branch
  # equals !is.na(coalesce(...)) == OR of the per-column presence masks
  expect_equal(ev, !is.na(.question_timestamp(d, c("intro", "intro_sp"), "scriptDate")))
  # an absent field is null-safe -> all FALSE, not an error
  expect_equal(.question_events(d, c("intro", "intro_sp"), "batchDate"), c(FALSE, FALSE))
})

test_that("latency and disposition resolve the SAME opener set and population", {
  # The alignment guarantee: for every flow shape, the latency config path and
  # the disposition path derive an identical opener set and opt-in population.
  flows <- list(
    pure_intro  = hdr_frame("id.intro.scriptDate", "id.intro.finalText",
                            "id.close.scriptDate"),
    firstnet    = hdr_frame("id.FIRSTNET.scriptDate", "id.FIRSTNET.finalText",
                            "id.close.scriptDate"),
    bilingual   = hdr_frame("id.intro.scriptDate", "id.intro.finalText",
                            "id.intro_latinos.scriptDate", "id.intro_latinos.finalText",
                            "id.close.scriptDate"),
    raw_bracket = hdr_frame("id[intro]scriptDate", "id[intro]finalText",
                            "id[intro_sp]scriptDate", "id[intro_sp]finalText",
                            "id[close]scriptDate")
  )
  for (nm in names(flows)) {
    d <- flows[[nm]]
    cfg <- latency_build_config(1L, d, field_timezone = "America/New_York")
    expect_equal(.discover_openers(d), .opening_questions(cfg$flow$questions),
                 info = nm)
    expect_equal(.disposition_default_population(d), cfg$filters$population,
                 info = nm)
  }
})

test_that(".funnel_masks composes sent / engaged / opted-in on the opener set", {
  ts <- "2026-01-26 15:00:00.000000Z"
  d <- data.frame(
    id.intro.scriptDate = c(ts, ts, ts, ""),   # r4 never sent
    id.intro.batchDate  = c(ts, ts, "", ts),   # r3 no reply; r4 reply WITHOUT a send
    id.intro.finalText  = c("Yes", "No", "Yes", "Yes"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  m <- .funnel_masks(d, "intro", 'id.intro.finalText == "Yes"')
  expect_s3_class(m$send, "POSIXct")                 # returned for date/hour bucketing
  expect_equal(m$sent,     c(TRUE, TRUE, TRUE, FALSE))
  # engaged is `!is.na(reply) & sent`: r3 has a send but no reply; r4 has a reply
  # but no send -- neither is engaged (a reply presupposes a send).
  expect_equal(m$engaged,  c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(m$opted_in, c(TRUE, FALSE, TRUE, FALSE))   # r2 "No", r4 not sent
})

test_that("latency counts and disposition flags agree on sent/engaged/opted-in", {
  # The end-to-end alignment guarantee: for the same data, the latency summary's
  # aggregate counts equal the disposition transform's per-row flag sums, because
  # both derive sent/engaged/opted-in from the one .funnel_masks().
  ts <- "2026-01-26 15:00:00.000000Z"
  d <- data.frame(
    phone = as.character(1:5),
    campaignid = 1L,
    id.intro.scriptDate    = c(ts, ts, ts, "", ts),
    id.intro.batchDate     = c(ts, ts, "", "", ts),
    id.intro.finalText     = c("Yes", "No", "", "", "Yes"),
    id.close.scriptDate    = c(ts, "", "", "", ""),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  cfg <- latency_build_config(1L, d, field_timezone = "America/New_York")
  sf  <- build_summary_frame(d, cfg, survey_mode = "sms")
  disp <- disposition_run(1L, d, contacted_only = FALSE)$consolidated
  expect_equal(sum(sf$n_sent),    sum(disp$sent))
  expect_equal(sum(sf$n_engaged),   sum(disp$engaged))
  expect_equal(sum(sf$n_opted_in), sum(disp$opted_in))
})

test_that(".closing_questions resolves the close family, else falls back to close", {
  expect_equal(.closing_questions(c("intro", "q1", "close")), "close")
  expect_equal(.closing_questions(c("intro", "close", "close_sp")),
               c("close", "close_sp"))
  expect_equal(.closing_questions(c("intro", "q1")), "close")  # no close -> fallback
  expect_equal(.closing_questions(character(0)), "close")
  expect_false("closed" %in% .closing_questions(c("closed", "close")))  # word boundary
})

test_that("SMS completed counts every close-family branch (close + close_sp)", {
  # Bilingual campaign: English completers reach id.close, Spanish reach
  # id.close_sp. Both are completions -- the union must be counted (matching the
  # app's phonelist.completed), not just id.close. Previously close_sp was dropped.
  ts <- "2026-01-26 15:00:00.000000Z"
  d <- data.frame(
    phone = as.character(1:4),
    campaignid = 1L,
    id.intro.scriptDate    = c(ts, ts, ts, ts),   # all texted
    id.close.scriptDate    = c(ts, "", "", ""),   # 1 English completer
    id.close_sp.scriptDate = c("", ts, ts, ""),   # 2 Spanish completers
    stringsAsFactors = FALSE, check.names = FALSE
  )
  cfg <- latency_build_config(1L, d, field_timezone = "America/New_York")
  sf <- build_summary_frame(d, cfg, survey_mode = "sms")
  expect_equal(sum(sf$n_completed), 3L)          # close (1) + close_sp (2); was 1

  disp <- disposition_run(1L, d, contacted_only = FALSE)$consolidated
  expect_equal(sum(disp$completed), 3L)
  expect_equal(sum(sf$n_completed), sum(disp$completed))   # two views agree

  # the disposition projection retains the close family (close_sp not pruned)
  expect_true("id.close_sp.scriptDate" %in%
                disposition_input_columns(available = names(d)))
})
