# R/opener.R is the single source of truth both the latency summary and the
# disposition transform resolve the opener set from. These tests lock the shared
# helpers and, crucially, the latency<->disposition alignment: if the two views
# ever resolved a different opener set or population, they would report different
# funnels. (The core .opening_questions / .opener_population / .opener_timestamp /
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

test_that(".opener_events is the send/reply disjunction over the opener set", {
  ts <- "2026-01-26 15:00:00.000000Z"
  d <- data.frame(
    id.intro.scriptDate    = c(ts, ""),
    id.intro_sp.scriptDate = c("", ts),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  ev <- .opener_events(d, c("intro", "intro_sp"), "scriptDate")
  expect_equal(ev, c(TRUE, TRUE))                     # each recipient's own branch
  # equals !is.na(coalesce(...)) == OR of the per-column presence masks
  expect_equal(ev, !is.na(.opener_timestamp(d, c("intro", "intro_sp"), "scriptDate")))
  # an absent field is null-safe -> all FALSE, not an error
  expect_equal(.opener_events(d, c("intro", "intro_sp"), "batchDate"), c(FALSE, FALSE))
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
