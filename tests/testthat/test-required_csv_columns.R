# Coverage for R/campaign_config.R::required_csv_columns and the
# column-projection parity contract: reading only the required columns must
# produce identical campaign_run() output to a full read. The projection guard
# is what protects against silently dropping a non-flow column the algorithm
# depends on (web_complete, id.ineligible.scriptDate, id.intro.finalText).

test_that("required_csv_columns covers timestamps + support + id columns", {
  cfg <- synthetic_config()  # questions intro, q1, q2, close; pop on finalText

  cols <- required_csv_columns(cfg)

  # Per-question scriptDate (all) + batchDate (all but last).
  expect_true(all(c("id.intro.scriptDate", "id.q1.scriptDate",
                    "id.q2.scriptDate", "id.close.scriptDate") %in% cols))
  expect_true(all(c("id.intro.batchDate", "id.q1.batchDate",
                    "id.q2.batchDate") %in% cols))
  # Fixed non-flow support columns that would silently change output if dropped.
  expect_true(all(c("id.intro.finalText", "web_complete",
                    "id.ineligible.scriptDate") %in% cols))
  # Population-filter column (from `id.intro.finalText == "Yes"`) + id columns.
  expect_true("campaignid" %in% cols)
  expect_true("userid" %in% cols)        # respondent_id_column in synthetic_config
  expect_false(anyDuplicated(cols) > 0)
})

test_that("required_csv_columns retains close-message Text columns only via `available`", {
  cfg <- synthetic_config()
  header <- c("campaignid", "id.intro.finalText", "id.close.scriptText",
              "id.closeB.batchText", "id.other.scriptText")

  # Without `available`, the data-dependent close-Text columns can't be matched.
  expect_false(any(c("id.close.scriptText", "id.closeB.batchText") %in%
                     required_csv_columns(cfg)))

  # With `available`, the close-message Text columns are retained (so
  # detect_survey_mode can still tell t2w_external from sms), but a non-close
  # *.scriptText column is not.
  cols <- required_csv_columns(cfg, available = header)
  expect_true(all(c("id.close.scriptText", "id.closeB.batchText") %in% cols))
  expect_false("id.other.scriptText" %in% cols)
})

test_that("required_csv_columns picks up a custom population column", {
  cfg <- synthetic_config()
  cfg$filters$population <- 'consent_flag == 1 & id.intro.finalText == "Yes"'

  cols <- required_csv_columns(cfg)

  expect_true("consent_flag" %in% cols)
  expect_true("id.intro.finalText" %in% cols)
})

test_that("required_csv_columns omits respondent id when unset, handles empty population", {
  cfg <- synthetic_config()
  cfg$filters$respondent_id_column <- NULL
  cfg$filters$population <- ""

  cols <- required_csv_columns(cfg)

  expect_false("userid" %in% cols)
  # Empty population contributes no extra columns but support cols stay.
  expect_true("id.intro.finalText" %in% cols)
})

test_that("projection read yields identical campaign_run output to a full read", {
  # Augment the canonical fixture with the two non-flow columns that the
  # projection must retain: a t2w web_complete signal and an ineligible
  # timestamp. Dropping either would change consolidated output, so this
  # asserts they survive the projection.
  base <- read.csv(test_path("fixtures/synthetic.csv"), stringsAsFactors = FALSE)
  base$web_complete <- c(1L, 0L, 0L, 0L)[seq_len(nrow(base))]
  base$id.ineligible.scriptDate <- ""
  base$id.ineligible.scriptDate[1] <- base$id.q1.scriptDate[1]

  tmp <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(base, tmp, row.names = FALSE)

  run_at <- as.POSIXct("2026-05-27 00:00:00", tz = "UTC")

  full <- s160_read_csv(tmp)
  res_full <- campaign_run(1, full, field_timezone = "America/New_York",
                           run_at = run_at)

  header <- s160_csv_header(tmp)
  cfg <- campaign_build_config(1, header, field_timezone = "America/New_York")
  pruned <- s160_read_csv(tmp, columns = required_csv_columns(cfg, header))
  res_pruned <- campaign_run(1, pruned, field_timezone = "America/New_York",
                             run_at = run_at)

  expect_identical(res_full$consolidated, res_pruned$consolidated)
})

test_that("projection retains close-Text cols so t2w_external survives pruning", {
  # No web completes + a per-respondent (distinct) survey URL in the close
  # message => detect_survey_mode classifies "t2w_external". The close-message
  # Text columns are data-dependent names; if the projection drops them the
  # campaign silently falls back to "sms". This asserts full == pruned AND
  # that the mode really is t2w_external (so the guard has teeth).
  base <- read.csv(test_path("fixtures/synthetic.csv"), stringsAsFactors = FALSE)
  base$web_complete <- 0L
  base$id.close.scriptText <- sprintf("see https://survey.example/r/%d",
                                      seq_len(nrow(base)))

  tmp <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(base, tmp, row.names = FALSE)
  run_at <- as.POSIXct("2026-05-27 00:00:00", tz = "UTC")

  full <- s160_read_csv(tmp)
  res_full <- campaign_run(1, full, field_timezone = "America/New_York",
                           run_at = run_at)

  header <- s160_csv_header(tmp)
  cfg <- campaign_build_config(1, header, field_timezone = "America/New_York")
  pruned <- s160_read_csv(tmp, columns = required_csv_columns(cfg, header))
  res_pruned <- campaign_run(1, pruned, field_timezone = "America/New_York",
                             run_at = run_at)

  expect_identical(res_full$consolidated, res_pruned$consolidated)
  expect_equal(unique(res_full$consolidated$survey_mode), "t2w_external")
})
