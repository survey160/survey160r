# Coverage for campaign_discover_questions() and campaign_build_config() in R/campaign_config.R:
# pure config derivation from a campaign id and CSV column names.

test_that("discover_questions picks question ids in column order, drops terminal states", {
  cols <- c("campaignid", "userid",
            "id.intro.scriptDate", "id.intro.batchDate",
            "id.q1.scriptDate", "id.q1.batchDate",
            "id.q2.scriptDate", "id.q2.batchDate",
            "id.refusal.scriptDate", "id.ineligible.scriptDate",
            "id.close.scriptDate")
  expect_equal(campaign_discover_questions(cols),
               c("intro", "q1", "q2", "close"))
})

test_that("discover_questions accepts a data frame or a character vector", {
  df <- data.frame(`id.intro.scriptDate` = NA,
                   `id.close.scriptDate` = NA,
                   check.names = FALSE)
  expect_equal(campaign_discover_questions(df), c("intro", "close"))
})

test_that("discover_questions matches the raw-bracket CSV header form too", {
  raw <- c("id[intro]scriptDate", "id[q1]scriptDate", "id[close]scriptDate")
  expect_equal(campaign_discover_questions(raw), c("intro", "q1", "close"))
})

test_that("discover_questions ignores non-scriptDate columns and dedupes", {
  cols <- c("id.intro.scriptDate", "id.intro.scriptDate",
            "id.intro.batchDate", "campaignid", "noise")
  expect_equal(campaign_discover_questions(cols), "intro")
})

.make_data <- function() {
  minimal_synthetic_data(with_rows = FALSE)
}

test_that("build_config applies stateless defaults", {
  cfg <- campaign_build_config(1L, .make_data())
  expect_equal(cfg$campaign_id, 1L)
  expect_equal(cfg$project_id, 1L)                # default = campaign_id
  expect_equal(cfg$field_timezone, "UTC")
  expect_equal(cfg$flow$questions, c("intro", "q1", "close"))
  expect_equal(cfg$filters$population, 'id.intro.finalText == "Yes"')
  expect_equal(cfg$filters$campaign_id_column, "campaignid")
  expect_null(cfg$filters$respondent_id_column)
  expect_null(cfg$filters$date_filter)
  # No dead fields leak through.
  expect_null(cfg$texting_windows)
  expect_null(cfg$reports)
  expect_null(cfg$project_name)
  expect_null(cfg$wave_run)
  expect_null(cfg$display_timezone)
})

test_that("build_config honors all named overrides", {
  cfg <- campaign_build_config(
    7L, .make_data(),
    field_timezone = "America/New_York",
    project_id = 9999,
    date_filter = "2026-01-26",
    respondent_id_column = "userid"
  )
  expect_equal(cfg$campaign_id, 7L)
  expect_equal(cfg$project_id, 9999L)
  expect_equal(cfg$field_timezone, "America/New_York")
  expect_equal(cfg$filters$respondent_id_column, "userid")
  expect_equal(cfg$filters$date_filter, "2026-01-26")
})

test_that("build_config errors when fewer than two questions are discoverable", {
  too_few <- data.frame(campaignid = integer(0),
                        `id.intro.scriptDate` = character(0),
                        check.names = FALSE)
  expect_error(
    campaign_build_config(1L, too_few),
    "at least two questions"
  )
})

test_that("build_config result passes validate_config against real data", {
  csv_path <- test_path("fixtures/synthetic.csv")
  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  cfg <- campaign_build_config(1L, data)
  expect_silent(campaign_validate_config(cfg, data))
})
