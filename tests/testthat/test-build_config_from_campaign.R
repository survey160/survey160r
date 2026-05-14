# Coverage for discover_questions() and build_config_from_campaign() in
# R/latency_config.R: stateless config derivation from CSV header + API.

test_that("discover_questions picks question ids in column order, drops terminal states", {
  cols <- c("campaignid", "userid",
            "id.intro.scriptDate", "id.intro.batchDate",
            "id.q1.scriptDate", "id.q1.batchDate",
            "id.q2.scriptDate", "id.q2.batchDate",
            "id.refusal.scriptDate", "id.ineligible.scriptDate",
            "id.close.scriptDate")
  expect_equal(discover_questions(cols),
               c("intro", "q1", "q2", "close"))
})

test_that("discover_questions accepts a data frame or a character vector", {
  df <- data.frame(`id.intro.scriptDate` = NA,
                   `id.close.scriptDate` = NA,
                   check.names = FALSE)
  expect_equal(discover_questions(df), c("intro", "close"))
})

test_that("discover_questions matches the raw-bracket CSV header form too", {
  raw <- c("id[intro]scriptDate", "id[q1]scriptDate", "id[close]scriptDate")
  expect_equal(discover_questions(raw), c("intro", "q1", "close"))
})

test_that("discover_questions ignores non-scriptDate columns and dedupes", {
  cols <- c("id.intro.scriptDate", "id.intro.scriptDate",
            "id.intro.batchDate", "campaignid", "noise")
  expect_equal(discover_questions(cols), "intro")
})

.csv_columns <- function() {
  c("campaignid", "userid", "id.intro.finalText",
    "id.intro.scriptDate", "id.intro.batchDate",
    "id.q1.scriptDate", "id.q1.batchDate",
    "id.close.scriptDate")
}

.make_data <- function() {
  cols <- .csv_columns()
  setNames(as.data.frame(matrix(NA, nrow = 0, ncol = length(cols)),
                         stringsAsFactors = FALSE), cols)
}

.stub_api <- function(name = "Demo", organizationid = 9L) {
  function(campaign_id) {
    data.frame(campaignid = as.integer(campaign_id),
               name = name,
               organizationid = organizationid,
               stringsAsFactors = FALSE)
  }
}

test_that("build_config_from_campaign applies stateless defaults", {
  cfg <- build_config_from_campaign(
    1L, .make_data(),
    campaign_api_get = .stub_api()
  )
  expect_equal(cfg$campaign_id, 1L)
  expect_equal(cfg$project_id, 1L)                # default = campaign_id
  expect_equal(cfg$project_name, "Demo")
  expect_equal(cfg$field_timezone, "UTC")
  expect_equal(cfg$display_timezone, "UTC")       # default = field_timezone
  expect_equal(cfg$flow$questions, c("intro", "q1", "close"))
  expect_equal(cfg$filters$population,
               'id.intro.finalText == "Yes"')
  expect_equal(cfg$filters$campaign_id_column, "campaignid")
  expect_null(cfg$filters$respondent_id_column)
  expect_null(cfg$filters$date_filter)
  expect_equal(cfg$texting_windows, list())
  expect_equal(cfg$reports$time_bucket, "day")
  expect_true(grepl("^1_\\d{8}T\\d{6}Z$", cfg$wave_run))
  expect_equal(attr(cfg, "organizationid"), 9L)
})

test_that("build_config_from_campaign honors all overrides", {
  cfg <- build_config_from_campaign(
    7L, .make_data(),
    overrides = list(
      field_timezone = "America/New_York",
      project_id = 9999,
      texting_windows = list(list(date = "2026-01-26",
                                  start_hour = 16, end_hour = 24)),
      date_filter = "2026-01-26",
      respondent_id_column = "userid",
      time_bucket = "hour"
    ),
    campaign_api_get = .stub_api(name = "Sample Wave W1", organizationid = 3L)
  )
  expect_equal(cfg$campaign_id, 7L)
  expect_equal(cfg$project_id, 9999L)
  expect_equal(cfg$project_name, "Sample Wave W1")
  expect_equal(cfg$field_timezone, "America/New_York")
  expect_equal(cfg$display_timezone, "America/New_York")
  expect_equal(cfg$filters$respondent_id_column, "userid")
  expect_equal(cfg$filters$date_filter, "2026-01-26")
  expect_equal(cfg$reports$time_bucket, "hour")
  expect_equal(length(cfg$texting_windows), 1L)
})

test_that("build_config_from_campaign falls back to a placeholder project_name when API missing it", {
  api_no_name <- function(campaign_id) {
    data.frame(campaignid = as.integer(campaign_id),
               organizationid = NA_integer_,
               stringsAsFactors = FALSE)
  }
  cfg <- build_config_from_campaign(
    42L, .make_data(),
    campaign_api_get = api_no_name
  )
  expect_equal(cfg$project_name, "Campaign 42")
  expect_null(attr(cfg, "organizationid"))
})

test_that("build_config_from_campaign errors when fewer than two questions are discoverable", {
  too_few <- data.frame(campaignid = integer(0),
                        `id.intro.scriptDate` = character(0),
                        check.names = FALSE)
  expect_error(
    build_config_from_campaign(
      1L, too_few, campaign_api_get = .stub_api()
    ),
    "at least two questions"
  )
})

test_that("build_config_from_campaign rejects non-list overrides", {
  expect_error(
    build_config_from_campaign(1L, .make_data(), overrides = "nope"),
    "overrides must be a list"
  )
})

test_that("build_config_from_campaign rejects a non-function campaign_api_get", {
  expect_error(
    build_config_from_campaign(1L, .make_data(),
                               campaign_api_get = "not a function"),
    "campaign_api_get must be a function"
  )
})

test_that("build_config_from_campaign result passes validate_config against real data", {
  csv_path <- test_path("fixtures/synthetic.csv")
  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  cfg <- build_config_from_campaign(
    1L, data,
    campaign_api_get = .stub_api()
  )
  expect_silent(validate_config(cfg, data))
})
