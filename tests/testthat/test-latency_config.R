# Coverage for R/latency_config.R: load, validate, hash.

# Helper to build a minimal valid config.
.minimal_config <- function(overrides = list()) {
  base <- list(
    project_id = 1L,
    project_name = "Test",
    campaign_id = 1L,
    field_timezone = "America/New_York",
    flow = list(questions = c("intro", "q1", "close")),
    filters = list(
      population = 'id.intro.finalText == "Yes"',
      campaign_id_column = "campaignid"
    ),
    reports = list(time_bucket = "day"),
    texting_windows = list(
      list(date = "2026-01-26", start_hour = 16, end_hour = 24)
    )
  )
  modifyList(base, overrides)
}

# Helper to build a minimal valid data frame matching the minimal config.
.minimal_data <- function() {
  data.frame(
    campaignid = 1L,
    id.intro.finalText = "Yes",
    id.intro.scriptDate = "2026-01-26 21:00:00.000000Z",
    id.intro.batchDate = "2026-01-26 21:00:30.000000Z",
    id.q1.scriptDate = "2026-01-26 21:00:40.000000Z",
    id.q1.batchDate = "2026-01-26 21:01:00.000000Z",
    id.close.scriptDate = "2026-01-26 21:01:30.000000Z",
    stringsAsFactors = FALSE
  )
}

test_that("apply_config_defaults fills omitted optional keys", {
  cfg <- list(project_id = 1, campaign_id = 1, field_timezone = "UTC",
              flow = list(questions = c("a", "b")))
  out <- survey160r:::apply_config_defaults(cfg)
  expect_equal(out$reports$time_bucket, "day")
  expect_equal(out$filters$campaign_id_column, "campaignid")
  expect_equal(out$filters$population, 'id.intro.finalText == "Yes"')
  expect_equal(out$display_timezone, "UTC")
})

test_that("validate_config rejects a config that still carries thresholds", {
  d <- .minimal_data()
  cfg <- .minimal_config(list(reports = list(time_bucket = "day",
                                              thresholds = c(1, 3, 5, 10))))
  expect_error(validate_config(cfg, d),
               "fleet-locked")
})

test_that("validate_config accepts a minimal valid config", {
  expect_invisible(validate_config(.minimal_config(), .minimal_data()))
})

test_that("validate_config rejects unknown top-level keys", {
  cfg <- .minimal_config(list(weird_key = 1))
  expect_error(validate_config(cfg, .minimal_data()),
               "Unknown config keys")
})

test_that("validate_config requires project_id, campaign_id, field_timezone", {
  expect_error(validate_config(.minimal_config(list(project_id = NULL)),
                               .minimal_data()), "project_id")
  expect_error(validate_config(.minimal_config(list(campaign_id = NULL)),
                               .minimal_data()), "campaign_id")
  expect_error(validate_config(.minimal_config(list(field_timezone = NULL)),
                               .minimal_data()), "field_timezone")
})

test_that("validate_config rejects empty / single-question / dup / terminal flow", {
  d <- .minimal_data()
  expect_error(
    validate_config(.minimal_config(list(flow = list(questions = character(0)))), d),
    "non-empty"
  )
  expect_error(
    validate_config(.minimal_config(list(flow = list(questions = c("intro")))), d),
    "at least two"
  )
  expect_error(
    validate_config(.minimal_config(
      list(flow = list(questions = c("intro", "intro", "close")))), d),
    "duplicates"
  )
  expect_error(
    validate_config(.minimal_config(
      list(flow = list(questions = c("intro", "refusal", "close")))), d),
    "terminal states"
  )
})

test_that("validate_config rejects bad time_bucket", {
  d <- .minimal_data()
  expect_error(
    validate_config(.minimal_config(
      list(reports = list(time_bucket = "weekly"))), d),
    "'day' or 'hour'"
  )
})

test_that("validate_config rejects missing required columns", {
  d <- .minimal_data()
  d$id.q1.batchDate <- NULL
  expect_error(validate_config(.minimal_config(), d),
               "Required columns missing")
})

test_that("validate_config rejects mis-ordered flow (script before batch)", {
  d <- .minimal_data()
  # Swap so q1.scriptDate < intro.batchDate (= negative diff)
  d$id.q1.scriptDate <- "2026-01-26 20:00:00.000000Z"
  d$id.intro.batchDate <- "2026-01-26 21:00:00.000000Z"
  d$id.q1.batchDate <- "2026-01-26 20:00:30.000000Z"
  d$id.close.scriptDate <- "2026-01-26 20:01:00.000000Z"
  expect_error(validate_config(.minimal_config(), d),
               "Flow order check failed")
})

test_that("validate_config rejects texting_windows that miss survey dates", {
  d <- .minimal_data()
  d$id.intro.scriptDate <- "2026-02-01 21:00:00.000000Z"
  expect_error(validate_config(.minimal_config(), d),
               "texting_windows do not cover")
})

test_that("validate_config allows empty texting_windows (all-in-window)", {
  cfg <- .minimal_config(list(texting_windows = list()))
  expect_invisible(validate_config(cfg, .minimal_data()))
})

test_that("config_hash is stable across canonically-equal configs", {
  c1 <- list(a = 1, b = list(x = 2, y = 3))
  c2 <- list(b = list(y = 3, x = 2), a = 1)
  expect_equal(config_hash(c1), config_hash(c2))
})

test_that("config_hash differs when content differs", {
  c1 <- list(a = 1)
  c2 <- list(a = 2)
  expect_false(identical(config_hash(c1), config_hash(c2)))
})
