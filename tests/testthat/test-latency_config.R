# Coverage for R/campaign_config.R: load, validate, hash.

# Helper to build a minimal valid config.
.minimal_config <- function(overrides = list()) {
  base <- list(
    project_id = 1L,
    campaign_id = 1L,
    field_timezone = "America/New_York",
    flow = list(questions = c("intro", "q1", "close")),
    filters = list(
      population = 'id.intro.finalText == "Yes"',
      campaign_id_column = "campaignid"
    )
  )
  modifyList(base, overrides)
}

# Minimal valid data frame matching the minimal config. The shared helper
# adds a `userid` column; this test's minimal_config does not declare a
# respondent_id_column, so the extra column is harmless (validate_config
# only checks required columns are present, not that none are extra).
.minimal_data <- function() minimal_synthetic_data()

test_that("validate_config accepts a minimal valid config", {
  expect_invisible(latency_validate_config(.minimal_config(), .minimal_data()))
})

test_that("validate_config rejects unknown top-level keys", {
  cfg <- .minimal_config(list(weird_key = 1))
  expect_error(latency_validate_config(cfg, .minimal_data()),
               "Unknown config keys")
})

test_that("validate_config requires project_id, campaign_id, field_timezone", {
  expect_error(latency_validate_config(.minimal_config(list(project_id = NULL)),
                               .minimal_data()), "project_id")
  expect_error(latency_validate_config(.minimal_config(list(campaign_id = NULL)),
                               .minimal_data()), "campaign_id")
  expect_error(latency_validate_config(.minimal_config(list(field_timezone = NULL)),
                               .minimal_data()), "field_timezone")
})

test_that("validate_config rejects empty / single-question / dup / terminal flow", {
  d <- .minimal_data()
  expect_error(
    latency_validate_config(.minimal_config(list(flow = list(questions = character(0)))), d),
    "non-empty"
  )
  expect_error(
    latency_validate_config(.minimal_config(list(flow = list(questions = c("intro")))), d),
    "at least two"
  )
  expect_error(
    latency_validate_config(.minimal_config(
      list(flow = list(questions = c("intro", "intro", "close")))), d),
    "duplicates"
  )
  expect_error(
    latency_validate_config(.minimal_config(
      list(flow = list(questions = c("intro", "refusal", "close")))), d),
    "terminal states"
  )
})

test_that("validate_config rejects missing required columns", {
  d <- .minimal_data()
  d$id.q1.batchDate <- NULL
  expect_error(latency_validate_config(.minimal_config(), d),
               "Required columns missing")
})

test_that("validate_config rejects mis-ordered flow (script before batch)", {
  d <- .minimal_data()
  # Swap so q1.scriptDate < intro.batchDate (= negative diff)
  d$id.q1.scriptDate <- "2026-01-26 20:00:00.000000Z"
  d$id.intro.batchDate <- "2026-01-26 21:00:00.000000Z"
  d$id.q1.batchDate <- "2026-01-26 20:00:30.000000Z"
  d$id.close.scriptDate <- "2026-01-26 20:01:00.000000Z"
  expect_error(latency_validate_config(.minimal_config(), d),
               "Flow order check failed")
})

test_that("config_hash is stable across canonically-equal configs", {
  c1 <- list(a = 1, b = list(x = 2, y = 3))
  c2 <- list(b = list(y = 3, x = 2), a = 1)
  expect_equal(latency_config_hash(c1), latency_config_hash(c2))
})

test_that("config_hash differs when content differs", {
  c1 <- list(a = 1)
  c2 <- list(a = 2)
  expect_false(identical(latency_config_hash(c1), latency_config_hash(c2)))
})
