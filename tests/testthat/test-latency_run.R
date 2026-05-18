# Coverage for R/latency_run.R: pull -> build_config -> report orchestration
# in both the convenience (auto-build) and custom-config paths. The runner
# returns an in-memory result list; no GCS writes.

.fixture_data <- function(csv_path = test_path("fixtures/synthetic.csv")) {
  d <- read.csv(csv_path, stringsAsFactors = FALSE)
  attr(d, "source_csv_hash") <- "sha256:fixture"
  attr(d, "source_csv_path") <- "gs://campaign_results/1/1_raw_data_download.csv"
  d
}

test_that("latency_run wires pull -> build_config -> report and returns result", {
  fx_data <- .fixture_data()
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    s160_gcs_pull_csv = function(campaign_id, filename = NULL, bucket = NULL) {
      captured$pull_id <- campaign_id
      captured$pull_bucket <- bucket
      fx_data
    }
  )

  result <- latency_run(
    campaign_id = 1,
    bucket = "campaign_results_dev",
    run_by = "test_runner"
  )

  expect_type(result, "list")
  expect_true(all(c("consolidated", "diagnostics", "meta") %in% names(result)))
  expect_equal(captured$pull_id, 1)
  expect_equal(captured$pull_bucket, "campaign_results_dev")
  expect_equal(result$meta$source_csv_hash, "sha256:fixture")
  expect_equal(result$meta$source_csv_path,
               "gs://campaign_results/1/1_raw_data_download.csv")
  if (nrow(result$consolidated) > 0L) {
    expect_true(all(result$consolidated$run_by == "test_runner"))
  }
})

test_that("latency_run leaves run_by as NA when not supplied", {
  fx_data <- .fixture_data()
  local_mocked_bindings(
    s160_gcs_pull_csv = function(campaign_id, filename = NULL, bucket = NULL) {
      fx_data
    }
  )
  result <- latency_run(campaign_id = 1)
  expect_true(all(is.na(result$consolidated$run_by)))
})

test_that("latency_run surfaces validate_config failures on a malformed CSV", {
  # Strip the population-filter column so validate_columns_present aborts.
  fx_data <- .fixture_data()
  fx_data$id.intro.finalText <- NULL
  local_mocked_bindings(
    s160_gcs_pull_csv = function(campaign_id, filename = NULL, bucket = NULL) {
      fx_data
    }
  )
  expect_error(
    latency_run(campaign_id = 1),
    "id\\.intro\\.finalText"
  )
})

test_that("latency_run forwards `...` overrides to latency_build_config", {
  fx_data <- .fixture_data()
  captured <- new.env(parent = emptyenv())
  captured$cfg <- NULL
  local_mocked_bindings(
    s160_gcs_pull_csv = function(campaign_id, filename = NULL, bucket = NULL) {
      fx_data
    },
    latency_report = function(data, config, run_at = NULL) {
      captured$cfg <- config
      list(
        consolidated = data.frame(),
        diagnostics = list(),
        meta = list(algorithm_version = "2.0.0", config_hash = "h",
                    schema_version = "2")
      )
    }
  )

  latency_run(
    campaign_id = 7,
    field_timezone = "America/New_York",
    project_id = 9999,
    date_filter = c("2026-01-26"),
    respondent_id_column = "userid"
  )

  expect_equal(captured$cfg$field_timezone, "America/New_York")
  expect_equal(captured$cfg$project_id, 9999L)
  expect_equal(captured$cfg$campaign_id, 7L)
  expect_equal(captured$cfg$filters$respondent_id_column, "userid")
  expect_equal(captured$cfg$filters$date_filter, "2026-01-26")
})

test_that("latency_run accepts a pre-built config and skips build_config", {
  fx_data <- .fixture_data()
  captured <- new.env(parent = emptyenv())
  custom_cfg <- latency_build_config(1, fx_data,
                                     field_timezone = "America/New_York")

  build_called <- FALSE
  local_mocked_bindings(
    s160_gcs_pull_csv = function(campaign_id, filename = NULL, bucket = NULL) {
      fx_data
    },
    latency_build_config = function(...) {
      build_called <<- TRUE
      NULL
    },
    latency_report = function(data, config, run_at = NULL) {
      captured$cfg <- config
      list(consolidated = data.frame(), diagnostics = list(), meta = list())
    }
  )

  latency_run(campaign_id = 1, config = custom_cfg)

  expect_false(build_called)
  expect_identical(captured$cfg, custom_cfg)
})

test_that("latency_run rejects both `config` and `...` being supplied", {
  fx_data <- .fixture_data()
  cfg <- latency_build_config(1, fx_data)
  local_mocked_bindings(
    s160_gcs_pull_csv = function(campaign_id, filename = NULL, bucket = NULL) {
      fx_data
    }
  )
  expect_error(
    latency_run(campaign_id = 1, config = cfg, field_timezone = "UTC"),
    "either .*config.* or"
  )
})

test_that("latency_run forwards an explicit run_at to latency_report", {
  fx_data <- .fixture_data()
  captured <- new.env(parent = emptyenv())
  fixed_at <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  local_mocked_bindings(
    s160_gcs_pull_csv = function(campaign_id, filename = NULL, bucket = NULL) {
      fx_data
    },
    latency_report = function(data, config, run_at = NULL) {
      captured$run_at <- run_at
      list(consolidated = data.frame(), diagnostics = list(), meta = list())
    }
  )
  latency_run(campaign_id = 1, run_at = fixed_at)
  expect_equal(captured$run_at, fixed_at)
})
