# Coverage for R/campaign_run.R: build_config -> report orchestration in
# both the convenience (auto-build) and custom-config paths. The runner
# takes caller-supplied data; no I/O, no source mocking required.

test_that("campaign_run wires build_config -> report and returns result", {
  result <- campaign_run(
    campaign_id = 1,
    data = load_synthetic_data(),
    run_by = "test_runner"
  )

  expect_type(result, "list")
  expect_true(all(c("consolidated", "diagnostics", "meta") %in% names(result)))
  expect_equal(result$meta$source_csv_hash, "sha256:fixture")
  expect_equal(result$meta$source_csv_path,
               "gs://campaign_results/1/1_raw_data_download.csv")
  if (nrow(result$consolidated) > 0L) {
    expect_true(all(result$consolidated$run_by == "test_runner"))
  }
})

test_that("campaign_run leaves run_by as NA when not supplied", {
  result <- campaign_run(campaign_id = 1, data = load_synthetic_data())
  expect_true(all(is.na(result$consolidated$run_by)))
})

test_that("campaign_run propagates NA provenance for un-stamped sources", {
  # Caller built `data` without the GCS-style attrs -- meta carries NA,
  # the algorithm still runs.
  data <- load_synthetic_data()
  attr(data, "source_csv_hash") <- NULL
  attr(data, "source_csv_path") <- NULL
  result <- campaign_run(campaign_id = 1, data = data)
  expect_true(is.na(result$meta$source_csv_hash))
  expect_true(is.na(result$meta$source_csv_path))
})

test_that("campaign_run surfaces validate_config failures on a malformed CSV", {
  # Strip the population-filter column so validate_columns_present aborts.
  data <- load_synthetic_data(mutate = function(d) {
    d$id.intro.finalText <- NULL
    d
  })
  expect_error(
    campaign_run(campaign_id = 1, data = data),
    "id\\.intro\\.finalText"
  )
})

test_that("campaign_run forwards `...` overrides to campaign_build_config", {
  captured <- new_capture()
  local_mocked_bindings(
    campaign_report = function(data, config, run_at = NULL) {
      captured$cfg <- config
      list(consolidated = data.frame(), diagnostics = list(),
           meta = list(algorithm_version = "2.0.0", config_hash = "h",
                       schema_version = "2"))
    }
  )

  campaign_run(
    campaign_id = 7,
    data = load_synthetic_data(),
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

test_that("campaign_run accepts a pre-built config and skips build_config", {
  data <- load_synthetic_data()
  custom_cfg <- campaign_build_config(1, data,
                                     field_timezone = "America/New_York")

  captured <- new_capture()
  build_called <- FALSE
  local_mocked_bindings(
    campaign_build_config = function(...) {
      build_called <<- TRUE
      NULL
    },
    campaign_report = function(data, config, run_at = NULL) {
      captured$cfg <- config
      list(consolidated = data.frame(), diagnostics = list(), meta = list())
    }
  )

  campaign_run(campaign_id = 1, data = data, config = custom_cfg)

  expect_false(build_called)
  expect_identical(captured$cfg, custom_cfg)
})

test_that("campaign_run rejects both `config` and `...` being supplied", {
  data <- load_synthetic_data()
  cfg <- campaign_build_config(1, data)
  expect_error(
    campaign_run(campaign_id = 1, data = data,
                config = cfg, field_timezone = "UTC"),
    "either .*config.* or"
  )
})

test_that("campaign_run forwards an explicit run_at to campaign_report", {
  captured <- new_capture()
  fixed_at <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  local_mocked_bindings(
    campaign_report = function(data, config, run_at = NULL) {
      captured$run_at <- run_at
      list(consolidated = data.frame(), diagnostics = list(), meta = list())
    }
  )
  campaign_run(campaign_id = 1, data = load_synthetic_data(), run_at = fixed_at)
  expect_equal(captured$run_at, fixed_at)
})
