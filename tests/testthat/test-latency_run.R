# Coverage for R/latency_run.R: end-to-end orchestration with pull and write
# mocked. The runner is stateless and API-free -- config is built from the
# CSV header alone.

test_that("run_latency wires pull -> build_config -> report -> write", {
  captured <- new_capture()
  stub_pull_csv(load_synthetic_data(), capture = captured)
  stub_upload(capture = captured)

  path <- run_latency(
    campaign_id = 1,
    bucket = "s160_analytics_dev",
    run_by = "test_runner"
  )

  expect_equal(path, "gs://s160_analytics_dev/latency/1_latency.parquet")
  expect_equal(captured$pull_id, 1)
  expect_equal(captured$bucket, "s160_analytics_dev")
  expect_equal(
    captured$metadata$`survey160.source_csv_hash`,
    "sha256:fixture"
  )
})

test_that("run_latency forwards source_bucket to pull_csv_from_gcs", {
  captured <- new_capture()
  stub_pull_csv(load_synthetic_data(), capture = captured)
  stub_upload()

  run_latency(
    campaign_id = 1,
    bucket = "s160_analytics_dev",
    source_bucket = "campaign_results"
  )

  expect_equal(captured$pull_bucket, "campaign_results")
})

test_that("run_latency surfaces validate_config failures on a malformed CSV", {
  # Strip the population-filter column so validate_columns_present aborts.
  bad <- load_synthetic_data(mutate = function(d) {
    d$id.intro.finalText <- NULL
    d
  })
  stub_pull_csv(bad)
  stub_upload(must_not_call = TRUE)

  expect_error(
    run_latency(campaign_id = 1, bucket = "s160_analytics_dev"),
    "id\\.intro\\.finalText"
  )
})

test_that("run_latency overrides flow through to the config", {
  captured <- new_capture()
  stub_pull_csv(load_synthetic_data())
  stub_upload()
  local_mocked_bindings(
    latency_report = function(data, config, run_at = NULL) {
      captured$cfg <- config
      captured$run_at <- run_at
      list(
        consolidated = data.frame(
          campaign_id = integer(0), project_id = integer(0),
          date = as.Date(character(0)), hour_local = integer(0),
          segment = character(0), segment_index = integer(0),
          threshold_min = integer(0), n = integer(0),
          pct_le = numeric(0), pct_resp_hit_gt = numeric(0),
          n_respondents = integer(0), pct_resp_worst_gt = numeric(0),
          algorithm_version = character(0), config_hash = character(0),
          source_csv_hash = character(0),
          run_at_utc = as.POSIXct(character(0), tz = "UTC"),
          run_by = character(0)
        ),
        meta = list(algorithm_version = "2.0.0", config_hash = "h",
                    schema_version = "2")
      )
    }
  )

  fixed_run_at <- as.POSIXct("2026-02-14 09:00:00", tz = "UTC")
  run_latency(
    campaign_id = 7,
    bucket = "s160_analytics_dev",
    field_timezone = "America/New_York",
    project_id = 9999,
    date_filter = c("2026-01-26"),
    respondent_id_column = "userid",
    run_at = fixed_run_at
  )

  expect_equal(captured$cfg$field_timezone, "America/New_York")
  expect_equal(captured$cfg$project_id, 9999L)
  expect_equal(captured$cfg$campaign_id, 7L)
  expect_equal(captured$cfg$filters$respondent_id_column, "userid")
  expect_equal(captured$cfg$filters$date_filter, "2026-01-26")
  expect_equal(captured$run_at, fixed_run_at)
})

test_that("run_latency routes a custom uploader through write_to_gcs", {
  captured <- new_capture()
  stub_pull_csv(load_synthetic_data())
  custom_uploader <- function(local_path, object_name, bucket, metadata) {
    captured$called <- TRUE
    captured$bucket <- bucket
    invisible(NULL)
  }

  run_latency(
    campaign_id = 1,
    bucket = "s160_analytics_dev",
    uploader = custom_uploader
  )

  expect_true(isTRUE(captured$called))
  expect_equal(captured$bucket, "s160_analytics_dev")
})
