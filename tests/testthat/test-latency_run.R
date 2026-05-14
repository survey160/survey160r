# Coverage for R/latency_run.R: end-to-end orchestration with pull and write
# mocked. The runner is stateless and API-free -- config is built from the
# CSV header alone.

.fixture_data <- function(csv_path = test_path("fixtures/synthetic.csv")) {
  d <- read.csv(csv_path, stringsAsFactors = FALSE)
  attr(d, "source_csv_hash") <- "sha256:fixture"
  attr(d, "source_csv_path") <- "gs://campaign_results/1/1_raw_data_download.csv"
  d
}

test_that("run_latency wires pull -> build_config -> report -> write", {
  fx_data <- .fixture_data()
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    pull_csv_from_gcs = function(campaign_id, filename = NULL) {
      captured$pull_id <- campaign_id
      fx_data
    },
    upload_object = function(local_path, object_name, bucket, metadata) {
      captured$object_name <- object_name
      captured$bucket <- bucket
      captured$metadata <- metadata
      invisible(NULL)
    }
  )

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

test_that("run_latency surfaces validate_config failures on a malformed CSV", {
  # Strip the population-filter column so validate_columns_present aborts.
  fx_data <- .fixture_data()
  fx_data$id.intro.finalText <- NULL
  local_mocked_bindings(
    pull_csv_from_gcs = function(campaign_id, filename = NULL) fx_data,
    upload_object = function(local_path, object_name, bucket, metadata) {
      stop("uploader should not be called when validation fails")
    }
  )
  expect_error(
    run_latency(campaign_id = 1, bucket = "s160_analytics_dev"),
    "id\\.intro\\.finalText"
  )
})

test_that("run_latency overrides flow through to the config", {
  fx_data <- .fixture_data()
  captured <- new.env(parent = emptyenv())
  captured$cfg <- NULL
  local_mocked_bindings(
    pull_csv_from_gcs = function(campaign_id, filename = NULL) fx_data,
    latency_report = function(data, config) {
      captured$cfg <- config
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
    },
    upload_object = function(local_path, object_name, bucket, metadata) {
      invisible(NULL)
    }
  )

  run_latency(
    campaign_id = 7,
    bucket = "s160_analytics_dev",
    field_timezone = "America/New_York",
    project_id = 9999,
    texting_windows = list(list(date = "2026-01-26",
                                start_hour = 16, end_hour = 24)),
    date_filter = c("2026-01-26"),
    respondent_id_column = "userid"
  )

  expect_equal(captured$cfg$field_timezone, "America/New_York")
  expect_equal(captured$cfg$project_id, 9999L)
  expect_equal(captured$cfg$campaign_id, 7L)
  expect_equal(captured$cfg$filters$respondent_id_column, "userid")
  expect_equal(captured$cfg$filters$date_filter, "2026-01-26")
  expect_equal(length(captured$cfg$texting_windows), 1L)
})
