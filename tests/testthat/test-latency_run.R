# Coverage for R/latency_run.R: end-to-end orchestration with both pull and
# write mocked.

test_that("run_latency wires pull -> report -> write and returns gs:// path", {
  cfg_path <- test_path("fixtures/synthetic_config.yaml")
  csv_path <- test_path("fixtures/synthetic.csv")
  fx_data <- read.csv(csv_path, stringsAsFactors = FALSE)
  attr(fx_data, "source_csv_hash") <- "sha256:fixture"

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
    config_path = cfg_path,
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
