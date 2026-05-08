# Coverage for R/latency_io.R: schema pinning, parquet write/read round-trip,
# pull_csv_from_gcs hash attribute, write_to_gcs validation.

.load_synthetic_result <- function() {
  csv_path <- test_path("fixtures/synthetic.csv")
  cfg_path <- test_path("fixtures/synthetic_config.yaml")
  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  config <- read_config(cfg_path)
  list(result = latency_report(data, config), config = config)
}

test_that("latency_parquet_schema returns the expected columns and types", {
  schema <- survey160r:::latency_parquet_schema()
  expect_equal(
    names(schema),
    c("campaign_id", "project_id", "date", "hour_local",
      "segment", "segment_index", "threshold_min", "n",
      "pct_le", "pct_resp_hit_gt",
      "n_respondents", "pct_resp_worst_gt",
      "algorithm_version", "config_hash", "source_csv_hash",
      "run_at_utc", "run_by")
  )
})

test_that("write_to_gcs produces a Parquet conforming to the pinned schema", {
  fx <- .load_synthetic_result()
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    upload_object = function(local_path, object_name, bucket, metadata) {
      captured$local_path <- local_path
      captured$object_name <- object_name
      captured$bucket <- bucket
      captured$metadata <- metadata
      # Copy the file out of the temp before write_to_gcs unlinks it.
      persisted <- tempfile(fileext = ".parquet")
      file.copy(local_path, persisted, overwrite = TRUE)
      captured$persisted <- persisted
      invisible(NULL)
    }
  )

  path <- write_to_gcs(
    result = fx$result,
    campaign_id = 1,
    bucket = "s160_analytics_dev",
    source_csv_hash = "sha256:test"
  )
  expect_equal(path, "gs://s160_analytics_dev/latency/1_latency.parquet")
  expect_equal(captured$object_name, "latency/1_latency.parquet")
  expect_equal(captured$bucket, "s160_analytics_dev")

  # Round-trip read.
  rt <- arrow::read_parquet(captured$persisted)
  expect_equal(nrow(rt), nrow(fx$result$consolidated))
  expect_equal(unique(rt$source_csv_hash), "sha256:test")
  expect_equal(unique(rt$algorithm_version), "2.0.0")

  unlink(captured$persisted)
})

test_that("write_to_gcs rejects a result frame with schema drift", {
  fx <- .load_synthetic_result()
  bad <- fx$result
  bad$consolidated$unexpected <- "drift"
  expect_error(
    write_to_gcs(bad, 1, "s160_analytics_dev"),
    "Schema drift"
  )
  bad2 <- fx$result
  bad2$consolidated$campaign_id <- NULL
  expect_error(
    write_to_gcs(bad2, 1, "s160_analytics_dev"),
    "Schema drift"
  )
})

test_that("write_to_gcs validates result and bucket arguments", {
  expect_error(write_to_gcs(list(), 1, "b"), "consolidated")
  fx <- .load_synthetic_result()
  expect_error(write_to_gcs(fx$result, 1, ""), "non-empty string")
  expect_error(write_to_gcs(fx$result, 1, c("a", "b")), "non-empty string")
})

test_that("write_to_gcs handles an empty consolidated frame", {
  fx <- .load_synthetic_result()
  fx$result$consolidated <- fx$result$consolidated[0, ]
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    upload_object = function(local_path, object_name, bucket, metadata) {
      captured$object_name <- object_name
      invisible(NULL)
    }
  )
  path <- write_to_gcs(fx$result, 999, "s160_analytics_dev",
                      source_csv_hash = "sha256:empty",
                      run_by = "test")
  expect_equal(path, "gs://s160_analytics_dev/latency/999_latency.parquet")
})

test_that("pull_csv_from_gcs sets a source_csv_hash attribute", {
  stub_gcs_base()
  stub_gcs_download_ok()
  data <- suppressMessages(pull_csv_from_gcs(1980))
  expect_true(grepl("^sha256:", attr(data, "source_csv_hash")))
})
