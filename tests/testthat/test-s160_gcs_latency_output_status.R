# Coverage for s160_gcs_latency_output_status(): returns destination Parquet
# metadata for skip_unchanged decisions in run_latency_all().

test_that("returns metadata for an existing latency Parquet", {
  local_mocked_bindings(
    gcs_list_objects = function(prefix, bucket, ...) {
      data.frame(
        name = c("latency/1980_latency.parquet"),
        updated = as.POSIXct("2026-01-02 12:00:00", tz = "UTC"),
        size = 4242,
        stringsAsFactors = FALSE
      )
    }
  )
  result <- s160_gcs_latency_output_status(1980, bucket = "analytics")
  expect_equal(result$name, "latency/1980_latency.parquet")
  expect_equal(result$size, 4242)
})

test_that("returns NULL when bucket is empty", {
  local_mocked_bindings(
    gcs_list_objects = function(prefix, bucket, ...) {
      data.frame(name = character(0), updated = as.POSIXct(character(0)),
                 size = numeric(0), stringsAsFactors = FALSE)
    }
  )
  expect_null(s160_gcs_latency_output_status(1980, bucket = "analytics"))
})

test_that("returns NULL when no exact match", {
  local_mocked_bindings(
    gcs_list_objects = function(prefix, bucket, ...) {
      data.frame(
        name = "latency/1980_latency.parquet.old",
        updated = as.POSIXct("2026-01-01", tz = "UTC"),
        size = 1,
        stringsAsFactors = FALSE
      )
    }
  )
  expect_null(s160_gcs_latency_output_status(1980, bucket = "analytics"))
})

test_that("errors on GCS failure", {
  local_mocked_bindings(
    gcs_list_objects = function(prefix, bucket, ...) stop("connection timeout")
  )
  expect_error(
    s160_gcs_latency_output_status(1980, bucket = "analytics"),
    "Failed to list latency output.*connection timeout"
  )
})

test_that("validates bucket argument", {
  expect_error(s160_gcs_latency_output_status(1980, bucket = ""),
               "bucket must be a non-empty string")
  expect_error(s160_gcs_latency_output_status(1980, bucket = c("a", "b")),
               "bucket must be a non-empty string")
})
