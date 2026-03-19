test_that("default filename uses campaign_id pattern", {
  captured_args <- NULL
  mockery::stub(s160_gcs_results_read, "check_gcs_ready", NULL)
  mockery::stub(s160_gcs_results_read, "validate_campaign_id", "1980")
  mockery::stub(s160_gcs_results_read, "gcs_get_global_bucket", "test_bucket")
  mockery::stub(s160_gcs_results_read, "gcs_get_object", function(...) {
    captured_args <<- list(...)
    TRUE
  })
  # Create a fake CSV so read.csv succeeds
  tmp <- file.path(tempdir(), "1980_raw_data_download.csv")
  writeLines("a,b\n1,2", tmp)
  mockery::stub(s160_gcs_results_read, "file.path", tmp)

  expect_message(s160_gcs_results_read(1980), "1980/1980_raw_data_download.csv")
  expect_equal(captured_args$object_name, "1980/1980_raw_data_download.csv")

  unlink(tmp)
})

test_that("custom filename overrides default", {
  captured_args <- NULL
  mockery::stub(s160_gcs_results_read, "check_gcs_ready", NULL)
  mockery::stub(s160_gcs_results_read, "validate_campaign_id", "1980")
  mockery::stub(s160_gcs_results_read, "gcs_get_global_bucket", "test_bucket")
  mockery::stub(s160_gcs_results_read, "gcs_get_object", function(...) {
    captured_args <<- list(...)
    TRUE
  })
  tmp <- file.path(tempdir(), "custom.csv")
  writeLines("a,b\n1,2", tmp)
  mockery::stub(s160_gcs_results_read, "file.path", tmp)

  expect_message(s160_gcs_results_read(1980, filename = "custom.csv"), "1980/custom.csv")
  expect_equal(captured_args$object_name, "1980/custom.csv")

  unlink(tmp)
})

test_that("404 error gives clear file not found message", {
  mockery::stub(s160_gcs_results_read, "check_gcs_ready", NULL)
  mockery::stub(s160_gcs_results_read, "validate_campaign_id", "9999")
  mockery::stub(s160_gcs_results_read, "gcs_get_global_bucket", "test_bucket")
  mockery::stub(s160_gcs_results_read, "gcs_get_object", function(...) {
    stop("http_404 Unspecified error")
  })

  expect_error(
    suppressMessages(s160_gcs_results_read(9999)),
    "File not found.*test_bucket"
  )
})

test_that("non-404 error gives download failed message", {
  mockery::stub(s160_gcs_results_read, "check_gcs_ready", NULL)
  mockery::stub(s160_gcs_results_read, "validate_campaign_id", "1980")
  mockery::stub(s160_gcs_results_read, "gcs_get_global_bucket", "test_bucket")
  mockery::stub(s160_gcs_results_read, "gcs_get_object", function(...) {
    stop("connection timeout")
  })

  expect_error(
    suppressMessages(s160_gcs_results_read(1980)),
    "Failed to download.*connection timeout"
  )
})
