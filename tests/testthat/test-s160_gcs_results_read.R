test_that("default filename uses campaign_id pattern", {
  captured_args <- NULL
  mockery::stub(s160_gcs_results_read, "check_gcs_ready", NULL)
  mockery::stub(s160_gcs_results_read, "validate_campaign_id", "1980")
  mockery::stub(s160_gcs_results_read, "gcs_get_global_bucket", "test_bucket")
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), tmp)
  mockery::stub(s160_gcs_results_read, "tempfile", tmp)
  mockery::stub(s160_gcs_results_read, "gcs_get_object", function(...) {
    captured_args <<- list(...)
    TRUE
  })

  expect_message(s160_gcs_results_read(1980), "1980/1980_raw_data_download.csv")
  expect_equal(captured_args$object_name, "1980/1980_raw_data_download.csv")
  expect_false(file.exists(tmp))
})

test_that("custom filename overrides default", {
  captured_args <- NULL
  mockery::stub(s160_gcs_results_read, "check_gcs_ready", NULL)
  mockery::stub(s160_gcs_results_read, "validate_campaign_id", "1980")
  mockery::stub(s160_gcs_results_read, "gcs_get_global_bucket", "test_bucket")
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), tmp)
  mockery::stub(s160_gcs_results_read, "tempfile", tmp)
  mockery::stub(s160_gcs_results_read, "gcs_get_object", function(...) {
    captured_args <<- list(...)
    TRUE
  })

  expect_message(s160_gcs_results_read(1980, filename = "custom.csv"), "1980/custom.csv")
  expect_equal(captured_args$object_name, "1980/custom.csv")
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

test_that("filename with path separator is rejected", {
  mockery::stub(s160_gcs_results_read, "check_gcs_ready", NULL)
  mockery::stub(s160_gcs_results_read, "validate_campaign_id", "1980")

  expect_error(
    s160_gcs_results_read(1980, filename = "../evil.csv"),
    "path separators"
  )
  expect_error(
    s160_gcs_results_read(1980, filename = "subdir/file.csv"),
    "path separators"
  )
})

test_that("nonexistent destdir is rejected", {
  mockery::stub(s160_gcs_results_read, "check_gcs_ready", NULL)
  mockery::stub(s160_gcs_results_read, "validate_campaign_id", "1980")
  mockery::stub(s160_gcs_results_read, "gcs_get_global_bucket", "test_bucket")

  expect_error(
    suppressMessages(s160_gcs_results_read(1980, destdir = "/nonexistent/path")),
    "does not exist"
  )
})

test_that("destdir saves file and shows message", {
  mockery::stub(s160_gcs_results_read, "check_gcs_ready", NULL)
  mockery::stub(s160_gcs_results_read, "validate_campaign_id", "1980")
  mockery::stub(s160_gcs_results_read, "gcs_get_global_bucket", "test_bucket")
  tmp_dir <- tempdir()
  dest_file <- file.path(tmp_dir, "1980_raw_data_download.csv")
  writeLines(c("a,b", "1,2"), dest_file)
  mockery::stub(s160_gcs_results_read, "gcs_get_object", function(...) TRUE)

  expect_message(
    s160_gcs_results_read(1980, destdir = tmp_dir),
    "Saved to:"
  )
  expect_true(file.exists(dest_file))

  unlink(dest_file)
})
