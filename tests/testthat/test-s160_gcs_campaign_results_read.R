test_that("default call uses campaign_id filename, cleans up temp, no working dir leak", {
  stub_gcs_base()
  captured <- new.env(parent = emptyenv())
  stub_gcs_download_ok(capture_env = captured)

  wd_before <- list.files(getwd(), pattern = "\\.csv$")
  expect_message(s160_gcs_campaign_results_read(1980), "1980/1980_raw_data_download.csv")
  wd_after <- list.files(getwd(), pattern = "\\.csv$")

  expect_equal(captured$args$object_name, "1980/1980_raw_data_download.csv")
  expect_false(file.exists(captured$args$saveToDisk))
  expect_equal(wd_after, wd_before)
})

test_that("custom filename overrides default", {
  stub_gcs_base()
  captured <- new.env(parent = emptyenv())
  stub_gcs_download_ok(capture_env = captured)

  expect_message(s160_gcs_campaign_results_read(1980, filename = "custom.csv"), "1980/custom.csv")
  expect_equal(captured$args$object_name, "1980/custom.csv")
})

test_that("404 error gives clear file not found message", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(...) data.frame(name = character(0), size = numeric(0), stringsAsFactors = FALSE),
    gcs_get_object = function(...) stop("http_404 Unspecified error")
  )

  expect_error(
    suppressMessages(s160_gcs_campaign_results_read(9999)),
    "File not found.*test_bucket"
  )
})

test_that("non-404 error gives download failed message", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(...) data.frame(name = character(0), size = numeric(0), stringsAsFactors = FALSE),
    gcs_get_object = function(...) stop("connection timeout")
  )

  expect_error(
    suppressMessages(s160_gcs_campaign_results_read(1980)),
    "Failed to download.*connection timeout"
  )
})

test_that("filename with path separator is rejected", {
  stub_gcs_base()

  expect_error(
    s160_gcs_campaign_results_read(1980, filename = "../evil.csv"),
    "path separators"
  )
  expect_error(
    s160_gcs_campaign_results_read(1980, filename = "subdir/file.csv"),
    "path separators"
  )
})

test_that("non-string destdir is rejected", {
  stub_gcs_base()

  expect_error(s160_gcs_campaign_results_read(1980, destdir = 123), "single character string")
  expect_error(s160_gcs_campaign_results_read(1980, destdir = TRUE), "single character string")
  expect_error(s160_gcs_campaign_results_read(1980, destdir = c("a", "b")), "single character string")
})

test_that("nonexistent destdir is rejected", {
  stub_gcs_base()

  expect_error(
    suppressMessages(s160_gcs_campaign_results_read(1980, destdir = "/nonexistent/path")),
    "does not exist"
  )
})

test_that("destdir saves file, prints path, and works with '.'", {
  stub_gcs_base()
  stub_gcs_download_ok()
  tmp_dir <- tempdir()
  dest_file <- file.path(tmp_dir, "1980_raw_data_download.csv")

  expect_message(s160_gcs_campaign_results_read(1980, destdir = tmp_dir), "Saved to:")
  expect_true(file.exists(dest_file))
  unlink(dest_file)

  withr::with_dir(tmp_dir, {
    expect_message(s160_gcs_campaign_results_read(1980, destdir = "."), "Saved to:")
    expect_true(file.exists("1980_raw_data_download.csv"))
  })
  unlink(dest_file)
})
