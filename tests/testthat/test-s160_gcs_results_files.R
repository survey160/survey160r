test_that("strips campaign prefix from filenames", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(...) {
      data.frame(
        name = c("1980/1980_raw_data_download.csv", "1980/1980_summary.csv"),
        stringsAsFactors = FALSE
      )
    }
  )
  expect_equal(s160_gcs_results_files(1980), c("1980_raw_data_download.csv", "1980_summary.csv"))
})

test_that("returns empty character with message when no files", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(...) data.frame(name = character(0), stringsAsFactors = FALSE)
  )
  expect_message(result <- s160_gcs_results_files(9999), "No files found")
  expect_equal(result, character(0))
})

test_that("wraps GCS list error with campaign context", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(...) stop("auth expired")
  )
  expect_error(s160_gcs_results_files(1980), "Failed to list files for campaign 1980.*auth expired")
})
