test_that("status returns metadata for existing export", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(prefix, ...) {
      data.frame(
        name = c("1980/1980_raw_data_download.csv", "1980/other.csv"),
        updated = c("2024-06-15T10:00:00Z", "2024-06-14T09:00:00Z"),
        size = c(12345, 678),
        stringsAsFactors = FALSE
      )
    }
  )

  result <- s160_gcs_results_status(1980)
  expect_equal(result$name, "1980_raw_data_download.csv")
  expect_equal(result$updated, "2024-06-15T10:00:00Z")
  expect_equal(result$size, 12345)
})

test_that("status returns NULL when no files exist", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(prefix, ...) {
      data.frame(name = character(0), updated = character(0), size = numeric(0))
    }
  )

  expect_null(s160_gcs_results_status(1980))
})

test_that("status errors on GCS failure", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(prefix, ...) stop("connection timeout")
  )

  expect_error(s160_gcs_results_status(1980), "Failed to list files.*connection timeout")
})

test_that("status returns NULL when export file not found among other files", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(prefix, ...) {
      data.frame(
        name = "1980/something_else.csv",
        updated = "2024-06-15T10:00:00Z",
        size = 100,
        stringsAsFactors = FALSE
      )
    }
  )

  expect_null(s160_gcs_results_status(1980))
})
