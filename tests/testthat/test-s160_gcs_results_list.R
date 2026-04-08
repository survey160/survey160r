test_that("extracts unique sorted campaign IDs", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(...) {
      data.frame(
        name = c("1980/file.csv", "1980/other.csv", "1975/file.csv", "1990/file.csv"),
        stringsAsFactors = FALSE
      )
    }
  )
  expect_equal(s160_gcs_campaign_results_list(), c("1975", "1980", "1990"))
})

test_that("ignores root-level objects without slash", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(...) {
      data.frame(
        name = c("1980/file.csv", "readme.txt"),
        stringsAsFactors = FALSE
      )
    }
  )
  expect_equal(s160_gcs_campaign_results_list(), "1980")
})

test_that("returns empty character with message when no objects", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(...) data.frame(name = character(0), stringsAsFactors = FALSE)
  )
  expect_message(result <- s160_gcs_campaign_results_list(), "No campaigns found")
  expect_equal(result, character(0))
})

test_that("wraps GCS list error with context", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(...) stop("permission denied")
  )
  expect_error(s160_gcs_campaign_results_list(), "Failed to list campaigns.*permission denied")
})
