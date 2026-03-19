test_that("extracts unique sorted campaign IDs", {
  fake_objects <- data.frame(
    name = c("1980/file.csv", "1980/other.csv", "1975/file.csv", "1990/file.csv"),
    stringsAsFactors = FALSE
  )
  mockery::stub(s160_gcs_results_list, "check_gcs_ready", NULL)
  mockery::stub(s160_gcs_results_list, "gcs_list_objects", fake_objects)
  expect_equal(s160_gcs_results_list(), c("1975", "1980", "1990"))
})

test_that("ignores root-level objects without slash", {
  fake_objects <- data.frame(
    name = c("1980/file.csv", "readme.txt"),
    stringsAsFactors = FALSE
  )
  mockery::stub(s160_gcs_results_list, "check_gcs_ready", NULL)
  mockery::stub(s160_gcs_results_list, "gcs_list_objects", fake_objects)
  expect_equal(s160_gcs_results_list(), "1980")
})

test_that("returns empty character with message when no objects", {
  fake_objects <- data.frame(name = character(0), stringsAsFactors = FALSE)
  mockery::stub(s160_gcs_results_list, "check_gcs_ready", NULL)
  mockery::stub(s160_gcs_results_list, "gcs_list_objects", fake_objects)
  expect_message(result <- s160_gcs_results_list(), "No campaigns found")
  expect_equal(result, character(0))
})
