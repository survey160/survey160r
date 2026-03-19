test_that("strips campaign prefix from filenames", {
  fake_objects <- data.frame(
    name = c("1980/1980_raw_data_download.csv", "1980/1980_summary.csv"),
    stringsAsFactors = FALSE
  )
  mockery::stub(s160_gcs_results_files, "check_gcs_ready", NULL)
  mockery::stub(s160_gcs_results_files, "validate_campaign_id", "1980")
  mockery::stub(s160_gcs_results_files, "gcs_list_objects", fake_objects)
  expect_equal(s160_gcs_results_files(1980), c("1980_raw_data_download.csv", "1980_summary.csv"))
})

test_that("returns empty character with message when no files", {
  fake_objects <- data.frame(name = character(0), stringsAsFactors = FALSE)
  mockery::stub(s160_gcs_results_files, "check_gcs_ready", NULL)
  mockery::stub(s160_gcs_results_files, "validate_campaign_id", "9999")
  mockery::stub(s160_gcs_results_files, "gcs_list_objects", fake_objects)
  expect_message(result <- s160_gcs_results_files(9999), "No files found")
  expect_equal(result, character(0))
})
