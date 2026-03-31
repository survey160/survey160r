test_that("errors when no bucket is set", {
  expect_error(survey160r:::check_gcs_ready(), "Run s160_gcs_init")
})
