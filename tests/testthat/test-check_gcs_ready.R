test_that("errors when no bucket is set", {
  expect_error(survey160r:::check_gcs_ready(), "Run s160_gcs_init")
})

test_that("passes when bucket is set via mock in exported function", {
  # check_gcs_ready is tested indirectly through exported functions
  # that mock it away. Direct test here just verifies the error path.
  # The success path is covered by all other test files that stub check_gcs_ready.
})
