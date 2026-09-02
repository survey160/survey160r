test_that("errors when not authenticated", {
  local_mocked_bindings(.gcs_has_token = function() FALSE)
  expect_error(survey160r:::check_gcs_ready(), "Run s160_gcs_init")
})

test_that("passes silently when authenticated", {
  local_mocked_bindings(.gcs_has_token = function() TRUE)
  expect_null(survey160r:::check_gcs_ready())
})

test_that(".gcs_has_token returns gargle's in-memory token state as a logical", {
  # Exercises the real wrapper (no network: gargle's token check is in-memory).
  # Assert the type, not the value, so the result does not depend on the process
  # credential state (which can vary by test order / host).
  expect_type(survey160r:::.gcs_has_token(), "logical")
})
