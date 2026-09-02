test_that("errors when not authenticated", {
  local_mocked_bindings(.gcs_has_token = function() FALSE)
  expect_error(survey160r:::check_gcs_ready(), "Run s160_gcs_init")
})

test_that("passes silently when authenticated", {
  local_mocked_bindings(.gcs_has_token = function() TRUE)
  expect_null(survey160r:::check_gcs_ready())
})

test_that(".gcs_has_token is FALSE without authentication", {
  # Exercises the real wrapper (no network: gargle's token check is in-memory).
  expect_false(survey160r:::.gcs_has_token())
})
