# Coverage for resolve_bucket(): the lazy bucket-resolution helper.

test_that("explicit bucket arg returns the bucket as-is", {
  expect_equal(survey160r:::resolve_bucket("my_bucket"), "my_bucket")
})

test_that("rejects an empty or whitespace-only explicit bucket", {
  expect_error(survey160r:::resolve_bucket(""),
               "`bucket` must be a non-empty string")
  expect_error(survey160r:::resolve_bucket("   "),
               "`bucket` must be a non-empty string")
})

test_that("rejects a non-character or vector explicit bucket", {
  expect_error(survey160r:::resolve_bucket(123L),
               "`bucket` must be a non-empty string")
  expect_error(survey160r:::resolve_bucket(c("a", "b")),
               "`bucket` must be a non-empty string")
})

test_that("rejects NA_character_ (nzchar(NA) is TRUE by default)", {
  expect_error(survey160r:::resolve_bucket(NA_character_),
               "`bucket` must be a non-empty string")
})

test_that("falls back to the global bucket when arg is NULL", {
  local_mocked_bindings(gcs_get_global_bucket = function() "global_bucket")
  expect_equal(survey160r:::resolve_bucket(NULL), "global_bucket")
})

test_that("errors with a clear message when neither arg nor global is set", {
  local_mocked_bindings(
    gcs_get_global_bucket = function() stop("not initialized")
  )
  expect_error(survey160r:::resolve_bucket(NULL),
               "No GCS bucket available")
})

test_that("errors when the global bucket is an empty string", {
  local_mocked_bindings(gcs_get_global_bucket = function() "")
  expect_error(survey160r:::resolve_bucket(NULL),
               "No GCS bucket available")
})
