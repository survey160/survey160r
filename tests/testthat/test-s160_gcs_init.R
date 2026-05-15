test_that("errors when bucket is missing", {
  expect_error(s160_gcs_init(), "bucket.*required")
})

test_that("errors when bucket is empty string", {
  expect_error(s160_gcs_init(bucket = ""), "non-empty string")
})

test_that("errors when bucket is not a string", {
  expect_error(s160_gcs_init(bucket = 123), "non-empty string")
  expect_error(s160_gcs_init(bucket = NULL), "non-empty string")
})

test_that("errors in non-interactive mode when secret is missing", {
  withr::local_envvar(S160_GCS_CLIENT_SECRET = "")
  local_mocked_bindings(interactive = function() FALSE, .package = "base")
  expect_error(s160_gcs_init(bucket = "campaign_results"),
               "S160_GCS_CLIENT_SECRET")
})

test_that("errors when oauth-client.json is not found", {
  withr::local_envvar(S160_GCS_CLIENT_SECRET = "fake-secret")
  # local_mocked_bindings can't intercept base::system.file from inside the
  # package namespace (the lookup happens before the rebind takes effect).
  # mockery::stub patches the symbol in the function's own environment, so
  # it still works here. A follow-up could add a private wrapper in R/ and
  # mock that instead.
  mockery::stub(s160_gcs_init, "system.file", "")
  expect_error(s160_gcs_init(bucket = "campaign_results"),
               "oauth-client.json not found")
})

test_that("interactive flow calls prompt_and_save_secret when secret is missing", {
  withr::local_envvar(S160_GCS_CLIENT_SECRET = "")
  # covr's instrumentation runs in a non-interactive subprocess and the
  # local_mocked_bindings rebind of base::interactive doesn't take effect
  # there (it works for `interactive() == FALSE` paths but not for forcing
  # TRUE). mockery::stub patches the symbol directly in the SUT and works
  # under covr.
  mockery::stub(s160_gcs_init, "interactive", TRUE)
  local_mocked_bindings(
    prompt_and_save_secret = function() "prompted-secret",
    gcs_auth = function(...) NULL,
    gcs_global_bucket = function(...) NULL
  )

  suppressMessages(s160_gcs_init(bucket = "campaign_results"))
  expect_equal(getOption("googleAuthR.client_secret"), "prompted-secret")
})

test_that("sets global bucket and configures OAuth client", {
  captured <- new_capture()
  withr::local_envvar(S160_GCS_CLIENT_SECRET = "fake-secret")
  local_mocked_bindings(
    gcs_auth = function(...) NULL,
    gcs_global_bucket = function(b) {
      captured$bucket <- b
    }
  )

  suppressMessages(s160_gcs_init(bucket = "campaign_results"))
  expect_equal(captured$bucket, "campaign_results")
  expect_equal(getOption("googleAuthR.client_secret"), "fake-secret")
})
