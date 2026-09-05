test_that("authenticates with no bucket and sets no global (new style)", {
  captured <- new_capture()
  withr::local_envvar(S160_GCS_CLIENT_SECRET = "fake-secret")
  withr::local_options(
    googleAuthR.client_id = NULL, googleAuthR.client_secret = NULL
  )
  local_mocked_bindings(
    gcs_auth = function(...) {
      captured$authed <- TRUE
      NULL
    },
    gcs_global_bucket = function(b) captured$bucket <- b
  )
  expect_no_warning(suppressMessages(s160_gcs_init()))
  expect_true(captured$authed)
  expect_null(captured$bucket)                 # no bucket -> no global set
})

test_that("deprecated `bucket` warns and still sets the session global", {
  captured <- new_capture()
  withr::local_envvar(S160_GCS_CLIENT_SECRET = "fake-secret")
  withr::local_options(
    googleAuthR.client_id = NULL, googleAuthR.client_secret = NULL
  )
  local_mocked_bindings(
    gcs_auth = function(...) NULL,
    gcs_global_bucket = function(b) captured$bucket <- b
  )
  expect_warning(suppressMessages(s160_gcs_init(bucket = "campaign_results")),
                 "deprecated")
  expect_equal(captured$bucket, "campaign_results")
})

test_that("rejects an invalid explicit bucket", {
  expect_error(s160_gcs_init(bucket = ""), "non-empty string")
  expect_error(s160_gcs_init(bucket = 123), "non-empty string")
})

test_that("errors in non-interactive mode when secret is missing", {
  withr::local_envvar(S160_GCS_CLIENT_SECRET = "")
  local_mocked_bindings(interactive = function() FALSE, .package = "base")
  expect_error(s160_gcs_init(), "S160_GCS_CLIENT_SECRET")
})

test_that("errors when oauth-client.json is not found", {
  withr::local_envvar(S160_GCS_CLIENT_SECRET = "fake-secret")
  # local_mocked_bindings can't intercept base::system.file from inside the
  # package namespace (the lookup happens before the rebind takes effect).
  # mockery::stub patches the symbol in the function's own environment, so
  # it still works here.
  mockery::stub(s160_gcs_init, "system.file", "")
  expect_error(s160_gcs_init(), "oauth-client.json not found")
})

test_that("interactive flow calls prompt_and_save_secret when secret is missing", {
  withr::local_envvar(S160_GCS_CLIENT_SECRET = "")
  withr::local_options(
    googleAuthR.client_id = NULL, googleAuthR.client_secret = NULL
  )
  # covr's instrumentation runs in a non-interactive subprocess and the
  # local_mocked_bindings rebind of base::interactive doesn't take effect there;
  # mockery::stub patches the symbol directly in the SUT and works under covr.
  mockery::stub(s160_gcs_init, "interactive", TRUE)
  local_mocked_bindings(
    prompt_and_save_secret = function() "prompted-secret",
    gcs_auth = function(...) NULL,
    gcs_global_bucket = function(...) NULL
  )

  suppressMessages(s160_gcs_init())
  expect_equal(getOption("googleAuthR.client_secret"), "prompted-secret")
})

test_that("adc = TRUE authenticates via Application Default Credentials", {
  captured <- new_capture()
  local_mocked_bindings(
    credentials_app_default = function(scopes = NULL) {
      captured$scopes <- scopes
      list(token = "fake-adc-token")
    },
    gcs_auth = function(...) {
      captured$authed <- TRUE
      captured$args <- list(...)
    },
    gcs_global_bucket = function(b) captured$bucket <- b
  )
  expect_no_warning(suppressMessages(s160_gcs_init(adc = TRUE)))
  expect_true(captured$authed)
  expect_true("token" %in% names(captured$args))   # authed with a token, not email
  expect_match(captured$scopes, "cloud-platform")
  expect_null(captured$bucket)
})

test_that("adc = TRUE errors when no ADC are available", {
  local_mocked_bindings(
    credentials_app_default = function(scopes = NULL) NULL,
    gcs_auth = function(...) stop("gcs_auth must not be called without a token")
  )
  expect_error(s160_gcs_init(adc = TRUE), "Application Default Credentials")
})

test_that("adc = TRUE surfaces the guided error when ADC lookup throws", {
  local_mocked_bindings(
    credentials_app_default = function(scopes = NULL) stop("malformed ADC file"),
    gcs_auth = function(...) stop("gcs_auth must not be called without a token")
  )
  expect_error(s160_gcs_init(adc = TRUE), "Application Default Credentials")
})

test_that("adc must be a single, non-NA logical", {
  expect_error(s160_gcs_init(adc = "yes"), "single TRUE or FALSE")
  expect_error(s160_gcs_init(adc = NA), "single TRUE or FALSE")
  expect_error(s160_gcs_init(adc = c(TRUE, FALSE)), "single TRUE or FALSE")
})
