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
  withr::with_envvar(c(S160_GCS_CLIENT_SECRET = ""), {
    local_mocked_bindings(
      system.file = function(...) oauth_client_path,
      interactive = function() FALSE
    )
    expect_error(s160_gcs_init(bucket = "campaign_results"), "S160_GCS_CLIENT_SECRET")
  })
})

test_that("errors when oauth-client.json is not found", {
  withr::with_envvar(c(S160_GCS_CLIENT_SECRET = "fake-secret"), {
    local_mocked_bindings(system.file = function(...) "")
    expect_error(s160_gcs_init(bucket = "campaign_results"), "oauth-client.json not found")
  })
})

test_that("interactive flow calls prompt_and_save_secret when secret is missing", {
  stub_gcs_init_base()
  local_mocked_bindings(
    interactive = function() TRUE,
    prompt_and_save_secret = function() "prompted-secret"
  )

  withr::with_envvar(c(S160_GCS_CLIENT_SECRET = ""), {
    suppressMessages(s160_gcs_init(bucket = "campaign_results"))
  })

  expect_equal(getOption("googleAuthR.client_secret"), "prompted-secret")
})

test_that("sets global bucket and configures OAuth client", {
  captured_bucket <- NULL
  withr::with_envvar(c(S160_GCS_CLIENT_SECRET = "fake-secret"), {
    local_mocked_bindings(
      system.file = function(...) oauth_client_path,
      gcs_auth = function(...) NULL,
      gcs_global_bucket = function(b) {
        captured_bucket <<- b
      }
    )
    suppressMessages(s160_gcs_init(bucket = "campaign_results"))
  })
  expect_equal(captured_bucket, "campaign_results")
  expect_equal(getOption("googleAuthR.client_secret"), "fake-secret")
})
