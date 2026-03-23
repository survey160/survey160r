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
    expect_error(s160_gcs_init(bucket = "campaign_results"), "S160_GCS_CLIENT_SECRET")
  })
})

test_that("sets global bucket and configures OAuth client", {
  captured_bucket <- NULL
  withr::with_envvar(c(S160_GCS_CLIENT_SECRET = "fake-secret"), {
    mockery::stub(s160_gcs_init, "gcs_auth", NULL)
    mockery::stub(s160_gcs_init, "gcs_global_bucket", function(b) { captured_bucket <<- b })
    suppressMessages(s160_gcs_init(bucket = "campaign_results"))
  })
  expect_equal(captured_bucket, "campaign_results")
  expect_equal(getOption("googleAuthR.client_secret"), "fake-secret")
})
