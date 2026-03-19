test_that("errors when env vars are missing", {
  withr::with_envvar(c(S160_GCS_CLIENT_ID = "", S160_GCS_CLIENT_SECRET = ""), {
    expect_error(s160_gcs_init(), "S160_GCS_CLIENT_ID")
  })
})

test_that("empty bucket defaults to campaign_results_qa", {
  captured_bucket <- NULL
  withr::with_envvar(c(
    S160_GCS_CLIENT_ID = "fake-id",
    S160_GCS_CLIENT_SECRET = "fake-secret",
    S160_RESULTS_BUCKET = ""
  ), {
    mockery::stub(s160_gcs_init, "gcs_auth", NULL)
    mockery::stub(s160_gcs_init, "gcs_global_bucket", function(b) { captured_bucket <<- b })
    suppressMessages(s160_gcs_init())
  })
  expect_equal(captured_bucket, "campaign_results_qa")
})
