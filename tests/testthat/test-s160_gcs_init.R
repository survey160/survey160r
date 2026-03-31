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
  skip_if(interactive(), "would prompt for input in interactive sessions")
  withr::with_envvar(c(S160_GCS_CLIENT_SECRET = ""), {
    expect_error(s160_gcs_init(bucket = "campaign_results"), "S160_GCS_CLIENT_SECRET")
  })
})

test_that("errors when oauth-client.json is not found", {
  withr::with_envvar(c(S160_GCS_CLIENT_SECRET = "fake-secret"), {
    mockery::stub(s160_gcs_init, "system.file", "")
    expect_error(s160_gcs_init(bucket = "campaign_results"), "oauth-client.json not found")
  })
})

test_that("interactive flow prompts for secret and saves to .Renviron", {
  mockery::stub(s160_gcs_init, "interactive", TRUE)
  mockery::stub(s160_gcs_init, "readline", "prompted-secret")
  mockery::stub(s160_gcs_init, "file.exists", FALSE)
  mockery::stub(s160_gcs_init, "gcs_auth", NULL)
  mockery::stub(s160_gcs_init, "gcs_global_bucket", NULL)

  captured_cat <- NULL
  mockery::stub(s160_gcs_init, "cat", function(...) { captured_cat <<- list(...) })

  withr::with_envvar(c(S160_GCS_CLIENT_SECRET = ""), {
    suppressMessages(s160_gcs_init(bucket = "campaign_results"))
  })

  expect_equal(getOption("googleAuthR.client_secret"), "prompted-secret")
  expect_true(grepl("S160_GCS_CLIENT_SECRET=prompted-secret", captured_cat[[1]]))
  expect_true(captured_cat$append)
})

test_that("interactive flow strips existing secret from .Renviron before writing", {
  mockery::stub(s160_gcs_init, "interactive", TRUE)
  mockery::stub(s160_gcs_init, "readline", "new-secret")
  mockery::stub(s160_gcs_init, "file.exists", TRUE)
  mockery::stub(s160_gcs_init, "readLines", c("OTHER_VAR=keep", "S160_GCS_CLIENT_SECRET=old", "ANOTHER=also_keep"))
  mockery::stub(s160_gcs_init, "gcs_auth", NULL)
  mockery::stub(s160_gcs_init, "gcs_global_bucket", NULL)

  written_lines <- NULL
  mockery::stub(s160_gcs_init, "writeLines", function(lines, ...) { written_lines <<- lines })
  mockery::stub(s160_gcs_init, "cat", function(...) NULL)

  withr::with_envvar(c(S160_GCS_CLIENT_SECRET = ""), {
    suppressMessages(s160_gcs_init(bucket = "campaign_results"))
  })

  expect_equal(written_lines, c("OTHER_VAR=keep", "ANOTHER=also_keep"))
})

test_that("interactive flow errors when readline returns empty", {
  mockery::stub(s160_gcs_init, "interactive", TRUE)
  mockery::stub(s160_gcs_init, "readline", "")
  mockery::stub(s160_gcs_init, "gcs_auth", NULL)
  mockery::stub(s160_gcs_init, "gcs_global_bucket", NULL)

  withr::with_envvar(c(S160_GCS_CLIENT_SECRET = ""), {
    expect_error(
      suppressMessages(s160_gcs_init(bucket = "campaign_results")),
      "Client secret cannot be empty"
    )
  })
})

test_that("sets global bucket and configures OAuth client", {
  captured_bucket <- NULL
  withr::with_envvar(c(S160_GCS_CLIENT_SECRET = "fake-secret"), {
    local_mocked_bindings(
      gcs_auth = function(...) NULL,
      gcs_global_bucket = function(b) { captured_bucket <<- b }
    )
    suppressMessages(s160_gcs_init(bucket = "campaign_results"))
  })
  expect_equal(captured_bucket, "campaign_results")
  expect_equal(getOption("googleAuthR.client_secret"), "fake-secret")
})
