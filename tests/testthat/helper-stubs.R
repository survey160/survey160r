# Shared test stubs -- loaded automatically by testthat before all tests

# Stub GCS dependencies for functions that call check_gcs_ready + validate_campaign_id
stub_gcs_base <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    check_gcs_ready = function() NULL,
    validate_campaign_id = function(id) as.character(id),
    gcs_get_global_bucket = function() "test_bucket",
    .env = env
  )
}

# Stub a successful GCS download that writes a minimal CSV to the target path
stub_gcs_download_ok <- function(capture_env = NULL, env = parent.frame()) {
  testthat::local_mocked_bindings(
    gcs_get_object = function(object_name, saveToDisk, ...) { # nolint object_name_linter
      writeLines(c("a,b", "1,2"), saveToDisk)
      if (!is.null(capture_env)) capture_env$args <- as.list(environment())
      TRUE
    },
    .env = env
  )
}

# Path to bundled oauth-client.json (works under both pkgload and R CMD check)
oauth_client_path <- base::system.file("oauth-client.json", package = "survey160r")
if (oauth_client_path == "") {
  # pkgload::load_all() -- file is still in inst/
  oauth_client_path <- file.path(
    testthat::test_path("..", ".."), "inst", "oauth-client.json"
  )
}

# Stub s160_gcs_init dependencies that would trigger real GCS auth
stub_gcs_init_base <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    system.file = function(...) oauth_client_path,
    gcs_auth = function(...) NULL,
    gcs_global_bucket = function(...) NULL,
    .env = env
  )
}
