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

# Stub a successful GCS download that writes a minimal CSV to the target path.
# Also stubs gcs_list_objects to return matching size for download verification.
stub_gcs_download_ok <- function(capture_env = NULL, env = parent.frame()) {
  csv_content <- c("a,b", "1,2")
  csv_size <- sum(nchar(csv_content)) + length(csv_content)  # bytes including newlines
  testthat::local_mocked_bindings(
    gcs_get_object = function(object_name, saveToDisk, ...) { # nolint object_name_linter
      writeLines(csv_content, saveToDisk)
      if (!is.null(capture_env)) capture_env$args <- as.list(environment())
      TRUE
    },
    gcs_list_objects = function(prefix = NULL, ...) {
      # Return a row whose name matches the object so size verification passes.
      # download_with_verify passes prefix = "campaign_id/" and looks up object_name.
      # We return a plausible name; the size will match the written CSV.
      name <- if (!is.null(prefix)) paste0(prefix, "data.csv") else "data.csv"
      data.frame(name = name, size = csv_size, stringsAsFactors = FALSE)
    },
    .env = env
  )
}
