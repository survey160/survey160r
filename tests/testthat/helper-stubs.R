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
# Size is computed from an actual write to avoid platform-dependent mismatches.
stub_gcs_download_ok <- function(capture_env = NULL, env = parent.frame()) {
  csv_content <- c("a,b", "1,2")
  # Compute actual file size by writing to a temp file
  size_probe <- tempfile()
  writeLines(csv_content, size_probe)
  csv_size <- file.info(size_probe)$size
  unlink(size_probe)
  testthat::local_mocked_bindings(
    gcs_get_object = function(object_name, saveToDisk, ...) { # nolint object_name_linter
      writeLines(csv_content, saveToDisk)
      if (!is.null(capture_env)) capture_env$args <- as.list(environment())
      TRUE
    },
    gcs_list_objects = function(prefix = NULL, ...) {
      # Build name matching the export convention so verification is exercised.
      name <- if (!is.null(prefix)) {
        campaign_id <- sub("/$", "", prefix)
        paste0(prefix, campaign_id, "_raw_data_download.csv")
      } else {
        "data.csv"
      }
      data.frame(name = name, size = csv_size, stringsAsFactors = FALSE)
    },
    .env = env
  )
}
