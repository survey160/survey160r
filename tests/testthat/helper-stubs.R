# Shared test stubs -- loaded automatically by testthat before all tests

# Inline replacement for the old tests/testthat/fixtures/synthetic_config.yaml.
# Used by test-latency_report.R and test-latency_io.R to drive latency_report
# against the synthetic.csv fixture. Field shape matches what latency_build_config()
# returns.
synthetic_config <- function() {
  list(
    project_id = 1L,
    campaign_id = 1L,
    field_timezone = "America/New_York",
    flow = list(questions = c("intro", "q1", "q2", "close")),
    filters = list(
      population = 'id.intro.finalText == "Yes"',
      campaign_id_column = "campaignid",
      respondent_id_column = "userid"
    )
  )
}

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

# --- run_latency / run_latency_all helpers --------------------------------

# Capture env for mock-recorded values. Use `<<-` or `env$field <- ...` from
# inside a mock body; read fields back after the call under test returns.
new_capture <- function() new.env(parent = emptyenv())

# Load the shared synthetic CSV with the source_csv_hash/path attributes
# that run_latency() expects pull_csv_from_gcs() to attach. `mutate` lets a
# test perturb the data inline (e.g. drop a column to trigger validation).
load_synthetic_data <- function(
    mutate = identity,
    source_csv_hash = "sha256:fixture",
    source_csv_path = "gs://campaign_results/1/1_raw_data_download.csv") {
  d <- mutate(read.csv(testthat::test_path("fixtures/synthetic.csv"),
                       stringsAsFactors = FALSE))
  attr(d, "source_csv_hash") <- source_csv_hash
  attr(d, "source_csv_path") <- source_csv_path
  d
}

# Stub pull_csv_from_gcs to return `data`. Captures `pull_id` and
# `pull_bucket` into `capture` when supplied.
stub_pull_csv <- function(data, capture = NULL, env = parent.frame()) {
  testthat::local_mocked_bindings(
    pull_csv_from_gcs = function(campaign_id, filename = NULL, bucket = NULL) {
      if (!is.null(capture)) {
        capture$pull_id <- campaign_id
        capture$pull_bucket <- bucket
      }
      data
    },
    .env = env
  )
}

# Stub upload_object. With `must_not_call = TRUE`, raises if invoked --
# useful for negative paths (e.g. validation failure must not upload).
# Otherwise records object_name/bucket/metadata into `capture`.
stub_upload <- function(capture = NULL, must_not_call = FALSE,
                        env = parent.frame()) {
  testthat::local_mocked_bindings(
    upload_object = function(local_path, object_name, bucket, metadata) {
      if (must_not_call) stop("uploader should not be called")
      if (!is.null(capture)) {
        capture$object_name <- object_name
        capture$bucket <- bucket
        capture$metadata <- metadata
      }
      invisible(NULL)
    },
    .env = env
  )
}

# Build a GCS object-status list as returned by s160_gcs_*_status helpers.
# `updated` accepts a POSIXct or an ISO-ish string parsed as UTC.
gcs_status <- function(name = "obj.csv",
                       updated = "2026-01-01 00:00:00",
                       size = 1L) {
  if (is.character(updated)) updated <- as.POSIXct(updated, tz = "UTC")
  list(name = name, updated = updated, size = size)
}

# Stub run_latency for run_latency_all tests. Records every campaign_id and
# the full per-call argument list into `capture`. Campaigns listed in
# `fail_on` raise `error_msg` (default "malformed CSV"). Returns a
# deterministic gs:// URI built from bucket + campaign_id.
stub_run_latency <- function(capture = NULL, fail_on = character(),
                             error_msg = "malformed CSV",
                             env = parent.frame()) {
  testthat::local_mocked_bindings(
    run_latency = function(campaign_id, bucket, source_bucket = NULL,
                           run_at = NULL, ...) {
      if (!is.null(capture)) {
        capture$ids <- c(capture$ids, as.character(campaign_id))
        capture$run_ats <- c(capture$run_ats, list(run_at))
        capture$last_args <- list(
          campaign_id = campaign_id, bucket = bucket,
          source_bucket = source_bucket, run_at = run_at, ...
        )
      }
      if (as.character(campaign_id) %in% fail_on) stop(error_msg)
      sprintf("gs://%s/latency/%s_latency.parquet", bucket, campaign_id)
    },
    .env = env
  )
}

# Stub s160_gcs_campaign_results_list to return `ids`.
stub_campaign_list <- function(ids, env = parent.frame()) {
  testthat::local_mocked_bindings(
    s160_gcs_campaign_results_list = function(bucket = NULL) ids,
    .env = env
  )
}
