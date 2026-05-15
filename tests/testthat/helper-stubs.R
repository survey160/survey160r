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

# Stub a GCS download for tests of download_with_verify() and its callers.
# Default behavior: gcs_get_object writes `content` to the target path;
# gcs_list_objects returns a single-row data frame whose `size` matches the
# bytes written, so verification passes.
#
# Hooks:
#   capture_env   -- if set, gcs_get_object records its call args.
#   content       -- character vector written by gcs_get_object (default "a,b","1,2").
#   name_override -- listing filename. NULL (default) uses the campaign-id-
#                    derived `<id>_raw_data_download.csv` pattern.
#   size_override -- listing `size` value. NULL (default) uses the real byte
#                    count. Pass a wrong number or a string like "483.3 Kb"
#                    to exercise the verification mismatch / unknown-size paths.
#   fail_list     -- character. If set, gcs_list_objects stops with this msg.
#   skip_write    -- if TRUE, gcs_get_object returns without writing -- for
#                    the "Download produced no file" path.
stub_gcs_download_ok <- function(capture_env = NULL,
                                 content = c("a,b", "1,2"),
                                 name_override = NULL,
                                 size_override = NULL,
                                 fail_list = NULL,
                                 skip_write = FALSE,
                                 env = parent.frame()) {
  size_probe <- tempfile()
  writeLines(content, size_probe)
  real_size <- file.info(size_probe)$size
  unlink(size_probe)
  reported_size <- if (is.null(size_override)) real_size else size_override
  testthat::local_mocked_bindings(
    gcs_get_object = function(object_name, saveToDisk, ...) { # nolint object_name_linter
      if (!skip_write) writeLines(content, saveToDisk)
      if (!is.null(capture_env)) capture_env$args <- as.list(environment())
      TRUE
    },
    gcs_list_objects = function(prefix = NULL, ...) {
      if (!is.null(fail_list)) stop(fail_list)
      name <- if (!is.null(name_override)) {
        name_override
      } else if (!is.null(prefix)) {
        # Match the export convention so verification is exercised.
        campaign_id <- sub("/$", "", prefix)
        paste0(prefix, campaign_id, "_raw_data_download.csv")
      } else {
        "data.csv"
      }
      data.frame(name = name, size = reported_size,
                 stringsAsFactors = FALSE)
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
# Otherwise records local_path/object_name/bucket/metadata into `capture`.
# When `persist = TRUE`, copies the local_path file to a captured temp path
# (`capture$persisted`) before returning -- write_to_gcs unlinks the source
# immediately, so this is how Parquet round-trip tests inspect output.
stub_upload <- function(capture = NULL, must_not_call = FALSE,
                        persist = FALSE, env = parent.frame()) {
  testthat::local_mocked_bindings(
    upload_object = function(local_path, object_name, bucket, metadata) {
      if (must_not_call) stop("uploader should not be called")
      if (!is.null(capture)) {
        capture$local_path <- local_path
        capture$object_name <- object_name
        capture$bucket <- bucket
        capture$metadata <- metadata
        if (persist) {
          persisted <- tempfile(fileext = ".parquet")
          file.copy(local_path, persisted, overwrite = TRUE)
          capture$persisted <- persisted
        }
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

# --- s160_api helpers -----------------------------------------------------

# Seed the package-private API auth env so check_api_ready() passes. Tears
# down on test exit. Use in any test that exercises a function gated by
# check_api_ready() (s160_api_request, s160_api_campaign_*).
stub_api_base <- function(env = parent.frame()) {
  api_env <- survey160r:::.s160_api_env
  api_env$jwt <- "test-jwt"
  api_env$base_url <- "https://test-api.survey160.com"
  api_env$userid <- "test-user"
  api_env$auth_time <- Sys.time()
  withr::defer({
    rm(list = ls(api_env), envir = api_env)
  }, envir = env)
}

# Stub the httr POST/GET response quartet (POST, GET, http_error, content).
# `status_msg` is the http_status fallback message used when the response
# body has no "error" field; supplied tests rely on it for fallback wording.
# `capture` records the URL each verb was called with.
stub_httr_response <- function(status = 200L,
                               body = list(success = TRUE),
                               http_error = FALSE,
                               status_msg = NULL,
                               capture = NULL,
                               env = parent.frame()) {
  responder <- function(url, ...) {
    if (!is.null(capture)) capture$url <- url
    structure(list(status_code = status, url = url), class = "response")
  }
  bindings <- list(
    POST = responder,
    GET = responder,
    http_error = function(resp) http_error,
    content = function(resp, ...) body,
    http_status = function(resp) {
      list(message = if (is.null(status_msg)) "Unknown" else status_msg)
    },
    .package = "httr",
    .env = env
  )
  do.call(testthat::local_mocked_bindings, bindings)
}

# Stub gcs_list_objects to return `rows` (a data frame). Use when a test
# only needs to control the listing payload. Pass a zero-row data frame to
# exercise the "no files found" branch.
stub_gcs_list <- function(rows, env = parent.frame()) {
  testthat::local_mocked_bindings(
    gcs_list_objects = function(prefix = NULL, ...) rows,
    .env = env
  )
}
