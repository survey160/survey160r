# Shared test stubs -- loaded automatically by testthat before all tests

# Inline replacement for the old tests/testthat/fixtures/synthetic_config.yaml.
# Used by test-latency_report.R to drive latency_report
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

# Force the read.csv fallback in fast_read_csv / s160_csv_header by making
# requireNamespace("data.table") report FALSE for the duration of a test.
stub_no_data_table <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "data.table")) FALSE
      else base::requireNamespace(package, ...)
    },
    .package = "base",
    .env = env
  )
}

# Stub GCS dependencies for functions that call check_gcs_ready + validate_campaign_id
stub_gcs_base <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    check_gcs_ready = function() NULL,
    validate_campaign_id = function(id) as.character(id),
    .env = env
  )
}

# Stub a GCS download for tests of download_with_verify() and its callers.
# download_with_verify() reads the authoritative object size from
# gcs_get_object(meta = TRUE); this stub answers both that metadata call and
# the actual download through a single gcs_get_object mock. Default behavior:
# the download writes `content` to the target path and the metadata reports a
# matching byte count, so verification passes.
#
# Hooks:
#   capture_env   -- if set, gcs_get_object records the DOWNLOAD call's args
#                    (not the metadata call).
#   content       -- character vector written by the download (default "a,b","1,2").
#   size_override -- metadata `size`. NULL (default) uses the real byte count of
#                    `content`. Pass a wrong number to force a mismatch, or a
#                    non-numeric string like "483.3 Kb" to exercise the
#                    unknown-size skip path.
#   fail_meta     -- character. If set, the metadata call (meta = TRUE) stops
#                    with this msg, exercising the metadata-unavailable skip.
#   skip_write    -- if TRUE, the download returns without writing -- for the
#                    "Download produced no file" path.
stub_gcs_download_ok <- function(capture_env = NULL,
                                 content = c("a,b", "1,2"),
                                 size_override = NULL,
                                 fail_meta = NULL,
                                 skip_write = FALSE,
                                 env = parent.frame()) {
  size_probe <- tempfile()
  writeLines(content, size_probe)
  real_size <- file.info(size_probe)$size
  unlink(size_probe)
  reported_size <- if (is.null(size_override)) real_size else size_override
  testthat::local_mocked_bindings(
    gcs_get_object = function(object_name, saveToDisk = NULL, meta = FALSE, ...) { # nolint object_name_linter
      if (isTRUE(meta)) {
        if (!is.null(fail_meta)) stop(fail_meta)
        return(structure(list(name = object_name, size = reported_size),
                         class = "gcs_objectmeta"))
      }
      if (!skip_write) writeLines(content, saveToDisk)
      if (!is.null(capture_env)) capture_env$args <- as.list(environment())
      TRUE
    },
    .env = env
  )
}

# --- latency_run helpers --------------------------------------------------

# Capture env for mock-recorded values. Use `<<-` or `env$field <- ...` from
# inside a mock body; read fields back after the call under test returns.
new_capture <- function() new.env(parent = emptyenv())

# Load the shared synthetic CSV with the source_csv_hash/path attributes
# that s160_gcs_campaign_results_read(hash = TRUE) / s160_read_csv() would attach in production.
# `mutate` lets a test perturb the data inline (e.g. drop a column to
# trigger validation).
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

# Load the 8-question parity fixture used by test-latency_parity_legacy.R.
# Six respondents, one day, designed so each cascade bucket has exactly one
# respondent. Returns a plain data frame (no GCS attrs) -- the parity tests
# call latency_report() directly.
load_synthetic_parity <- function() {
  read.csv(testthat::test_path("fixtures/synthetic_parity.csv"),
           stringsAsFactors = FALSE)
}

# Load the cross-hour rollup fixture used by test-day_rollup_equivalence.R.
load_synthetic_cross_hour <- function() {
  read.csv(testthat::test_path("fixtures/synthetic_cross_hour.csv"),
           stringsAsFactors = FALSE)
}

# Build a minimal valid Survey160 v2 data frame matching `questions`.
# The terminal question has only a scriptDate (no batchDate), matching the
# real export convention. `with_rows = FALSE` returns a column-only frame
# (useful for latency_build_config tests that only inspect column names).
minimal_synthetic_data <- function(questions = c("intro", "q1", "close"),
                                   with_rows = TRUE) {
  cols <- c("campaignid", "userid", "id.intro.finalText")
  terminal <- questions[length(questions)]
  for (q in questions) {
    cols <- c(cols, sprintf("id.%s.scriptDate", q))
    if (q != terminal) cols <- c(cols, sprintf("id.%s.batchDate", q))
  }
  if (!with_rows) {
    return(setNames(
      as.data.frame(matrix(NA, nrow = 0, ncol = length(cols)),
                    stringsAsFactors = FALSE),
      cols
    ))
  }
  values <- list(campaignid = 1L, userid = "r1", id.intro.finalText = "Yes")
  base <- as.POSIXct("2026-01-26 21:00:00", tz = "UTC")
  for (i in seq_along(questions)) {
    q <- questions[i]
    values[[sprintf("id.%s.scriptDate", q)]] <-
      paste0(format(base + (i - 1) * 30, "%Y-%m-%d %H:%M:%OS6", tz = "UTC"), "Z")
    if (q != terminal) {
      values[[sprintf("id.%s.batchDate", q)]] <-
        paste0(format(base + (i - 1) * 30 + 5, "%Y-%m-%d %H:%M:%OS6",
                      tz = "UTC"), "Z")
    }
  }
  as.data.frame(values, stringsAsFactors = FALSE)
}

# Build a GCS object-status list as returned by s160_gcs_*_status helpers.
# `updated` accepts a POSIXct or an ISO-ish string parsed as UTC.
gcs_status <- function(name = "obj.csv",
                       updated = "2026-01-01 00:00:00",
                       size = 1L) {
  if (is.character(updated)) updated <- as.POSIXct(updated, tz = "UTC")
  list(name = name, updated = updated, size = size)
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

# Stub the httr verbs to return a *sequence* of responses across successive
# calls, for retry tests. `steps` is a list; each element is either an integer
# HTTP status code or the string "error" (raise a curl/network failure). The
# last step repeats once the list is exhausted (so a single "error"/5xx models a
# persistent failure). `http_error` is derived from the status (>= 400) so it
# always matches the sequence; `content` returns `body` (used on a 2xx step).
# `capture$calls` counts how many requests were issued.
stub_httr_seq <- function(steps, body = list(success = TRUE, data = "ok"),
                          capture = NULL, env = parent.frame()) {
  state <- new_capture()
  state$i <- 0L
  responder <- function(url, ...) {
    state$i <- state$i + 1L
    step <- steps[[min(state$i, length(steps))]]
    if (!is.null(capture)) capture$calls <- c(capture$calls, TRUE)
    if (identical(step, "error")) {
      stop("Could not resolve host: api.survey160.com", call. = FALSE)
    }
    structure(list(status_code = as.integer(step), url = url), class = "response")
  }
  testthat::local_mocked_bindings(
    POST = responder,
    GET = responder,
    http_error = function(resp) httr::status_code(resp) >= 400,
    content = function(resp, ...) body,
    http_status = function(resp) list(message = "Server Error"),
    .package = "httr",
    .env = env
  )
}

# Neutralize the retry backoff's Sys.sleep so transient-failure tests don't
# actually wait. When `capture` is supplied, each requested wait (seconds) is
# recorded to capture$waits so a test can assert the exponential schedule.
stub_no_sleep <- function(capture = NULL, env = parent.frame()) {
  testthat::local_mocked_bindings(
    Sys.sleep = function(time) {
      if (!is.null(capture)) capture$waits <- c(capture$waits, time)
      invisible(NULL)
    },
    .package = "base",
    .env = env
  )
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

# --- disposition fixtures -------------------------------------------------

# Write disposition rows to a temp Parquet via nanoparquet and return the path.
# Shared by the disposition reader tests (disposition_summary/records/screen),
# which build small per-(phone, campaign) frames and read them back through the
# real nanoparquet path -- no arrow, no network.
write_disposition_parquet <- function(rows) {
  p <- tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(rows, p)
  p
}

# One (phone, campaign) row carrying exactly the columns disposition_summary()
# projects (`.DISPOSITION_READ_COLS`), with sensible funnel defaults; override
# via args. Shared by the disposition_summary / disposition_screen reader tests.
.disposition_row <- function(phone, campaign_id, engaged = 0L, opted_in = 0L,
                             completed = 0L, web_complete = 0L, terminated = 0L,
                             date_closed_on = as.Date(NA)) {
  data.frame(phone = phone, campaign_id = as.integer(campaign_id),
             engaged = as.integer(engaged), opted_in = as.integer(opted_in),
             completed = as.integer(completed),
             web_complete = as.integer(web_complete),
             terminated = as.integer(terminated),
             date_closed_on = as.Date(date_closed_on), stringsAsFactors = FALSE)
}

# Write an opt-out fixture (phone + date_added) to a temp Parquet, read back
# through the real nanoparquet path -- no arrow, no network. Shared by the
# opt_out_screen reader tests.
write_opt_out_parquet <- function(rows) {
  p <- tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(rows, p)
  p
}

# One opt-out row (phone + date_added). date_added is carried through the screen
# uninterpreted, so the fixture uses a plain string for a deterministic assertion
# (the real snapshot stores a timestamp; opt_out_screen is type-agnostic).
.opt_out_row <- function(phone, date_added = NA_character_) {
  data.frame(phone = phone, date_added = as.character(date_added),
             stringsAsFactors = FALSE)
}

# One full-schema (phone, campaign) record; override any column. Defaults model
# a contacted-but-no-reply t2w record. Shared by the disposition_records tests.
.record_row <- function(phone, campaign_id, sent = 1L, engaged = 0L,
                        opted_in = 0L, completed = 0L, web_complete = 0L,
                        terminated = 0L, error = NA_character_, loi = NA_real_,
                        topic = NA_character_, mode = "t2w",
                        date_closed_on = as.Date(NA)) {
  data.frame(
    phone = phone, campaign_id = as.integer(campaign_id),
    sent = as.integer(sent), engaged = as.integer(engaged),
    opted_in = as.integer(opted_in), completed = as.integer(completed),
    web_complete = as.integer(web_complete),
    terminated = as.integer(terminated), error = as.character(error),
    loi = as.numeric(loi), topic = as.character(topic),
    mode = as.character(mode), date_closed_on = as.Date(date_closed_on),
    stringsAsFactors = FALSE
  )
}
