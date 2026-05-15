# Coverage for R/s160_api.R. Mocks the httr quartet (POST/GET/http_error/
# content) via stub_httr_response(), and seeds the package-private auth env
# via stub_api_base(). See helper-stubs.R for both.

.api_env <- function() survey160r:::.s160_api_env

.defer_api_env_reset <- function(env = parent.frame()) {
  e <- .api_env()
  withr::defer(rm(list = ls(e), envir = e), envir = env)
}

# --- s160_api_auth ------------------------------------------------------------

test_that("auth succeeds and stores JWT", {
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = "key123")
  stub_httr_response(
    body = list(success = TRUE, data = "jwt-token-123", userid = "svc")
  )
  .defer_api_env_reset()

  suppressMessages(s160_api_auth(base_url = "https://api.example.com"))

  env <- .api_env()
  expect_equal(env$jwt, "jwt-token-123")
  expect_equal(env$userid, "svc")
  expect_equal(env$base_url, "https://api.example.com")
})

test_that("auth fails with clear error on 401", {
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = "bad-key")
  stub_httr_response(
    status = 401L,
    body = list(error = "Invalid API key"),
    http_error = TRUE
  )
  expect_error(
    s160_api_auth(base_url = "https://api.example.com"),
    "Authentication failed"
  )
})

test_that("auth errors when S160_API_USERID not set in non-interactive mode", {
  withr::local_envvar(S160_API_USERID = NA, S160_API_KEY = "key123")
  local_mocked_bindings(interactive = function() FALSE, .package = "base")
  expect_error(s160_api_auth(), "S160_API_USERID not set")
})

test_that("auth errors when S160_API_KEY not set in non-interactive mode", {
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = NA)
  local_mocked_bindings(interactive = function() FALSE, .package = "base")
  expect_error(s160_api_auth(), "S160_API_KEY not set")
})

test_that("auth strips trailing slash from base_url", {
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = "key")
  stub_httr_response(
    body = list(success = TRUE, data = "jwt", userid = "svc")
  )
  .defer_api_env_reset()

  suppressMessages(s160_api_auth(base_url = "https://api.example.com/"))
  expect_equal(.api_env()$base_url, "https://api.example.com")
})

test_that("auth defaults to production base_url", {
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = "key")
  captured <- new_capture()
  stub_httr_response(
    body = list(success = TRUE, data = "jwt", userid = "svc"),
    capture = captured
  )
  .defer_api_env_reset()

  suppressMessages(s160_api_auth())
  expect_equal(captured$url, "https://api.survey160.com/auth/serviceAccount")
})

test_that("auth errors on unexpected response format", {
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = "key")
  stub_httr_response(body = list(success = FALSE))
  expect_error(s160_api_auth(), "unexpected response format")
})

test_that("auth falls back to http_status when error field is NULL", {
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = "key")
  stub_httr_response(
    status = 503L,
    body = list(detail = "unavailable"),
    http_error = TRUE,
    status_msg = "Service Unavailable"
  )
  expect_error(s160_api_auth(), "Authentication failed")
})

# --- get_credential -----------------------------------------------------------

test_that("get_credential returns value when env var is set", {
  withr::local_envvar(S160_TEST_VAR = "test-value")
  result <- survey160r:::get_credential("S160_TEST_VAR", "prompt msg")
  expect_equal(result, "test-value")
})

test_that("get_credential errors in non-interactive when env var missing", {
  withr::local_envvar(S160_TEST_VAR = NA)
  local_mocked_bindings(interactive = function() FALSE, .package = "base")
  expect_error(
    survey160r:::get_credential("S160_TEST_VAR", "prompt msg"),
    "S160_TEST_VAR not set"
  )
})

# --- base_url validation ------------------------------------------------------

test_that("auth errors on empty base_url", {
  expect_error(s160_api_auth(base_url = ""), "non-empty")
})

test_that("auth errors on non-string base_url", {
  expect_error(s160_api_auth(base_url = 123), "non-empty")
  expect_error(s160_api_auth(base_url = NULL), "non-empty")
})

# --- check_api_ready ----------------------------------------------------------

test_that("check_api_ready errors when not authenticated", {
  expect_error(survey160r:::check_api_ready(), "Run s160_api_auth")
})

# --- s160_api_request ---------------------------------------------------------

test_that("request refreshes JWT when older than 8 minutes", {
  stub_api_base()
  env <- .api_env()
  env$auth_time <- Sys.time() - 600  # 10 min ago

  auth_called <- FALSE
  local_mocked_bindings(
    s160_api_auth = function(...) {
      auth_called <<- TRUE
      env$jwt <- "refreshed-jwt"
      env$auth_time <- Sys.time()
    }
  )
  stub_httr_response(body = list(ok = TRUE))

  survey160r:::s160_api_request("GET", "/test")
  expect_true(auth_called)
})

test_that("request does not refresh JWT when fresh", {
  stub_api_base()
  auth_called <- FALSE
  local_mocked_bindings(s160_api_auth = function(...) auth_called <<- TRUE)
  stub_httr_response(body = list(ok = TRUE))

  survey160r:::s160_api_request("GET", "/test")
  expect_false(auth_called)
})

test_that("request refreshes JWT just past the 480-second threshold", {
  # Threshold lives in R/s160_api.R: `if (elapsed > 480) refresh`. Pinning a
  # test at 481s (just over) guards against off-by-one drift; the 600s test
  # above only proves the "well past" path.
  stub_api_base()
  env <- .api_env()
  env$auth_time <- Sys.time() - 481

  auth_called <- FALSE
  local_mocked_bindings(s160_api_auth = function(...) {
    auth_called <<- TRUE
    env$auth_time <- Sys.time()
  })
  stub_httr_response(body = list(ok = TRUE))

  survey160r:::s160_api_request("GET", "/test")
  expect_true(auth_called)
})

test_that("request does NOT refresh when auth is just inside the threshold", {
  stub_api_base()
  env <- .api_env()
  env$auth_time <- Sys.time() - 470  # 10s of slack below 480

  auth_called <- FALSE
  local_mocked_bindings(s160_api_auth = function(...) auth_called <<- TRUE)
  stub_httr_response(body = list(ok = TRUE))

  survey160r:::s160_api_request("GET", "/test")
  expect_false(auth_called)
})

test_that("request raises error on HTTP failure", {
  stub_api_base()
  stub_httr_response(
    status = 500L,
    body = list(error = "Internal server error"),
    http_error = TRUE,
    status_msg = "Server Error"
  )
  expect_error(
    survey160r:::s160_api_request("POST", "/fail", body = list(x = 1)),
    "API error"
  )
})

test_that("request falls back to http_status when error field is NULL", {
  stub_api_base()
  stub_httr_response(
    status = 502L,
    body = list(message = "something else"),
    http_error = TRUE,
    status_msg = "Bad Gateway"
  )
  expect_error(
    survey160r:::s160_api_request("POST", "/fail", body = list(x = 1)),
    "API error"
  )
})

# --- s160_api_campaign_results ---------------------------------------------------------

test_that("results triggers export and returns data frame after GCS update", {
  stub_api_base()
  stub_gcs_base()

  poll_count <- 0
  local_mocked_bindings(
    get_gcs_file_updated = function(campaign_id, filename) {
      poll_count <<- poll_count + 1
      if (poll_count <= 1) "2024-01-01T00:00:00Z" else "2024-01-01T01:00:00Z"
    },
    s160_api_request = function(method, path, body = NULL) {
      list(status = "processing")
    },
    s160_gcs_campaign_results_read = function(campaign_id, ...) {
      data.frame(campaignid = 1980, phone = "5551234567")
    }
  )

  df <- suppressMessages(s160_api_campaign_results(1980, timeout = 10, poll_interval = 0.1))
  expect_equal(df$campaignid, 1980)
  expect_equal(df$phone, "5551234567")
})

test_that("results works when no prior export exists (baseline is NULL)", {
  stub_api_base()
  stub_gcs_base()

  poll_count <- 0
  local_mocked_bindings(
    get_gcs_file_updated = function(campaign_id, filename) {
      poll_count <<- poll_count + 1
      if (poll_count <= 1) NULL else "2024-01-01T01:00:00Z"
    },
    s160_api_request = function(method, path, body = NULL) {
      list(status = "processing")
    },
    s160_gcs_campaign_results_read = function(campaign_id, ...) {
      data.frame(campaignid = 42)
    }
  )

  df <- suppressMessages(s160_api_campaign_results(42, timeout = 10, poll_interval = 0.1))
  expect_equal(df$campaignid, 42)
})

test_that("results propagates the trigger error before polling starts", {
  stub_api_base()
  stub_gcs_base()
  poll_called <- FALSE
  local_mocked_bindings(
    get_gcs_file_updated = function(campaign_id, filename) {
      poll_called <<- TRUE
      "2024-01-01T00:00:00Z"
    },
    s160_api_request = function(method, path, body = NULL) {
      stop("API error (POST /startCampaignResultsExport): Service Unavailable",
           call. = FALSE)
    }
  )
  expect_error(
    suppressMessages(s160_api_campaign_results(1980, timeout = 10,
                                               poll_interval = 0.1)),
    "Service Unavailable"
  )
  # get_gcs_file_updated is called once for the baseline before the trigger,
  # but never again -- the polling loop must not run.
  expect_true(poll_called)
})

test_that("results times out when GCS never updates", {
  stub_api_base()
  stub_gcs_base()

  local_mocked_bindings(
    get_gcs_file_updated = function(campaign_id, filename) "2024-01-01T00:00:00Z",
    s160_api_request = function(method, path, body = NULL) {
      list(status = "processing")
    }
  )

  expect_error(
    suppressMessages(s160_api_campaign_results(1980, timeout = 0.2, poll_interval = 0.1)),
    "timed out"
  )
})

test_that("results errors when API not authenticated", {
  stub_gcs_base()
  expect_error(s160_api_campaign_results(1980), "Run s160_api_auth")
})

test_that("results errors when GCS not initialized", {
  stub_api_base()
  expect_error(s160_api_campaign_results(1980), "Run s160_gcs_init")
})

test_that("results errors on invalid timeout", {
  stub_api_base()
  stub_gcs_base()
  expect_error(s160_api_campaign_results(1980, timeout = 0), "positive number")
  expect_error(s160_api_campaign_results(1980, timeout = -1), "positive number")
  expect_error(s160_api_campaign_results(1980, timeout = "abc"), "positive number")
})

test_that("results errors on invalid poll_interval", {
  stub_api_base()
  stub_gcs_base()
  expect_error(s160_api_campaign_results(1980, poll_interval = 0), "positive number")
  expect_error(s160_api_campaign_results(1980, poll_interval = -5), "positive number")
})

# --- get_gcs_file_updated -----------------------------------------------------

test_that("get_gcs_file_updated returns timestamp for matching file", {
  stub_gcs_base()
  stub_gcs_list(data.frame(
    name = "1980/1980_raw_data_download.csv",
    updated = "2024-06-15T10:00:00Z",
    stringsAsFactors = FALSE
  ))

  result <- survey160r:::get_gcs_file_updated("1980", "1980_raw_data_download.csv")
  expect_equal(result, "2024-06-15T10:00:00Z")
})

test_that("get_gcs_file_updated returns NULL when file not found", {
  stub_gcs_base()
  stub_gcs_list(data.frame(
    name = character(0), updated = character(0),
    stringsAsFactors = FALSE
  ))
  expect_null(survey160r:::get_gcs_file_updated("1980", "1980_raw_data_download.csv"))
})

test_that("get_gcs_file_updated returns NULL when target file not in list", {
  stub_gcs_base()
  stub_gcs_list(data.frame(
    name = "1980/other_file.csv",
    updated = "2024-06-15T10:00:00Z",
    stringsAsFactors = FALSE
  ))
  expect_null(survey160r:::get_gcs_file_updated("1980", "1980_raw_data_download.csv"))
})

test_that("get_gcs_file_updated returns NULL on GCS error", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(prefix, ...) stop("connection failed")
  )

  expect_null(survey160r:::get_gcs_file_updated("1980", "1980_raw_data_download.csv"))
})

# --- s160_api_campaign_get ----------------------------------------------------

test_that("campaign_get returns single-row data frame with base columns", {
  stub_api_base()

  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      list(
        success = TRUE,
        data = list(
          campaignid = 2107,
          name = "Test Campaign",
          active = "active",
          archive_scheduled_date = "2026-06-01T00:00:00Z",
          script = list(intro = list(id = "intro"), close = list(id = "close")),
          # Enriched fields should be dropped:
          listlength = 1234,
          list = list(list(phone = "5551234567")),
          login = list("agent1"),
          exports = list(),
          has_texting_started = TRUE,
          sandbox_configuration = list(),
          aggregator = "bandwidth",
          has_assigned_registration = TRUE
        )
      )
    }
  )

  df <- s160_api_campaign_get(2107)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 1L)
  expect_equal(df$campaignid, 2107)
  expect_equal(df$name, "Test Campaign")
  expect_equal(df$active, "active")
  expect_s3_class(df$archive_scheduled_date, "POSIXct")
  expect_equal(
    df$archive_scheduled_date,
    as.POSIXct("2026-06-01 00:00:00", tz = "UTC")
  )

  # script is a list-column (parsed JSON)
  expect_true(is.list(df$script))
  expect_equal(df$script[[1]]$intro$id, "intro")

  # Enriched fields dropped
  enriched <- c("listlength", "list", "login", "exports",
                "has_texting_started", "sandbox_configuration",
                "aggregator", "has_assigned_registration")
  expect_false(any(enriched %in% names(df)))
})

test_that("campaign_get treats NULL fields as NA", {
  stub_api_base()
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      list(success = TRUE,
           data = list(campaignid = 5, archive_scheduled_date = NULL))
    }
  )

  df <- s160_api_campaign_get(5)
  expect_true(is.na(df$archive_scheduled_date))
})

test_that("campaign_get maps 400 not-found to clear error", {
  stub_api_base()
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      stop("API error (GET /campaigns/9999): Bad Request", call. = FALSE)
    }
  )

  expect_error(s160_api_campaign_get(9999), "Campaign 9999 not found")
})

test_that("campaign_get maps 404 to clear error", {
  stub_api_base()
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      stop("API error (GET /campaigns/9999): Not Found", call. = FALSE)
    }
  )

  expect_error(s160_api_campaign_get(9999), "Campaign 9999 not found")
})

test_that("campaign_get propagates non-not-found errors unchanged", {
  stub_api_base()
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      stop("API error (GET /campaigns/1): Internal Server Error", call. = FALSE)
    }
  )

  expect_error(s160_api_campaign_get(1), "Internal Server Error")
})

test_that("campaign_get errors on success=false response", {
  stub_api_base()
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      list(success = FALSE)
    }
  )

  expect_error(s160_api_campaign_get(42), "Campaign 42 not found")
})

test_that("campaign_get errors when API not authenticated", {
  expect_error(s160_api_campaign_get(2107), "Run s160_api_auth")
})

test_that("campaign_get errors on invalid campaign_id", {
  stub_api_base()
  expect_error(s160_api_campaign_get(c(1, 2)), "single value")
  expect_error(s160_api_campaign_get(""), "non-empty")
})

test_that("campaign_get parses ISO-8601 timestamp columns to POSIXct", {
  stub_api_base()
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      list(success = TRUE, data = list(
        campaignid = 1,
        startdate = "2026-01-15 09:30:00",
        archive_scheduled_date = "2026-06-01T00:00:00Z",
        name = "Not a timestamp"
      ))
    }
  )

  df <- s160_api_campaign_get(1)
  expect_s3_class(df$startdate, "POSIXct")
  expect_equal(format(df$startdate, tz = "UTC"), "2026-01-15 09:30:00")
  expect_s3_class(df$archive_scheduled_date, "POSIXct")
  expect_equal(df$name, "Not a timestamp")
})

test_that("campaign_get leaves unparseable timestamp-shaped strings unchanged", {
  stub_api_base()
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      list(success = TRUE, data = list(
        campaignid = 1,
        weird = "2026-13-45 99:99:99"  # ISO-8601 shape, invalid values
      ))
    }
  )

  df <- s160_api_campaign_get(1)
  expect_type(df$weird, "character")
  expect_equal(df$weird, "2026-13-45 99:99:99")
})

test_that("campaign_get parses sub-second-precision timestamps", {
  stub_api_base()
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      list(success = TRUE, data = list(
        campaignid = 1,
        startdate = "2026-01-15T09:30:00.123456Z"
      ))
    }
  )

  df <- s160_api_campaign_get(1)
  expect_s3_class(df$startdate, "POSIXct")
  expect_equal(df$startdate, as.POSIXct("2026-01-15 09:30:00", tz = "UTC"))
})

test_that("campaign_get normalizes numeric UTC offsets when parsing timestamps", {
  stub_api_base()
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      list(success = TRUE, data = list(
        campaignid = 1,
        startdate    = "2026-01-15T09:30:00+05:30",
        archive_scheduled_date = "2026-02-20T12:00:00-0400"
      ))
    }
  )

  df <- s160_api_campaign_get(1)
  expect_s3_class(df$startdate, "POSIXct")
  expect_equal(format(df$startdate, tz = "UTC"), "2026-01-15 09:30:00")
  expect_s3_class(df$archive_scheduled_date, "POSIXct")
  expect_equal(format(df$archive_scheduled_date, tz = "UTC"),
               "2026-02-20 12:00:00")
})
