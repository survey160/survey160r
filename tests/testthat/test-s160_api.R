# --- Helper -------------------------------------------------------------------

# Set up a fake API auth state for tests that need it
stub_api_base <- function(env = parent.frame()) {
  .s160_api_env <- survey160r:::.s160_api_env
  .s160_api_env$jwt <- "test-jwt"
  .s160_api_env$base_url <- "https://test-api.survey160.com"
  .s160_api_env$userid <- "test-user"
  .s160_api_env$auth_time <- Sys.time()
  withr::defer({
    rm(list = ls(.s160_api_env), envir = .s160_api_env)
  }, envir = env)
}

# --- s160_api_auth ------------------------------------------------------------

test_that("auth succeeds and stores JWT", {
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = "key123")

  local_mocked_bindings(
    POST = function(url, ...) {
      structure(list(
        status_code = 200L,
        content = charToRaw('{"success":true,"data":"jwt-token-123","userid":"svc"}')
      ), class = "response")
    },
    http_error = function(resp) FALSE,
    content = function(resp, ...) list(success = TRUE, data = "jwt-token-123", userid = "svc"),
    .package = "httr"
  )

  env <- survey160r:::.s160_api_env
  withr::defer(rm(list = ls(env), envir = env))

  suppressMessages(s160_api_auth(base_url = "https://api.example.com"))

  expect_equal(env$jwt, "jwt-token-123")
  expect_equal(env$userid, "svc")
  expect_equal(env$base_url, "https://api.example.com")
})

test_that("auth fails with clear error on 401", {
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = "bad-key")

  local_mocked_bindings(
    POST = function(url, ...) {
      structure(list(status_code = 401L), class = "response")
    },
    http_error = function(resp) TRUE,
    content = function(resp, ...) list(error = "Invalid API key"),
    .package = "httr"
  )

  expect_error(
    s160_api_auth(base_url = "https://api.example.com"),
    "Authentication failed.*Invalid API key"
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

  local_mocked_bindings(
    POST = function(url, ...) {
      structure(list(
        status_code = 200L,
        url = url
      ), class = "response")
    },
    http_error = function(resp) FALSE,
    content = function(resp, ...) list(success = TRUE, data = "jwt", userid = "svc"),
    .package = "httr"
  )

  env <- survey160r:::.s160_api_env
  withr::defer(rm(list = ls(env), envir = env))

  suppressMessages(s160_api_auth(base_url = "https://api.example.com/"))
  expect_equal(env$base_url, "https://api.example.com")
})

test_that("auth defaults to production base_url", {
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = "key")

  captured_url <- NULL
  local_mocked_bindings(
    POST = function(url, ...) {
      captured_url <<- url
      structure(list(status_code = 200L), class = "response")
    },
    http_error = function(resp) FALSE,
    content = function(resp, ...) list(success = TRUE, data = "jwt", userid = "svc"),
    .package = "httr"
  )

  env <- survey160r:::.s160_api_env
  withr::defer(rm(list = ls(env), envir = env))

  suppressMessages(s160_api_auth())
  expect_equal(captured_url, "https://api.survey160.com/auth/serviceAccount")
})

test_that("auth errors on unexpected response format", {
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = "key")

  local_mocked_bindings(
    POST = function(url, ...) structure(list(status_code = 200L), class = "response"),
    http_error = function(resp) FALSE,
    content = function(resp, ...) list(success = FALSE),
    .package = "httr"
  )

  expect_error(s160_api_auth(), "unexpected response format")
})

test_that("auth falls back to http_status when error field is NULL", {
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = "key")

  local_mocked_bindings(
    POST = function(url, ...) structure(list(status_code = 503L), class = "response"),
    http_error = function(resp) TRUE,
    content = function(resp, ...) list(detail = "unavailable"),
    http_status = function(resp) list(message = "Service Unavailable"),
    .package = "httr"
  )

  expect_error(s160_api_auth(), "Authentication failed.*Service Unavailable")
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
  env <- survey160r:::.s160_api_env
  env$auth_time <- Sys.time() - 600  # 10 min ago

  auth_called <- FALSE
  local_mocked_bindings(
    s160_api_auth = function(...) {
      auth_called <<- TRUE
      env$jwt <- "refreshed-jwt"
      env$auth_time <- Sys.time()
    }
  )
  local_mocked_bindings(
    GET = function(url, ...) structure(list(status_code = 200L), class = "response"),
    http_error = function(resp) FALSE,
    content = function(resp, ...) list(ok = TRUE),
    .package = "httr"
  )

  survey160r:::s160_api_request("GET", "/test")
  expect_true(auth_called)
})

test_that("request does not refresh JWT when fresh", {
  stub_api_base()

  auth_called <- FALSE
  local_mocked_bindings(
    s160_api_auth = function(...) auth_called <<- TRUE
  )
  local_mocked_bindings(
    GET = function(url, ...) structure(list(status_code = 200L), class = "response"),
    http_error = function(resp) FALSE,
    content = function(resp, ...) list(ok = TRUE),
    .package = "httr"
  )

  survey160r:::s160_api_request("GET", "/test")
  expect_false(auth_called)
})

test_that("request raises error on HTTP failure", {
  stub_api_base()
  local_mocked_bindings(
    POST = function(url, ...) structure(list(status_code = 500L), class = "response"),
    http_error = function(resp) TRUE,
    content = function(resp, ...) list(error = "Internal server error"),
    http_status = function(resp) list(message = "Server Error"),
    .package = "httr"
  )

  expect_error(
    survey160r:::s160_api_request("POST", "/fail", body = list(x = 1)),
    "API error.*Internal server error"
  )
})

test_that("request falls back to http_status when error field is NULL", {
  stub_api_base()
  local_mocked_bindings(
    POST = function(url, ...) structure(list(status_code = 502L), class = "response"),
    http_error = function(resp) TRUE,
    content = function(resp, ...) list(message = "something else"),
    http_status = function(resp) list(message = "Bad Gateway"),
    .package = "httr"
  )

  expect_error(
    survey160r:::s160_api_request("POST", "/fail", body = list(x = 1)),
    "API error.*Bad Gateway"
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
  local_mocked_bindings(
    gcs_list_objects = function(prefix, ...) {
      data.frame(
        name = "1980/1980_raw_data_download.csv",
        updated = "2024-06-15T10:00:00Z",
        stringsAsFactors = FALSE
      )
    }
  )

  result <- survey160r:::get_gcs_file_updated("1980", "1980_raw_data_download.csv")
  expect_equal(result, "2024-06-15T10:00:00Z")
})

test_that("get_gcs_file_updated returns NULL when file not found", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(prefix, ...) {
      data.frame(name = character(0), updated = character(0), stringsAsFactors = FALSE)
    }
  )

  expect_null(survey160r:::get_gcs_file_updated("1980", "1980_raw_data_download.csv"))
})

test_that("get_gcs_file_updated returns NULL when target file not in list", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(prefix, ...) {
      data.frame(
        name = "1980/other_file.csv",
        updated = "2024-06-15T10:00:00Z",
        stringsAsFactors = FALSE
      )
    }
  )

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
  for (f in c("listlength", "list", "login", "exports",
              "has_texting_started", "sandbox_configuration",
              "aggregator", "has_assigned_registration")) {
    expect_false(f %in% names(df), info = f)
  }
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
