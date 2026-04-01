# --- Helper -------------------------------------------------------------------

# Set up a fake API auth state for tests that need it
stub_api_base <- function(env = parent.frame()) {
  .s160_api_env <- survey160r:::.s160_api_env
  .s160_api_env$jwt <- "test-jwt"
  .s160_api_env$base_url <- "https://test-api.survey160.com"
  .s160_api_env$userid <- "test-user"
  .s160_api_env$api_key <- "test-key"
  .s160_api_env$auth_time <- Sys.time()
  withr::defer({
    rm(list = ls(.s160_api_env), envir = .s160_api_env)
  }, envir = env)
}

# --- s160_api_auth ------------------------------------------------------------

test_that("auth succeeds and stores JWT", {
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

  suppressMessages(s160_api_auth("svc", "key123", "https://api.example.com"))

  expect_equal(env$jwt, "jwt-token-123")
  expect_equal(env$userid, "svc")
  expect_equal(env$api_key, "key123")
  expect_equal(env$base_url, "https://api.example.com")
})

test_that("auth fails with clear error on 401", {
  local_mocked_bindings(
    POST = function(url, ...) {
      structure(list(status_code = 401L), class = "response")
    },
    http_error = function(resp) TRUE,
    content = function(resp, ...) list(error = "Invalid API key"),
    .package = "httr"
  )

  expect_error(
    s160_api_auth("svc", "bad-key", "https://api.example.com"),
    "Authentication failed.*Invalid API key"
  )
})

test_that("auth errors on missing arguments", {
  expect_error(s160_api_auth(), "required")
  expect_error(s160_api_auth("svc"), "required")
  expect_error(s160_api_auth("svc", "key"), "required")
})

test_that("auth errors on empty string arguments", {
  expect_error(s160_api_auth("", "key", "url"), "non-empty")
  expect_error(s160_api_auth("svc", "", "url"), "non-empty")
  expect_error(s160_api_auth("svc", "key", ""), "non-empty")
})

test_that("auth strips trailing slash from base_url", {
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

  suppressMessages(s160_api_auth("svc", "key", "https://api.example.com/"))
  expect_equal(env$base_url, "https://api.example.com")
})

test_that("auth errors on unexpected response format", {
  local_mocked_bindings(
    POST = function(url, ...) structure(list(status_code = 200L), class = "response"),
    http_error = function(resp) FALSE,
    content = function(resp, ...) list(success = FALSE),
    .package = "httr"
  )

  expect_error(
    s160_api_auth("svc", "key", "https://api.example.com"),
    "unexpected response format"
  )
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

# --- s160_api_results ---------------------------------------------------------

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
    s160_gcs_results_read = function(campaign_id, ...) {
      data.frame(campaignid = 1980, phone = "5551234567")
    }
  )

  df <- suppressMessages(s160_api_results(1980, timeout = 10, poll_interval = 0.1))
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
    s160_gcs_results_read = function(campaign_id, ...) {
      data.frame(campaignid = 42)
    }
  )

  df <- suppressMessages(s160_api_results(42, timeout = 10, poll_interval = 0.1))
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
    suppressMessages(s160_api_results(1980, timeout = 0.2, poll_interval = 0.1)),
    "timed out"
  )
})

test_that("results errors when API not authenticated", {
  stub_gcs_base()
  expect_error(s160_api_results(1980), "Run s160_api_auth")
})

test_that("results errors when GCS not initialized", {
  stub_api_base()
  expect_error(s160_api_results(1980), "Run s160_gcs_init")
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
