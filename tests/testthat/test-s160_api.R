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

# --- s160_api_batch_archive_campaigns -----------------------------------------

test_that("batch archive schedules campaigns with given date", {
  stub_api_base()

  calls <- list()
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      calls[[length(calls) + 1]] <<- list(method = method, path = path, body = body)
      list(success = TRUE)
    }
  )

  result <- s160_api_batch_archive_campaigns(
    c(1001, 1002, 1003),
    archive_date = as.Date("2026-05-15")
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(result$campaign_id, c("1001", "1002", "1003"))
  expect_true(all(result$success))

  expect_equal(length(calls), 3)
  expect_equal(calls[[1]]$method, "POST")
  expect_equal(calls[[1]]$path, "/campaigns/1001")
  expect_equal(
    calls[[1]]$body$ncd$archive_scheduled_date,
    "2026-05-15T00:00:00.000Z"
  )
  expect_equal(calls[[2]]$path, "/campaigns/1002")
  expect_equal(calls[[3]]$path, "/campaigns/1003")
})

test_that("batch archive defaults to today", {
  stub_api_base()

  captured <- NULL
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      captured <<- body
      list(success = TRUE)
    }
  )

  s160_api_batch_archive_campaigns(1001)

  expected <- paste0(format(Sys.Date(), "%Y-%m-%d"), "T00:00:00.000Z")
  expect_equal(captured$ncd$archive_scheduled_date, expected)
})

test_that("batch archive accepts YYYY-MM-DD string", {
  stub_api_base()

  captured <- NULL
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      captured <<- body
      list(success = TRUE)
    }
  )

  s160_api_batch_archive_campaigns(1001, archive_date = "2026-07-04")

  expect_equal(
    captured$ncd$archive_scheduled_date,
    "2026-07-04T00:00:00.000Z"
  )
})

test_that("batch archive sends JSON null when archive_date is NULL", {
  stub_api_base()

  captured <- NULL
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      captured <<- body
      list(success = TRUE)
    }
  )

  res <- s160_api_batch_archive_campaigns(c(1001, 1002), archive_date = NULL)

  expect_true(is.na(captured$ncd$archive_scheduled_date))
  expect_true(all(res$success))

  json <- jsonlite::toJSON(captured, auto_unbox = TRUE)
  expect_match(as.character(json), '"archive_scheduled_date":null', fixed = TRUE)
})

test_that("batch archive errors on invalid date string", {
  stub_api_base()
  expect_error(
    s160_api_batch_archive_campaigns(1001, archive_date = "not-a-date"),
    "Date"
  )
})

test_that("batch archive errors on non-Date non-string archive_date", {
  stub_api_base()
  expect_error(
    s160_api_batch_archive_campaigns(1001, archive_date = 12345),
    "Date"
  )
})

test_that("batch archive captures invalid per-campaign IDs without aborting", {
  stub_api_base()

  call_count <- 0
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      call_count <<- call_count + 1
      list(success = TRUE)
    }
  )

  result <- s160_api_batch_archive_campaigns(c("1001", "", "1003"))

  expect_equal(nrow(result), 3)
  expect_equal(result$success, c(TRUE, FALSE, TRUE))
  expect_match(result$message[2], "non-empty")
  expect_equal(call_count, 2)
})

test_that("batch archive errors when archive_date is a Date vector", {
  stub_api_base()
  expect_error(
    s160_api_batch_archive_campaigns(
      1001,
      archive_date = as.Date(c("2026-05-15", "2026-05-16"))
    ),
    "single Date"
  )
})

test_that("batch archive errors on empty campaign_ids", {
  stub_api_base()
  expect_error(
    s160_api_batch_archive_campaigns(integer(0)),
    "at least one ID"
  )
})

test_that("batch archive errors when not authenticated", {
  expect_error(s160_api_batch_archive_campaigns(1001), "Run s160_api_auth")
})

test_that("batch archive collects per-campaign failures without aborting", {
  stub_api_base()

  call_count <- 0
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      call_count <<- call_count + 1
      if (call_count == 2) stop("API error (POST /campaigns/1002): boom")
      list(success = TRUE)
    }
  )

  result <- s160_api_batch_archive_campaigns(c(1001, 1002, 1003))

  expect_equal(nrow(result), 3)
  expect_equal(result$success, c(TRUE, FALSE, TRUE))
  expect_match(result$message[2], "boom")
  expect_equal(result$message[c(1, 3)], c("", ""))
})

test_that("batch archive reports unsuccessful API responses per campaign", {
  stub_api_base()

  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL) {
      list(success = FALSE, message = "Invalid date format for archive_scheduled_date")
    }
  )

  result <- s160_api_batch_archive_campaigns(c(1001, 1002))
  expect_false(any(result$success))
  expect_true(all(result$message == "Invalid date format for archive_scheduled_date"))
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
