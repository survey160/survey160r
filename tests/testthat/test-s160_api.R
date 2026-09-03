# Coverage for R/s160_api.R. Mocks the httr quartet (POST/GET/http_error/
# content) via stub_httr_response(), and seeds the package-private auth env
# via stub_api_base(). See helper-stubs.R for both.

.api_env <- function() survey160r:::.s160_api_env

.defer_api_env_reset <- function(env = parent.frame()) {
  e <- .api_env()
  withr::defer(rm(list = ls(e), envir = e), envir = env)
}

# --- s160_api_auth (env-addressed) --------------------------------------------

test_that("auth succeeds and stores JWT (default env = prod)", {
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = "key123")
  stub_httr_response(
    body = list(success = TRUE, data = "jwt-token-123", userid = "svc")
  )
  .defer_api_env_reset()

  suppressMessages(s160_api_auth())

  env <- .api_env()
  expect_equal(env$jwt, "jwt-token-123")
  expect_equal(env$userid, "svc")
  expect_equal(env$base_url, "https://api.survey160.com")
  expect_equal(env$env, "prod")
  expect_equal(env$bucket, "campaign_results")
})

test_that("auth fails with clear error on 401", {
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = "bad-key")
  stub_httr_response(
    status = 401L,
    body = list(error = "Invalid API key"),
    http_error = TRUE
  )
  expect_error(s160_api_auth(), "Authentication failed.*Invalid API key")
})

test_that("auth errors when S160_API_USERID not set in non-interactive mode", {
  withr::local_envvar(S160_API_USERID = NA, S160_PROD_API_KEY = "key123")
  local_mocked_bindings(interactive = function() FALSE, .package = "base")
  expect_error(s160_api_auth(), "S160_API_USERID not set")
})

test_that("auth errors when no prod key var is set in non-interactive mode", {
  withr::local_envvar(S160_API_USERID = "svc", S160_PROD_API_KEY = NA,
                      S160_API_KEY = NA)
  local_mocked_bindings(interactive = function() FALSE, .package = "base")
  expect_error(s160_api_auth(env = "prod"), "S160_PROD_API_KEY not set")
})

test_that("auth rejects an unknown environment", {
  expect_error(s160_api_auth(env = "banana"), "should be one of")
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
  stub_no_sleep()  # 503 is transient: retried then surfaced, don't really wait
  expect_error(s160_api_auth(), "Authentication failed.*Service Unavailable")
})

test_that("auth errors clearly when the response body is not a list", {
  # A gateway/proxy returning HTML (not JSON) parses to a bare string; the
  # is.list() guard must surface the format error, not crash on `parsed$success`.
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = "key")
  stub_httr_response(body = "a gateway returned HTML, not JSON")
  expect_error(s160_api_auth(), "unexpected response format")
})

test_that("auth tolerates a multi-element error field, falling back to status", {
  # A vector `error` field must not crash nzchar(); http_error_message
  # short-circuits on length and falls back to the HTTP status text.
  withr::local_envvar(S160_API_USERID = "svc", S160_API_KEY = "key")
  stub_httr_response(
    status = 500L,
    body = list(error = c("err1", "err2")),
    http_error = TRUE,
    status_msg = "Server Error"
  )
  stub_no_sleep()  # 500 is transient: retried then surfaced, don't really wait
  expect_error(s160_api_auth(), "Authentication failed.*Server Error")
})

# --- s160_api_auth: env resolution + multi-env connections --------------------

# Mock api_do_auth to capture the resolved (url, userid, key) without a network
# call, mimicking the real field writes onto the connection.
fake_do_auth_capture <- function(seen) {
  function(conn, base_url, userid, api_key) {
    seen$base_url <- base_url
    seen$userid <- userid
    seen$api_key <- api_key
    conn$jwt <- "fake-jwt"
    conn$userid <- userid
    conn$api_key <- api_key
    conn$base_url <- base_url
    conn$auth_time <- Sys.time()
  }
}

test_that("env name resolves url + bucket atomically; prod prefers S160_PROD_API_KEY", {
  withr::local_envvar(S160_API_USERID = "svc",
                      S160_PROD_API_KEY = "prod-key", S160_API_KEY = "legacy")
  seen <- new_capture()
  local_mocked_bindings(api_do_auth = fake_do_auth_capture(seen))
  .defer_api_env_reset()

  conn <- suppressMessages(s160_api_auth(env = "prod"))
  expect_equal(seen$base_url, "https://api.survey160.com")
  expect_equal(seen$api_key, "prod-key")           # not the legacy fallback
  expect_equal(conn$bucket, "campaign_results")
  expect_equal(conn$env, "prod")
})

test_that("prod falls back to legacy S160_API_KEY when S160_PROD_API_KEY unset", {
  withr::local_envvar(S160_API_USERID = "svc",
                      S160_PROD_API_KEY = NA, S160_API_KEY = "legacy-key")
  seen <- new_capture()
  local_mocked_bindings(api_do_auth = fake_do_auth_capture(seen))
  .defer_api_env_reset()

  suppressMessages(s160_api_auth(env = "prod"))
  expect_equal(seen$api_key, "legacy-key")
})

test_that("staging resolves its own url, bucket, and key var", {
  withr::local_envvar(S160_API_USERID = "svc", S160_STAGING_API_KEY = "stg-key")
  seen <- new_capture()
  local_mocked_bindings(api_do_auth = fake_do_auth_capture(seen))
  .defer_api_env_reset()

  conn <- suppressMessages(s160_api_auth(env = "staging"))
  expect_equal(seen$base_url, "https://staging-api.survey160.com")
  expect_equal(seen$api_key, "stg-key")
  expect_equal(conn$bucket, "campaign_results_staging")
  expect_equal(conn$env, "staging")
})

test_that("dev resolves its own url, bucket, and derived key var", {
  withr::local_envvar(S160_API_USERID = "svc", S160_DEV_API_KEY = "dev-key")
  seen <- new_capture()
  local_mocked_bindings(api_do_auth = fake_do_auth_capture(seen))
  .defer_api_env_reset()

  conn <- suppressMessages(s160_api_auth(env = "dev"))
  expect_equal(seen$base_url, "https://dev-api.survey160.com")
  expect_equal(seen$api_key, "dev-key")            # from S160_DEV_API_KEY
  expect_equal(conn$bucket, "campaign_results_dev")
  expect_equal(conn$env, "dev")
})

test_that("s160_api_auth errors when the config has no api_url for the env", {
  # Defensive guard: an env in the enum but absent an api_url in the config.
  local_mocked_bindings(get_config = function() {
    list(environments = list(
      prod = list(api_url = "https://api.survey160.com"),
      staging = list(api_url = "https://staging-api.survey160.com"),
      dev = list()
    ))
  })
  expect_error(s160_api_auth(env = "dev"),
               "no dev API environment.*prod, staging")
})

test_that("s160_api_auth deprecates passing the environment positionally", {
  withr::local_envvar(S160_API_USERID = "svc", S160_PROD_API_KEY = "pk")
  local_mocked_bindings(api_do_auth = function(conn, base_url, userid, api_key) {
    conn$jwt <- "jwt"
    conn$auth_time <- Sys.time()
  })
  .defer_api_env_reset()

  w <- capture_warnings(suppressMessages(s160_api_auth("prod")))
  expect_match(w, "positionally.*deprecated")
  # The suggestion names the concrete analyst calls, not an abstract env = "...".
  expect_match(w, "s160_api_auth\\(\\) for prod")
  expect_match(w, 's160_api_auth\\(env = "staging"\\) for staging')
  # Named passing (the fixed form) is silent.
  expect_silent(suppressMessages(s160_api_auth(env = "prod")))
})

test_that("prod and staging connections are independent; default tracks latest", {
  withr::local_envvar(S160_API_USERID = "svc",
                      S160_PROD_API_KEY = "pk", S160_STAGING_API_KEY = "sk")
  local_mocked_bindings(api_do_auth = function(conn, base_url, userid, api_key) {
    conn$jwt <- paste0("jwt-", api_key)
    conn$base_url <- base_url
    conn$userid <- userid
    conn$api_key <- api_key
    conn$auth_time <- Sys.time()
  })
  .defer_api_env_reset()

  prod <- suppressMessages(s160_api_auth(env = "prod"))
  stg  <- suppressMessages(s160_api_auth(env = "staging"))

  expect_false(identical(prod, stg))
  expect_equal(prod$base_url, "https://api.survey160.com")
  expect_equal(prod$bucket, "campaign_results")
  expect_equal(stg$base_url, "https://staging-api.survey160.com")
  expect_equal(stg$bucket, "campaign_results_staging")

  # default mirrors the most recent auth (staging)
  env <- .api_env()
  expect_equal(env$env, "staging")
  expect_equal(env$bucket, "campaign_results_staging")
})

test_that("a connection prints as an opaque handle, masking the key", {
  withr::local_envvar(S160_API_USERID = "svc", S160_PROD_API_KEY = "secret-key")
  local_mocked_bindings(api_do_auth = fake_do_auth_capture(new_capture()))
  .defer_api_env_reset()

  conn <- suppressMessages(s160_api_auth(env = "prod"))
  expect_s3_class(conn, "s160_api_conn")

  out <- capture.output(print(conn))
  expect_true(any(grepl("survey160 API connection: prod", out)))
  expect_true(any(grepl("hidden", out)))
  expect_false(any(grepl("secret-key", out)))   # the key is never printed
})

# --- api_do_auth credential guard ---------------------------------------------

test_that("api_do_auth rejects missing credentials before POSTing", {
  conn <- new.env(parent = emptyenv())
  expect_error(survey160r:::api_do_auth(conn, "https://x", NULL, "k"),
               "`userid` must be a non-empty string")
  expect_error(survey160r:::api_do_auth(conn, "https://x", "u", NULL),
               "`api_key` must be a non-empty string")
  expect_error(survey160r:::api_do_auth(conn, "  ", "u", "k"),
               "`base_url` must be a non-empty string")
})

test_that("api_do_auth strips a trailing slash from base_url", {
  conn <- new.env(parent = emptyenv())
  stub_httr_response(body = list(success = TRUE, data = "jwt", userid = "u"))
  survey160r:::api_do_auth(conn, "https://api.example.com/", "u", "k")
  expect_equal(conn$base_url, "https://api.example.com")
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
    api_do_auth = function(conn, base_url, userid, api_key) {
      auth_called <<- TRUE
      conn$jwt <- "refreshed-jwt"
      conn$auth_time <- Sys.time()
    }
  )
  stub_httr_response(body = list(ok = TRUE))

  survey160r:::s160_api_request("GET", "/test")
  expect_true(auth_called)
})

test_that("request does not refresh JWT when fresh", {
  stub_api_base()
  auth_called <- FALSE
  local_mocked_bindings(api_do_auth = function(...) auth_called <<- TRUE)
  stub_httr_response(body = list(ok = TRUE))

  survey160r:::s160_api_request("GET", "/test")
  expect_false(auth_called)
})

test_that("request refresh reuses the connection's stored credentials", {
  # The refresh must re-auth with the creds that authenticated THIS connection
  # (stored on the env by api_do_auth), not re-read the S160_API_* env vars --
  # otherwise a connection pointed at a non-default environment silently breaks
  # at the 8-minute mark. Decoy env vars; assert the refresh ignored them.
  stub_api_base()                       # sets userid = "test-user"
  env <- .api_env()
  env$api_key <- "stored-key"
  env$auth_time <- Sys.time() - 600
  withr::local_envvar(S160_API_USERID = "DECOY", S160_API_KEY = "DECOY-KEY")

  seen <- new_capture()
  local_mocked_bindings(
    api_do_auth = function(conn, base_url, userid, api_key) {
      seen$base_url <- base_url
      seen$userid <- userid
      seen$api_key <- api_key
      conn$auth_time <- Sys.time()
    }
  )
  stub_httr_response(body = list(ok = TRUE))

  survey160r:::s160_api_request("GET", "/test")
  expect_equal(seen$base_url, "https://test-api.survey160.com")
  expect_equal(seen$userid, "test-user")
  expect_equal(seen$api_key, "stored-key")
})

test_that("refresh on a captured connection uses its creds, default untouched", {
  # The multi-env isolation guarantee: refreshing a held prod handle must
  # re-auth with prod's creds and leave the (staging) default connection alone.
  env <- .api_env()                                   # default = staging
  env$jwt <- "stg-jwt"
  env$base_url <- "https://staging-api.survey160.com"
  env$userid <- "stg-u"
  env$api_key <- "stg-key"
  env$auth_time <- Sys.time()
  .defer_api_env_reset()

  prod <- new.env(parent = emptyenv())                # captured prod handle, stale JWT
  prod$jwt <- "prod-jwt"
  prod$base_url <- "https://api.survey160.com"
  prod$userid <- "prod-u"
  prod$api_key <- "prod-key"
  prod$auth_time <- Sys.time() - 600

  seen <- new_capture()
  local_mocked_bindings(
    api_do_auth = function(conn, base_url, userid, api_key) {
      seen$refreshed_prod <- identical(conn, prod)
      seen$userid <- userid
      seen$api_key <- api_key
      conn$auth_time <- Sys.time()
    }
  )
  stub_httr_response(body = list(ok = TRUE))

  survey160r:::s160_api_request("GET", "/x", conn = prod)

  expect_true(seen$refreshed_prod)        # refreshed the prod handle, not the default
  expect_equal(seen$userid, "prod-u")     # with prod's credentials
  expect_equal(seen$api_key, "prod-key")
  expect_equal(env$userid, "stg-u")       # default (staging) left untouched
  expect_equal(env$jwt, "stg-jwt")
})

test_that("request refreshes JWT just past the 480-second threshold", {
  # Threshold lives in R/s160_api.R: `if (elapsed > 480) refresh`. Pinning a
  # test at 481s (just over) guards against off-by-one drift; the 600s test
  # above only proves the "well past" path.
  stub_api_base()
  env <- .api_env()
  env$auth_time <- Sys.time() - 481

  auth_called <- FALSE
  local_mocked_bindings(api_do_auth = function(conn, ...) {
    auth_called <<- TRUE
    conn$auth_time <- Sys.time()
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
  local_mocked_bindings(api_do_auth = function(...) auth_called <<- TRUE)
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
  stub_no_sleep()  # 500 is transient: retried then surfaced, don't really wait
  expect_error(
    survey160r:::s160_api_request("POST", "/fail", body = list(x = 1)),
    "API error.*Internal server error"
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
  stub_no_sleep()  # 502 is transient: retried then surfaced, don't really wait
  expect_error(
    survey160r:::s160_api_request("POST", "/fail", body = list(x = 1)),
    "API error.*Bad Gateway"
  )
})

# --- s160_api_request: bounded timeout + transient retry (R2/R3) --------------

test_that("request retries a transient 5xx and then succeeds", {
  stub_api_base()
  waits <- new_capture()
  calls <- new_capture()
  stub_httr_seq(list(503L, 200L), capture = calls)
  stub_no_sleep(capture = waits)
  out <- survey160r:::s160_api_request("GET", "/ok")
  expect_equal(out, list(success = TRUE, data = "ok"))
  expect_length(calls$calls, 2L)   # one transient failure + one success
  expect_equal(waits$waits, 1)     # a single 1s backoff before the retry
})

test_that("request retries a persistent 5xx with exponential backoff, then fails", {
  stub_api_base()
  waits <- new_capture()
  calls <- new_capture()
  stub_httr_seq(list(503L), capture = calls)   # 503 on every attempt
  stub_no_sleep(capture = waits)
  expect_error(survey160r:::s160_api_request("GET", "/down"), "API error")
  expect_length(calls$calls, 4L)               # 1 initial + .http_max_retries (3)
  expect_equal(waits$waits, c(1, 2, 4))        # exponential (cap not reached)
})

test_that("request does not retry a 4xx client error (fails fast, no backoff)", {
  stub_api_base()
  waits <- new_capture()
  calls <- new_capture()
  stub_httr_seq(list(400L), capture = calls)
  stub_no_sleep(capture = waits)
  expect_error(survey160r:::s160_api_request("GET", "/missing"), "API error")
  expect_length(calls$calls, 1L)   # terminal on the first response
  expect_null(waits$waits)         # never slept
})

test_that("request retries a persistent network error and then re-raises it", {
  stub_api_base()
  waits <- new_capture()
  calls <- new_capture()
  stub_httr_seq(list("error"), capture = calls)
  stub_no_sleep(capture = waits)
  expect_error(
    survey160r:::s160_api_request("POST", "/x", body = list(a = 1)),
    "Could not resolve host"
  )
  expect_length(calls$calls, 4L)
  expect_equal(waits$waits, c(1, 2, 4))
})

test_that("request recovers when a network error is followed by success", {
  stub_api_base()
  waits <- new_capture()
  stub_httr_seq(list("error", 200L))
  stub_no_sleep(capture = waits)
  out <- survey160r:::s160_api_request("GET", "/ok")
  expect_equal(out$data, "ok")
  expect_equal(waits$waits, 1)
})

# --- s160_api_campaign_results ---------------------------------------------------------

test_that("results triggers export and returns data frame after GCS update", {
  stub_api_base()
  stub_gcs_base()

  poll_count <- 0
  local_mocked_bindings(
    get_gcs_file_updated = function(campaign_id, filename, ...) {
      poll_count <<- poll_count + 1
      if (poll_count <= 1) "2024-01-01T00:00:00Z" else "2024-01-01T01:00:00Z"
    },
    s160_api_request = function(method, path, body = NULL, ...) {
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
    get_gcs_file_updated = function(campaign_id, filename, ...) {
      poll_count <<- poll_count + 1
      if (poll_count <= 1) NULL else "2024-01-01T01:00:00Z"
    },
    s160_api_request = function(method, path, body = NULL, ...) {
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
    get_gcs_file_updated = function(campaign_id, filename, ...) {
      poll_called <<- TRUE
      "2024-01-01T00:00:00Z"
    },
    s160_api_request = function(method, path, body = NULL, ...) {
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
    get_gcs_file_updated = function(campaign_id, filename, ...) "2024-01-01T00:00:00Z",
    s160_api_request = function(method, path, body = NULL, ...) {
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
  # Non-finite values must not slip past into an unbounded poll loop.
  expect_error(s160_api_campaign_results(1980, timeout = Inf), "positive number")
  expect_error(s160_api_campaign_results(1980, timeout = NA_real_), "positive number")
  expect_error(s160_api_campaign_results(1980, timeout = NaN), "positive number")
})

test_that("results errors on invalid poll_interval", {
  stub_api_base()
  stub_gcs_base()
  expect_error(s160_api_campaign_results(1980, poll_interval = 0), "positive number")
  expect_error(s160_api_campaign_results(1980, poll_interval = -5), "positive number")
  expect_error(s160_api_campaign_results(1980, poll_interval = Inf), "positive number")
  expect_error(s160_api_campaign_results(1980, poll_interval = NA_real_), "positive number")
  expect_error(s160_api_campaign_results(1980, poll_interval = NaN), "positive number")
})

# --- s160_api_campaign_results: per-connection bucket -------------------------

test_that("results targets the connection's environment for poll and read", {
  stub_gcs_base()
  conn <- new.env(parent = emptyenv())
  conn$jwt <- "jwt"
  conn$base_url <- "https://staging-api.survey160.com"
  conn$userid <- "analytics"
  conn$auth_time <- Sys.time()
  conn$env <- "staging"

  seen <- new_capture()
  poll_count <- 0
  local_mocked_bindings(
    get_gcs_file_updated = function(campaign_id, filename, env = NULL) {
      seen$poll_env <- env
      poll_count <<- poll_count + 1
      if (poll_count <= 1) "t0" else "t1"
    },
    s160_api_request = function(method, path, body = NULL, conn = NULL) {
      seen$req_userid <- body$userid
      seen$req_conn <- conn
      list(status = "processing")
    },
    s160_gcs_campaign_results_read = function(campaign_id, ..., env = NULL) {
      seen$read_env <- env
      data.frame(campaignid = 744)
    }
  )

  df <- suppressMessages(
    s160_api_campaign_results(744, timeout = 10, poll_interval = 0.1, conn = conn)
  )
  expect_equal(df$campaignid, 744)
  expect_equal(seen$poll_env, "staging")
  expect_equal(seen$read_env, "staging")
  expect_equal(seen$req_userid, "analytics")
  # The trigger must route through THIS connection (its base_url decides which
  # environment the export fires against), not the default one.
  expect_identical(seen$req_conn, conn)
  expect_equal(seen$req_conn$base_url, "https://staging-api.survey160.com")
})

# --- get_gcs_file_updated -----------------------------------------------------

test_that("get_gcs_file_updated returns timestamp for matching file", {
  stub_gcs_base()
  stub_gcs_list(data.frame(
    name = "1980/1980_raw_data_download.csv",
    updated = "2024-06-15T10:00:00Z",
    stringsAsFactors = FALSE
  ))

  result <- survey160r:::get_gcs_file_updated("1980", "1980_raw_data_download.csv", "prod")
  expect_equal(result, "2024-06-15T10:00:00Z")
})

test_that("get_gcs_file_updated returns NULL when file not found", {
  stub_gcs_base()
  stub_gcs_list(data.frame(
    name = character(0), updated = character(0),
    stringsAsFactors = FALSE
  ))
  expect_null(survey160r:::get_gcs_file_updated("1980", "1980_raw_data_download.csv", "prod"))
})

test_that("get_gcs_file_updated returns NULL when target file not in list", {
  stub_gcs_base()
  stub_gcs_list(data.frame(
    name = "1980/other_file.csv",
    updated = "2024-06-15T10:00:00Z",
    stringsAsFactors = FALSE
  ))
  expect_null(survey160r:::get_gcs_file_updated("1980", "1980_raw_data_download.csv", "prod"))
})

test_that("get_gcs_file_updated surfaces a persistent GCS listing failure (R5)", {
  # A real failure (auth/permission/bucket, e.g. a forgotten s160_gcs_init())
  # must NOT be swallowed to NULL -- that masked it as "file not there yet" and
  # made the export poll spin until a misleading timeout. A genuinely-absent file
  # yields an EMPTY listing (covered above), not an error, so surfacing here only
  # affects real failures.
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(prefix, ...) stop("connection failed")
  )

  expect_error(
    survey160r:::get_gcs_file_updated("1980", "1980_raw_data_download.csv", "prod"),
    "Failed to list campaign export files.*connection failed"
  )
})

test_that("get_gcs_file_updated lists the campaign bucket for the env", {
  seen <- new_capture()
  local_mocked_bindings(
    gcs_list_objects = function(prefix = NULL, ...) {
      seen$bucket <- list(...)$bucket
      data.frame(name = "744/744_raw_data_download.csv",
                 updated = "2024-06-15T10:00:00Z", stringsAsFactors = FALSE)
    }
  )
  result <- survey160r:::get_gcs_file_updated(
    "744", "744_raw_data_download.csv", "staging"
  )
  expect_equal(result, "2024-06-15T10:00:00Z")
  expect_equal(seen$bucket, "campaign_results_staging")
})

# --- s160_api_campaign_get ----------------------------------------------------

test_that("campaign_get returns single-row data frame with base columns", {
  stub_api_base()

  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL, ...) {
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
    s160_api_request = function(method, path, body = NULL, ...) {
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
    s160_api_request = function(method, path, body = NULL, ...) {
      survey160r:::stop_http_error(
        400L, "API error (GET /campaigns/9999): Bad Request"
      )
    }
  )

  expect_error(s160_api_campaign_get(9999), "campaign not found: 9999")
})

test_that("campaign_get maps 404 to clear error", {
  stub_api_base()
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL, ...) {
      survey160r:::stop_http_error(
        404L, "API error (GET /campaigns/9999): Not Found"
      )
    }
  )

  expect_error(s160_api_campaign_get(9999), "campaign not found: 9999")
})

test_that("campaign_get propagates non-not-found errors unchanged", {
  stub_api_base()
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL, ...) {
      survey160r:::stop_http_error(
        500L, "API error (GET /campaigns/1): Internal Server Error"
      )
    }
  )

  expect_error(s160_api_campaign_get(1), "Internal Server Error")
})

test_that("campaign_get errors on success=false response", {
  stub_api_base()
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL, ...) {
      list(success = FALSE)
    }
  )

  expect_error(s160_api_campaign_get(42),
               "Failed to read campaign: unexpected response format")
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
    s160_api_request = function(method, path, body = NULL, ...) {
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
    s160_api_request = function(method, path, body = NULL, ...) {
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
    s160_api_request = function(method, path, body = NULL, ...) {
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
    s160_api_request = function(method, path, body = NULL, ...) {
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

test_that("campaign_get routes the request through the supplied connection", {
  conn <- new.env(parent = emptyenv())
  conn$jwt <- "jwt"
  conn$base_url <- "https://staging-api.survey160.com"
  conn$userid <- "analytics"
  conn$auth_time <- Sys.time()

  seen <- new_capture()
  local_mocked_bindings(
    s160_api_request = function(method, path, body = NULL, conn = NULL) {
      seen$conn <- conn
      list(success = TRUE, data = list(campaignid = 7))
    }
  )

  df <- s160_api_campaign_get(7, conn = conn)
  expect_identical(seen$conn, conn)
  expect_equal(df$campaignid, 7)
})
