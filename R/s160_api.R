# Survey160 API client -- trigger exports and poll for completion
#
# Auth strategy: API key-based service account authentication.
#
# Credentials are read from ~/.Renviron: S160_API_USERID for the user, and an
# environment-specific API key (S160_STAGING_API_KEY for staging;
# S160_PROD_API_KEY, falling back to the legacy S160_API_KEY, for prod). On
# first interactive run, missing values are prompted and saved automatically.
#
# JWT refresh: Access tokens expire after 10 minutes. Rather than
# implementing a refresh token flow, we re-authenticate with the stored
# API key when the JWT is older than 8 minutes.
#
# Connections: a "connection" is an environment holding one authenticated
# session's state (jwt, userid, api_key, base_url, env, and the paired GCS
# bucket). The package owns one default connection (.s160_api_env) that every
# API function defaults to. Each s160_api_auth() call builds a fresh, independent
# connection AND mirrors it into the default, so a caller can either ignore the
# return value (single environment -- conn-less calls use the latest auth) or
# capture it and pass `conn =` to hold prod and staging live at once.

# --- Internal state -----------------------------------------------------------

# The default connection. s160_api_auth() authenticates into this one; all API
# functions default `conn` to it.
.s160_api_env <- new.env(parent = emptyenv())

check_api_ready <- function(conn = NULL) {
  conn <- conn %||% .s160_api_env
  if (is.null(conn$jwt) || conn$jwt == "") {
    stop_not_initialized("API", "s160_api_auth")
  }
}

# --- HTTP transport (bounded timeout + transient retry) -----------------------

# Every network call is bounded by a per-request timeout so a hung server can't
# wedge the R session or the scheduled producer (the requests were previously
# unbounded). Transient failures -- a curl/network error, or one of the HTTP
# statuses in `.http_retry_status` (429, 500, 502, 503, 504) -- are retried with
# exponential backoff. Every other status is terminal and returned immediately,
# so a 400/404 (and a non-transient 5xx such as 501/505) fails fast -- e.g. the
# not-found path in s160_api_campaign_get, which must not incur retry pauses.
.http_timeout_seconds <- 60
.http_max_retries <- 3L
.http_retry_status <- c(429L, 500L, 502L, 503L, 504L)

# Perform one HTTP request via `do_request` (a thunk that calls a single httr
# GET/POST with httr::timeout() already attached) and retry transient failures.
# Returns the httr response for the caller's own http_error()/content()
# handling; re-raises a persistent network error once retries are exhausted.
# Retrying the export-trigger POST is safe: the server just regenerates the
# results CSV, so a duplicate trigger is wasteful but not harmful. Passing the
# request as a thunk keeps the mocked httr verbs as the retry seam in tests.
http_send <- function(do_request, describe, max_retries = .http_max_retries) {
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    resp <- tryCatch(do_request(), error = function(e) e)
    is_err <- inherits(resp, "error")
    transient <- is_err || (httr::status_code(resp) %in% .http_retry_status)
    if (!transient || attempt > max_retries) {
      if (is_err) stop(resp)
      return(resp)
    }
    detail <- if (is_err) conditionMessage(resp) else paste("HTTP", httr::status_code(resp))
    wait <- min(2^(attempt - 1L), 30)
    message(sprintf(
      "Request failed (%s: %s); retrying in %ds (attempt %d/%d)...",
      describe, detail, wait, attempt + 1L, max_retries + 1L
    ))
    Sys.sleep(wait)
  }
}

# POST service-account credentials and store the resulting JWT (and its
# provenance) into `conn`. Shared by s160_api_auth() (which builds the fresh
# connection) and the in-session refresh in s160_api_request() (re-auth into
# whichever connection issued the request). Stashing userid/api_key on `conn`
# is what lets the refresh reuse the exact credentials that authenticated the
# session rather than re-reading the environment -- critical when a connection
# points at a non-default environment.
api_do_auth <- function(conn, base_url, userid, api_key) {
  # Defensive: s160_api_auth resolves these before calling, but the refresh path
  # reuses whatever is stored on `conn`, so guard against a half-built
  # connection rather than POSTing "ApiKey " / userid = NULL / a relative URL.
  check_nonempty_string(userid, "userid")
  check_nonempty_string(api_key, "api_key")
  check_nonempty_string(base_url, "base_url")
  base_url <- sub("/$", "", trimws(base_url))

  url <- paste0(base_url, "/auth/serviceAccount")
  resp <- http_send(
    function() {
      httr::POST(
        url,
        httr::add_headers(Authorization = paste("ApiKey", api_key)),
        httr::content_type_json(),
        body = list(userid = userid),
        encode = "json",
        httr::timeout(.http_timeout_seconds)
      )
    },
    describe = "authenticate"
  )

  if (httr::http_error(resp)) {
    stop(sprintf("Authentication failed: %s", http_error_message(resp)),
         call. = FALSE)
  }

  parsed <- httr::content(resp, as = "parsed")
  if (!is.list(parsed) || !isTRUE(parsed$success) || is.null(parsed$data)) {
    stop("Authentication failed: unexpected response format.", call. = FALSE)
  }

  conn$jwt <- parsed$data
  conn$userid <- userid
  conn$api_key <- api_key
  conn$base_url <- base_url
  conn$auth_time <- Sys.time()
  invisible(conn)
}

# Read a credential from env, prompting interactively if missing
get_credential <- function(var_name, prompt_msg, secret = FALSE) {
  value <- Sys.getenv(var_name)
  if (nzchar(value)) return(value)

  if (!interactive()) {
    stop(
      sprintf("%s not set in the current R session.\n", var_name),
      "Run s160_api_auth() interactively to set it up, ",
      sprintf("or add %s to ~/.Renviron and restart R.", var_name),
      call. = FALSE
    )
  }

  prompt_and_save_renviron(var_name, prompt_msg, secret = secret) # nocov # nolint object_usage_linter.
}

# Extract a single human-readable error message from an httr error response.
# The API normally returns {"error": "..."}, but a gateway/proxy can return
# HTML or a multi-element body; coerce defensively to one string (the
# length != 1 guard short-circuits before nzchar() would choke on a vector) and
# fall back to the HTTP status text.
http_error_message <- function(resp) {
  msg <- tryCatch(
    httr::content(resp, as = "parsed")$error,
    error = function(e) NULL
  )
  if (is.null(msg) || !is.character(msg) || length(msg) != 1L || !nzchar(msg)) {
    msg <- httr::http_status(resp)$message
  }
  msg
}

# Copy every field of connection `src` into connection `dst` (both environments).
# Used to mirror a freshly authenticated connection into the package default so
# that conn-less calls track the most recent s160_api_auth().
copy_connection <- function(src, dst) {
  for (k in ls(src, all.names = TRUE)) {
    assign(k, get(k, envir = src), envir = dst)
  }
  invisible(dst)
}

# Resolve an environment's API key from an ordered list of candidate env vars
# (e.g. prod tries S160_PROD_API_KEY then falls back to S160_API_KEY). When none
# is set, defer to get_credential on the canonical variable so the first-run
# interactive prompt + save still works (and a clear error in non-interactive
# mode).
resolve_env_api_key <- function(candidates, prompt_var) {
  for (v in candidates) {
    val <- Sys.getenv(v)
    if (nzchar(trimws(val))) return(val)
  }
  get_credential(prompt_var,
                 sprintf("Enter your Survey160 API key (%s).", prompt_var),
                 secret = TRUE)
}

# Authenticated HTTP request with auto JWT refresh, against a given connection.
s160_api_request <- function(method, path, body = NULL, conn = NULL) {
  conn <- conn %||% .s160_api_env
  check_api_ready(conn)

  # Re-auth if JWT is older than 8 minutes, reusing the credentials stored on
  # this connection (set by api_do_auth) rather than re-reading the
  # environment. A connection authenticated via explicit params -- e.g. a
  # staging connection while the default env points at prod -- must refresh
  # against its own credentials, not the global S160_API_* vars.
  elapsed <- as.numeric(difftime(Sys.time(), conn$auth_time, units = "secs"))
  if (elapsed > 480) {
    api_do_auth(conn, conn$base_url, conn$userid, conn$api_key)
  }

  url <- paste0(conn$base_url, path)
  auth_header <- httr::add_headers(Authorization = conn$jwt)
  describe <- sprintf("%s %s", method, path)

  if (method == "GET") {
    resp <- http_send(
      function() httr::GET(url, auth_header, httr::timeout(.http_timeout_seconds)),
      describe = describe
    )
  } else {
    resp <- http_send(
      function() {
        httr::POST(
          url, auth_header,
          httr::content_type_json(),
          body = body, encode = "json",
          httr::timeout(.http_timeout_seconds)
        )
      },
      describe = describe
    )
  }

  if (httr::http_error(resp)) {
    stop(sprintf("API error (%s %s): %s", method, path,
                 http_error_message(resp)), call. = FALSE)
  }

  httr::content(resp, as = "parsed")
}

# --- Exported functions -------------------------------------------------------

#' Authenticate to a Survey160 environment and return a connection
#'
#' One entry point, addressed by environment \emph{name} so the base URL, the
#' campaign-results GCS bucket, and the API key are resolved together and cannot
#' be mismatched. It authenticates and returns a \emph{connection} -- an opaque
#' handle holding the JWT, credentials, base URL, environment name, and paired
#' bucket. How you use the return value gives single- or multi-environment
#' behaviour from the same call:
#'
#' \itemize{
#'   \item \strong{Single environment}: ignore the return value. The call also
#'     refreshes the package's default connection, so subsequent
#'     \code{\link{s160_api_campaign_results}} / \code{\link{s160_api_campaign_get}}
#'     calls with no \code{conn} use it. \code{s160_api_auth(); df <-
#'     s160_api_campaign_results(744)}.
#'   \item \strong{Both environments at once}: capture each connection and pass
#'     it as \code{conn =}. \code{prod <- s160_api_auth("prod"); stg <-
#'     s160_api_auth("staging")}, then \code{s160_api_campaign_results(744, conn
#'     = stg)}. Each connection is independent, so prod and staging can be held
#'     live in the same session -- e.g. to compare a campaign across both.
#' }
#'
#' Credentials come from \code{~/.Renviron} (never typed into code): the user ID
#' from \code{S160_API_USERID}, and the API key from a per-environment variable
#' -- \code{S160_STAGING_API_KEY} for staging, and \code{S160_PROD_API_KEY} (or,
#' as a fallback, the legacy \code{S160_API_KEY}) for prod. Missing values prompt
#' once on an interactive run and are saved.
#'
#' The in-session JWT refresh (tokens expire after 10 minutes; re-auth at 8)
#' reuses the credentials stored on the connection, so a staging connection held
#' alongside prod keeps refreshing against staging rather than the default.
#'
#' @param env Environment name: \code{"prod"} (default) or \code{"staging"}.
#' @return A connection object (an environment) to pass as \code{conn}, returned
#'   invisibly. As a side effect, the package's default connection is updated to
#'   this one so conn-less calls use the most recent authentication.
#' @examples
#' \dontrun{
#' # Single environment -- ignore the return, use the default connection:
#' s160_api_auth()                     # defaults to prod
#' df <- s160_api_campaign_results(744)
#'
#' # Both environments at once -- capture each, pass conn =:
#' s160_gcs_init(bucket = "campaign_results")  # one GCS auth covers all buckets
#' prod <- s160_api_auth("prod")
#' stg  <- s160_api_auth("staging")
#' df_prod <- s160_api_campaign_results(744, conn = prod)
#' df_stg  <- s160_api_campaign_results(744, conn = stg)
#' }
#' @importFrom httr GET POST add_headers content_type_json content http_error http_status
#' @export
s160_api_auth <- function(env = c("prod", "staging")) {
  env <- match.arg(env)
  cfg <- list(
    prod = list(
      url = "https://api.survey160.com", bucket = "campaign_results",
      key_candidates = c("S160_PROD_API_KEY", "S160_API_KEY"),
      key_prompt = "S160_PROD_API_KEY"
    ),
    staging = list(
      url = "https://staging-api.survey160.com", bucket = "campaign_results_staging",
      key_candidates = "S160_STAGING_API_KEY", key_prompt = "S160_STAGING_API_KEY"
    )
  )[[env]]

  userid <- get_credential(
    "S160_API_USERID",
    "Enter your Survey160 API user ID (ask your survey manager)."
  )
  api_key <- resolve_env_api_key(cfg$key_candidates, cfg$key_prompt)

  # Build an independent connection so prod and staging can coexist in one
  # session, then mirror it into the package default so conn-less calls track
  # the most recent authentication.
  conn <- new.env(parent = emptyenv())
  conn$env <- env
  conn$bucket <- cfg$bucket
  api_do_auth(conn, cfg$url, userid, api_key)
  # Class the handle so it prints as an opaque connection (masking the key)
  # rather than a bare <environment>. The mirrored default stays unclassed.
  class(conn) <- c("s160_api_conn", "environment")
  copy_connection(conn, .s160_api_env)

  message(sprintf("API authenticated (%s).", env))
  invisible(conn)
}

#' @export
print.s160_api_conn <- function(x, ...) {
  cat(sprintf("<survey160 API connection: %s -> %s>\n",
              x$env %||% "?", x$base_url %||% "?"))
  cat("  credentials: present (hidden)\n")
  if (!is.null(x$bucket)) cat(sprintf("  bucket: %s\n", x$bucket))
  invisible(x)
}

#' Download campaign results via API
#'
#' Triggers a fresh campaign results export, polls GCS until the file is
#' updated, and returns the results as a data frame. Requires both API auth
#' (\code{s160_api_auth}) and GCS auth (\code{s160_gcs_init}).
#'
#' @param campaign_id Campaign ID (numeric or character).
#' @param filter_open Logical. Exclude open/uncontacted conversations?
#'   Default \code{FALSE}.
#' @param timeout Timeout in seconds for export completion. Default 300.
#' @param poll_interval Maximum polling interval in seconds. Default 5.
#'   Polling uses exponential backoff starting at the smaller of 2s and
#'   this value, capped at this value.
#' @param destdir Directory to save the downloaded CSV. \code{NULL} (default)
#'   uses a temporary file.
#' @param conn Connection to use. Defaults to the package's default connection
#'   (the most recent \code{\link{s160_api_auth}}). Pass a connection returned by
#'   \code{\link{s160_api_auth}} to target a specific environment; the export
#'   trigger, the completion poll, and the CSV read all use that connection's
#'   environment and paired GCS bucket.
#' @param ... Additional arguments passed to \code{read.csv()}.
#' @return A data frame with one row per survey response.
#' @examples
#' \dontrun{
#' s160_gcs_init(bucket = "campaign_results")
#' s160_api_auth()
#' df <- s160_api_campaign_results(1980)
#' df <- s160_api_campaign_results(1980, filter_open = TRUE, timeout = 600)
#'
#' # Compare the same campaign across two environments concurrently:
#' prod <- s160_api_auth("prod")
#' stg  <- s160_api_auth("staging")
#' df_prod <- s160_api_campaign_results(744, conn = prod)
#' df_stg  <- s160_api_campaign_results(744, conn = stg)
#' }
#' @importFrom googleCloudStorageR gcs_list_objects
#' @export
s160_api_campaign_results <- function(campaign_id, filter_open = FALSE,
                                      timeout = 300, poll_interval = 5,
                                      destdir = NULL, conn = NULL,
                                      ...) {
  conn <- conn %||% .s160_api_env
  check_api_ready(conn)
  check_gcs_ready()
  campaign_id <- validate_campaign_id(campaign_id)

  check_positive_number(timeout, "timeout", fn = "s160_api_campaign_results")
  check_positive_number(poll_interval, "poll_interval",
                        fn = "s160_api_campaign_results")

  export_filename <- paste0(campaign_id, "_raw_data_download.csv")

  # The export trigger, the completion poll, and the read all target this
  # connection's GCS bucket, which s160_api_auth() pairs with the environment.
  # `bucket` is only NULL for a connection not built by s160_api_auth() (a
  # hand-constructed env, or a test seed); that path falls back to the global
  # bucket set by s160_gcs_init().
  bucket <- conn$bucket
  poll_updated <- function() {
    if (is.null(bucket)) {
      get_gcs_file_updated(campaign_id, export_filename)
    } else {
      get_gcs_file_updated(campaign_id, export_filename, bucket = bucket)
    }
  }

  # Step 1: Get baseline GCS timestamp
  baseline_updated <- poll_updated()

  # Step 2: Trigger export
  s160_api_request("POST", "/startCampaignResultsExport", body = list(
    campaignid = as.integer(campaign_id),
    userid = conn$userid,
    filterOpen = filter_open
  ), conn = conn)
  message("Export triggered. Polling GCS for completion...")

  # Step 3: Poll GCS until timestamp changes
  elapsed <- 0
  interval <- min(2, poll_interval)
  while (elapsed < timeout) {
    Sys.sleep(interval)
    elapsed <- elapsed + interval

    current_updated <- poll_updated()
    if (!is.null(current_updated) &&
          (is.null(baseline_updated) || current_updated != baseline_updated)) {
      message("Export complete.")
      return(s160_gcs_campaign_results_read(campaign_id, destdir = destdir, # nolint object_usage_linter
                                            bucket = bucket, ...))
    }

    interval <- min(interval * 2, poll_interval)
    message(sprintf("  Waiting... (%ds elapsed)", as.integer(elapsed)))
  }

  stop_s160(sprintf("Export timed out after %g seconds.", timeout),
            fn = "s160_api_campaign_results")
}

#' Read a single campaign's attributes
#'
#' Wraps the Survey160 API endpoint \code{GET /campaigns/<campaign_id>}, which
#' returns every column on the \code{campaigns} table for one campaign. Useful
#' for confirming attributes after a state-changing call (for example, reading
#' \code{archive_scheduled_date} after scheduling an archive) without dropping
#' to direct database access.
#'
#' Enriched, API-only fields returned by the endpoint
#' (\code{listlength}, \code{list}, \code{login}, \code{exports},
#' \code{has_texting_started}, \code{sandbox_configuration}, \code{aggregator},
#' \code{has_assigned_registration}) are dropped; the result mirrors the
#' \code{campaigns} table only. JSON-valued columns (\code{script}, \code{prompt},
#' \code{quotas}, ...) come back as length-1 list-columns holding the parsed
#' structure.
#'
#' The endpoint runs several server-side subqueries on each call; this is a
#' per-campaign read, not appropriate for tight loops over hundreds of IDs.
#' A batch variant would need a backend extension and is out of scope.
#'
#' @param campaign_id Campaign ID (numeric or character).
#' @param conn Connection to use. Defaults to the package's default connection
#'   (the most recent \code{\link{s160_api_auth}}). Pass a connection returned by
#'   \code{\link{s160_api_auth}} to target a specific environment.
#' @return A single-row data frame. Scalar columns are scalar; ISO-8601
#'   timestamp columns are coerced to \code{POSIXct} in UTC; JSON columns
#'   are list-columns of length 1.
#' @examples
#' \dontrun{
#' s160_api_auth()
#' info <- s160_api_campaign_get(2107)
#' info$active
#' info$script[[1]]  # parsed JSON
#' }
#' @export
s160_api_campaign_get <- function(campaign_id, conn = NULL) {
  conn <- conn %||% .s160_api_env
  check_api_ready(conn)
  campaign_id <- validate_campaign_id(campaign_id)

  path <- paste0("/campaigns/", campaign_id)
  resp <- tryCatch(
    s160_api_request("GET", path, conn = conn),
    error = function(e) {
      msg <- conditionMessage(e)
      # The endpoint returns HTTP 400 with {"success":"false"} when no row is
      # found, which surfaces as "Bad Request" through s160_api_request. Map
      # that (and a real 404, defensively) to a clear not-found error.
      if (grepl("Bad Request|Not Found", msg, ignore.case = TRUE)) {
        stop_not_found("campaign", campaign_id, fn = "s160_api_campaign_get")
      }
      stop(e)
    }
  )

  # Backstop for an unexpected 200 with `success != TRUE` or no data payload.
  # The real not-found path is the HTTP 400 handled by the tryCatch above; this
  # guard would only fire if the server changed shape -- a malformed read, not a
  # missing campaign, so it reports a failed read rather than "not found".
  if (!isTRUE(resp$success) || is.null(resp$data)) {
    stop_failed("read campaign", "unexpected response format",
                fn = "s160_api_campaign_get")
  }

  enriched_fields <- c(
    "listlength", "list", "login", "exports",
    "has_texting_started", "sandbox_configuration",
    "aggregator", "has_assigned_registration"
  )
  fields <- resp$data[!names(resp$data) %in% enriched_fields]

  # Scalars stay scalar; everything else (parsed JSON, multi-element vectors)
  # becomes a length-1 list-column so the result is always a single-row frame.
  cols <- lapply(fields, function(v) {
    if (is.null(v)) NA
    else if (is.list(v) || length(v) > 1L) I(list(v))
    else v
  })

  df <- as.data.frame(cols, stringsAsFactors = FALSE)

  # Coerce ISO-8601-looking character columns to POSIXct (UTC). The campaigns
  # table has many timestamp columns (startdate, enddate, archive_scheduled_date,
  # ...) which travel the wire as naive strings; parsing here saves every
  # caller from doing it. Falls back to the original string if parsing fails.
  parse_iso <- function(col) {
    if (!is.character(col) || length(col) != 1L || is.na(col)) return(col)
    if (!grepl("^\\d{4}-\\d{2}-\\d{2}[T ]\\d{2}:\\d{2}:\\d{2}", col)) return(col)
    # Normalize to "YYYY-MM-DD HH:MM:SS": drop the T separator, drop any
    # sub-second precision (PostgreSQL can emit `.123456`), drop a Z suffix
    # or numeric UTC offset. The wire format is always UTC.
    s <- sub("T", " ", col)
    s <- sub("\\.\\d+", "", s)
    s <- sub("Z$", "", s)
    s <- sub("[+-]\\d{2}:?\\d{2}$", "", s)
    parsed <- suppressWarnings(
      as.POSIXct(s, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
    )
    if (is.na(parsed)) col else parsed
  }
  df[] <- lapply(df, parse_iso)

  df
}

# --- Internal helpers ---------------------------------------------------------

# Get the GCS `updated` timestamp for a specific export file.
# Returns NULL if the file does not exist. `bucket` defaults to NULL, which
# lists against the global bucket (set by s160_gcs_init); a connection with a
# paired bucket passes it explicitly so the poll targets the same environment
# as the export it triggered.
get_gcs_file_updated <- function(campaign_id, filename, bucket = NULL) {
  prefix <- paste0(campaign_id, "/")
  objects <- tryCatch(
    if (is.null(bucket)) {
      gcs_list_objects(prefix = prefix)
    } else {
      gcs_list_objects(prefix = prefix, bucket = bucket)
    },
    error = function(e) NULL
  )

  if (is.null(objects) || nrow(objects) == 0) return(NULL)

  target <- paste0(prefix, filename)
  match_idx <- which(objects$name == target)
  if (length(match_idx) == 0) return(NULL)

  objects$updated[match_idx[1]]
}
