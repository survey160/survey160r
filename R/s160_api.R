# Survey160 API client -- trigger exports and poll for completion
#
# Auth strategy: API key-based service account authentication.
#
# An admin creates a service account user and API key via the Survey160
# admin UI or API.  The R user only needs the userid and raw API key.
# s160_api_auth() exchanges these for a JWT stored in memory.
#
# JWT refresh: Access tokens expire after 10 minutes. Rather than
# implementing a refresh token flow, we re-authenticate with the stored
# API key when the JWT is older than 8 minutes.

# --- Internal state -----------------------------------------------------------

.s160_api_env <- new.env(parent = emptyenv())

check_api_ready <- function() {
  if (is.null(.s160_api_env$jwt) || .s160_api_env$jwt == "") {
    stop("API not initialized. Run s160_api_auth() first.", call. = FALSE)
  }
}

# Authenticated HTTP request with auto JWT refresh
s160_api_request <- function(method, path, body = NULL) {
  check_api_ready()

  # Re-auth if JWT is older than 8 minutes
  elapsed <- as.numeric(difftime(Sys.time(), .s160_api_env$auth_time, units = "secs"))
  if (elapsed > 480) {
    s160_api_auth(
      .s160_api_env$userid,
      .s160_api_env$api_key,
      .s160_api_env$base_url
    )
  }

  url <- paste0(.s160_api_env$base_url, path)
  auth_header <- httr::add_headers(Authorization = .s160_api_env$jwt)

  if (method == "GET") {
    resp <- httr::GET(url, auth_header)
  } else {
    resp <- httr::POST(
      url, auth_header,
      httr::content_type_json(),
      body = body, encode = "json"
    )
  }

  if (httr::http_error(resp)) {
    msg <- tryCatch(
      httr::content(resp, as = "parsed")$error,
      error = function(e) NULL
    )
    if (is.null(msg) || !nzchar(msg)) msg <- httr::http_status(resp)$message
    stop(sprintf("API error (%s %s): %s", method, path, msg), call. = FALSE)
  }

  httr::content(resp, as = "parsed")
}

# --- Exported functions -------------------------------------------------------

#' Authenticate to the Survey160 API
#'
#' Exchanges a service account API key for a JWT token. The token is stored
#' in memory for subsequent API calls. Get your userid and API key from your
#' survey manager.
#'
#' @param userid Service account user ID.
#' @param api_key Raw API key string.
#' @param base_url API base URL (e.g. \code{"https://qa-api.survey160.com"}).
#' @return Invisible NULL. Stores JWT as side effect.
#' @examples
#' \dontrun{
#' s160_api_auth("svc-analytics", "abc123...", "https://qa-api.survey160.com")
#' }
#' @importFrom httr POST add_headers content_type_json content http_error http_status
#' @export
s160_api_auth <- function(userid, api_key, base_url) {
  if (missing(userid) || missing(api_key) || missing(base_url)) {
    stop("userid, api_key, and base_url are all required.", call. = FALSE)
  }
  if (!is.character(userid) || length(userid) != 1 || !nzchar(trimws(userid))) {
    stop("userid must be a non-empty string.", call. = FALSE)
  }
  if (!is.character(api_key) || length(api_key) != 1 || !nzchar(trimws(api_key))) {
    stop("api_key must be a non-empty string.", call. = FALSE)
  }
  if (!is.character(base_url) || length(base_url) != 1 || !nzchar(trimws(base_url))) {
    stop("base_url must be a non-empty string.", call. = FALSE)
  }

  # Normalize inputs
  userid <- trimws(userid)
  api_key <- trimws(api_key)
  base_url <- sub("/$", "", trimws(base_url))

  url <- paste0(base_url, "/auth/serviceAccount")
  resp <- httr::POST(
    url,
    httr::add_headers(Authorization = paste("ApiKey", api_key)),
    httr::content_type_json(),
    body = list(userid = userid),
    encode = "json"
  )

  if (httr::http_error(resp)) {
    msg <- tryCatch(
      httr::content(resp, as = "parsed")$error,
      error = function(e) NULL
    )
    if (is.null(msg) || !nzchar(msg)) msg <- httr::http_status(resp)$message
    stop(sprintf("Authentication failed: %s", msg), call. = FALSE)
  }

  parsed <- httr::content(resp, as = "parsed")
  if (!isTRUE(parsed$success) || is.null(parsed$data)) {
    stop("Authentication failed: unexpected response format.", call. = FALSE)
  }

  .s160_api_env$jwt <- parsed$data
  .s160_api_env$userid <- userid
  .s160_api_env$api_key <- api_key
  .s160_api_env$base_url <- base_url
  .s160_api_env$auth_time <- Sys.time()

  message("API authenticated.")
  invisible(NULL)
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
#' @param ... Additional arguments passed to \code{read.csv()}.
#' @return A data frame with one row per survey response.
#' @examples
#' \dontrun{
#' s160_gcs_init(bucket = "campaign_results")
#' s160_api_auth("svc-analytics", "key...", "https://qa-api.survey160.com")
#' df <- s160_api_results(1980)
#' df <- s160_api_results(1980, filter_open = TRUE, timeout = 600)
#' }
#' @importFrom googleCloudStorageR gcs_list_objects
#' @export
s160_api_results <- function(campaign_id, filter_open = FALSE,
                             timeout = 300, poll_interval = 5,
                             destdir = NULL, ...) {
  check_api_ready()
  check_gcs_ready()
  campaign_id <- validate_campaign_id(campaign_id)

  if (!is.numeric(timeout) || length(timeout) != 1 || timeout <= 0) {
    stop("timeout must be a positive number.", call. = FALSE)
  }
  if (!is.numeric(poll_interval) || length(poll_interval) != 1 ||
        poll_interval <= 0) {
    stop("poll_interval must be a positive number.", call. = FALSE)
  }

  export_filename <- paste0(campaign_id, "_raw_data_download.csv")

  # Step 1: Get baseline GCS timestamp
  baseline_updated <- get_gcs_file_updated(campaign_id, export_filename)

  # Step 2: Trigger export
  s160_api_request("POST", "/startCampaignResultsExport", body = list(
    campaignid = as.integer(campaign_id),
    userid = .s160_api_env$userid,
    filterOpen = filter_open
  ))
  message("Export triggered. Polling GCS for completion...")

  # Step 3: Poll GCS until timestamp changes
  elapsed <- 0
  interval <- min(2, poll_interval)
  while (elapsed < timeout) {
    Sys.sleep(interval)
    elapsed <- elapsed + interval

    current_updated <- get_gcs_file_updated(campaign_id, export_filename)
    if (!is.null(current_updated) &&
          (is.null(baseline_updated) || current_updated != baseline_updated)) {
      message("Export complete.")
      return(s160_gcs_results_read(campaign_id, destdir = destdir, ...))
    }

    interval <- min(interval * 2, poll_interval)
    message(sprintf("  Waiting... (%ds elapsed)", as.integer(elapsed)))
  }

  stop(sprintf("Export timed out after %g seconds.", timeout), call. = FALSE)
}

# --- Internal helpers ---------------------------------------------------------

# Get the GCS `updated` timestamp for a specific export file.
# Returns NULL if the file does not exist.
get_gcs_file_updated <- function(campaign_id, filename) {
  prefix <- paste0(campaign_id, "/")
  objects <- tryCatch(
    gcs_list_objects(prefix = prefix),
    error = function(e) NULL
  )

  if (is.null(objects) || nrow(objects) == 0) return(NULL)

  target <- paste0(prefix, filename)
  match_idx <- which(objects$name == target)
  if (length(match_idx) == 0) return(NULL)

  objects$updated[match_idx[1]]
}
