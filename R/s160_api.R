# Survey160 API client -- trigger exports and poll for completion
#
# Auth strategy: API key-based service account authentication.
#
# Credentials (S160_API_USERID, S160_API_KEY) are read from ~/.Renviron.
# On first interactive run, the user is prompted and values are saved
# automatically.
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

# Authenticated HTTP request with auto JWT refresh
s160_api_request <- function(method, path, body = NULL) {
  check_api_ready()

  # Re-auth if JWT is older than 8 minutes
  elapsed <- as.numeric(difftime(Sys.time(), .s160_api_env$auth_time, units = "secs"))
  if (elapsed > 480) {
    s160_api_auth(base_url = .s160_api_env$base_url)
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
#' Reads service account credentials (\code{S160_API_USERID} and
#' \code{S160_API_KEY}) from \code{~/.Renviron}. On first interactive run,
#' prompts for both values and saves them automatically.
#'
#' @param base_url API base URL. Defaults to
#'   \code{"https://api.survey160.com"}.
#' @return Invisible NULL. Stores JWT as side effect.
#' @examples
#' \dontrun{
#' s160_api_auth()
#' }
#' @importFrom httr GET POST add_headers content_type_json content http_error http_status
#' @export
s160_api_auth <- function(base_url = "https://api.survey160.com") {
  if (!is.character(base_url) || length(base_url) != 1 || !nzchar(trimws(base_url))) {
    stop("base_url must be a non-empty string.", call. = FALSE)
  }

  userid <- get_credential(
    "S160_API_USERID",
    "Enter your Survey160 API user ID (ask your survey manager)."
  )
  api_key <- get_credential(
    "S160_API_KEY",
    "Enter your Survey160 API key (ask your survey manager).",
    secret = TRUE
  )

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
#' s160_api_auth()
#' df <- s160_api_campaign_results(1980)
#' df <- s160_api_campaign_results(1980, filter_open = TRUE, timeout = 600)
#' }
#' @importFrom googleCloudStorageR gcs_list_objects
#' @export
s160_api_campaign_results <- function(campaign_id, filter_open = FALSE,
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
      return(s160_gcs_campaign_results_read(campaign_id, destdir = destdir, ...)) # nolint object_usage_linter
    }

    interval <- min(interval * 2, poll_interval)
    message(sprintf("  Waiting... (%ds elapsed)", as.integer(elapsed)))
  }

  stop(sprintf("Export timed out after %g seconds.", timeout), call. = FALSE)
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
s160_api_campaign_get <- function(campaign_id) {
  check_api_ready()
  campaign_id <- validate_campaign_id(campaign_id)

  path <- paste0("/campaigns/", campaign_id)
  resp <- tryCatch(
    s160_api_request("GET", path),
    error = function(e) {
      msg <- conditionMessage(e)
      # The endpoint returns HTTP 400 with {"success":"false"} when no row is
      # found, which surfaces as "Bad Request" through s160_api_request. Map
      # that (and a real 404, defensively) to a clear not-found error.
      if (grepl("Bad Request|Not Found", msg, ignore.case = TRUE)) {
        stop(sprintf("Campaign %s not found.", campaign_id), call. = FALSE)
      }
      stop(e)
    }
  )

  # Backstop for an unexpected 200 with `success != TRUE` or no data payload.
  # The real not-found path is the HTTP 400 handled by the tryCatch above; this
  # guard would only fire if the server changed shape.
  if (!isTRUE(resp$success) || is.null(resp$data)) {
    stop(sprintf("Campaign %s not found.", campaign_id), call. = FALSE)
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
