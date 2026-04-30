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
#' @importFrom httr POST add_headers content_type_json content http_error http_status
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

#' Batch-schedule campaigns for archiving
#'
#' For each campaign ID, sets \code{archive_scheduled_date} via
#' \code{POST /campaigns/<id>}. The Survey160 Celery beat task
#' \code{mytasks.archiveCampaigns} runs nightly and archives any campaign
#' whose scheduled date has passed (campaign must already be deactivated).
#'
#' Each campaign is sent as a separate request; failures are collected and
#' reported per-campaign rather than aborting the batch.
#'
#' @param campaign_ids Vector of campaign IDs (numeric or character).
#' @param archive_date Date or \code{"YYYY-MM-DD"} string. Defaults to
#'   today (\code{Sys.Date()}). Sent as UTC midnight in the API's expected
#'   ISO format.
#' @return A data frame with columns \code{campaign_id}, \code{success},
#'   \code{message}.
#' @examples
#' \dontrun{
#' s160_api_auth()
#' s160_api_batch_archive_campaigns(c(1980, 1981, 1982), "2026-05-15")
#' }
#' @export
s160_api_batch_archive_campaigns <- function(campaign_ids,
                                             archive_date = Sys.Date()) {
  check_api_ready()

  if (length(campaign_ids) == 0) {
    stop("campaign_ids must contain at least one ID.", call. = FALSE)
  }

  if (inherits(archive_date, "Date") && length(archive_date) == 1 &&
        !is.na(archive_date)) {
    date_str <- format(archive_date, "%Y-%m-%d")
  } else if (is.character(archive_date) && length(archive_date) == 1 &&
               !is.na(archive_date) && nzchar(archive_date)) {
    parsed <- as.Date(archive_date, format = "%Y-%m-%d")
    if (is.na(parsed)) {
      stop("archive_date must be a Date or a 'YYYY-MM-DD' string.",
           call. = FALSE)
    }
    date_str <- format(parsed, "%Y-%m-%d")
  } else {
    stop("archive_date must be a single Date or 'YYYY-MM-DD' string.",
         call. = FALSE)
  }

  archive_iso <- paste0(date_str, "T00:00:00.000Z")

  results <- lapply(campaign_ids, function(cid) {
    cid_validated <- tryCatch(validate_campaign_id(cid),
                              error = function(e) {
                                structure(conditionMessage(e),
                                          class = "validation_error")
                              })
    if (inherits(cid_validated, "validation_error")) {
      return(list(campaign_id = as.character(cid), success = FALSE,
                  message = as.character(cid_validated)))
    }

    path <- paste0("/campaigns/", cid_validated)
    body <- list(ncd = list(archive_scheduled_date = archive_iso))

    parsed <- tryCatch(
      s160_api_request("POST", path, body = body),
      error = function(e) {
        list(success = FALSE, message = conditionMessage(e))
      }
    )

    success <- isTRUE(parsed$success)
    msg <- if (!is.null(parsed$message)) as.character(parsed$message) else ""
    list(campaign_id = cid_validated, success = success, message = msg)
  })

  data.frame(
    campaign_id = vapply(results, `[[`, character(1), "campaign_id"),
    success = vapply(results, `[[`, logical(1), "success"),
    message = vapply(results, `[[`, character(1), "message"),
    stringsAsFactors = FALSE
  )
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
