# GCS authentication and data access for Survey160
#
# Auth strategy: Browser OAuth via the project's own GCP OAuth client.
# Client ID and secret are read from S160_GCS_CLIENT_ID / S160_GCS_CLIENT_SECRET
# env vars (set in .Renviron). Tokens are cached between sessions so the
# browser prompt only appears on first use or when the token expires.
#
# Bucket is read from S160_RESULTS_BUCKET env var (set in .Renviron).

# --- Internal helpers --------------------------------------------------------

# Stop with a clear message if GCS is not initialized
check_gcs_ready <- function() {
  bucket <- tryCatch(gcs_get_global_bucket(), error = function(e) NULL)
  if (is.null(bucket) || bucket == "") {
    stop("GCS not initialized. Run s160_gcs_init() first.", call. = FALSE)
  }
}

# Validate campaign_id is a non-empty scalar
validate_campaign_id <- function(campaign_id) {
  if (length(campaign_id) != 1) {
    stop("campaign_id must be a single value, not a vector.", call. = FALSE)
  }
  campaign_id <- as.character(campaign_id)
  if (is.na(campaign_id) || !nzchar(trimws(campaign_id))) {
    stop("campaign_id must be a non-empty scalar value.", call. = FALSE)
  }
  campaign_id
}

# --- Exported functions ------------------------------------------------------

#' Initialize GCS connection
#'
#' Authenticates to GCS using the project's OAuth client and sets the global
#' bucket. Opens a browser for Google sign-in on first run; subsequent runs
#' use the cached token automatically. Tokens are cached in
#' \code{~/.config/gargle/} by default.
#'
#' The authenticated Google account needs Storage Object Viewer permission
#' on the target bucket.
#'
#' @param bucket GCS bucket name. Defaults to S160_RESULTS_BUCKET env var,
#'   falling back to "campaign_results_qa".
#' @return Invisible NULL. Sets global bucket as side effect.
#' @examples
#' \dontrun{
#' s160_gcs_init()
#' s160_gcs_init(bucket = "campaign_results")
#' }
#' @importFrom googleCloudStorageR gcs_auth gcs_global_bucket
#' @export
s160_gcs_init <- function(bucket = Sys.getenv("S160_RESULTS_BUCKET", "campaign_results_qa")) {
  if (bucket == "") {
    bucket <- "campaign_results_qa"
  }

  # Read project OAuth client from env vars
  client_id <- Sys.getenv("S160_GCS_CLIENT_ID")
  client_secret <- Sys.getenv("S160_GCS_CLIENT_SECRET")

  if (client_id == "" || client_secret == "") {
    stop(
      "S160_GCS_CLIENT_ID and S160_GCS_CLIENT_SECRET must be set in .Renviron.\n",
      "See ?s160_gcs_init for details, then restart your R session.",
      call. = FALSE
    )
  }

  # Configure googleAuthR to use the project's OAuth client
  options(
    googleAuthR.client_id = client_id,
    googleAuthR.client_secret = client_secret
  )

  # Browser OAuth via gargle -- opens Google sign-in page on first run.
  # email = TRUE tells gargle to reuse the cached email on subsequent runs.
  gcs_auth(email = TRUE)
  message("Authenticated via browser OAuth (project client)")

  gcs_global_bucket(bucket)
  message(sprintf("GCS ready. Bucket: %s", bucket))
  invisible(NULL)
}

#' Read campaign results CSV from GCS into a data frame
#'
#' Downloads the CSV from GCS and reads it into R. By default, the file is
#' downloaded to a temporary location and cleaned up automatically. Set
#' \code{destdir} to keep a local copy.
#'
#' GCS path: \code{gs://<bucket>/<campaign_id>/<filename>}
#'
#' @param campaign_id Campaign ID (numeric or character). Must be a single value.
#' @param filename File name in the campaign folder. Defaults to
#'   \code{<campaign_id>_raw_data_download.csv} (the standard export filename).
#' @param destdir Directory to save the downloaded file. When \code{NULL}
#'   (default), a temporary file is used and cleaned up automatically. Use
#'   \code{"."} for the current directory.
#' @param ... Additional arguments passed to \code{read.csv()}, e.g.
#'   \code{stringsAsFactors}, \code{na.strings}, \code{nrows}.
#' @return A data frame with one row per survey response.
#' @examples
#' \dontrun{
#' s160_gcs_init()
#' df <- s160_gcs_results_read(1980)
#' df <- s160_gcs_results_read(1980, destdir = ".")
#' df <- s160_gcs_results_read(1980, destdir = "~/data")
#' }
#' @importFrom googleCloudStorageR gcs_get_object gcs_list_objects gcs_get_global_bucket
#' @importFrom utils read.csv
#' @export
s160_gcs_results_read <- function(campaign_id, filename = NULL, destdir = NULL, ...) {
  check_gcs_ready()
  campaign_id <- validate_campaign_id(campaign_id)

  if (is.null(filename)) {
    filename <- paste0(campaign_id, "_raw_data_download.csv")
  }
  if (filename != basename(filename)) {
    stop("filename must not contain path separators.", call. = FALSE)
  }
  object_name <- paste0(campaign_id, "/", filename)

  bucket <- gcs_get_global_bucket()
  gcs_path <- sprintf("gs://%s/%s", bucket, object_name)
  message(sprintf("Reading: %s", gcs_path))

  if (is.null(destdir)) {
    local_path <- tempfile(fileext = ".csv")
    on.exit(unlink(local_path), add = TRUE)
  } else {
    destdir <- normalizePath(destdir, mustWork = TRUE)
    local_path <- file.path(destdir, filename)
  }

  tryCatch(
    gcs_get_object(object_name = object_name, saveToDisk = local_path, overwrite = TRUE),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("404", msg, fixed = TRUE)) {
        stop(sprintf("File not found: %s", gcs_path), call. = FALSE)
      }
      stop(sprintf("Failed to download %s: %s", gcs_path, msg), call. = FALSE)
    }
  )

  if (!is.null(destdir)) {
    message(sprintf("Saved to: %s", local_path))
  }

  read.csv(local_path, fileEncoding = "UTF-8", ...)
}

#' List files in a campaign's GCS folder
#'
#' Returns the file names inside a campaign's folder in the results bucket.
#' Returns \code{character(0)} with a message if the campaign has no files.
#'
#' @param campaign_id Campaign ID (numeric or character). Must be a single value.
#' @return Character vector of file names (without the campaign_id prefix).
#' @examples
#' \dontrun{
#' s160_gcs_init()
#' s160_gcs_results_files(1980)
#' }
#' @export
s160_gcs_results_files <- function(campaign_id) {
  check_gcs_ready()
  campaign_id <- validate_campaign_id(campaign_id)

  prefix <- paste0(campaign_id, "/")
  objects <- tryCatch(
    gcs_list_objects(prefix = prefix),
    error = function(e) {
      stop(sprintf("Failed to list files for campaign %s: %s", campaign_id, conditionMessage(e)), call. = FALSE)
    }
  )

  if (nrow(objects) == 0) {
    message(sprintf("No files found for campaign %s", campaign_id))
    return(character(0))
  }

  # Strip the prefix to return just filenames
  sub(prefix, "", objects$name, fixed = TRUE)
}

#' List all campaign IDs in the current bucket
#'
#' Returns a sorted character vector of campaign IDs (top-level folder names)
#' in the results bucket. Objects at the bucket root (not inside a folder) are
#' excluded.
#'
#' @return Character vector of campaign IDs, sorted.
#' @examples
#' \dontrun{
#' s160_gcs_init()
#' s160_gcs_results_list()
#' }
#' @export
s160_gcs_results_list <- function() {
  check_gcs_ready()
  objects <- tryCatch(
    gcs_list_objects(),
    error = function(e) {
      stop(sprintf("Failed to list campaigns: %s", conditionMessage(e)), call. = FALSE)
    }
  )

  if (nrow(objects) == 0) {
    message("No campaigns found in bucket")
    return(character(0))
  }

  # Extract unique top-level prefixes (campaign IDs), ignoring root-level objects
  folder_objects <- grep("/", objects$name, value = TRUE)
  campaign_ids <- unique(sub("/.*", "", folder_objects))
  sort(campaign_ids)
}
