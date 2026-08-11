# GCS authentication and data access for Survey160
#
# Auth strategy: Browser OAuth via a GCP "Desktop" OAuth client.
#
# The client ID is bundled in inst/oauth-client.json (public, not a secret).
# The client secret is read from S160_GCS_CLIENT_SECRET in ~/.Renviron.
# On first run, s160_gcs_init() prompts for the secret and saves it to
# ~/.Renviron so analysts only need to paste it once.
#
# Tokens are cached between sessions so the browser prompt only appears on
# first use or when the token expires.
#
# Bucket is passed as a required parameter to s160_gcs_init().

# --- Internal helpers --------------------------------------------------------

# Stop with a clear message if GCS is not initialized
check_gcs_ready <- function() {
  bucket <- tryCatch(gcs_get_global_bucket(), error = function(e) NULL)
  if (is.null(bucket) || bucket == "") {
    stop_not_initialized("GCS", "s160_gcs_init")
  }
}

# Resolve an explicit `bucket` arg, falling back to the global set by
# s160_gcs_init(). Returns a non-empty string; errors with a clear message
# when neither is available. Used by every reader so callers can either
# (a) call s160_gcs_init() once and let functions default, or (b) pass an
# explicit `bucket =` and skip the global state entirely.
resolve_bucket <- function(bucket = NULL) {
  if (!is.null(bucket)) {
    check_nonempty_string(bucket, "bucket")
    return(bucket)
  }
  resolved <- tryCatch(gcs_get_global_bucket(), error = function(e) NULL)
  if (is.null(resolved) || resolved == "") {
    stop(paste(
      "No GCS bucket available. Pass `bucket = \"...\"` explicitly, or",
      "call s160_gcs_init() to set a default for the session."
    ), call. = FALSE)
  }
  resolved
}

# Run a gcs_list_objects() call, converting any failure into a uniform
# "<fn>: Failed to <action>: <cause>" error. Returns the listing data frame.
gcs_list_or_stop <- function(action, fn, ...) {
  tryCatch(
    gcs_list_objects(...),
    error = function(e) stop_failed(action, conditionMessage(e), fn = fn)
  )
}

# Prompt for the client secret and persist it to ~/.Renviron
prompt_and_save_secret <- function() { # nocov start
  prompt_and_save_renviron( # nolint object_usage_linter
    "S160_GCS_CLIENT_SECRET",
    "First-time setup: paste the survey160r OAuth client secret (ask your team lead).",
    secret = TRUE
  )
} # nocov end

# Validate campaign_id is a non-empty scalar
validate_campaign_id <- function(campaign_id) {
  if (length(campaign_id) != 1) {
    stop("`campaign_id` must be a single value, not a vector.", call. = FALSE)
  }
  campaign_id <- as.character(campaign_id)
  if (is.na(campaign_id) || !nzchar(trimws(campaign_id))) {
    stop("`campaign_id` must be a non-empty scalar value.", call. = FALSE)
  }
  campaign_id
}

# Download a GCS object to disk with size verification and retry.
# Compares the local file size against GCS object metadata after download.
# Retries up to max_retries times on size mismatch with exponential backoff.
download_with_verify <- function(object_name, local_path, max_retries = 2L,
                                 bucket = NULL) {
  # Get expected size from GCS metadata. If listing fails (permissions or
  # transient error), fall back to downloading without verification.
  # googleCloudStorageR's gcs_list_objects() returns `size` as a formatted
  # string ("483.3 Kb"), so as.numeric() yields NA -- treat that as
  # "unknown size" and skip verification rather than crashing the compare.
  expected_size <- tryCatch({
    prefix <- sub("/[^/]+$", "/", object_name)
    objects <- if (is.null(bucket)) gcs_list_objects(prefix = prefix)
               else gcs_list_objects(prefix = prefix, bucket = bucket)
    size <- NULL
    if (nrow(objects) > 0) {
      match_idx <- which(objects$name == object_name)
      if (length(match_idx) > 0) {
        coerced <- suppressWarnings(as.numeric(objects$size[match_idx[1]]))
        if (!is.na(coerced)) size <- coerced
      }
    }
    size
  }, error = function(e) NULL)

  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    if (is.null(bucket)) {
      gcs_get_object(object_name = object_name, saveToDisk = local_path, overwrite = TRUE)
    } else {
      gcs_get_object(object_name = object_name, saveToDisk = local_path,
                     overwrite = TRUE, bucket = bucket)
    }

    if (is.null(expected_size)) break  # can't verify, trust the download

    actual_size <- file.info(local_path)$size
    if (!file.exists(local_path) || is.na(actual_size)) {
      stop(sprintf("Download produced no file for '%s'.", object_name), call. = FALSE)
    }
    if (actual_size == expected_size) break

    if (attempt > max_retries) {
      stop(sprintf(
        "Download incomplete after %d attempts. Expected %s bytes, got %s bytes.",
        attempt, format(expected_size, big.mark = ","),
        format(actual_size, big.mark = ",")
      ), call. = FALSE)
    }

    wait <- 2^(attempt - 1)
    message(sprintf(
      "Download size mismatch (expected %s, got %s). Retrying in %ds... (attempt %d/%d)",
      format(expected_size, big.mark = ","), format(actual_size, big.mark = ","),
      wait, attempt + 1L, max_retries + 1L
    ))
    Sys.sleep(wait)
  }

  invisible(local_path)
}

# Read just the raw (un-munged) header names of a CSV, without parsing the body.
# Uses fread when available, else read.csv; `encoding` is honoured in BOTH so a
# UTF-8/BOM file maps to the same names regardless of reader. Shared by
# fast_read_csv (to map a munged projection set back to file names) and
# s160_csv_header (which munges the result).
read_header_raw <- function(path, encoding = "UTF-8") {
  if (requireNamespace("data.table", quietly = TRUE)) {
    return(names(data.table::fread(path, nrows = 0L, check.names = FALSE,
                                   encoding = encoding, showProgress = FALSE)))
  }
  names(utils::read.csv(path, nrows = 0L, check.names = FALSE,
                        fileEncoding = encoding))
}

# Fast CSV reader. Uses data.table::fread when available (multithreaded, with
# native column projection via `select`), falling back to utils::read.csv.
#
# Both branches return a plain data.frame whose names are make.names()-munged
# (check.names = TRUE), because the latency algorithm assumes dot-form headers
# (id.<q>.scriptDate) -- the on-disk Survey160 export emits bracket form
# (id[<q>]scriptDate). Keeping the munge identical across readers is what makes
# the swap byte-for-byte safe (guarded by the parity test suites).
#
# Types are pinned to read.csv-like behaviour for interactive callers:
# stringsAsFactors = FALSE, integer64 = "character" (so large IDs come back as
# character, not a bit64::integer64 class that surprises View()/joins/==), and
# strip.white = FALSE (fread strips unquoted-field whitespace by default, which
# read.csv does not -- pinning it off keeps a value like " Yes" byte-identical
# so population-filter equality and dedupe keys do not silently shift).
# Callers can override any of these (and pass reader-specific args like `sep`,
# `na.strings`, `nrows`) through `...`.
#
# `columns`, when supplied, is a vector of *munged* (dot-form) names to keep.
# fread selects by raw header name, so we peek the header, map munged -> raw,
# and project at parse time. The read.csv fallback reads in full then subsets
# to the same set, so both readers return identical columns. If columns are
# requested but NONE match the header (a desync, e.g. a renamed export), we
# warn and read in full -- identical in both branches, and visible rather than
# a silent OOM-inducing full read.
fast_read_csv <- function(path, columns = NULL, encoding = "UTF-8", ...) {
  extra <- list(...)
  select_raw <- NULL
  munged_keep <- NULL
  if (!is.null(columns)) {
    raw <- read_header_raw(path, encoding = encoding)
    # make.names(unique = TRUE) reproduces what check.names = TRUE does on the
    # body read (both fread and read.csv), so the requested set, the selected
    # raw names, and the parsed column names stay aligned even when two raw
    # headers munge to the same syntactic name (e.g. id[q1]scriptDate vs
    # id.q1.scriptDate). s160_csv_header() munges identically, so `columns`
    # derived from it matches `munged` here.
    munged <- make.names(raw, unique = TRUE)
    keep <- munged %in% columns
    select_raw <- raw[keep]       # fread selects by raw (file) name
    munged_keep <- munged[keep]   # read.csv fallback subsets by munged name
    if (length(select_raw) == 0L) {
      warning(sprintf(paste0(
        "None of the %d requested column(s) matched the header ",
        "of '%s'; reading all columns."), length(columns), path), call. = FALSE)
      select_raw <- NULL
    }
  }
  if (requireNamespace("data.table", quietly = TRUE)) {
    args <- list(input = path, data.table = FALSE, check.names = TRUE,
                 stringsAsFactors = FALSE, integer64 = "character",
                 strip.white = FALSE, encoding = encoding, showProgress = FALSE)
    args[names(extra)] <- extra
    if (!is.null(select_raw)) args$select <- select_raw
    return(do.call(data.table::fread, args))
  }
  # Fallback: base read.csv (full read), then project to the same set. read.csv
  # munges names with the same unique make.names rule, so `munged_keep` matches
  # the parsed names; intersect guards against any residual divergence.
  args <- list(file = path, check.names = TRUE, stringsAsFactors = FALSE,
               fileEncoding = encoding)
  args[names(extra)] <- extra
  data <- do.call(utils::read.csv, args)
  if (!is.null(select_raw)) {
    data <- data[, intersect(munged_keep, names(data)), drop = FALSE]
  }
  data
}

# --- Exported functions ------------------------------------------------------

#' Initialize GCS connection
#'
#' Authenticates to GCS using the Survey160 Desktop OAuth client and sets
#' the global bucket.
#'
#' On first run, prompts for the client secret (get it from your team lead) and saves
#' it to \code{~/.Renviron}. Subsequent runs read it automatically. Also
#' opens a browser for Google sign-in on first use; the OAuth token is
#' cached in a platform-dependent directory (run
#' \code{gargle::gargle_oauth_sitrep()} to locate it).
#'
#' The authenticated Google account needs Storage Object Viewer permission
#' on the target bucket.
#'
#' @param bucket GCS bucket name (e.g. \code{"campaign_results"}).
#' @return Invisible NULL. Sets global bucket as side effect.
#' @examples
#' \dontrun{
#' s160_gcs_init(bucket = "campaign_results")
#' }
#' @importFrom googleCloudStorageR gcs_auth gcs_global_bucket
#' @export
s160_gcs_init <- function(bucket) {
  # Validate bucket
  if (missing(bucket)) {
    stop_s160(
      "`bucket` is required. Example: s160_gcs_init(bucket = \"campaign_results\")",
      fn = "s160_gcs_init"
    )
  }
  check_nonempty_string(bucket, "bucket", fn = "s160_gcs_init")

  # Client ID from bundled JSON (public, not a secret)
  client_json <- system.file("oauth-client.json", package = "survey160r")
  if (client_json == "") {
    stop_s160(
      "oauth-client.json not found. Is the survey160r package installed correctly?",
      fn = "s160_gcs_init"
    )
  }
  client_info <- jsonlite::fromJSON(client_json)
  client_id <- client_info$installed$client_id

  # Client secret from ~/.Renviron (prompted on first run)
  client_secret <- Sys.getenv("S160_GCS_CLIENT_SECRET")
  if (client_secret == "") {
    if (!interactive()) {
      stop_s160(
        paste0(
          "S160_GCS_CLIENT_SECRET not found in .Renviron.\n",
          "Run s160_gcs_init(bucket = \"campaign_results\") interactively to set it up, ",
          "or add S160_GCS_CLIENT_SECRET manually to ~/.Renviron."
        ),
        fn = "s160_gcs_init"
      )
    }
    client_secret <- prompt_and_save_secret()
  }

  options(
    googleAuthR.client_id = client_id,
    googleAuthR.client_secret = client_secret
  )

  # Browser OAuth via gargle -- opens Google sign-in page on first run.
  # email = TRUE tells gargle to reuse the cached email on subsequent runs.
  gcs_auth(email = TRUE)
  message("Authenticated via browser OAuth")

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
#'   Must not contain path separators.
#' @param destdir Directory to save the downloaded file. When \code{NULL}
#'   (default), a temporary file is used and cleaned up automatically. Use
#'   \code{"."} for the current directory.
#' @param bucket Source GCS bucket. \code{NULL} (default) falls back to the
#'   global bucket set by \code{s160_gcs_init()}.
#' @param columns Optional character vector of (dot-form) column names to keep,
#'   e.g. from \code{latency_input_columns()}. When set, only those columns are
#'   parsed (via \code{data.table::fread}'s column projection), cutting read
#'   time and memory on wide exports. \code{NULL} (default) reads every column.
#' @param ... Additional arguments forwarded to the CSV reader
#'   (\code{data.table::fread}, or \code{utils::read.csv} when data.table is
#'   unavailable), e.g. \code{na.strings}, \code{nrows}, \code{sep}.
#' @return A data frame with one row per survey response.
#' @examples
#' \dontrun{
#' s160_gcs_init(bucket = "campaign_results")
#' df <- s160_gcs_campaign_results_read(1980)
#' df <- s160_gcs_campaign_results_read(1980, destdir = ".")
#' df <- s160_gcs_campaign_results_read(1980, destdir = "~/data")
#' }
#' @importFrom googleCloudStorageR gcs_get_object gcs_get_global_bucket
#' @export
s160_gcs_campaign_results_read <- function(campaign_id, filename = NULL,
                                           destdir = NULL, bucket = NULL,
                                           columns = NULL, ...) {
  campaign_id <- validate_campaign_id(campaign_id)
  bucket <- resolve_bucket(bucket)

  if (is.null(filename)) {
    filename <- paste0(campaign_id, "_raw_data_download.csv")
  }
  if (filename != basename(filename)) {
    stop_s160("`filename` must not contain path separators.",
              fn = "s160_gcs_campaign_results_read")
  }
  object_name <- paste0(campaign_id, "/", filename)

  gcs_path <- sprintf("gs://%s/%s", bucket, object_name)
  message(sprintf("Reading: %s", gcs_path))

  if (is.null(destdir)) {
    local_path <- tempfile(pattern = paste0("s160_", campaign_id, "_"), fileext = ".csv")
    on.exit(unlink(local_path), add = TRUE)
  } else if (!is.character(destdir) || length(destdir) != 1) {
    stop_s160("`destdir` must be a single character string.",
              fn = "s160_gcs_campaign_results_read")
  } else {
    destdir <- normalizePath(destdir, mustWork = FALSE)
    if (!dir.exists(destdir)) {
      stop_s160(sprintf("`destdir` does not exist or is not a directory: %s", destdir),
                fn = "s160_gcs_campaign_results_read")
    }
    local_path <- file.path(destdir, filename)
  }

  tryCatch(
    download_with_verify(object_name = object_name, local_path = local_path,
                         bucket = bucket),
    error = function(e) {
      msg <- conditionMessage(e)
      fn <- "s160_gcs_campaign_results_read"
      if (grepl("404", msg, fixed = TRUE)) {
        stop_not_found("file", gcs_path, fn = fn)
      }
      stop_failed(sprintf("download %s", gcs_path), msg, fn = fn)
    }
  )

  if (!is.null(destdir)) {
    message(sprintf("Saved to: %s", local_path))
  }

  fast_read_csv(local_path, columns = columns, ...)
}

#' List files in a campaign's GCS folder
#'
#' Returns the file names inside a campaign's folder in the results bucket.
#' Returns \code{character(0)} with a message if the campaign has no files.
#'
#' @param campaign_id Campaign ID (numeric or character). Must be a single value.
#' @param bucket Source GCS bucket. \code{NULL} (default) falls back to the
#'   global bucket set by \code{s160_gcs_init()}.
#' @return Character vector of file names (without the campaign_id prefix).
#' @examples
#' \dontrun{
#' s160_gcs_init(bucket = "campaign_results")
#' s160_gcs_campaign_results_files(1980)
#' }
#' @importFrom googleCloudStorageR gcs_list_objects
#' @export
s160_gcs_campaign_results_files <- function(campaign_id, bucket = NULL) {
  campaign_id <- validate_campaign_id(campaign_id)
  bucket <- resolve_bucket(bucket)

  prefix <- paste0(campaign_id, "/")
  objects <- gcs_list_or_stop(
    sprintf("list files for campaign %s", campaign_id),
    fn = "s160_gcs_campaign_results_files",
    prefix = prefix, bucket = bucket
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
#' @param bucket Source GCS bucket. \code{NULL} (default) falls back to the
#'   global bucket set by \code{s160_gcs_init()}.
#' @return Character vector of campaign IDs, sorted.
#' @examples
#' \dontrun{
#' s160_gcs_init(bucket = "campaign_results")
#' s160_gcs_campaign_results_list()
#' }
#' @export
s160_gcs_campaign_results_list <- function(bucket = NULL) {
  bucket <- resolve_bucket(bucket)
  objects <- gcs_list_or_stop(
    "list campaigns",
    fn = "s160_gcs_campaign_results_list",
    bucket = bucket
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

#' Read a campaign CSV from GCS, hashing it for provenance
#'
#' Thin wrapper over \code{s160_gcs_campaign_results_read} that also
#' computes a sha256 of the downloaded CSV bytes. The hash and the
#' canonical \code{gs://} path travel back on the returned data frame
#' as the \code{source_csv_hash} and \code{source_csv_path}
#' attributes; \code{latency_report()} reads them and copies them onto
#' \code{result$meta} so downstream consumers (e.g. persistence layers)
#' don't have to fish them off attributes.
#'
#' @param campaign_id Campaign id (numeric or character).
#' @param filename Optional override for the CSV filename.
#' @param bucket Source GCS bucket. \code{NULL} (default) falls back to the
#'   global bucket set by \code{s160_gcs_init()}; pass an explicit value to
#'   skip the global entirely.
#' @param columns Optional character vector of (dot-form) column names to keep
#'   (e.g. from \code{latency_input_columns()}). Forwarded to
#'   \code{s160_gcs_campaign_results_read()} to parse only those columns.
#' @return A data frame with attributes \code{source_csv_hash} and
#'   \code{source_csv_path} set.
#' @export
s160_gcs_pull_csv <- function(campaign_id, filename = NULL, bucket = NULL,
                              columns = NULL) {
  bucket <- resolve_bucket(bucket)
  tmpdir <- tempfile(pattern = "s160_latency_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  data <- s160_gcs_campaign_results_read(
    campaign_id = campaign_id,
    filename = filename,
    destdir = tmpdir,
    bucket = bucket,
    columns = columns
  )
  fn <- if (is.null(filename)) {
    paste0(as.character(campaign_id), "_raw_data_download.csv")
  } else {
    filename
  }
  csv_path <- file.path(tmpdir, fn)
  attr(data, "source_csv_hash") <- if (file.exists(csv_path)) {
    paste0("sha256:", digest::digest(file = csv_path, algo = "sha256"))
  } else {
    NA_character_
  }
  # Canonical GCS source path (not the local temp path, which is unlinked on
  # return). Lets downstream callers record provenance without re-deriving
  # the path from campaign_id + filename.
  attr(data, "source_csv_path") <-
    sprintf("gs://%s/%s/%s", bucket, as.character(campaign_id), fn)
  data
}

#' Read a campaign CSV from a local path, hashing it for provenance
#'
#' Local-source sibling of \code{s160_gcs_pull_csv()}. Reads the CSV
#' via \code{data.table::fread} (falling back to \code{utils::read.csv})
#' and stamps \code{source_csv_hash} and \code{source_csv_path}
#' attributes on the returned data frame so downstream
#' \code{latency_report()} / \code{latency_run()} surface them on
#' \code{result$meta}. Use for backfills (archived campaign CSVs stored
#' on disk, Dropbox, S3 mounts, etc.).
#'
#' @param path Path to the CSV. Recorded verbatim on
#'   \code{attr(., "source_csv_path")}.
#' @param columns Optional character vector of (dot-form) column names to keep
#'   (e.g. from \code{latency_input_columns()}). When set, only those columns
#'   are parsed, cutting read time and memory on wide exports. \code{NULL}
#'   (default) reads every column.
#' @param hash When \code{TRUE} (default), compute the sha256 of the file for
#'   \code{source_csv_hash}. Set \code{FALSE} to skip the hashing pass (a full
#'   second read of the file) on large backfills where provenance hashing is
#'   not needed; \code{source_csv_hash} is then \code{NA}.
#' @param ... Forwarded to the CSV reader (\code{data.table::fread}, or
#'   \code{utils::read.csv} when data.table is unavailable), e.g.
#'   \code{na.strings}, \code{sep}. \code{stringsAsFactors} defaults to
#'   \code{FALSE}.
#' @return A data frame with \code{source_csv_hash} and
#'   \code{source_csv_path} attributes set.
#' @examples
#' \dontrun{
#' data <- s160_read_csv("~/Dropbox/archive/campaign_500.csv")
#' attr(data, "source_csv_hash")
#' latency_run(500, data, field_timezone = "America/New_York")
#' }
#' @export
s160_read_csv <- function(path, columns = NULL, hash = TRUE, ...) {
  if (!file.exists(path)) {
    stop_not_found("file", path, fn = "s160_read_csv")
  }
  if (!is.logical(hash) || length(hash) != 1L || is.na(hash)) {
    stop_s160("`hash` must be a single TRUE or FALSE.", fn = "s160_read_csv")
  }
  data <- fast_read_csv(path, columns = columns, ...)
  attr(data, "source_csv_hash") <- if (hash) {
    paste0("sha256:", digest::digest(file = path, algo = "sha256"))
  } else {
    NA_character_
  }
  attr(data, "source_csv_path") <- path
  data
}

#' Read just the header (column names) of a CSV
#'
#' Peeks the first line of a CSV and returns its column names in the same
#' \code{make.names()}-munged (dot-form) form the readers produce, without
#' parsing the body. Pair with \code{latency_build_config()} +
#' \code{latency_input_columns()} to derive a column-projection set for a
#' large file before reading it:
#'
#' \preformatted{
#' path <- "campaign_500.csv"
#' campaign_id <- 500L
#' field_timezone <- "America/New_York"
#' header <- s160_csv_header(path)
#' config <- latency_build_config(campaign_id, header,
#'                                field_timezone = field_timezone)
#' data   <- s160_read_csv(path, columns = latency_input_columns(config, header))
#' }
#'
#' @param path Path to the CSV.
#' @param encoding File encoding for the header peek (\code{"UTF-8"} default),
#'   kept consistent with the body read so a UTF-8/BOM file munges to the same
#'   names regardless of reader.
#' @return Character vector of dot-form column names.
#' @export
s160_csv_header <- function(path, encoding = "UTF-8") {
  if (!file.exists(path)) {
    stop_not_found("file", path, fn = "s160_csv_header")
  }
  make.names(read_header_raw(path, encoding = encoding), unique = TRUE)
}

#' Check campaign results export status
#'
#' Returns GCS file metadata for the campaign's export file without
#' triggering a new export. Requires GCS auth (\code{s160_gcs_init}).
#'
#' @param campaign_id Campaign ID (numeric or character).
#' @param bucket Source GCS bucket. \code{NULL} (default) falls back to the
#'   global bucket set by \code{s160_gcs_init()}.
#' @return Named list with \code{name}, \code{updated}, and \code{size},
#'   or \code{NULL} if no export file exists.
#' @examples
#' \dontrun{
#' s160_gcs_init(bucket = "campaign_results")
#' s160_gcs_campaign_results_status(1980)
#' }
#' @importFrom googleCloudStorageR gcs_list_objects
#' @export
s160_gcs_campaign_results_status <- function(campaign_id, bucket = NULL) {
  campaign_id <- validate_campaign_id(campaign_id)
  bucket <- resolve_bucket(bucket)

  export_filename <- paste0(campaign_id, "_raw_data_download.csv")
  prefix <- paste0(campaign_id, "/")

  objects <- gcs_list_or_stop(
    sprintf("list files for campaign %s", campaign_id),
    fn = "s160_gcs_campaign_results_status",
    prefix = prefix, bucket = bucket
  )

  if (nrow(objects) == 0) return(NULL)

  match_idx <- which(objects$name == paste0(prefix, export_filename))
  if (length(match_idx) == 0) return(NULL)

  row <- objects[match_idx[1], ]
  list(
    name = export_filename,
    updated = row$updated,
    size = row$size
  )
}
