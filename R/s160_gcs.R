# GCS authentication and data access for Survey160
#
# Auth strategy: Browser OAuth via a GCP "Desktop" OAuth client.
#
# The client ID is bundled in inst/oauth-client.json (public, not a secret).
# The client secret is read from S160_GCS_CLIENT_SECRET in ~/.Renviron.
# On first run, s160_gcs_init() prompts for the secret and saves it to
# ~/.Renviron so the user only needs to paste it once.
#
# Tokens are cached between sessions so the browser prompt only appears on
# first use or when the token expires.
#
# s160_gcs_init() authenticates only; readers resolve their own bucket
# (defaulting to "campaign_results").

# --- Internal helpers --------------------------------------------------------

# TRUE when a GCS OAuth token is available (i.e. gcs_auth() has run this
# session). Wrapped so tests can mock it without reaching into googleAuthR.
.gcs_has_token <- function() {
  googleAuthR::gar_has_token()
}

# Stop with a clear message if GCS has not been authenticated.
check_gcs_ready <- function() {
  if (!.gcs_has_token()) {
    stop_not_initialized("GCS", "s160_gcs_init")
  }
}

# Resolve the bucket for a reader, in priority order: an explicit `bucket` arg,
# then the session global set by a (deprecated) s160_gcs_init(bucket = ...), then
# the caller-supplied `default`. Errors only when none is available. Returns a
# non-empty string.
resolve_bucket <- function(bucket = NULL, default = NULL) {
  if (!is.null(bucket)) {
    check_nonempty_string(bucket, "bucket")
    return(bucket)
  }
  resolved <- tryCatch(gcs_get_global_bucket(), error = function(e) NULL)
  if (!is.null(resolved) && nzchar(resolved)) {
    return(resolved)
  }
  if (!is.null(default)) {
    return(default)
  }
  stop("No GCS bucket available. Pass `bucket = \"...\"` to the reader.",
       call. = FALSE)
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

# Resolve the authoritative expected byte size for a GCS object from its
# metadata, for download_with_verify()'s post-download size check. Returns the
# byte count, or NULL when the size can't be used for verification -- emitting a
# message (never a silent skip) in the NULL cases. NULL when:
#   - metadata can't be fetched (permissions / transient error / object absent);
#   - the object carries a Content-Encoding (e.g. gzip): GCS applies
#     decompressive transcoding on download, so the saved file is the
#     DECOMPRESSED size while `meta$size` is the COMPRESSED byte count -- the two
#     never match, so a byte check would always fail;
#   - `meta$size` is not a usable number. gcs_list_objects() formats `size` as a
#     human string ("483.3 Kb"), which as.numeric() turns to NA; gcs_get_object(
#     meta = TRUE) returns the raw byte count, but this guards the string case
#     defensively.
.expected_download_size <- function(object_name, bucket) {
  meta <- tryCatch({
    if (is.null(bucket)) {
      gcs_get_object(object_name, meta = TRUE)
    } else {
      gcs_get_object(object_name, bucket = bucket, meta = TRUE)
    }
  }, error = function(e) NULL)

  enc <- if (is.null(meta)) NULL else meta$contentEncoding
  encoded <- !is.null(enc) && length(enc) == 1L && !is.na(enc) &&
    nzchar(as.character(enc))
  size <- if (is.null(meta) || encoded) {
    NULL
  } else {
    coerced <- suppressWarnings(as.numeric(meta$size))
    if (length(coerced) == 1L && !is.na(coerced)) coerced else NULL
  }

  if (is.null(size)) {
    reason <- if (encoded) {
      "stored with a Content-Encoding; the download is decompressed"
    } else {
      "object size unavailable"
    }
    message(sprintf(
      "Skipping size verification for '%s' (%s).", object_name, reason
    ))
  }
  size
}

# Download a GCS object to disk with size verification and retry.
# Compares the local file size against GCS object metadata after download.
# Retries up to max_retries times on size mismatch with exponential backoff.
download_with_verify <- function(object_name, local_path, max_retries = 2L,
                                 bucket = NULL, progress = FALSE) {
  expected_size <- .expected_download_size(object_name, bucket)

  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    download_err <- tryCatch({
      fetch <- function() {
        if (is.null(bucket)) {
          gcs_get_object(object_name = object_name, saveToDisk = local_path, overwrite = TRUE)
        } else {
          gcs_get_object(object_name = object_name, saveToDisk = local_path,
                         overwrite = TRUE, bucket = bucket)
        }
      }
      # gcs_get_object() streams to disk through httr, so layering httr::progress()
      # via the global config shows a live download bar with no change to the auth
      # path. Interactive callers opt in (disposition_pull defaults progress to
      # interactive()); batch and scheduled runs stay quiet.
      if (isTRUE(progress)) {
        httr::with_config(httr::progress(), fetch())
      } else {
        fetch()
      }
      NULL
    }, error = function(e) e)
    if (!is.null(download_err)) {
      # Boundary translation of the googleCloudStorageR error into a classed
      # s160_not_found so callers dispatch on the class, not the message.
      # gcs_get_object()'s own 404 branch raises "File not found. Check
      # object_name..." with NO "404" in the text (googleCloudStorageR 0.7.0);
      # an httr/googleAuthR-level failure instead surfaces as "http_404 ...".
      # Match both real forms -- deliberately NOT a bare "404" substring, which
      # could appear in an unrelated message. Anything else propagates.
      emsg <- conditionMessage(download_err)
      if (grepl("File not found", emsg, fixed = TRUE) ||
            grepl("http_404", emsg, fixed = TRUE)) {
        stop_not_found("object", object_name)
      }
      stop(download_err)
    }

    actual_size <- file.info(local_path)$size
    if (!file.exists(local_path) || is.na(actual_size)) {
      stop(sprintf("Download produced no file for '%s'.", object_name), call. = FALSE)
    }

    if (is.null(expected_size)) break  # metadata unavailable; trust the download
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
fast_read_csv <- function(path, columns = NULL, encoding = "UTF-8",
                          fn = "s160_read_csv", ...) {
  extra <- list(...)
  # Reject `...` names the active CSV reader would not accept, so a typo or a
  # wrong-function argument (e.g. filter_open=) fails with a clear message here
  # rather than a cryptic "unused argument" (fread) or a silently-dropped extra
  # (read.csv fallback assembly no-ops an unnamed arg). Validate against
  # whichever reader will run.
  if (length(extra) > 0L) {
    valid <- if (requireNamespace("data.table", quietly = TRUE)) {
      names(formals(data.table::fread))
    } else {
      c(names(formals(utils::read.csv)), names(formals(utils::read.table)))
    }
    nms <- names(extra)
    bad <- setdiff(if (is.null(nms)) "" else nms, valid)
    if (length(bad) > 0L) {
      bad[!nzchar(bad)] <- "unnamed"
      stop_s160(sprintf("argument(s) not accepted by the CSV reader: %s",
                        paste(unique(bad), collapse = ", ")), fn = fn)
    }
  }
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
#' Authenticates to GCS using the Survey160 Desktop OAuth client.
#'
#' On first run, prompts for the client secret (get it from your team lead) and saves
#' it to \code{~/.Renviron}. Subsequent runs read it automatically. Also
#' opens a browser for Google sign-in on first use; the OAuth token is
#' cached in a platform-dependent directory (run
#' \code{gargle::gargle_oauth_sitrep()} to locate it).
#'
#' Authentication is account-level: one call covers every bucket the account can
#' read. Reader functions resolve their own bucket (defaulting to prod), so you
#' no longer pass a bucket here.
#'
#' @param bucket \strong{Deprecated.} Formerly set a session-global default
#'   bucket. It is now optional and, if supplied, is only kept as a back-compat
#'   session default (with a warning). Pass \code{bucket =} to an individual
#'   reader instead when you need a non-default bucket.
#' @return Invisible \code{NULL}.
#' @examples
#' \dontrun{
#' s160_gcs_init()
#' }
#' @importFrom googleCloudStorageR gcs_auth gcs_global_bucket
#' @export
s160_gcs_init <- function(bucket = NULL) {
  if (!is.null(bucket)) {
    check_nonempty_string(bucket, "bucket", fn = "s160_gcs_init")
    warning(
      "`bucket` is deprecated and will be removed in a future release. ",
      "s160_gcs_init() now only authenticates; readers default to prod. ",
      "Pass `bucket =` to a reader for a non-default bucket.",
      call. = FALSE
    )
  }

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
          "Run s160_gcs_init() interactively to set it up, ",
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

  # Deprecated: honor an explicit bucket as the session default so existing
  # global-bucket flows keep working during the deprecation window.
  if (!is.null(bucket)) {
    gcs_global_bucket(bucket)
    message(sprintf("GCS ready. Bucket: %s", bucket))
  } else {
    message("GCS ready.")
  }
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
#' @param bucket Source GCS bucket. \code{NULL} (default) uses
#'   \code{"campaign_results"} (or a session default from a deprecated
#'   \code{s160_gcs_init(bucket = ...)}).
#' @param columns Optional character vector of (dot-form) column names to keep,
#'   e.g. from \code{latency_input_columns()}. When set, only those columns are
#'   parsed (via \code{data.table::fread}'s column projection), cutting read
#'   time and memory on wide exports. \code{NULL} (default) reads every column.
#' @param hash When \code{TRUE}, stamp provenance on the returned frame -- the
#'   sha256 of the downloaded CSV bytes as \code{source_csv_hash} and the
#'   canonical \code{gs://} source as \code{source_csv_path}, which
#'   \code{latency_run()} / \code{latency_report()} then surface on
#'   \code{result$meta}. \code{FALSE} (default) skips the extra hashing read and
#'   returns a plain frame. (The local-file sibling is
#'   \code{\link{s160_read_csv}}, whose \code{hash} stamps the same provenance.)
#' @param ... Additional arguments forwarded to the CSV reader
#'   (\code{data.table::fread}, or \code{utils::read.csv} when data.table is
#'   unavailable), e.g. \code{na.strings}, \code{nrows}, \code{sep}.
#' @return A data frame with one row per survey response. With
#'   \code{hash = TRUE} it also carries \code{source_csv_hash} and
#'   \code{source_csv_path} attributes.
#' @examples
#' \dontrun{
#' s160_gcs_init()
#' df <- s160_gcs_campaign_results_read(1980)
#' df <- s160_gcs_campaign_results_read(1980, destdir = ".")
#' df <- s160_gcs_campaign_results_read(1980, destdir = "~/data")
#' df <- s160_gcs_campaign_results_read(1980, hash = TRUE)   # + provenance attrs
#' }
#' @importFrom googleCloudStorageR gcs_get_object gcs_get_global_bucket
#' @export
s160_gcs_campaign_results_read <- function(campaign_id, filename = NULL,
                                           destdir = NULL, bucket = NULL,
                                           columns = NULL, hash = FALSE, ...) {
  campaign_id <- validate_campaign_id(campaign_id)
  bucket <- resolve_bucket(bucket, default = "campaign_results")
  if (!is.logical(hash) || length(hash) != 1L || is.na(hash)) {
    stop_s160("`hash` must be a single TRUE or FALSE.",
              fn = "s160_gcs_campaign_results_read")
  }

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

  fn <- "s160_gcs_campaign_results_read"
  tryCatch(
    download_with_verify(object_name = object_name, local_path = local_path,
                         bucket = bucket),
    s160_not_found = function(e) stop_not_found("file", gcs_path, fn = fn),
    error = function(e) {
      stop_failed(sprintf("download %s", gcs_path), conditionMessage(e), fn = fn)
    }
  )

  if (!is.null(destdir)) {
    message(sprintf("Saved to: %s", local_path))
  }

  data <- fast_read_csv(local_path, columns = columns,
                        fn = "s160_gcs_campaign_results_read", ...)
  # Provenance (opt-in): hash the downloaded bytes + record the canonical gs://
  # source, so latency_run()/latency_report() can surface them on result$meta.
  # Done before the on.exit() cleanup of a NULL-destdir tempfile, so the file is
  # still present. `gcs_path` is gs://<bucket>/<campaign_id>/<filename>.
  if (hash) {
    attr(data, "source_csv_hash") <-
      paste0("sha256:", digest::digest(file = local_path, algo = "sha256"))
    attr(data, "source_csv_path") <- gcs_path
  }
  data
}

#' List files in a campaign's GCS folder
#'
#' Returns the file names inside a campaign's folder in the results bucket.
#' Returns \code{character(0)} with a message if the campaign has no files.
#'
#' @param campaign_id Campaign ID (numeric or character). Must be a single value.
#' @param bucket Source GCS bucket. \code{NULL} (default) uses
#'   \code{"campaign_results"} (or a session default from a deprecated
#'   \code{s160_gcs_init(bucket = ...)}).
#' @return Character vector of file names (without the campaign_id prefix).
#' @examples
#' \dontrun{
#' s160_gcs_init()
#' s160_gcs_campaign_results_files(1980)
#' }
#' @importFrom googleCloudStorageR gcs_list_objects
#' @export
s160_gcs_campaign_results_files <- function(campaign_id, bucket = NULL) {
  campaign_id <- validate_campaign_id(campaign_id)
  bucket <- resolve_bucket(bucket, default = "campaign_results")

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
#' @param bucket Source GCS bucket. \code{NULL} (default) uses
#'   \code{"campaign_results"} (or a session default from a deprecated
#'   \code{s160_gcs_init(bucket = ...)}).
#' @return Character vector of campaign IDs, sorted.
#' @examples
#' \dontrun{
#' s160_gcs_init()
#' s160_gcs_campaign_results_list()
#' }
#' @export
s160_gcs_campaign_results_list <- function(bucket = NULL) {
  bucket <- resolve_bucket(bucket, default = "campaign_results")
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

#' Read a campaign CSV from a local path, hashing it for provenance
#'
#' Local-source sibling of \code{s160_gcs_campaign_results_read()} (with
#' \code{hash = TRUE}). Reads the CSV
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
  data <- fast_read_csv(path, columns = columns, fn = "s160_read_csv", ...)
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
#' large file before reading it (see the projection example under
#' \code{\link{latency_input_columns}}).
#'
#' @param path Path to the CSV.
#' @param encoding File encoding for the header peek (\code{"UTF-8"} default),
#'   kept consistent with the body read so a UTF-8/BOM file munges to the same
#'   names regardless of reader.
#' @return Character vector of dot-form column names.
#' @examples
#' path <- tempfile(fileext = ".csv")
#' writeLines(c("campaignid,id.intro.scriptDate", "1,2026-01-26 21:00:00Z"), path)
#' s160_csv_header(path)
#' unlink(path)
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
#' @param bucket Source GCS bucket. \code{NULL} (default) uses
#'   \code{"campaign_results"} (or a session default from a deprecated
#'   \code{s160_gcs_init(bucket = ...)}).
#' @return Named list with \code{name}, \code{updated}, and \code{size},
#'   or \code{NULL} if no export file exists.
#' @examples
#' \dontrun{
#' s160_gcs_init()
#' s160_gcs_campaign_results_status(1980)
#' }
#' @importFrom googleCloudStorageR gcs_list_objects
#' @export
s160_gcs_campaign_results_status <- function(campaign_id, bucket = NULL) {
  campaign_id <- validate_campaign_id(campaign_id)
  bucket <- resolve_bucket(bucket, default = "campaign_results")

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

# Human-readable age of a file from its mtime, for the cache-hit message. Skew-
# safe: an mtime slightly in the future clamps to "0 min old".
.format_file_age <- function(path) {
  mins <- max(0, as.numeric(difftime(Sys.time(), file.mtime(path), units = "mins")),
              na.rm = TRUE)
  if (mins < 60) {
    sprintf("%d min old", as.integer(round(mins)))
  } else if (mins < 60 * 48) {
    sprintf("%d hr old", as.integer(round(mins / 60)))
  } else {
    sprintf("%d days old", as.integer(round(mins / 1440)))
  }
}

# Shared cached GCS pull behind disposition_pull() and opt_out_pull(): resolve
# the bucket and cache/dest path, serve a cache hit without auth, else download
# to a temp file and atomically move it into place so a failed or partial
# download never poisons the cache. `env` is match.arg'd by the caller.
# Callers differ in `object_name` (fetched object), `cache_suffix` (default
# cache file <bucket><cache_suffix>, so two artifacts in one bucket never
# collide), `noun` (name in messages and errors), and `fn` (for classed errors).
.gcs_pull_cached <- function(fn, env, dest, bucket, refresh, progress,
                             object_name, cache_suffix, noun) {
  .require_single_logical(refresh, "refresh", fn)
  .require_single_logical(progress, "progress", fn)
  if (is.null(bucket)) bucket <- sprintf("s160_disposition_%s", env)
  bucket <- resolve_bucket(bucket)
  default_name <- paste0(bucket, cache_suffix)

  if (is.null(dest)) {
    cache_dir <- tools::R_user_dir("survey160r", "cache")
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    local_path <- file.path(cache_dir, default_name)
  } else if (!is.character(dest) || length(dest) != 1L || !nzchar(trimws(dest))) {
    stop_s160("`dest` must be a single non-empty path or directory.", fn = fn)
  } else if (dir.exists(dest)) {
    local_path <- file.path(dest, default_name)
  } else {
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    local_path <- dest
  }

  gcs_path <- sprintf("gs://%s/%s", bucket, object_name)
  if (!refresh && file.exists(local_path)) {
    message(sprintf("Using cached %s (%s): %s",
                    noun, .format_file_age(local_path), local_path))
    return(local_path)
  }

  # A download needs an authenticated GCS session; checked after the cache-hit
  # return, since reusing a local copy needs no auth. Explicit so an
  # un-initialized session gets the standard clear message rather than a raw
  # googleCloudStorageR error wrapped as "Failed to download".
  check_gcs_ready()

  message(sprintf("Downloading %s", gcs_path))
  # Download to a temp file in the destination dir, then atomically move it into
  # place on success -- a failed or partial download never poisons the cache,
  # and any existing good copy survives.
  tmp <- tempfile(tmpdir = dirname(local_path), fileext = ".part")
  on.exit(unlink(tmp), add = TRUE)
  tryCatch(
    download_with_verify(object_name = object_name, local_path = tmp,
                         bucket = bucket, progress = progress),
    s160_not_found = function(e) stop_not_found(noun, gcs_path, fn = fn),
    error = function(e) {
      stop_failed(sprintf("download %s", gcs_path), conditionMessage(e), fn = fn)
    }
  )
  # Move the temp file into place. file.rename is atomic on the same filesystem
  # (tmp lives in dirname(local_path)); when it cannot overwrite an existing
  # dest (e.g. on Windows), fall back to a copy -- but move any existing cache
  # aside first so a failed copy is rolled back rather than left as a partial
  # file that a later refresh = FALSE would serve.
  if (!file.rename(tmp, local_path)) {
    backup <- paste0(local_path, ".bak")
    had_cache <- file.exists(local_path) && file.rename(local_path, backup)
    if (file.copy(tmp, local_path, overwrite = TRUE)) {
      if (had_cache) unlink(backup)
    } else {
      if (had_cache) file.rename(backup, local_path)
      stop_failed("move the downloaded file into place", local_path, fn = fn)
    }
  }
  local_path
}
