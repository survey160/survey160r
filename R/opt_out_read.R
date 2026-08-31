# Opt-out reader -- the R-only consumer of the opt-out list.
#
# The opt-out list is one row per opted-out phone (phone + date_added),
# produced upstream as a phone-keyed Parquet snapshot. A caller screens a
# fresh sample against it -- "which of these numbers have opted out?" -- to
# clean the list before a send. opt_out_pull() fetches the list from GCS;
# opt_out_screen() annotates the sample in place and never drops rows, and the
# caller decides what to drop.
#
# The reader stays bare and in the opt_out family (not s160_-prefixed): it
# reads a survey160r-derived artifact (the opt-out projection), not a raw
# Survey160 source. It parallels disposition_screen(): chain both to screen a
# sample on prior disposition and current opt-out status in one pass. Phone
# matching uses the shared .normalize_phone (aaa_utils.R), so a sample matches
# the disposition and opt-out datasets identically. The Parquet read uses
# nanoparquet, and since the list is one small consolidated file a full read is
# sub-second.

# Columns the screen reads (the Parquet read is projected to just these).
# `date_added` is optional -- it only enriches the annotation, so a list that
# omits it still screens (opt_out_date comes back NA).
.OPT_OUT_READ_COLS <- c("phone", "date_added")

# Columns of the block appended by opt_out_screen() (besides the phone key).
.OPT_OUT_SCREEN_COLS <- c("opted_out", "opt_out_date")

# Read the opt-out Parquet, projected to the columns the screen needs. Mirrors
# .disposition_read_parquet: a single existing path, projected against the
# file's actual schema (nanoparquet errors on a col_select naming a missing
# column, so intersect first).
.opt_out_read_parquet <- function(dataset, columns = .OPT_OUT_READ_COLS) {
  if (!is.character(dataset) || length(dataset) != 1L || !nzchar(dataset)) {
    stop("`dataset` must be a single Parquet path.", call. = FALSE)
  }
  if (!file.exists(dataset)) {
    stop_not_found("opt-out dataset", dataset)
  }
  if (!is.null(columns)) {
    columns <- intersect(columns, nanoparquet::read_parquet_schema(dataset)$name)
  }
  as.data.frame(nanoparquet::read_parquet(dataset, col_select = columns))
}

# Collapse the raw opt-out rows to one row per normalized phone: the lookup
# opt_out_screen() matches against. `phone` is required; `date_added` is
# optional (all-NA when absent). Blank/unparseable phones drop out (they can
# never match a sample row meaningfully), and a phone listed twice keeps its
# first occurrence -- the snapshot is already deduped upstream, so this is a
# defensive guard, not a reconciliation.
.opt_out_lookup <- function(data, fn) {
  if (!"phone" %in% names(data)) {
    stop_s160("input is missing required column(s): phone", fn = fn)
  }
  phone <- .normalize_phone(data$phone)
  date_added <- if ("date_added" %in% names(data)) {
    data$date_added
  } else {
    rep(NA, nrow(data))
  }
  keep <- !is.na(phone) & !duplicated(phone)
  data.frame(phone = phone[keep], date_added = date_added[keep],
             stringsAsFactors = FALSE)
}

#' Screen a sample against the opt-out list (annotate in place)
#'
#' Takes a sample data frame (a phone column plus whatever else -- strata,
#' quota cells, ...) and returns it \strong{unchanged with two opt-out columns
#' appended}, 1:1 with the input rows and preserving the original phone
#' formatting. This function \strong{never drops rows}: the caller decides
#' whether to suppress the opted-out numbers, on its own rules. Parallels
#' \code{\link{disposition_screen}} -- chain both to screen a sample on prior
#' disposition and current opt-out status in one pass, then filter once.
#'
#' @param sample A data frame with a phone-number column.
#' @param dataset Path to an opt-out Parquet file (a \code{phone} column and an
#'   optional \code{date_added}).
#' @param phone_col Name of the phone column in \code{sample}
#'   (default \code{"phone"}).
#' @return \code{sample} with \code{opted_out} (logical) and \code{opt_out_date}
#'   appended, 1:1 with the input rows and in input order. A valid phone present
#'   in the list gets \code{opted_out = TRUE} and its \code{date_added} value as
#'   \code{opt_out_date}; a valid phone absent from the list gets
#'   \code{opted_out = FALSE} and \code{opt_out_date = NA}; a phone that
#'   digit-normalizes to nothing (blank/unparseable) gets \code{opted_out = NA}
#'   -- unknown, not a claim that it is contactable -- and \code{opt_out_date =
#'   NA}. \code{opt_out_date} carries the list's \code{date_added} value
#'   uninterpreted (\code{NA} throughout when the list omits that column).
#' @seealso \code{\link{disposition_screen}}
#' @examples
#' \dontrun{
#' cleaned <- opt_out_screen(my_sample, opt_out_pull())
#' # chain with disposition, then drop on the caller's own rules:
#' cleaned <- disposition_screen(cleaned, disposition_pull())
#' subset(cleaned, !(opted_out %in% TRUE | ever_completed %in% TRUE))
#' }
#' @export
opt_out_screen <- function(sample, dataset, phone_col = "phone") {
  check_data_frame(sample, "sample", fn = "opt_out_screen")
  if (!is.character(phone_col) || length(phone_col) != 1L ||
        !phone_col %in% names(sample)) {
    stop_s160(sprintf("phone column %s not found in `sample`.",
                      deparse(phone_col)), fn = "opt_out_screen")
  }
  clash <- intersect(.OPT_OUT_SCREEN_COLS, names(sample))
  if (length(clash) > 0L) {
    stop_s160(sprintf(paste0("`sample` already has ",
                             "opt-out column(s) [%s]; rename them first."),
                      paste(clash, collapse = ", ")),
              fn = "opt_out_screen")
  }
  lst <- .opt_out_lookup(.opt_out_read_parquet(dataset), fn = "opt_out_screen")
  norm <- .normalize_phone(sample[[phone_col]])
  idx <- match(norm, lst$phone)
  opted <- !is.na(idx)
  opted[is.na(norm)] <- NA # blank/unparseable phone -> unknown, not FALSE
  sample[["opted_out"]] <- opted
  sample[["opt_out_date"]] <- lst$date_added[idx]
  sample
}

#' Download the opt-out list from Cloud Storage
#'
#' Pulls the opt-out Parquet (\code{global_opt_out/global_opt_out.parquet}) from
#' the environment's disposition bucket to a local file and returns the path --
#' ready to hand to \code{\link{opt_out_screen}}. Downloaded once and reused from
#' the local cache on later calls (pass \code{refresh = TRUE} to force a fresh
#' download). Parallels \code{\link{disposition_pull}} and reaches GCS the same
#' way: authenticate first with \code{\link{s160_gcs_init}} (any bucket) so the
#' session's GCS credentials are set. A download without an initialized session
#' errors with \dQuote{GCS not initialized. Run s160_gcs_init() first.} (a cache
#' hit is served without needing auth).
#'
#' The opt-out list shares the environment's \code{s160_disposition_<env>} bucket
#' with the disposition projection -- it is the same phone-keyed PII class -- so
#' the two \code{*_pull()} helpers fetch different objects from one bucket, into
#' distinct cache files.
#'
#' @param env Environment for the source bucket: \code{"prod"} (default) or
#'   \code{"dev"} (the \code{s160_disposition_<env>} buckets).
#' @param dest Where to save. \code{NULL} (default) caches under
#'   \code{tools::R_user_dir("survey160r", "cache")}. A directory saves the
#'   default filename (\code{<bucket>.global_opt_out.parquet}) inside it; any
#'   other single string is treated as the exact output path (its parent is
#'   created).
#' @param bucket Source GCS bucket. \code{NULL} (default) derives it from
#'   \code{env}; pass a bucket name to override.
#' @param refresh When \code{FALSE} (default), reuse an existing local copy;
#'   \code{TRUE} always re-downloads (the list is republished on every sync, so
#'   refresh to pick up a newer one).
#' @param progress Show a download progress bar. Defaults to
#'   \code{interactive()}: a live bar in an interactive session, silent in batch
#'   or scheduled runs.
#' @return The local path to the downloaded Parquet (a single string), ready for
#'   \code{\link{opt_out_screen}}.
#' @seealso \code{\link{opt_out_screen}}, \code{\link{disposition_pull}},
#'   \code{\link{s160_gcs_init}}
#' @examples
#' \dontrun{
#' s160_gcs_init(bucket = "s160_disposition_prod")   # one-time browser OAuth
#' cleaned <- opt_out_screen(my_sample, opt_out_pull())
#' }
#' @export
opt_out_pull <- function(env = c("prod", "dev"), dest = NULL,
                         bucket = NULL, refresh = FALSE,
                         progress = interactive()) {
  env <- match.arg(env)
  .require_single_logical(refresh, "refresh", "opt_out_pull")
  .require_single_logical(progress, "progress", "opt_out_pull")
  if (is.null(bucket)) bucket <- sprintf("s160_disposition_%s", env)
  bucket <- resolve_bucket(bucket)
  object_name <- "global_opt_out/global_opt_out.parquet"
  # The opt-out list shares the disposition bucket, so key the default cache on
  # the object too -- keying on the bucket alone would collide with the
  # disposition projection's cache file (<bucket>.parquet).
  default_name <- sprintf("%s.global_opt_out.parquet", bucket)

  if (is.null(dest)) {
    cache_dir <- tools::R_user_dir("survey160r", "cache")
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    local_path <- file.path(cache_dir, default_name)
  } else if (!is.character(dest) || length(dest) != 1L || !nzchar(trimws(dest))) {
    stop_s160("`dest` must be a single non-empty path or directory.",
              fn = "opt_out_pull")
  } else if (dir.exists(dest)) {
    local_path <- file.path(dest, default_name)
  } else {
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    local_path <- dest
  }

  gcs_path <- sprintf("gs://%s/%s", bucket, object_name)
  if (!refresh && file.exists(local_path)) {
    message(sprintf("Using cached opt-out list (%s): %s",
                    .format_file_age(local_path), local_path))
    return(local_path)
  }

  # A download needs an authenticated GCS session; checked after the cache-hit
  # return, since reusing a local copy needs no auth (mirrors disposition_pull).
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
    s160_not_found = function(e) {
      stop_not_found("opt-out list", gcs_path, fn = "opt_out_pull")
    },
    error = function(e) {
      stop_failed(sprintf("download %s", gcs_path), conditionMessage(e),
                  fn = "opt_out_pull")
    }
  )
  if (!file.rename(tmp, local_path) &&
        !file.copy(tmp, local_path, overwrite = TRUE)) {
    stop_failed("move the downloaded file into place", local_path,
                fn = "opt_out_pull")
  }
  local_path
}
