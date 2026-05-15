# I/O wrappers for the latency layer.
# Reads campaign CSV from GCS, writes a per-campaign Parquet to a dedicated
# analytics bucket, and exposes a DuckDB read view for downstream consumers.

# Object path under the analytics bucket. Single file per campaign:
# `latency/<campaign_id>_latency.parquet`. No Hive partitioning.
.latency_object_path <- function(campaign_id) {
  sprintf("latency/%s_latency.parquet", campaign_id)
}

# Pinned arrow schema. Writers fail fast on drift so methodology bumps and
# bugs cannot silently change column types or order.
#' @noRd
latency_parquet_schema <- function() {
  arrow::schema(
    campaign_id = arrow::int32(),
    project_id = arrow::int32(),
    date = arrow::date32(),
    hour_local = arrow::int32(),
    segment = arrow::utf8(),
    segment_index = arrow::int32(),
    threshold_min = arrow::int32(),
    n = arrow::int32(),
    pct_le = arrow::float64(),
    pct_resp_hit_gt = arrow::float64(),
    n_respondents = arrow::int32(),
    pct_resp_worst_gt = arrow::float64(),
    algorithm_version = arrow::utf8(),
    config_hash = arrow::utf8(),
    source_csv_hash = arrow::utf8(),
    run_at_utc = arrow::timestamp("us", "UTC"),
    run_by = arrow::utf8()
  )
}

#' Read campaign CSV from GCS for latency analysis
#'
#' Thin wrapper over \code{s160_gcs_campaign_results_read} that also computes
#' a sha256 of the downloaded CSV bytes for provenance. The hash travels back
#' on the returned object as the \code{source_csv_hash} attribute.
#'
#' @param campaign_id Campaign id (numeric or character).
#' @param filename Optional override for the CSV filename.
#' @param bucket Source GCS bucket. \code{NULL} (default) falls back to the
#'   global bucket set by \code{s160_gcs_init()}; pass an explicit value to
#'   skip the global entirely.
#' @return A data frame with attributes \code{source_csv_hash} and
#'   \code{source_csv_path} set.
#' @export
pull_csv_from_gcs <- function(campaign_id, filename = NULL, bucket = NULL) {
  bucket <- resolve_bucket(bucket)
  tmpdir <- tempfile(pattern = "s160_latency_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  data <- s160_gcs_campaign_results_read(
    campaign_id = campaign_id,
    filename = filename,
    destdir = tmpdir,
    bucket = bucket
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

# Coerce a consolidated data.frame to the pinned schema, raising on drift.
.coerce_to_schema <- function(consolidated) {
  schema <- latency_parquet_schema()
  expected <- names(schema)
  actual <- names(consolidated)
  missing_cols <- setdiff(expected, actual)
  extra_cols <- setdiff(actual, expected)
  if (length(missing_cols) > 0 || length(extra_cols) > 0) {
    stop(sprintf("Schema drift in consolidated. Missing: [%s]. Extra: [%s].",
                 paste(missing_cols, collapse = ", "),
                 paste(extra_cols, collapse = ", ")), call. = FALSE)
  }
  consolidated <- consolidated[, expected, drop = FALSE]
  arrow::Table$create(consolidated, schema = schema)
}

#' Write a latency report Parquet to GCS
#'
#' Writes the consolidated table for one campaign as
#' \code{gs://<bucket>/latency/<campaign_id>_latency.parquet} with ZSTD
#' compression and the pinned schema. Idempotent overwrite. Provenance fields
#' (algorithm_version, config_hash, source_csv_hash, run_at_utc, run_by) are
#' embedded as columns; a small set is also written as Parquet file-level
#' metadata for human inspection.
#'
#' @param result A list returned by \code{latency_report()}.
#' @param campaign_id Campaign id (numeric or character).
#' @param bucket Destination GCS bucket.
#' @param source_csv_hash Optional sha256 of the source CSV; if omitted, the
#'   value already on \code{result$consolidated} is preserved.
#' @param run_by Optional string for the run_by provenance column.
#' @param uploader Function called once the Parquet has been written locally.
#'   Signature: \code{function(local_path, object_name, bucket, metadata)}.
#'   Defaults to \code{upload_object} (real GCS upload). Pass an alternative
#'   to redirect uploads (e.g., batch scripts copying to a local staging dir,
#'   or tests capturing the call).
#' @return The full \code{gs://...} path written.
#' @export
write_to_gcs <- function(result, campaign_id, bucket,
                         source_csv_hash = NULL,
                         run_by = NULL,
                         uploader = upload_object) {
  if (!is.list(result) || is.null(result$consolidated)) {
    stop("write_to_gcs: result must include $consolidated.", call. = FALSE)
  }
  campaign_id <- as.character(campaign_id)
  if (!is.character(bucket) || length(bucket) != 1 || !nzchar(trimws(bucket))) {
    stop("bucket must be a non-empty string.", call. = FALSE)
  }

  consolidated <- result$consolidated
  n_rows <- nrow(consolidated)
  if (!is.null(source_csv_hash)) {
    consolidated$source_csv_hash <- rep(source_csv_hash, n_rows)
  }
  if (!is.null(run_by)) {
    consolidated$run_by <- rep(run_by, n_rows)
  } else if (n_rows > 0 && all(is.na(consolidated$run_by))) {
    consolidated$run_by <- rep(as.character(Sys.info()[["user"]]), n_rows)
  }

  consolidated <- consolidated[order(
    consolidated$segment_index,
    consolidated$threshold_min,
    is.na(consolidated$hour_local),
    consolidated$hour_local
  ), , drop = FALSE]

  table <- .coerce_to_schema(consolidated)

  meta_pairs <- list(
    `survey160.algorithm_version` = result$meta$algorithm_version %||% "unknown",
    `survey160.config_hash` = result$meta$config_hash %||% "unknown",
    `survey160.schema_version` = result$meta$schema_version %||% "1",
    `survey160.source_csv_hash` =
      if (!is.null(source_csv_hash)) source_csv_hash
      else if (!all(is.na(consolidated$source_csv_hash))) consolidated$source_csv_hash[[1]]
      else "unknown"
  )

  tmp_path <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp_path), add = TRUE)
  arrow::write_parquet(
    table, tmp_path,
    compression = "zstd",
    compression_level = 3,
    use_dictionary = TRUE,
    write_statistics = TRUE
  )

  object_name <- .latency_object_path(campaign_id)
  uploader(local_path = tmp_path, object_name = object_name, bucket = bucket,
           metadata = meta_pairs)
  sprintf("gs://%s/%s", bucket, object_name)
}

# Upload a local file to GCS at <bucket>/<object_name>. Writes the per-bucket
# upload via googleCloudStorageR. `metadata` is set as object metadata for
# human inspection in the GCS console. Marked # nocov because it is a thin
# wrapper around a real network call; tests mock at the write_to_gcs(uploader=)
# seam instead (see test-latency_io.R).
upload_object <- function(local_path, object_name, bucket, metadata) { # nocov start
  googleCloudStorageR::gcs_upload(
    file = local_path,
    bucket = bucket,
    name = object_name,
    object_metadata = googleCloudStorageR::gcs_metadata_object(
      object_name = object_name,
      metadata = metadata
    ),
    predefinedAcl = "bucketLevel"
  )
  invisible(NULL)
} # nocov end

#' GCS metadata for a campaign's latency Parquet output
#'
#' Returns object metadata (\code{name}, \code{updated}, \code{size}) for
#' \code{gs://<bucket>/latency/<campaign_id>_latency.parquet}, or \code{NULL}
#' if no such object exists. Used by \code{run_latency_all(skip_unchanged =
#' TRUE)} to decide whether a campaign's existing output is newer than its
#' source CSV.
#'
#' @param campaign_id Campaign id (numeric or character).
#' @param bucket Destination GCS bucket containing the latency output.
#' @return Named list with \code{name}, \code{updated} (POSIXct), and
#'   \code{size}, or \code{NULL}.
#' @importFrom googleCloudStorageR gcs_list_objects
#' @export
s160_gcs_latency_output_status <- function(campaign_id, bucket) {
  campaign_id <- as.character(campaign_id)
  if (!is.character(bucket) || length(bucket) != 1L ||
        !nzchar(trimws(bucket))) {
    stop("bucket must be a non-empty string.", call. = FALSE)
  }
  object_name <- .latency_object_path(campaign_id)
  # List the `latency/` folder rather than passing the full object name as a
  # prefix: prefix-matching would also return `<name>.bak` / `<name>v2`
  # variants, and the parent listing is the same one-round-trip cost.
  objects <- tryCatch(
    gcs_list_objects(prefix = "latency/", bucket = bucket),
    error = function(e) {
      stop(sprintf("Failed to list latency output for %s: %s",
                   campaign_id, conditionMessage(e)), call. = FALSE)
    }
  )
  if (nrow(objects) == 0) return(NULL)
  match_idx <- which(objects$name == object_name)
  if (length(match_idx) == 0) return(NULL)
  list(
    name = object_name,
    updated = objects$updated[match_idx[1]],
    size = objects$size[match_idx[1]]
  )
}

#' Read latency Parquet output from GCS via DuckDB
#'
#' Returns a DuckDB connection and a view name (\code{latency}) over
#' \code{gs://<bucket>/latency/*_latency.parquet}. The caller is responsible
#' for closing the connection.
#'
#' @param bucket Source GCS bucket name.
#' @param connection Optional pre-existing DuckDB connection.
#' @return A list with \code{con} (DBI connection) and \code{view} (string).
#' @export
read_latency <- function(bucket, connection = NULL) { # nocov start
  if (!requireNamespace("duckdb", quietly = TRUE) ||
      !requireNamespace("DBI", quietly = TRUE)) {
    stop("read_latency requires the 'duckdb' and 'DBI' packages.", call. = FALSE)
  }
  con <- connection %||% DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")
  glob <- sprintf("gs://%s/latency/*_latency.parquet", bucket)
  DBI::dbExecute(con, sprintf(
    "CREATE OR REPLACE VIEW latency AS SELECT * FROM read_parquet('%s');",
    glob
  ))
  list(con = con, view = "latency")
} # nocov end
