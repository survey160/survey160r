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
#' @return A data frame with attribute \code{source_csv_hash} set.
#' @export
pull_csv_from_gcs <- function(campaign_id, filename = NULL) {
  tmpdir <- tempfile(pattern = "s160_latency_")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  data <- s160_gcs_campaign_results_read(
    campaign_id = campaign_id,
    filename = filename,
    destdir = tmpdir
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
#' @return The full \code{gs://...} path written.
#' @export
write_to_gcs <- function(result, campaign_id, bucket,
                         source_csv_hash = NULL,
                         run_by = NULL) {
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
  upload_object(local_path = tmp_path, object_name = object_name, bucket = bucket,
                metadata = meta_pairs)
  sprintf("gs://%s/%s", bucket, object_name)
}

# Upload a local file to GCS at <bucket>/<object_name>. Writes the per-bucket
# upload via googleCloudStorageR. `metadata` is set as object metadata for
# human inspection in the GCS console.
upload_object <- function(local_path, object_name, bucket, metadata) {
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
