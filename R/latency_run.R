# Stateless runner for the latency pipeline.
# Pulls the campaign CSV from GCS, derives the config from the CSV header,
# runs latency_report(), and writes the result to the analytics bucket.
#
# Pre-condition: caller must have run s160_gcs_init(bucket = "campaign_results")
# in the current R session. No API auth needed -- the config is derived from
# the CSV alone.

#' Run the full latency pipeline for one campaign
#'
#' Stateless: every invocation re-derives the report config from the CSV
#' header. No YAML, no API call, no auth precondition beyond GCS.
#'
#' Sensible defaults are baked in; override per-call when needed. The
#' \code{field_timezone} default is \code{"UTC"} (matches the CSV format and
#' is fully reproducible); production callers running operator-local
#' dashboards typically pass \code{"America/New_York"}. The \code{project_id}
#' default is the campaign id, matching the legacy \code{bulk_reprocess.R}
#' placeholder behaviour; callers that know the real Survey160 project id
#' should pass it explicitly.
#'
#' @param campaign_id Campaign id (numeric or character).
#' @param bucket Destination analytics bucket.
#' @param source_bucket Source GCS bucket containing the campaign CSV.
#'   \code{NULL} (default) falls back to the global bucket set by
#'   \code{s160_gcs_init()}; pass an explicit value to skip the global.
#' @param field_timezone Tz used to bucket the Parquet \code{date} and
#'   \code{hour_local} columns. Default \code{"UTC"}.
#' @param project_id Optional Survey160 project id; defaults to the
#'   campaign id (placeholder, see Details).
#' @param texting_windows Optional list of \code{{date, start_hour, end_hour}}
#'   windows for the spec's out-of-window filtering. Default \code{list()}
#'   means "all-in-window" (no filtering).
#' @param date_filter Optional character/Date vector restricting which
#'   survey dates are processed (in \code{field_timezone}).
#' @param respondent_id_column Optional column name used to dedupe rows by
#'   respondent. Default \code{NULL} (no dedupe; matches legacy R scripts).
#' @param run_by Optional string for the \code{run_by} provenance column.
#' @param run_at Optional \code{POSIXct} timestamp to stamp on every row's
#'   \code{run_at_utc} column. \code{NULL} (default) uses \code{Sys.time()}.
#'   \code{run_latency_all()} passes one stamp here so every campaign in a
#'   fleet pass shares the same \code{run_at_utc}.
#' @param uploader Forwarded to \code{write_to_gcs()}; see its docs.
#' @return The full \code{gs://...} path written.
#' @examples
#' \dontrun{
#' s160_gcs_init(bucket = "campaign_results")
#' run_latency(1980, "s160_analytics_prod")
#' run_latency(1980, "s160_analytics_prod",
#'             field_timezone = "America/New_York",
#'             project_id = 9999)
#' }
#' @export
run_latency <- function(campaign_id, bucket,
                        source_bucket = NULL,
                        field_timezone = "UTC",
                        project_id = NULL,
                        texting_windows = list(),
                        date_filter = NULL,
                        respondent_id_column = NULL,
                        run_by = NULL,
                        run_at = NULL,
                        uploader = upload_object) {
  data <- pull_csv_from_gcs(campaign_id, bucket = source_bucket)
  source_csv_hash <- attr(data, "source_csv_hash")
  config <- latency_build_config(
    campaign_id, data,
    field_timezone = field_timezone,
    project_id = project_id,
    texting_windows = texting_windows,
    date_filter = date_filter,
    respondent_id_column = respondent_id_column
  )
  result <- latency_report(data, config, run_at = run_at)
  write_to_gcs(
    result = result,
    campaign_id = campaign_id,
    bucket = bucket,
    source_csv_hash = source_csv_hash,
    run_by = run_by,
    uploader = uploader
  )
}

#' Run the latency pipeline for every campaign in a source bucket
#'
#' Discovers every campaign with an export CSV under \code{source_bucket} via
#' \code{s160_gcs_campaign_results_list()} and runs \code{run_latency()} for
#' each, writing the per-campaign Parquet to \code{bucket}. Per-campaign
#' failures are caught and recorded; the loop continues so one bad CSV does
#' not block the rest of the fleet.
#'
#' Reads and writes use explicit \code{bucket} arguments throughout, so this
#' function never touches the session-global GCS bucket.
#'
#' All override arguments (\code{field_timezone}, \code{texting_windows},
#' \code{date_filter}, \code{respondent_id_column}) apply uniformly to
#' every campaign. \code{project_id} is always set to \code{campaign_id}
#' (placeholder) -- callers needing per-campaign project ids should iterate
#' \code{run_latency()} themselves with their own mapping.
#'
#' @param source_bucket Source GCS bucket containing per-campaign CSV exports
#'   (e.g. \code{"campaign_results"}).
#' @param bucket Destination GCS bucket for the Parquet outputs
#'   (e.g. \code{"s160_analytics_dev"}).
#' @param campaign_ids Optional character/numeric vector of campaign ids to
#'   process. \code{NULL} (default) processes every campaign found in
#'   \code{source_bucket}.
#' @param field_timezone Forwarded to \code{run_latency()}. Default
#'   \code{"UTC"}; pass \code{"America/New_York"} to match historical fleet
#'   bucketing.
#' @param texting_windows,date_filter,respondent_id_column Forwarded to
#'   \code{run_latency()}.
#' @param run_by Forwarded to \code{run_latency()}. Default
#'   \code{"run_latency_all"}.
#' @param uploader Forwarded to \code{run_latency()}; see \code{write_to_gcs}.
#' @param continue_on_error Logical. \code{TRUE} (default) records the
#'   error and moves on; \code{FALSE} re-raises the first error.
#' @return A data frame with one row per attempted campaign:
#'   \code{campaign_id}, \code{status} (\code{"ok"} / \code{"failed"}),
#'   \code{parquet_uri} (NA on failure), \code{error_message} (NA on
#'   success), \code{elapsed_s}.
#' @examples
#' \dontrun{
#' s160_gcs_init(bucket = "campaign_results")  # any session GCS auth works
#' results <- run_latency_all(
#'   source_bucket = "campaign_results",
#'   bucket = "s160_analytics_dev",
#'   field_timezone = "America/New_York",
#'   run_by = "bulk_reprocess"
#' )
#' subset(results, status == "failed")
#' }
#' @export
run_latency_all <- function(source_bucket, bucket,
                            campaign_ids = NULL,
                            field_timezone = "UTC",
                            texting_windows = list(),
                            date_filter = NULL,
                            respondent_id_column = NULL,
                            run_by = "run_latency_all",
                            uploader = upload_object,
                            continue_on_error = TRUE) {
  if (!is.character(source_bucket) || length(source_bucket) != 1L ||
        !nzchar(trimws(source_bucket))) {
    stop("source_bucket must be a non-empty string.", call. = FALSE)
  }
  if (!is.character(bucket) || length(bucket) != 1L ||
        !nzchar(trimws(bucket))) {
    stop("bucket must be a non-empty string.", call. = FALSE)
  }

  if (is.null(campaign_ids)) {
    campaign_ids <- s160_gcs_campaign_results_list(bucket = source_bucket)
  }
  campaign_ids <- as.character(campaign_ids)

  # Single fleet-wide timestamp. Every per-campaign Parquet in this run
  # carries the same `run_at_utc`, so "show me the latest fleet pass" is a
  # one-liner: SELECT * FROM latency WHERE run_at_utc = (SELECT MAX(...)).
  fleet_run_at <- Sys.time()
  attr(fleet_run_at, "tzone") <- "UTC"

  results <- vector("list", length(campaign_ids))
  for (i in seq_along(campaign_ids)) {
    cid <- campaign_ids[[i]]
    message(sprintf("[%d/%d] %s", i, length(campaign_ids), cid))
    results[[i]] <- .run_one_campaign(
      cid, source_bucket, bucket, field_timezone, texting_windows,
      date_filter, respondent_id_column, run_by, fleet_run_at, uploader,
      continue_on_error
    )
  }
  do.call(rbind, results)
}

# Single-campaign worker for run_latency_all(). Extracted to keep the outer
# function's cyclomatic complexity under the linter threshold.
.run_one_campaign <- function(cid, source_bucket, bucket, field_timezone,
                              texting_windows, date_filter,
                              respondent_id_column, run_by, run_at, uploader,
                              continue_on_error) {
  t0 <- Sys.time()
  path <- tryCatch(
    run_latency(
      campaign_id = cid,
      bucket = bucket,
      source_bucket = source_bucket,
      field_timezone = field_timezone,
      texting_windows = texting_windows,
      date_filter = date_filter,
      respondent_id_column = respondent_id_column,
      run_by = run_by,
      run_at = run_at,
      uploader = uploader
    ),
    error = function(e) {
      if (!continue_on_error) stop(e)
      e
    }
  )
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (inherits(path, "error")) {
    message(sprintf("  failed: %s", conditionMessage(path)))
    return(data.frame(
      campaign_id = cid, status = "failed",
      parquet_uri = NA_character_,
      error_message = conditionMessage(path),
      elapsed_s = elapsed,
      stringsAsFactors = FALSE
    ))
  }
  message(sprintf("  ok: %s (%.1fs)", path, elapsed))
  data.frame(
    campaign_id = cid, status = "ok",
    parquet_uri = path,
    error_message = NA_character_,
    elapsed_s = elapsed,
    stringsAsFactors = FALSE
  )
}
