# Stateless runner for the latency pipeline.
# Pulls the campaign CSV from GCS, derives the config from
# s160_api_campaign_get() + the CSV header (no YAML required), runs
# latency_report(), and writes the result to the analytics bucket.
#
# Pre-conditions: caller must have run s160_gcs_init(bucket = "campaign_results")
# (for the source CSV) and s160_api_auth() (for campaign metadata) in the
# current R session.

#' Run the full latency pipeline for one campaign
#'
#' Stateless: every invocation re-derives the report config from the
#' campaign API and the CSV header. No YAML required.
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
#' @param uploader Forwarded to \code{write_to_gcs()}; see its docs.
#' @return The full \code{gs://...} path written.
#' @examples
#' \dontrun{
#' s160_gcs_init(bucket = "campaign_results")
#' s160_api_auth()
#' run_latency(1980, "s160_analytics_prod")
#' run_latency(1980, "s160_analytics_prod",
#'             field_timezone = "America/New_York",
#'             project_id = 9999)
#' }
#' @export
run_latency <- function(campaign_id, bucket,
                        field_timezone = "UTC",
                        project_id = NULL,
                        texting_windows = list(),
                        date_filter = NULL,
                        respondent_id_column = NULL,
                        run_by = NULL,
                        uploader = upload_object) {
  data <- pull_csv_from_gcs(campaign_id)
  source_csv_hash <- attr(data, "source_csv_hash")
  config <- build_config_from_campaign(
    campaign_id,
    data,
    overrides = list(
      field_timezone = field_timezone,
      project_id = project_id,
      texting_windows = texting_windows,
      date_filter = date_filter,
      respondent_id_column = respondent_id_column
    )
  )
  result <- latency_report(data, config)
  write_to_gcs(
    result = result,
    campaign_id = campaign_id,
    bucket = bucket,
    source_csv_hash = source_csv_hash,
    run_by = run_by,
    uploader = uploader
  )
}
