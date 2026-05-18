# Analyst-facing entry point for the latency algorithm.
# Pulls the campaign CSV from GCS, derives the report config from its
# header, runs the algorithm, and returns the in-memory result. No GCS
# writes -- fleet orchestration and Parquet emission live in
# survey160-shiny (scripts/run_latency.R).

#' Run the latency report for one campaign
#'
#' Analyst-facing one-campaign runner. Pulls the campaign CSV from GCS,
#' derives the report config from the CSV header, runs the latency
#' algorithm, and returns the in-memory result. No GCS writes.
#'
#' Sensible defaults are baked in; override per-call when needed. The
#' \code{field_timezone} default is \code{"UTC"} (matches the CSV format
#' and is fully reproducible); operator-local dashboards typically pass
#' \code{"America/New_York"}. The \code{project_id} default is the
#' campaign id (placeholder); callers that know the real Survey160
#' project id should pass it explicitly.
#'
#' The sha256 of the source CSV is attached to the returned object as
#' the \code{source_csv_hash} attribute so downstream writers can stamp
#' it on persisted output without re-hashing.
#'
#' @param campaign_id Campaign id (numeric or character).
#' @param source_bucket Source GCS bucket containing the campaign CSV.
#'   \code{NULL} (default) falls back to the global bucket set by
#'   \code{s160_gcs_init()}; pass an explicit value to skip the global.
#' @param field_timezone Tz used to bucket the result's \code{date} and
#'   \code{hour_local} columns. Default \code{"UTC"}.
#' @param project_id Optional Survey160 project id; defaults to the
#'   campaign id (placeholder, see Details).
#' @param date_filter Optional character/Date vector restricting which
#'   survey dates are processed (in \code{field_timezone}).
#' @param respondent_id_column Optional column name used to dedupe rows by
#'   respondent. Default \code{NULL} (no dedupe).
#' @param run_by Optional string stamped on every row's \code{run_by}
#'   provenance column. \code{NULL} (default) leaves the column as
#'   \code{NA_character_}; callers persisting the result typically fill
#'   it at write time.
#' @param run_at Optional \code{POSIXct} timestamp stamped on every row's
#'   \code{run_at_utc} column. \code{NULL} (default) uses
#'   \code{Sys.time()}. Fleet runners pass one stamp here so every
#'   campaign in a pass shares the same \code{run_at_utc}.
#' @return The list returned by \code{latency_report()}: \code{consolidated}
#'   (the per-campaign result frame), \code{latency_frame},
#'   \code{diagnostics}, and \code{meta}. The source CSV's sha256 is
#'   attached as the \code{source_csv_hash} attribute on this list.
#' @examples
#' \dontrun{
#' s160_gcs_init(bucket = "campaign_results")
#' result <- run_latency(1980)
#' head(result$consolidated)
#'
#' # Operator-local bucketing, explicit project id.
#' run_latency(1980,
#'             field_timezone = "America/New_York",
#'             project_id = 9999)
#' }
#' @export
run_latency <- function(campaign_id,
                        source_bucket = NULL,
                        field_timezone = "UTC",
                        project_id = NULL,
                        date_filter = NULL,
                        respondent_id_column = NULL,
                        run_by = NULL,
                        run_at = NULL) {
  data <- pull_csv_from_gcs(campaign_id, bucket = source_bucket)
  source_csv_hash <- attr(data, "source_csv_hash")
  config <- latency_build_config(
    campaign_id, data,
    field_timezone = field_timezone,
    project_id = project_id,
    date_filter = date_filter,
    respondent_id_column = respondent_id_column
  )
  result <- latency_report(data, config, run_at = run_at)
  if (!is.null(run_by) && !is.null(result$consolidated) &&
        nrow(result$consolidated) > 0L) {
    result$consolidated$run_by <- rep(run_by, nrow(result$consolidated))
  }
  attr(result, "source_csv_hash") <- source_csv_hash
  result
}
