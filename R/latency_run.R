# Convenience runner for the manual v2.0 workflow.
# Wraps pull_csv_from_gcs + latency_report + write_to_gcs into a single call
# for a campaign. The pure latency_report() remains the recommended entry
# point for tests and ad-hoc analysis; this function is sugar.

#' Run the full latency pipeline for one campaign
#'
#' @param campaign_id Campaign id (numeric or character).
#' @param config_path Path to a YAML config matching \code{latency_scripts.md}
#'   §4 schema.
#' @param bucket Destination analytics bucket.
#' @param run_by Optional string for the run_by provenance column.
#' @param uploader Forwarded to \code{write_to_gcs()}; see its docs.
#' @return The full \code{gs://...} path written.
#' @export
run_latency <- function(campaign_id, config_path, bucket, run_by = NULL,
                        uploader = upload_object) {
  config <- read_config(config_path)
  data <- pull_csv_from_gcs(campaign_id)
  source_csv_hash <- attr(data, "source_csv_hash")
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
