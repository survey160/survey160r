# Analyst-facing entry point for the latency algorithm.
# Pulls the campaign CSV from GCS, derives the report config from its
# header (unless an explicit one is supplied), runs the algorithm, and
# returns the in-memory result. No GCS writes -- fleet orchestration
# and Parquet emission live in survey160-shiny (scripts/run_latency.R).

#' Run the latency report for one campaign
#'
#' Analyst-facing one-campaign runner. Reads the campaign CSV from GCS,
#' (optionally) builds the report config from the CSV header, runs the
#' latency algorithm, and returns the in-memory result. No GCS writes.
#'
#' Two call shapes:
#'
#' \itemize{
#'   \item Convenience -- omit \code{config} and pass any
#'         \code{latency_build_config()} overrides through \code{...}.
#'         \code{latency_run()} derives the config from the CSV header.
#'   \item Custom -- pre-build the config with
#'         \code{latency_build_config()} (mutating as needed) and pass it
#'         via \code{config}. \code{...} is ignored in that case.
#' }
#'
#' Provenance fields (sha256 of the source CSV, canonical \code{gs://}
#' path) are populated by \code{latency_report()} onto
#' \code{result$meta$source_csv_hash} and
#' \code{result$meta$source_csv_path}; downstream persistence layers
#' read them from there.
#'
#' @param campaign_id Campaign id (numeric or character).
#' @param bucket Source GCS bucket containing the campaign CSV.
#'   \code{NULL} (default) falls back to the global bucket set by
#'   \code{s160_gcs_init()}; pass an explicit value to skip the global.
#' @param config Optional pre-built config (the list returned by
#'   \code{latency_build_config()}). When \code{NULL} (default), the
#'   config is auto-built from the CSV header; pass any
#'   \code{latency_build_config()} overrides (\code{field_timezone},
#'   \code{project_id}, \code{date_filter}, \code{respondent_id_column})
#'   through \code{...}. Mutually exclusive with \code{...} arguments
#'   forwarded to \code{latency_build_config()}.
#' @param run_at Optional \code{POSIXct} timestamp stamped on every
#'   row's \code{run_at_utc} column. \code{NULL} (default) uses
#'   \code{Sys.time()}. Fleet runners pass one stamp here so every
#'   campaign in a pass shares the same \code{run_at_utc}.
#' @param run_by Optional string stamped on every row's \code{run_by}
#'   provenance column. \code{NULL} (default) leaves the column as
#'   \code{NA_character_}; callers persisting the result typically fill
#'   it at write time.
#' @param ... Forwarded to \code{latency_build_config()} when
#'   \code{config} is \code{NULL}. Ignored otherwise.
#' @return The list returned by \code{latency_report()}:
#'   \code{consolidated}, \code{latency_frame}, \code{diagnostics},
#'   \code{meta} (with \code{source_csv_hash} and \code{source_csv_path}
#'   populated).
#' @examples
#' \dontrun{
#' s160_gcs_init(bucket = "campaign_results")
#'
#' # Convenience: zero-config.
#' result <- latency_run(1980)
#' head(result$consolidated)
#' result$meta$source_csv_hash
#'
#' # Convenience with overrides forwarded to latency_build_config().
#' latency_run(1980, field_timezone = "America/New_York", project_id = 9999)
#'
#' # Custom: pre-build the config so you can mutate it.
#' data   <- s160_gcs_pull_csv(1980)
#' config <- latency_build_config(1980, data, field_timezone = "America/New_York")
#' config$flow$questions <- c("intro", "q1_custom")
#' latency_run(1980, config = config)
#' }
#' @export
latency_run <- function(campaign_id,
                        bucket = NULL,
                        config = NULL,
                        run_at = NULL,
                        run_by = NULL,
                        ...) {
  data <- s160_gcs_pull_csv(campaign_id, bucket = bucket)
  if (is.null(config)) {
    config <- latency_build_config(campaign_id, data, ...)
  } else if (...length() > 0L) {
    stop("latency_run: pass either `config` or `latency_build_config()` ",
         "overrides via `...`, not both.", call. = FALSE)
  }
  result <- latency_report(data, config, run_at = run_at)
  if (!is.null(run_by) && !is.null(result$consolidated) &&
        nrow(result$consolidated) > 0L) {
    result$consolidated$run_by <- rep(run_by, nrow(result$consolidated))
  }
  result
}
