# User-facing entry point for the latency algorithm.
# Pure composition of latency_build_config() + latency_report() over a
# caller-supplied data frame. No I/O: pair with s160_gcs_campaign_results_read(hash = TRUE) for
# the GCS source path, or bring your own data frame from anywhere
# (Dropbox, local disk, S3, an email attachment).

#' Run the latency report for one campaign
#'
#' User-facing one-campaign runner. Given an in-memory campaign CSV
#' (already read by the caller), (optionally) builds the report config
#' from the CSV header and runs the latency algorithm. No I/O; pair
#' with \code{s160_gcs_campaign_results_read(hash = TRUE)} for the GCS source path or
#' \code{read.csv()} / \code{readr::read_csv()} / anything else for
#' off-GCS sources.
#'
#' Two call shapes:
#'
#' \itemize{
#'   \item Convenience -- omit \code{config} and pass any
#'         \code{latency_build_config()} overrides through \code{...}.
#'         \code{latency_run()} derives the config from the CSV header.
#'   \item Custom -- pre-build the config with
#'         \code{latency_build_config()} (mutating as needed) and pass
#'         it via \code{config}. \code{...} must be empty in that case
#'         (passing both errors).
#' }
#'
#' Provenance: if \code{data} carries \code{source_csv_hash} or
#' \code{source_csv_path} attributes (set by \code{s160_gcs_campaign_results_read(hash = TRUE)}
#' for GCS reads), \code{latency_report()} surfaces them on
#' \code{result$meta}. Callers pulling CSVs from other sources can
#' attach the attributes themselves before calling, e.g.
#' \preformatted{
#' attr(df, "source_csv_path") <- "dropbox:campaign_1234.csv"
#' attr(df, "source_csv_hash") <- paste0(
#'   "sha256:", digest::digest(file = local_path, algo = "sha256"))
#' }
#'
#' @param campaign_id Campaign id (numeric or character).
#' @param data In-memory campaign CSV as a data frame (one row per
#'   respondent, columns named \code{id.<q>.scriptDate} /
#'   \code{id.<q>.batchDate} per question plus the population-filter
#'   column \code{id.intro.finalText} and the campaign id column).
#' @param config Optional pre-built config. When \code{NULL} (default),
#'   the config is auto-built from \code{data}'s header; pass any
#'   \code{latency_build_config()} overrides (\code{field_timezone},
#'   \code{project_id}, \code{date_filter}, \code{respondent_id_column})
#'   through \code{...}. Mutually exclusive with \code{...}.
#' @param run_at Optional \code{POSIXct} timestamp stamped on every
#'   row's \code{run_at_utc} column. \code{NULL} (default) uses
#'   \code{Sys.time()}. Fleet runners pass one stamp here so every
#'   campaign in a pass shares the same \code{run_at_utc}.
#' @param run_by Optional string stamped on every row's \code{run_by}
#'   provenance column. \code{NULL} (default) leaves the column as
#'   \code{NA_character_}; typically filled at write time by the step
#'   that persists the result.
#' @param ... Forwarded to \code{latency_build_config()} when
#'   \code{config} is \code{NULL}. Must be empty when \code{config} is
#'   supplied (passing both errors).
#' @return The list returned by \code{latency_report()}:
#'   \code{consolidated}, \code{latency_frame}, \code{diagnostics},
#'   \code{meta} (with \code{source_csv_hash} and
#'   \code{source_csv_path} from \code{data}'s attributes, or \code{NA}
#'   when absent).
#' @examples
#' \dontrun{
#' # GCS source -- pair with s160_gcs_campaign_results_read(hash = TRUE).
#' s160_gcs_init(bucket = "campaign_results")
#' data   <- s160_gcs_campaign_results_read(1980, hash = TRUE)
#' result <- latency_run(1980, data, field_timezone = "America/New_York")
#' result$meta$source_csv_hash
#'
#' # Off-GCS source -- bring your own CSV.
#' data   <- read.csv("~/Dropbox/campaign_1980.csv", stringsAsFactors = FALSE)
#' result <- latency_run(1980, data)
#'
#' # Custom config (mutate before running).
#' config <- latency_build_config(1980, data, field_timezone = "America/New_York")
#' config$flow$questions <- c("intro", "q1_custom")
#' latency_run(1980, data, config = config)
#' }
#' @export
latency_run <- function(campaign_id, data,
                        config = NULL,
                        run_at = NULL,
                        run_by = NULL,
                        ...) {
  if (is.null(config)) {
    config <- latency_build_config(campaign_id, data, ...)
  } else if (...length() > 0L) {
    stop_s160(paste0("pass either `config` or `latency_build_config()` ",
                     "overrides via `...`, not both."),
              fn = "latency_run")
  }
  result <- latency_report(data, config, run_at = run_at)
  if (!is.null(run_by) && !is.null(result$consolidated) &&
        nrow(result$consolidated) > 0L) {
    result$consolidated$run_by <- rep(run_by, nrow(result$consolidated))
  }
  result
}
