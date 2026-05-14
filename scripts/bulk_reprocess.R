# Re-runs the latency pipeline for every campaign currently in the source
# bucket and writes the results to the analytics bucket. Used to refresh the
# fleet after methodology bumps (e.g., new universal-thresholds rollout).
#
# Auth: gcloud (ADC, Workload Identity, or `gcloud auth login`) for GCS.
# No Survey160 API auth needed -- run_latency derives the config from the
# CSV header alone.
#
# Usage:
#   Rscript scripts/bulk_reprocess.R [src_bucket] [dst_bucket]
#   defaults: src=campaign_results dst=s160_analytics_dev

# Find the package root (parent of `scripts/`) regardless of where this is
# invoked from. Falls back to cwd if the script is sourced interactively.
.script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  m <- regmatches(args, regexpr("(?<=--file=).+", args, perl = TRUE))
  if (length(m) > 0L) return(normalizePath(dirname(m), mustWork = FALSE))
  getwd()
}
suppressPackageStartupMessages({
  pkgload::load_all(file.path(.script_dir(), ".."))
})

# --- Args ----
args <- commandArgs(trailingOnly = TRUE)
SRC_BUCKET <- if (length(args) >= 1) args[1] else "campaign_results"
DST_BUCKET <- if (length(args) >= 2) args[2] else "s160_analytics_dev"

cat(sprintf("[bulk_reprocess] src=gs://%s/  dst=gs://%s/latency/\n",
            SRC_BUCKET, DST_BUCKET))

# --- Helpers ---------------------------------------------------------------

# List campaign IDs that have a `<id>/<id>_raw_data_download.csv` object.
list_campaigns <- function(bucket) {
  uris <- system2("gcloud",
                  c("storage", "ls", sprintf("gs://%s/", bucket)),
                  stdout = TRUE, stderr = TRUE)
  ids <- sub("/$", "", basename(uris))
  ids[grepl("^[0-9]+$", ids)]
}

# Run latency for one campaign, returning a status row. run_latency handles
# the pull + config + report + write internally; we wrap it for batch error
# isolation and per-campaign status reporting.
process_one <- function(campaign_id, dst_bucket) {
  result <- tryCatch(
    run_latency(
      campaign_id = campaign_id,
      bucket = dst_bucket,
      field_timezone = "America/New_York",
      run_by = "bulk_reprocess"
    ),
    error = function(e) {
      message(sprintf("[%s] %s", campaign_id, conditionMessage(e)))
      NULL
    }
  )
  list(
    campaign_id = campaign_id,
    status = if (!is.null(result)) "ok" else "failed",
    parquet_uri = result
  )
}

# --- Main ------------------------------------------------------------------

s160_gcs_init(bucket = SRC_BUCKET)

campaigns <- list_campaigns(SRC_BUCKET)
cat(sprintf("[bulk_reprocess] found %d campaigns\n\n", length(campaigns)))
results <- list()
for (cid in campaigns) {
  cat(sprintf("[%s] ", cid))
  r <- process_one(cid, DST_BUCKET)
  cat(sprintf("%s%s\n",
              r$status,
              if (!is.null(r$parquet_uri))
                paste0(" -> ", r$parquet_uri)
              else ""))
  results[[length(results) + 1L]] <- r
}

cat("\n=== Summary ===\n")
status_tbl <- table(vapply(results, `[[`, character(1), "status"))
print(status_tbl)
