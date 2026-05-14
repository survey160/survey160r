# Re-runs the latency pipeline for every campaign currently in the source
# bucket and writes the results to the analytics bucket. Used to refresh the
# fleet after methodology bumps (e.g., new universal-thresholds rollout).
#
# Auth: gcloud (ADC, Workload Identity, or `gcloud auth login`) for GCS.
# No Survey160 API auth needed -- run_latency derives the config from the
# CSV header alone.
#
# Usage:
#   Rscript scripts/bulk_reprocess.R [source_bucket] [bucket]
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

args <- commandArgs(trailingOnly = TRUE)
SRC_BUCKET <- if (length(args) >= 1) args[1] else "campaign_results"
DST_BUCKET <- if (length(args) >= 2) args[2] else "s160_analytics_dev"

cat(sprintf("[bulk_reprocess] src=gs://%s/  dst=gs://%s/latency/\n",
            SRC_BUCKET, DST_BUCKET))

results <- run_latency_all(
  source_bucket = SRC_BUCKET,
  bucket = DST_BUCKET,
  field_timezone = "America/New_York",
  run_by = "bulk_reprocess"
)

cat("\n=== Summary ===\n")
print(table(results$status))
cat(sprintf("Total: %d campaigns, %d ok, %d failed\n",
            nrow(results),
            sum(results$status == "ok"),
            sum(results$status == "failed")))
if (any(results$status == "failed")) {
  cat("\nFailures:\n")
  failed <- results[results$status == "failed", c("campaign_id", "error_message")]
  print(failed, row.names = FALSE)
}
