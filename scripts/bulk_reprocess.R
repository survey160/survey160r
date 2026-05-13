# Re-runs the latency pipeline for every campaign currently in the source
# bucket and writes the results to the analytics bucket. Used to refresh the
# fleet after methodology bumps (e.g., new universal-thresholds rollout).
#
# Auth: gcloud (ADC, Workload Identity, or `gcloud auth login`).
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
  library(dplyr)
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

# Discover the survey flow from a CSV header. Drops terminal states and
# preserves column order, which is the survey flow order.
discover_questions <- function(csv_path) {
  hdr <- readLines(csv_path, n = 1)
  cols <- strsplit(hdr, ",", fixed = TRUE)[[1]]
  cols <- gsub('^"|"$', "", cols)
  m <- regmatches(cols, regexec("id\\[([a-z0-9_]+)\\]scriptDate", cols))
  qs <- vapply(m, function(x) if (length(x) == 2) x[2] else NA_character_,
               character(1))
  qs <- qs[!is.na(qs)]
  qs <- qs[!qs %in% c("ineligible", "refusal")]
  unique(qs)
}

# Build a config without per-campaign thresholds (universal in v0.7.1+).
# NOTE: There is no campaign-id -> project-id mapping available here, so
# project_id is set to campaign_id as a placeholder. Outputs from this bulk
# run will have project_id == campaign_id, which is wrong for any
# project-level rollup. Production runs must use a per-wave YAML config that
# carries the real project_id; this script is for methodology refresh of the
# *_latency.parquet files only.
build_config <- function(campaign_id, questions) {
  list(
    project_id = as.integer(campaign_id),
    project_name = sprintf("Campaign %s (auto)", campaign_id),
    campaign_id = as.integer(campaign_id),
    field_timezone = "America/New_York",
    flow = list(questions = questions),
    filters = list(
      population = 'id.intro.finalText == "Yes"',
      campaign_id_column = "campaignid",
      respondent_id_column = NULL,
      date_filter = NULL
    ),
    texting_windows = list(),  # all-in-window
    reports = list(time_bucket = "day")
  )
}

# Run latency_report and upload via gcloud cp (avoids googleCloudStorageR
# auth complexity for this batch job; production run path can use the
# package's write_to_gcs once IAM is wired).
process_one <- function(campaign_id, src_bucket, dst_bucket, work_dir) {
  csv_uri <- sprintf("gs://%s/%s/%s_raw_data_download.csv",
                     src_bucket, campaign_id, campaign_id)
  csv_local <- file.path(work_dir, sprintf("%s.csv", campaign_id))
  rc <- system2("gcloud",
                c("storage", "cp", csv_uri, csv_local),
                stdout = NULL, stderr = NULL)
  if (rc != 0 || !file.exists(csv_local)) {
    return(list(campaign_id = campaign_id, status = "csv_missing", rows = 0))
  }
  questions <- tryCatch(discover_questions(csv_local),
                        error = function(e) character(0))
  if (length(questions) < 2) {
    unlink(csv_local)
    return(list(campaign_id = campaign_id, status = "flow_too_short",
                rows = 0))
  }
  data <- tryCatch(
    read.csv(csv_local, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(data)) {
    unlink(csv_local)
    return(list(campaign_id = campaign_id, status = "csv_read_failed",
                rows = 0))
  }
  csv_hash <- paste0("sha256:", digest::digest(file = csv_local,
                                                algo = "sha256"))
  config <- build_config(campaign_id, questions)
  result <- tryCatch(latency_report(data, config),
                     error = function(e) {
                       message(sprintf("[%s] latency_report failed: %s",
                                       campaign_id, e$message))
                       NULL
                     })
  unlink(csv_local)
  if (is.null(result)) {
    return(list(campaign_id = campaign_id, status = "report_failed",
                rows = 0))
  }
  parquet_local <- file.path(work_dir,
                             sprintf("%s_latency.parquet", campaign_id))
  local_uploader <- function(local_path, object_name, bucket, metadata) {
    file.copy(local_path, parquet_local, overwrite = TRUE)
    invisible(NULL)
  }
  write_to_gcs(result = result,
               campaign_id = campaign_id,
               bucket = dst_bucket,
               source_csv_hash = csv_hash,
               run_by = "bulk_reprocess",
               uploader = local_uploader)
  parquet_uri <- sprintf("gs://%s/latency/%s_latency.parquet",
                         dst_bucket, campaign_id)
  rc <- system2("gcloud",
                c("storage", "cp", parquet_local, parquet_uri),
                stdout = NULL, stderr = NULL)
  unlink(parquet_local)
  list(
    campaign_id = campaign_id,
    status = if (rc == 0) "ok" else "upload_failed",
    rows = nrow(result$consolidated),
    flow = paste(questions, collapse = " -> ")
  )
}

# --- Main ------------------------------------------------------------------

work_dir <- tempfile("bulk_reprocess_")
dir.create(work_dir)
on.exit(unlink(work_dir, recursive = TRUE))

campaigns <- list_campaigns(SRC_BUCKET)
cat(sprintf("[bulk_reprocess] found %d campaigns\n\n", length(campaigns)))
results <- list()
for (cid in campaigns) {
  cat(sprintf("[%s] ", cid))
  r <- process_one(cid, SRC_BUCKET, DST_BUCKET, work_dir)
  cat(sprintf("%s (rows=%d)%s\n",
              r$status, r$rows,
              if (!is.null(r$flow) && r$status == "ok")
                paste0("  flow=", r$flow)
              else ""))
  results[[length(results) + 1L]] <- r
}

cat("\n=== Summary ===\n")
status_tbl <- table(vapply(results, `[[`, character(1), "status"))
print(status_tbl)
cat(sprintf("Total consolidated rows: %d\n",
            sum(vapply(results, `[[`, integer(1), "rows"))))
