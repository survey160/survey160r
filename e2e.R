# End-to-end test for survey160r
#
# Runs against real GCS and the Survey160 API (QA environment).
# Requires ~/.Renviron with: S160_GCS_CLIENT_SECRET, S160_API_USERID,
#   S160_API_KEY. Also requires a cached OAuth token.
#
# Usage:  make e2e

# -- Test harness --------------------------------------------------------------

passed <- 0L
failed <- 0L
errors <- character()

check <- function(desc, expr) {
  result <- tryCatch(
    {
      ok <- eval(expr, parent.frame())
      if (!isTRUE(ok)) stop("returned FALSE")
      TRUE
    },
    error = function(e) conditionMessage(e)
  )
  if (isTRUE(result)) {
    passed <<- passed + 1L
    message(sprintf("  PASS: %s", desc))
  } else {
    failed <<- failed + 1L
    msg <- sprintf("  FAIL: %s -- %s", desc, result)
    errors <<- c(errors, msg)
    message(msg)
  }
}

abort <- function(msg) {
  message(msg)
  quit(status = 1)
}

# -- Preflight -----------------------------------------------------------------

message("\n== Preflight ==")

# Credentials (read from ~/.Renviron)
if (Sys.getenv("S160_API_USERID") == "") abort("  S160_API_USERID not set in ~/.Renviron")
if (Sys.getenv("S160_API_KEY") == "") abort("  S160_API_KEY not set in ~/.Renviron")

# OAuth cache
cache_path <- rappdirs::user_cache_dir("gargle")
tokens <- list.files(cache_path, full.names = TRUE)
if (length(tokens) == 0) {
  abort(paste0(
    "  No cached OAuth token found.\n",
    "  Run interactively first:\n\n",
    "    pkgload::load_all(\".\")\n",
    "    s160_gcs_init(bucket = \"campaign_results_qa\")\n"
  ))
}
message(sprintf("  OAuth: %d cached token(s)", length(tokens)))
message("  API credentials: set")

# -- Load package --------------------------------------------------------------

message("\n== Loading survey160r ==")
pkgload::load_all(".", quiet = TRUE)
message("  OK")

# -- GCS: init -----------------------------------------------------------------

message("\n== GCS: init ==")
s160_gcs_init(bucket = "campaign_results_qa")

# -- GCS: list campaigns -------------------------------------------------------

message("\n== GCS: list campaigns ==")
campaigns <- s160_gcs_campaign_results_list()
check("returns character vector", is.character(campaigns))
check("at least one campaign exists", length(campaigns) > 0)

# Pick a campaign that has an export file
test_campaign_id <- NULL
for (cid in campaigns) {
  status <- s160_gcs_campaign_results_status(cid)
  if (!is.null(status)) {
    test_campaign_id <- cid
    break
  }
}
if (is.null(test_campaign_id)) abort("  No campaign with an export file found in bucket.")
message(sprintf("  Using campaign %s", test_campaign_id))

# -- GCS: list files -----------------------------------------------------------

message("\n== GCS: list files ==")
files <- s160_gcs_campaign_results_files(test_campaign_id)
check("returns character vector", is.character(files))
check("at least one file", length(files) > 0)
check("contains CSV export", any(grepl("_raw_data_download\\.csv$", files)))

# -- GCS: export status --------------------------------------------------------

message("\n== GCS: export status ==")
status <- s160_gcs_campaign_results_status(test_campaign_id)
check("returns a list", is.list(status))
check("has 'name' field", !is.null(status$name))
check("has 'updated' field", !is.null(status$updated))
check("has 'size' field", !is.null(status$size))

# -- GCS: read results ---------------------------------------------------------

message("\n== GCS: read results ==")
df <- s160_gcs_campaign_results_read(test_campaign_id)
check("returns data.frame", is.data.frame(df))
check("has rows", nrow(df) > 0)
check("has columns", ncol(df) > 0)

# -- GCS: read to destdir ------------------------------------------------------

message("\n== GCS: read to destdir ==")
tmpdir <- tempdir()
df2 <- s160_gcs_campaign_results_read(test_campaign_id, destdir = tmpdir)
expected_file <- file.path(tmpdir, paste0(test_campaign_id, "_raw_data_download.csv"))
check("returns data.frame", is.data.frame(df2))
check("file saved to destdir", file.exists(expected_file))
check("same row count", nrow(df2) == nrow(df))
unlink(expected_file)

# -- API: auth -----------------------------------------------------------------

message("\n== API: auth ==")
s160_api_auth(base_url = "https://qa-api.survey160.com")
check("JWT stored", !is.null(survey160r:::.s160_api_env$jwt))

# -- API: export and download --------------------------------------------------

message("\n== API: export and download ==")
df_api <- s160_api_campaign_results(test_campaign_id, timeout = 300)
check("returns data.frame", is.data.frame(df_api))
check("has rows", nrow(df_api) > 0)
check("has columns", ncol(df_api) > 0)

# -- Summary -------------------------------------------------------------------

message(sprintf("\n== Results: %d passed, %d failed ==", passed, failed))
if (failed > 0) {
  message("\nFailures:")
  for (e in errors) message(e)
  quit(status = 1)
}
message("All tests passed.")
