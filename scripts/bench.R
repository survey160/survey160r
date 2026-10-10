# Performance harness for the hot paths. Not part of the built package.
#
#   Rscript scripts/bench.R [label] [n_respondents] [n_questions]
#   make bench                      # label "bench", 200k respondents, 12 questions
#
# Times each workload and reports its peak R-heap allocation (gc "max used"
# since a reset) so a change can be judged on both axes. Appends one JSON line
# per workload to scripts/bench.jsonl (git-ignored) so runs can be compared:
#
#   jq -r '[.label,.bench,.sec,.peak_mb] | @tsv' scripts/bench.jsonl | column -t
#
# The synthetic export comes from tests/testthat/helper-synthetic.R in its
# STRING form (timestamps as the export's "...Z" strings), which is the
# heavier case: s160_read_csv() / fread usually hand the transforms POSIXct
# columns already. Wall time on a laptop is noisy at +-10%; compare peaks and
# order-of-magnitude changes, not the second decimal.
suppressPackageStartupMessages(pkgload::load_all(".", quiet = TRUE))
source("tests/testthat/helper-synthetic.R")

args <- commandArgs(trailingOnly = TRUE)
label <- if (length(args) >= 1L) args[[1L]] else "bench"
n <- if (length(args) >= 2L) as.integer(args[[2L]]) else 200000L
n_questions <- if (length(args) >= 3L) as.integer(args[[3L]]) else 12L
out <- file.path("scripts", "bench.jsonl")

measure <- function(name, expr) {
  invisible(gc(reset = TRUE, full = TRUE))
  t0 <- proc.time()[["elapsed"]]
  force(expr)
  elapsed <- proc.time()[["elapsed"]] - t0
  g <- gc(full = TRUE)
  peak_mb <- sum(g[, "max used"] * c(56, 8)) / 2^20
  cat(sprintf("%-26s %8.2fs  peak %6.0f MB\n", name, elapsed, peak_mb))
  cat(sprintf('{"label":"%s","bench":"%s","n":%d,"sec":%.3f,"peak_mb":%.0f}\n',
              label, name, n, elapsed, peak_mb), file = out, append = TRUE)
  invisible(NULL)
}

cat(sprintf("== %s: %d respondents x %d questions\n", label, n, n_questions))
export <- synthetic_export(n = n, n_questions = n_questions)
run_at <- as.POSIXct("2026-01-01", tz = "UTC")

measure("parse_timestamps", parse_campaign_timestamps(export$id.intro.scriptDate))
measure("latency_run_full", latency_run(1980, export, field_timezone = "America/New_York",
                                        respondent_id_column = "phone", run_at = run_at))
measure("latency_run_compact", latency_run(1980, export, field_timezone = "America/New_York",
                                           respondent_id_column = "phone", run_at = run_at,
                                           compact = TRUE))
measure("disposition_run", disposition_run(1980, export))
parsed <- synthetic_parse_timestamps(export)
measure("latency_run_posixct_in", latency_run(1980, parsed, field_timezone = "America/New_York",
                                              respondent_id_column = "phone", run_at = run_at))
rm(export, parsed)
invisible(gc())

disposition <- synthetic_disposition(n_rows = n * 15L, n_phones = n * 5L)
measure("disposition_summary", disposition_summary(disposition))
sample_phones <- sample(disposition$phone, min(50000L, nrow(disposition)))
measure("disposition_summary_phones", disposition_summary(disposition, phones = sample_phones))
measure("disposition_funnel", disposition_funnel(disposition))
measure("normalize_phone", .normalize_phone(disposition$phone))
