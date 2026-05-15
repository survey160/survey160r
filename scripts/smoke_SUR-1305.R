# Smoke test for SUR-1305. NOT shipped (scripts/ is in .Rbuildignore).
# Two-pass run on the dev fleet to validate parallelism and skip_unchanged.

suppressMessages(library(survey160r))

s160_gcs_init(bucket = "campaign_results")

future::plan(future::multisession, workers = 4)
on.exit(future::plan(future::sequential), add = TRUE)

cat("\n=== Pass 1: workers=4, skip_unchanged=TRUE (cold) ===\n")
t1 <- Sys.time()
out1 <- run_latency_all(
  source_bucket = "campaign_results",
  bucket = "s160_analytics_dev",
  workers = 4,
  skip_unchanged = TRUE
)
elapsed1 <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
cat(sprintf("Pass 1 wall: %.1fs\n", elapsed1))
print(out1)

cat("\n=== Pass 2: re-run, every campaign should now be skipped ===\n")
t2 <- Sys.time()
out2 <- run_latency_all(
  source_bucket = "campaign_results",
  bucket = "s160_analytics_dev",
  workers = 4,
  skip_unchanged = TRUE
)
elapsed2 <- as.numeric(difftime(Sys.time(), t2, units = "secs"))
cat(sprintf("Pass 2 wall: %.1fs\n", elapsed2))
print(out2)

cat("\n=== Summary ===\n")
cat(sprintf("Pass 1: %d ok, %d skipped, %d failed -- %.1fs\n",
            sum(out1$status == "ok"),
            sum(out1$status == "skipped"),
            sum(out1$status == "failed"),
            elapsed1))
cat(sprintf("Pass 2: %d ok, %d skipped, %d failed -- %.1fs\n",
            sum(out2$status == "ok"),
            sum(out2$status == "skipped"),
            sum(out2$status == "failed"),
            elapsed2))
