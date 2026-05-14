# Single-session pre-commit gate: tests + lint + coverage.
#
# Equivalent to `make test && make lint && make coverage` but runs in one R
# session, avoiding ~10-15s of repeated cold startup. Used by `make verify`.
#
# Exits non-zero on any failure so it composes with shell pipelines and CI.

suppressPackageStartupMessages({
  pkgload::load_all(".")
})

cat("\n=== tests ===\n")
results <- testthat::test_local(reporter = testthat::SummaryReporter$new())
df <- as.data.frame(results)
if (any(df$failed > 0) || any(df$error)) {
  stop(sprintf("%d failed / %d errored.",
               sum(df$failed > 0), sum(df$error)),
       call. = FALSE)
}

cat("\n=== lint ===\n")
lints <- lintr::lint_package(".")
if (length(lints) > 0L) {
  print(lints)
  stop(sprintf("%d lint(s).", length(lints)), call. = FALSE)
}
cat("no lints.\n")

cat("\n=== coverage ===\n")
cov <- covr::package_coverage(".")
print(cov)
pct <- covr::percent_coverage(cov)
if (pct < 100) {
  stop(sprintf("Coverage %.2f%% is below 100%% threshold.", pct),
       call. = FALSE)
}

cat("\n✓ tests pass, lint clean, coverage 100%\n")
