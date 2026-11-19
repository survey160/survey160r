# Pre-commit gate: tests + lint + coverage. Used by `make verify`.
#
# Equivalent to `make test && make lint && make coverage`, with the three
# phases running at the same time: lint and coverage each in a background R
# process while the tests run here, so the gate takes as long as its slowest
# phase rather than the sum of the three. Test files run in parallel workers
# (`Config/testthat/parallel` in DESCRIPTION); TESTTHAT_CPUS sets how many.
#
# Exits non-zero on any failure so it composes with shell pipelines and CI.

if (!nzchar(Sys.getenv("TESTTHAT_CPUS"))) {
  # Tests and coverage each start this many workers, so half the cores apiece.
  cores <- parallel::detectCores()
  if (is.na(cores)) cores <- 2L
  Sys.setenv(TESTTHAT_CPUS = max(2L, min(8L, cores %/% 2L)))
}

started <- Sys.time()
elapsed <- function() sprintf("%.0fs", as.numeric(Sys.time() - started, units = "secs"))

# lintr's cache keeps the lints of every unchanged expression (the configured
# linters all work one expression or file at a time, so a cached result cannot
# go stale); LINT_CACHE=FALSE lints from scratch, as CI does.
lint_bg <- callr::r_bg(
  function(cache) {
    lints <- lintr::lint_package(".", cache = cache)
    list(n = length(lints), text = utils::capture.output(print(lints)))
  },
  args = list(cache = !toupper(Sys.getenv("LINT_CACHE")) %in% c("0", "FALSE")),
  supervise = TRUE
)
coverage_bg <- callr::r_bg(
  function() {
    cov <- covr::package_coverage(".")
    lines <- as.data.frame(cov)
    list(pct = covr::percent_coverage(cov),
         files = 100 * tapply(lines$value > 0, lines$filename, mean),
         uncovered = unique(lines[lines$value == 0,
                                  c("filename", "functions", "first_line")]))
  },
  supervise = TRUE
)
# A background phase's result, or its error as the gate's own.
collect <- function(process, phase) {
  process$wait()
  tryCatch(process$get_result(), error = function(e) {
    stop(sprintf("%s failed:\n%s", phase, conditionMessage(e)), call. = FALSE)
  })
}

suppressPackageStartupMessages({
  pkgload::load_all(".")
})

cat("\n=== tests ===\n")
results <- testthat::test_local(reporter = testthat::SummaryReporter$new(),
                                stop_on_failure = FALSE)
df <- as.data.frame(results)
if (any(df$failed > 0) || any(df$error)) {
  lint_bg$kill()
  coverage_bg$kill()
  stop(sprintf("%d failed / %d errored.",
               sum(df$failed > 0), sum(df$error)),
       call. = FALSE)
}
cat(sprintf("%d tests pass (%s).\n", nrow(df), elapsed()))

cat("\n=== lint ===\n")
lints <- collect(lint_bg, "lint")
if (lints$n > 0L) {
  coverage_bg$kill()
  cat(lints$text, sep = "\n")
  stop(sprintf("%d lint(s).", lints$n), call. = FALSE)
}
cat(sprintf("no lints (%s).\n", elapsed()))

cat("\n=== coverage ===\n")
cov <- collect(coverage_bg, "coverage")
below <- cov$files[cov$files < 100]
if (length(below) > 0L) print(round(below, 2))
if (cov$pct < 100) {
  print(cov$uncovered, row.names = FALSE)
  stop(sprintf("Coverage %.2f%% is below 100%% threshold.", cov$pct),
       call. = FALSE)
}
cat(sprintf("coverage %.2f%% over %d files (%s).\n", cov$pct, length(cov$files), elapsed()))

cat("\n✓ tests pass, lint clean, coverage 100%\n")
