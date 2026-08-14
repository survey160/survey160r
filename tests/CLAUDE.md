# survey160r tests

testthat (edition 3) suite. 100% coverage is enforced (`covr::package_coverage()`); use `# nocov` only for branches that genuinely require a live worker / interactive session.

## Layout

```
tests/
├── testthat.R                  # entry point: library(survey160r) + test_check()
└── testthat/
    ├── helper-stubs.R          # shared helpers, auto-loaded before all tests
    ├── fixtures/
    │   ├── synthetic.csv             # canonical Survey160 v2 CSV (4 rows, 1 campaign, 1 day)
    │   ├── synthetic_parity.csv      # 8-question, 6-respondent legacy-parity input
    │   └── synthetic_cross_hour.csv  # 4 respondents straddling an hour boundary (day-rollup tests)
    └── test-<module>.R         # one file per R/<module>.R (mostly)
```

`helper-*.R` files are sourced by testthat automatically — never call them manually. Put new shared utilities there, not in individual test files.

## Run the suite

`make verify` from the package root is the canonical gate — it loads the package once and runs `testthat::test_package()` + `lintr::lint_package()` + `covr::package_coverage()` in a single R session. About 3x faster than three cold `Rscript -e` calls. Prefer it over ad-hoc invocations.

`make check` runs the full `R CMD check` (used at release time).

When iterating on a single file, run interactively after `pkgload::load_all()`:

```r
pkgload::load_all()
testthat::test_file("tests/testthat/test-latency_run.R")
```

## Mocking

The suite uses `testthat::local_mocked_bindings()` (testthat 3.x) for almost everything. `mockery::stub` is retained in two `test-s160_gcs_init.R` spots where `local_mocked_bindings` can't reach: (1) `system.file` -- the base-namespace lookup happens before the rebind takes effect; (2) forcing `interactive() == TRUE` inside `covr::package_coverage()`'s non-interactive subprocess. `interactive() == FALSE` mocks via `local_mocked_bindings(.package = "base")` work fine elsewhere. New tests should always default to `local_mocked_bindings`.

`local_mocked_bindings()` rebinds within the calling test's `local()` scope and auto-restores on exit. When wrapping it in a helper, **always pass `.env = parent.frame()`** so the mock applies in the caller's scope, not the helper's:

```r
stub_campaign_list <- function(ids, env = parent.frame()) {
  testthat::local_mocked_bindings(
    s160_gcs_campaign_results_list = function(bucket = NULL) ids,
    .env = env
  )
}
```

To mock a base-R function, pass `.package = "base"`.

## Shared helpers (helper-stubs.R)

### Capture pattern

```r
captured <- new_capture()              # = new.env(parent = emptyenv())
# ... mocks write fields into `captured` from inside their bodies ...
expect_equal(captured$pull_id, 1)
```

Prefer a capture env over `<<-` to a free variable — it survives `local_mocked_bindings` scope changes and reads cleanly.

### GCS / latency stubs

| Helper | Purpose |
|---|---|
| `stub_gcs_base()` | `check_gcs_ready` + `validate_campaign_id` + `gcs_get_global_bucket` no-ops |
| `stub_gcs_download_ok(capture)` | `gcs_get_object` answers the `meta = TRUE` size probe and writes a minimal CSV |
| `stub_campaign_list(ids)` | mocks `s160_gcs_campaign_results_list` |
| `gcs_status(name, updated, size)` | builds the list returned by `s160_gcs_*_status` (`updated` accepts an ISO string or POSIXct) |

### Fixture loaders

```r
synthetic_config()                # latency config matching synthetic.csv
load_synthetic_data()             # CSV + source_csv_hash/source_csv_path attrs
load_synthetic_data(mutate = \(d) { d$id.intro.finalText <- NULL; d })
load_synthetic_parity()           # 8-question, 6-respondent legacy-parity input
load_synthetic_cross_hour()       # 4-respondent cross-hour fixture
minimal_synthetic_data(           # programmatic builder; no file I/O
  questions = c("intro", "q1", "close"),
  with_rows = TRUE                # FALSE -> column-only frame
)
```

The `mutate` hook is how you trigger negative paths (drop a column, perturb a value) without copying the CSV. `minimal_synthetic_data()` is preferred when a test only needs a column-shape or one synthetic row -- it has no fixture file dependency.

## Conventions

- **Latency runner tests**: `latency_run()` is now source-agnostic -- it takes caller-supplied `data`. Tests pass a `load_synthetic_data()` frame directly; no I/O boundary to stub. Don't mock `latency_report` unless you specifically need to capture the config it was called with (see `test-latency_run.R`'s "forwards `...` overrides" case).
- **Reader tests**: `s160_gcs_campaign_results_read` uses `stub_gcs_base()` + `stub_gcs_download_ok()`. `s160_read_csv` reads real files via `tempfile()`.
- **Times are UTC**: tests pass POSIXct stamps with `tz = "UTC"` explicitly. `gcs_status()` parses character `updated` as UTC.
- **Don't share fixture data across tests by file mutation**: each test loads its own copy via `load_synthetic_data()`. The fixture file is read-only.
- **`# nocov start` / `# nocov end`** is only used for paths that require interactive auth or a live network (e.g. the OAuth bootstrap in `R/s160_gcs.R`). Document the integration test or manual run that exercises them.
- **Test file names**: `test-<module>.R` matching `R/<module>.R` where possible. Cross-cutting suites (`test-latency_coverage.R`, `test-day_rollup_equivalence.R`, `test-latency_parity_legacy.R`) are named after the property under test.

## Adding a new test file

1. Decide if it's per-module (`test-foo.R` against `R/foo.R`) or a property/parity check (named after the property).
2. Use existing helpers from `helper-stubs.R`. If you find yourself copying a mock block twice, add a helper rather than a third copy.
3. Run `make verify` before committing — the lint + coverage gates will catch most regressions.
4. Per the package `CLAUDE.md`: any change under `R/`, `man/`, or `src/` requires a `Version:` bump in `DESCRIPTION` and a matching `NEWS.md` edit. Test-only changes don't.
