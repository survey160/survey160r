# survey160r tests

testthat (edition 3) suite. 100% coverage is enforced (`covr::package_coverage()`); use `# nocov` only for branches that genuinely require a live worker / interactive session.

## Layout

```
tests/
├── testthat.R                  # entry point: library(survey160r) + test_check()
└── testthat/
    ├── helper-stubs.R          # shared helpers, auto-loaded before all tests
    ├── helper-synthetic.R      # synthetic export / disposition generators (also used by scripts/bench.R)
    ├── helper-raw-export.R     # synthetic RAW export file (production byte shape) + writer
    ├── fixtures/
    │   ├── synthetic.csv             # canonical Survey160 v2 CSV (4 rows, 1 campaign, 1 day)
    │   ├── synthetic_parity.csv      # 8-question, 6-respondent legacy-parity input
    │   ├── synthetic_cross_hour.csv  # 4 respondents straddling an hour boundary (day-rollup tests)
    │   └── raw_export.csv            # 30-row raw export in the production byte shape (generated)
    └── test-<module>.R         # one file per R/<module>.R (mostly)
```

`helper-*.R` files are sourced by testthat automatically — never call them manually. Put new shared utilities there, not in individual test files.

## Run the suite

`make verify` from the package root is the canonical gate — it loads the package once and runs `testthat::test_package()` + `lintr::lint_package()` + `covr::package_coverage()` in a single R session. About 3x faster than three cold `Rscript -e` calls. Prefer it over ad-hoc invocations.

`make check` runs the full `R CMD check` (used at release time).

When iterating on a single file, `make test-file FILE=tests/testthat/test-latency_run.R` (one
R session, package loaded once), or interactively after `pkgload::load_all()`:

```r
pkgload::load_all()
testthat::test_file("tests/testthat/test-latency_run.R")
```

## Mocking

The suite uses `testthat::local_mocked_bindings()` (testthat 3.x) for almost everything. `mockery::stub` is retained where `local_mocked_bindings` can't reach or would be too broad: in `test-s160_gcs_init.R`, (1) `system.file` -- the base-namespace lookup happens before the rebind takes effect -- and (2) forcing `interactive() == TRUE` inside `covr::package_coverage()`'s non-interactive subprocess; and in `test-gcs_pull_cached.R` / `test-s160_read_csv.R` / `test-resolve_dataset.R`, to stub a base or other-package call (`file.rename`, `file.copy`, `tools::R_user_dir`, ...) inside one function only, where a global base rebind would leak into testthat itself. `interactive() == FALSE` mocks via `local_mocked_bindings(.package = "base")` work fine elsewhere. New tests should always default to `local_mocked_bindings`.

`local_mocked_bindings()` rebinds within the calling test's `local()` scope and auto-restores on exit. When wrapping it in a helper, **always pass `.env = parent.frame()`** so the mock applies in the caller's scope, not the helper's:

```r
stub_gcs_base <- function(env = parent.frame()) {
  testthat::local_mocked_bindings(
    check_gcs_ready = function() NULL,
    validate_campaign_id = function(id) as.character(id),
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
| `stub_gcs_base()` | `check_gcs_ready` + `validate_campaign_id` no-ops |
| `stub_gcs_download_ok(capture)` | `gcs_get_object` answers the `meta = TRUE` size probe and writes a minimal CSV |
| `written_size(lines)` | bytes `writeLines(lines)` puts on disk -- the size a metadata probe should report |
| `stub_no_sleep(capture)` | mocks `Sys.sleep`; records the waits in `capture$waits` |

### Fixture loaders

```r
synthetic_config()                # latency config matching synthetic.csv
synthetic_fixture()               # list(data = synthetic.csv via read.csv, config = synthetic_config())
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

### Synthetic generators (helper-synthetic.R)

```r
synthetic_export(n = 2000L, n_questions = 6L, bilingual = FALSE)  # string-form export, drop-off,
                                                                  # blank replies, parse failures
synthetic_parse_timestamps(export)   # the POSIXct form a reader (timestamps = "POSIXct") returns
synthetic_disposition(n_rows, n_phones)  # (phone, campaign_id) projection with 0/1 flags + dates
```

Deterministic for a seed. Use them for property / parity tests that need a campaign larger than the
fixtures (`test-synthetic_parity.R`: full vs compact, string vs POSIXct input, fast-path helpers
vs their reference forms). `scripts/bench.R` sources this file for the perf harness, so keep the
generators dependency-free and fast.

### Raw exports (helper-raw-export.R)

```r
lines <- synthetic_raw_export(n = 40L, n_body = 2L, seed = 1L)  # file lines, header first
path  <- write_raw_export(lines, eol = "\r\n")                 # byte-exact write (LF default)
```

The bytes a real `<id>_raw_data_download.csv` holds, modeled on a profile of the historical
export corpus (no real values): every field quoted, bracket headers, `phone` / `userid` repeated
after the flow, `status` mixing words and carrier codes, `"None"` / `"NA"` tokens, trimmed
fractional seconds, multilingual text with an embedded line break. Use it for anything that must
go through the CSV readers (`test-raw_export.R`). `fixtures/raw_export.csv` is
`synthetic_raw_export(n = 30L, seed = 1L)`; a test fails if the two drift.

### Disposition engines

`with_engine("duckdb" | "nanoparquet", expr)` runs a reader under one engine;
`expect_engine_parity(expr)` asserts both engines return the identical value (skips without
duckdb). Every Parquet-path disposition reader must keep that parity.
`stub_duckdb_created_by()` makes nanoparquet report a DuckDB `created_by`, so the nanoparquet
engine takes its DuckDB-written-file path on a fixture it wrote itself.

## Conventions

- **Latency runner tests**: `latency_run()` is now source-agnostic -- it takes caller-supplied `data`. Tests pass a `load_synthetic_data()` frame directly; no I/O boundary to stub. Don't mock `latency_report` unless you specifically need to capture the config it was called with (see `test-latency_run.R`'s "forwards `...` overrides" case).
- **Reader tests**: `s160_gcs_campaign_results_read` uses `stub_gcs_base()` + `stub_gcs_download_ok()`. `s160_read_csv` reads real files via `tempfile()`.
- **Times are UTC**: tests pass POSIXct stamps with `tz = "UTC"` explicitly.
- **Don't share fixture data across tests by file mutation**: each test loads its own copy via `load_synthetic_data()`. The fixture file is read-only.
- **`# nocov start` / `# nocov end`** is only used for paths that require interactive auth or a live network (e.g. the OAuth bootstrap in `R/s160_gcs.R`). Document the integration test or manual run that exercises them.
- **Test file names**: `test-<module>.R` matching `R/<module>.R` where possible. Cross-cutting suites (`test-latency_coverage.R`, `test-day_rollup_equivalence.R`, `test-latency_parity_legacy.R`, `test-pull_wrappers.R`) are named after the property under test.

## Adding a new test file

1. Decide if it's per-module (`test-foo.R` against `R/foo.R`) or a property/parity check (named after the property).
2. Use existing helpers from `helper-stubs.R`. If you find yourself copying a mock block twice, add a helper rather than a third copy.
3. Run `make verify` before committing — the lint + coverage gates will catch most regressions.
4. Per the package `CLAUDE.md`: any change under `R/`, `man/`, or `src/` requires a `Version:` bump in `DESCRIPTION` and a matching `NEWS.md` edit. Test-only changes don't.
