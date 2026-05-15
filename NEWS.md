# survey160r (development version)

## New features

* `run_latency_all()` gains a `workers` argument that dispatches per-campaign
  work via `future.apply::future_lapply()` when greater than 1, allowing
  fleet passes to process campaigns in parallel. Callers set up the future
  plan (e.g. `future::plan(future::multisession, workers = 4)`) themselves
  (SUR-1305).
* `run_latency_all()` gains a `skip_unchanged` argument. When `TRUE`, each
  campaign's source CSV `updated` timestamp is compared against the existing
  destination Parquet's `updated` timestamp and campaigns whose output is
  already current are reported with `status = "skipped"` and the existing
  `parquet_uri` without re-downloading or re-emitting (SUR-1305).
* New exported helper `s160_gcs_latency_output_status()` returns GCS
  metadata for a campaign's existing latency Parquet output, or `NULL` if
  none exists (SUR-1305).

## Breaking changes

* `run_latency()` no longer takes a `config_path` argument. The function is
  now stateless: it derives `flow.questions` from the CSV header (via the
  new `discover_questions()`) and assembles the rest of the config from its
  named arguments. Sensible defaults are baked in (
  `field_timezone = "UTC"`, `project_id = campaign_id`,
  `texting_windows = list()`); each is overridable via a named argument.
  `run_latency()` no longer requires `s160_api_auth()` -- the config is
  derived from the CSV alone (SUR-1299).
* `read_config()` and the YAML config schema are removed entirely. Configs
  are now built programmatically via `build_config()` or as hand-written
  lists with the same shape. The `yaml` package is dropped from `Imports`.
  Existing per-wave YAMLs under `latency-scripts/*.yaml` must be translated
  to `run_latency(..., field_timezone=..., project_id=...,
  texting_windows=..., date_filter=...)` calls; the YAML files themselves
  are retained outside this repo as historical record (SUR-1299).
* The config schema is trimmed to the fields `latency_report()` actually
  reads: `project_id`, `campaign_id`, `field_timezone`, `flow`, `filters`,
  `texting_windows`, `reports`. Previously accepted but never-used keys
  (`project_name`, `wave_run`, `display_timezone`,
  `reports$extra_grouping_columns`, `input`, `output`) are no longer
  recognized; `validate_config()` rejects them as unknown (SUR-1299).
* The Parquet `date` and `hour_local` columns are now bucketed in UTC by
  default. Callers consuming
  `gs://s160_analytics_*/latency/*_latency.parquet` that previously
  depended on an `America/New_York`-bucketed output must pass
  `field_timezone = "America/New_York"` explicitly.

## New features

* `discover_questions(data)` derives the question flow from CSV column
  names (either a data frame or a character vector of header tokens).
  Accepts both the raw `id[<q>]scriptDate` bracket form and the dotted
  `id.<q>.scriptDate` form produced by `read.csv()`. Terminal flow states
  (`refusal`, `ineligible`) are dropped (SUR-1299).
* `build_config(campaign_id, data, ...)` is a pure function that assembles
  a validated config from the CSV header alone. Named arguments for every
  override (`field_timezone`, `project_id`, `texting_windows`,
  `date_filter`, `respondent_id_column`, `time_bucket`). No I/O, no API
  call (SUR-1299).
* `pull_csv_from_gcs()` now stamps a `source_csv_path` attribute on the
  returned data frame (the canonical `gs://...` URI) alongside the
  existing `source_csv_hash`. Lets downstream callers record provenance
  without re-deriving the path (SUR-1299).
* All reader functions and the latency runners now take an explicit
  `bucket` (or `source_bucket`) argument that defaults to the global set
  by `s160_gcs_init()`. Callers can either keep using `s160_gcs_init()`
  once-per-session or pass `bucket = "..."` per call and skip the global
  entirely. `run_latency_all()` no longer needs to stash/restore the
  global bucket since its inner calls thread `source_bucket` through
  every layer. Affects `s160_gcs_campaign_results_read`,
  `s160_gcs_campaign_results_list`, `s160_gcs_campaign_results_files`,
  `s160_gcs_campaign_results_status`, `pull_csv_from_gcs`, and
  `run_latency` (SUR-1299).
* `R/latency_report.R` (531 lines) is split into five cohesive files:
  `latency_report.R` keeps the orchestrator and shared constants;
  `latency_filter.R` holds the population / dedupe / date filters;
  `latency_frame.R` holds the per-respondent x per-segment frame builder;
  `latency_aggregate.R` holds the consolidated-table aggregation;
  `latency_diagnostics.R` holds the diagnostics-list assembly. The `%||%`
  operator (used in three files) moves to `aaa_utils.R`. Pure internal
  refactor; no behavior change, verified by the legacy-parity test
  (SUR-1299).
* The four unprefixed latency exports have been renamed under the
  `latency_*` namespace to prevent collisions with other R packages and
  signal cohesion: `discover_questions` -> `latency_discover_questions`,
  `build_config` -> `latency_build_config`, `validate_config` ->
  `latency_validate_config`, `config_hash` -> `latency_config_hash`. The
  old names are removed without a deprecation period; callers using the
  pre-0.8.0 names must update (SUR-1299).
* `latency_report()` and `run_latency()` accept an optional `run_at`
  argument (defaults to `Sys.time()`). `run_latency_all()` stamps a single
  fleet-wide timestamp on every campaign in one pass so the latest fleet
  output can be selected with `WHERE run_at_utc = (SELECT MAX(run_at_utc)
  FROM latency)` (SUR-1299).
* `run_latency_all(source_bucket, bucket, ...)` runs the latency pipeline
  for every campaign with an export CSV under `source_bucket` and writes the
  per-campaign Parquet to `bucket`. Per-campaign failures are caught by
  default (`continue_on_error = TRUE`) and recorded in the returned status
  data frame so one bad CSV does not block the rest of the fleet. Saves
  and restores the global GCS bucket so the caller's session state is
  untouched. Replaces the bespoke iteration loop in
  `scripts/bulk_reprocess.R`, which is now a thin shell wrapper around
  this function (SUR-1299).
* `scripts/bulk_reprocess.R` is refactored to call `run_latency_all()`;
  the inline `discover_questions`, `build_config`, and process-one helpers
  are removed, and the script no longer needs API auth (SUR-1299).

## Bug fixes

* `download_with_verify()` no longer crashes when `googleCloudStorageR`'s
  `gcs_list_objects()` returns a human-readable `size` string (e.g.
  `"483.3 Kb"`). The previous code did `as.numeric(size)`, got `NA`, then
  hit `if (actual_size == NA)` and aborted with "missing value where
  TRUE/FALSE needed". A non-numeric size is now treated as "unknown" and
  the download proceeds without verification. Discovered while running
  `run_latency` against the production `campaign_results` bucket (SUR-1299).
* `s160_api_campaign_get()` now strips sub-second precision when parsing
  ISO-8601 timestamp columns, so values like
  `"2026-01-15T09:30:00.123456Z"` (which PostgreSQL can emit) come back as
  `POSIXct` rather than falling through to the string fallback. Numeric UTC
  offsets (`+05:30`, `-0400`) are also covered. The `httr::GET` import is
  now declared explicitly to match the other `httr` imports.

## New features

* `s160_api_campaign_get(campaign_id)` reads a single campaign's attributes
  via `GET /campaigns/<id>`. Returns a single-row data frame with the
  `campaigns` table columns; enriched API-only fields (`listlength`,
  `list`, `login`, `exports`, `has_texting_started`, `sandbox_configuration`,
  `aggregator`, `has_assigned_registration`) are dropped, and JSON columns
  (`script`, `prompt`, `quotas`, ...) come back as length-1 list-columns.
  Useful for confirming attributes after a state-changing call without
  dropping to direct database access. Per-campaign read; not intended for
  tight loops over hundreds of IDs. ISO-8601 timestamp columns
  (`startdate`, `archive_scheduled_date`, ...) are parsed to `POSIXct`
  in UTC so callers do not have to re-parse them (SUR-1253).

## Documentation

* Declare `R (>= 4.1)` in `DESCRIPTION` to match what the current `arrow`,
  `dplyr`, and `lubridate` imports already require.
* `RELEASING.md` clarifies that the release tag must point at the release
  PR's merge SHA, not `HEAD` (#14).
* README latency YAML example sets `respondent_id_column: ~` instead of
  the misleading `userid`, which in Survey160 v2 CSVs is the agent login
  rather than a per-respondent identifier.
* README first-time-setup notes that producing latency outputs requires
  Storage Object Creator on the destination analytics bucket, in addition
  to Storage Object Viewer on the source bucket.

# survey160r 0.6.0

## New features

* Latency analysis pipeline (#13). Supersedes the per-wave inline R scripts
  that the analytics team used to maintain by hand with a single algorithm,
  output schema, and YAML config per campaign; existing wave scripts will be
  migrated client by client. New public functions:

  * `latency_report(data, config)` -- pure, deterministic; returns
    `consolidated`, `latency_frame`, `diagnostics`, `meta`.
  * `read_config(path)` / `validate_config(config, data)` -- YAML loader
    plus fail-fast schema and flow-order validation.
  * `pull_csv_from_gcs(campaign_id)` -- thin wrapper that also computes a
    source CSV sha256 for provenance.
  * `write_to_gcs(result, campaign_id, bucket, uploader = upload_object)` --
    writes one Parquet per campaign to
    `gs://<bucket>/latency/<campaign_id>_latency.parquet` with a pinned
    Arrow schema, ZSTD compression, and provenance columns
    (`algorithm_version`, `config_hash`, `source_csv_hash`, `run_at_utc`,
    `run_by`). Accepts a custom uploader for batch jobs and tests.
  * `read_latency(bucket)` -- returns a DuckDB connection and a `latency`
    view over all per-campaign Parquet files.
  * `run_latency(...)` -- orchestrator for the manual happy-path flow.

* Fleet-locked universal latency thresholds (1, 3, 5, 10 minutes). Configs
  that still carry per-wave thresholds are rejected with a named error.

* Per-segment NA classification in diagnostics (`parse_failure`,
  `missing_endpoint`, `chain_break`); sum-conserving against
  `n_segments_na`.

* Legacy-parity CI gate: a generic re-implementation of the four legacy
  primitives (`timestamp_diff`, `texting_hour_by_date`,
  `percent_below_thresholds_data`, `latency_indicator_vars`) asserts
  cell-for-cell match against the new pipeline on a synthetic fixture.

* New dependencies: `arrow`, `lubridate`, `yaml`, `digest`, `dplyr`, `rlang`.
  `duckdb` and `DBI` are Suggests (required only for `read_latency`).

# survey160r 0.5.0

## New features

* `s160_gcs_campaign_results_read()` verifies the downloaded CSV size
  against the GCS object metadata and retries on truncation (#9).

## CI / infrastructure

* PRs that touch `R/`, `man/`, or `src/` without bumping `Version:` in
  `DESCRIPTION` now fail the check workflow (#11).
* CI runs in the pre-built `ghcr.io/r-hub/containers/ubuntu-release`
  image instead of installing R from scratch, cutting workflow time
  substantially (#10).

# survey160r 0.4.0

## New features

* `s160_api_auth()` reads the Survey160 API key from `.Renviron`
  instead of taking it as a function argument, and masks the secret
  in error output. README updated with the new setup flow and `pak`
  install instructions (#8).

# survey160r 0.3.0

## New features

* API client for triggering campaign results exports. New functions
  under the `s160_api_*` and `s160_gcs_campaign_results_*` namespaces
  let R callers kick off a fresh export and then read it back from
  GCS in one workflow (#7).

# survey160r 0.2.0

## New features

* Zero-config OAuth: the public client ID ships in
  `inst/oauth-client.json`; on first interactive run,
  `s160_gcs_init()` prompts for the client secret and persists it to
  `~/.Renviron`. `bucket` is now a required named parameter on the
  GCS readers to prevent silent reads from the wrong environment
  (#2).
* `s160_gcs_campaign_results_read()` gains a `destdir` parameter for
  persistent downloads (default is a tempdir that is cleaned up on
  exit) and sanitizes the resolved filename (#4).

## CI / infrastructure

* GitHub Actions runs `R CMD check` and the testthat suite on every
  push and PR; warnings fail the build (#3).
* `lintr` runs in CI and fails the build on any lint violation (#5).
* `covr` reports test coverage on every CI run; the threshold is
  enforced at 100% (#6).

# survey160r 0.1.0

Initial release. Converts the previous loose script collection into a
proper R package with a `DESCRIPTION`, `NAMESPACE`, exported help
pages, and a `testthat` suite that runs offline via mocks. Public
surface area:

* `s160_gcs_init()` -- OAuth bootstrap for Google Cloud Storage.
* `s160_gcs_campaign_results_read()` -- download and parse a campaign
  CSV from the configured bucket.
* `s160_gcs_campaign_results_list()` -- list available campaign IDs.
* `s160_gcs_campaign_results_files()` -- enumerate files for one
  campaign.

Internal: `validate_campaign_id()` is a shared input guard reused by
the GCS readers; not exported.

Published to R-universe at
`https://survey160.r-universe.dev` (#1).
