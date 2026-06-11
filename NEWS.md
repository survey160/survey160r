# survey160r (development version)

# survey160r 0.16.0

## New features

* **Multi-environment API sessions, one entry point.** `s160_api_auth(env)`
  now takes an environment *name* -- `"prod"` (default) or `"staging"` -- and
  *returns a connection*: an opaque handle carrying the JWT, credentials, base
  URL, environment name, and the paired campaign-results GCS bucket. Addressing
  by name resolves the URL, bucket, and key var **together**, so they cannot be
  mismatched (no pointing a prod URL at a staging key/bucket). Credentials come
  from `~/.Renviron`: `S160_API_USERID` for the user, and a per-environment key
  var -- `S160_STAGING_API_KEY` for staging, `S160_PROD_API_KEY` (falling back
  to the legacy `S160_API_KEY`) for prod -- prompted and saved on first
  interactive run.
  - **Single environment**: ignore the return value -- `s160_api_auth()`
    refreshes the package default connection and conn-less
    `s160_api_campaign_results()` / `s160_api_campaign_get()` calls use it.
  - **Both environments at once**: capture each connection and pass it as the
    new `conn =` argument. `prod <- s160_api_auth("prod"); stg <-
    s160_api_auth("staging")`, then `s160_api_campaign_results(744, conn = stg)`.
    Prod and staging stay live in the same session -- e.g. comparing a campaign
    across both. Each connection's paired bucket makes the export trigger,
    completion poll, and read all target its own environment (closing the
    footgun where a staging export was polled against the production bucket).

  The in-session JWT refresh reuses the credentials stored on the connection, so
  a staging connection held alongside prod keeps refreshing against staging.
  **Breaking:** `s160_api_auth()` no longer takes a `base_url` argument (use
  `env`) and now returns a connection object (invisibly) instead of `NULL`;
  existing zero-argument single-environment calls are unaffected.

* **Faster CSV reads via `data.table::fread`.** `s160_read_csv()` and
  `s160_gcs_campaign_results_read()` now read through `data.table::fread`
  (multithreaded; ~5-10x faster than `utils::read.csv` on large exports),
  falling back to `read.csv` if `data.table` is unavailable. `data.table`
  is now a hard dependency -- on Windows it installs as a precompiled
  R-universe/CRAN binary (no Rtools needed) and is usually already present.
  Output is pinned to stay close to the old `read.csv` behaviour
  (`stringsAsFactors = FALSE`, `check.names = TRUE`, and crucially
  `integer64 = "character"` so large IDs come back as character strings
  rather than a `bit64::integer64` class). Existing calls are unchanged --
  this is a transparent speedup.

* **Column projection for the latency pipeline.** New exported helpers
  `required_csv_columns(config)` and `s160_csv_header(path)`, plus a
  `columns =` argument on `s160_read_csv()` /
  `s160_gcs_campaign_results_read()` / `s160_gcs_pull_csv()`. Passing the
  algorithm's required column set lets `fread` parse only those columns,
  cutting read time and (importantly for parallel fleet runs) per-worker
  memory on very wide survey exports. The projection keeps the
  non-flow columns the report depends on (`id.intro.finalText`,
  `web_complete`, `id.ineligible.scriptDate`) so output is identical to a
  full read.

* **Optional provenance hashing.** `s160_read_csv(path, hash = FALSE)`
  skips the sha256 `digest()` pass (a full second read of the file),
  setting `source_csv_hash = NA`. Useful for large local backfills where
  the per-file hash is not needed.

# survey160r 0.15.1

## New features

* **Three-way `survey_mode` (SUR-1368).** Adds a third `survey_mode`
  value `"t2w_external"` -- a personalized survey link in the close
  message but no web completes (external platform, no webhook).
  Completion is not computable from the export, so `n_completed` is
  `NA` (`n_texted` / `n_consented` remain valid). A "survey link" is
  detected as a personalized URL in the close message (one that varies
  per respondent); a single static stimulus link (e.g. a shared video
  URL) is not.

# survey160r 0.15.0

## New features

* **Text-to-Web support + `survey_mode` column (SUR-1368).**
  `consolidated` gains a per-campaign `survey_mode` column, classified
  from the source CSV:
  * `"t2w"` -- web completes present; `n_completed` counts the
    `web_complete` callback (not `id.close.scriptDate`, which for
    Text-to-Web is just the link sent to every consenter and overstated
    completion 2-7x, making `n_completed == n_consented`).
  * `"sms"` -- no web completes and no survey link; live SMS, completes
    on `id.close.scriptDate` (unchanged).

  The authoritative campaign flag (`campaigns.use_web_completes`) is not
  in the CSV export, so this is a data-only heuristic.

# survey160r 0.14.1

## Bug fixes

* `pct_le` is now always a numeric (double) column, even when a
  campaign has no valid latency cells and every value is NA. The
  populated assembly path took `pct_le` straight from the joined
  frame without the `as.numeric()` coercion its sibling numeric
  columns use, so an all-NA join result collapsed to a logical
  vector. Downstream the fleet writer casts this column to a
  float64 Arrow schema; a logical vector failed that cast
  (`Invalid: cannot convert`) and silently dropped the campaign's
  Parquet output. Affected campaigns are valid but degenerate --
  every recipient hit a carrier delivery error or sat in limbo, so
  none produced a measurable latency delta (SUR-1365).

# survey160r 0.14.0

## New features

* `consolidated` now carries four denormalised **summary metrics**
  columns (Phase 1 PR 4, spec §4): `n_texted`, `n_consented`,
  `n_completed`, `n_ineligible`. Counts are anchored by
  `id.intro.batchDate` -- cohort-by-send-time, matching the latency
  view's hour bucketing. `n_texted` is the count of rows with a
  non-NA `id.intro.batchDate` (intro dispatched); `n_consented` is
  the subset that passes `config$filters$population` (re-using the
  existing consent definition rather than a parallel
  finalValue/finalText anchor); `n_completed` is the subset with a
  non-NA `id.close.scriptDate`. `n_ineligible` is per-segment: the
  count of respondents whose `id.ineligible.scriptDate` is non-NA
  AND whose last reached question lands at the segment's endpoint,
  joined to latency cells on `(campaign_id, date, hour_local,
  segment_index)`. The four counts denormalise across the latency
  rows that share their bucket keys; Parquet RLE compresses the
  repetition. `.algorithm_version` bumps to `"2.1.0"`,
  `.schema_version` to `"4"`. Consumers that don't read the new
  columns are unaffected; consumers that do can gate on a
  `has_summary_data` probe (see survey160-shiny#TBD).

* **Scaffold-first consolidated seeding.** `aggregate_consolidated()`
  now builds output rows from the UNION of latency-frame and
  summary-frame bucket keys (cross-joined with segments × thresholds),
  not just from latency cells. This preserves summary-only buckets:
  hours where every respondent was filtered out (e.g. 100 texted, 0
  consented) still appear in the parquet with `n_texted` populated
  and latency cell counts at 0 / NA. Without this, the pre-filter
  summary contract would be defeated whenever the population filter
  rejected an entire hour.

* **Symmetric NA → 0 backfill for count columns.** Scaffold rows with
  no matching summary or ineligible row fill all four summary counts
  (`n_texted`, `n_consented`, `n_completed`, `n_ineligible`) plus the
  existing latency count columns (`n`, `n_le`, `n_resp_over`,
  `n_na_*`) with 0L. The previous design left summary counts as NA
  and only filled `n_ineligible` to 0 -- consumers couldn't tell
  "no data" from "no respondents", and the asymmetry was a footgun.

* **`date_filter` now restricts the summary view too.** Previously
  `date_filter` only narrowed the latency frame; the summary
  computation ran on the full pre-filter population. Symmetric
  semantics ("show me this date's data") matches user intent and
  avoids the case where a date_filter that excludes everyone still
  emits summary rows for the excluded dates.

# survey160r 0.13.0

## Breaking changes

* The latency pipeline is renamed to the **campaign pipeline** -- the
  per-campaign Parquet is becoming a general per-campaign metrics
  artifact (latency view today, summary metrics view next). All
  orchestrator exports rename:

  | Before | After |
  |---|---|
  | `latency_run()` | `campaign_run()` |
  | `latency_report()` | `campaign_report()` |
  | `latency_build_config()` | `campaign_build_config()` |
  | `latency_validate_config()` | `campaign_validate_config()` |
  | `latency_config_hash()` | `campaign_config_hash()` |
  | `latency_discover_questions()` | `campaign_discover_questions()` |

  The latency sub-view files (`R/latency_aggregate.R`,
  `R/latency_frame.R`, `R/latency_filter.R`,
  `R/latency_diagnostics.R`, `R/latency_primitives.R`) keep their
  names -- they implement latency-specific computations and sit
  alongside the new orchestrator files as one named view of the
  campaign pipeline. Behaviour is unchanged; output Parquet schema
  is byte-identical to 0.12.0. `algorithm_version` stays `"2.0.0"`
  because the algorithm did not change; the rename is API only.

* The algorithm spec doc moves from `r-scripts/latency_scripts.md` to
  `r-scripts/campaign_scripts.md` (lives in the meta-workspace, not
  this repo).

# survey160r 0.12.0

## New features

* `consolidated` now carries seven new per-cell columns (SUR-1316):
  `mean_delta_min`, `p50_delta_min`, `p90_delta_min`, `p95_delta_min`
  (distribution shape, threshold-independent so identical across the
  four threshold rows of a cell) and `n_na_parse`, `n_na_missing`,
  `n_na_chain` (per-cell NA-reason counts derived from
  `na_reason`). `.schema_version` bumps to `"3"`. The new columns
  unlock per-cell distribution and data-quality visualisations
  downstream; existing consumers that read columns by name are
  unaffected.

# survey160r 0.11.0

## Breaking changes

* The package is now algorithm-only. Fleet orchestration, GCS writes, and
  scheduling have moved to the survey160-shiny repo (SUR-1313).
* `run_latency()` is renamed to `latency_run()` and is now
  source-agnostic. The signature is
  \code{latency_run(campaign_id, data, config = NULL, run_at = NULL,
  run_by = NULL, ...)}: `data` is a caller-supplied data frame, so the
  function works equally well for CSVs pulled from GCS via
  `s160_gcs_pull_csv()` and for off-GCS sources (Dropbox, local disk,
  S3, etc.). `bucket`, `source_bucket`, `uploader`, `field_timezone`,
  `project_id`, `date_filter`, and `respondent_id_column` are no
  longer arguments on `latency_run()` itself; the build-config knobs
  flow through `...` to `latency_build_config()`. Optional `config =`
  lets callers pre-build (and mutate) the config, skipping the
  auto-build. The function returns the result list from
  `latency_report()`.
* `pull_csv_from_gcs()` is renamed to `s160_gcs_pull_csv()` to match
  the `s160_gcs_*` family; behaviour unchanged.
* New exported reader `s160_read_csv(path, ...)` reads a CSV from a
  local path and stamps the same `source_csv_hash` /
  `source_csv_path` attributes that `s160_gcs_pull_csv()` does. Use
  for backfilling archived campaigns from disk / Dropbox / S3 mounts;
  hand the result to `latency_run()` and provenance flows through to
  `result$meta` like it does for active GCS campaigns.
* `latency_report()` now populates `result$meta$source_csv_hash` and
  `result$meta$source_csv_path` from the input data's attributes (in
  addition to stamping `consolidated$source_csv_hash` per-row). Meta
  survives data-frame subsetting and is the contract downstream
  persistence layers should read.
* `run_latency_all()`, `write_to_gcs()`, `s160_gcs_latency_output_status()`,
  and `read_latency()` are removed. The first three move to
  survey160-shiny; `read_latency()` had no in-tree consumers and is
  dropped (reintroduce if a real consumer surfaces).
* `scripts/bulk_reprocess.R` is removed; survey160-shiny's
  `scripts/run_latency.R` is now the supported fleet entry point.
* `future`, `future.apply`, `duckdb`, and `DBI` leave Suggests. `arrow`
  leaves Imports (no remaining call sites in this package).

# survey160r 0.10.1

## Internal

* Cleanup pass on the latency internals: unified Survey160 CSV timestamp
  parsing behind `parse_s160_timestamps_chr()`, added a `safe_pct()` helper
  for the "percent of X, NA if denominator is zero" pattern, encapsulated
  the data + parse-failed-mask plumbing behind `subset_parsed_input()`,
  extracted `classify_na_reason()` from the segment loop, and split
  `aggregate_consolidated()` into per-aggregation helpers
  (`aggregate_totals()`, `aggregate_worst_cascade()`,
  `aggregate_segment_cells()`, `assemble_consolidated()`). Numeric output
  is unchanged; the refactor only reshapes the call graph (SUR-1305).

# survey160r 0.10.0

## New features

* Parallel fleet runs and skip-unchanged campaigns (SUR-1305, #20).
  `run_latency_all()` parallelizes per-campaign processing and skips
  campaigns whose source CSV is unchanged since the last run.
  (`run_latency_all()` itself is later removed in 0.11.0, when fleet
  orchestration moved to survey160-shiny.)

# survey160r 0.9.0

## Breaking changes

* The consolidated frame now carries **two grains** in one file: hour
  rows (one per `(campaign_id, date, hour_local, segment, threshold_min)`
  with `hour_local` 0-23) for time-of-day analysis, plus day rollup rows
  (`hour_local = NA`) carrying correct day-grain `n`, `pct_le`, and
  respondent-cascade columns. Downstream consumers filter on
  `hour_local IS NULL` for day rollups, `hour_local IS NOT NULL` for
  time-of-day; both are arithmetically correct without any further
  rollup. The `time_bucket` config knob and the `reports` config slot
  are removed -- `latency_build_config()` no longer accepts a
  `time_bucket` argument, and `validate_config()` rejects `reports` as
  an unknown key. Existing Parquets in `gs://s160_analytics_*/latency/`
  (which carried only one grain) must be regenerated via the
  survey160-shiny fleet runner (SUR-1304, SUR-1313).
* Note for naive aggregators: summing the hour rows' `n_respondents`
  over-counts cross-hour respondents (a respondent active in two hours
  appears in both hours' distinct-respondent counts). Always read the
  day rollup row (`hour_local IS NULL`) for correct day-grain cascade;
  do not attempt to recompute it by aggregating the hour rows.
* The `texting_windows` config field is removed. The algorithm no longer
  filters dispatches by an analyst-declared texting plan; `n` and
  `pct_le` now count every valid dispatch. The pre-removal feature
  excluded out-of-window dispatches from the in-window denominator;
  with the cube schema introduced in this release downstream consumers
  can see which hours had high volume directly from the hour rows.
  Diagnostics field `n_out_of_window_dropped` and
  `windows_normalized_utc` are dropped along with the feature.
  `latency_build_config()` and `latency_run()` no longer accept a
  `texting_windows` argument (SUR-1304).

# survey160r 0.8.0

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

## Documentation

* Declare `R (>= 4.1)` in `DESCRIPTION` to match what the current `arrow`,
  `dplyr`, and `lubridate` imports already require.
* README latency YAML example sets `respondent_id_column: ~` instead of
  the misleading `userid`, which in Survey160 v2 CSVs is the agent login
  rather than a per-respondent identifier.
* README first-time-setup notes that producing latency outputs requires
  Storage Object Creator on the destination analytics bucket, in addition
  to Storage Object Viewer on the source bucket.

# survey160r 0.7.1

## Bug fixes

* `s160_api_campaign_get()` now strips sub-second precision when parsing
  ISO-8601 timestamp columns, so values like
  `"2026-01-15T09:30:00.123456Z"` (which PostgreSQL can emit) come back as
  `POSIXct` rather than falling through to the string fallback. Numeric UTC
  offsets (`+05:30`, `-0400`) are also covered. The `httr::GET` import is
  now declared explicitly to match the other `httr` imports (SUR-1253).

# survey160r 0.7.0

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

* `RELEASING.md` clarifies that the release tag must point at the release
  PR's merge SHA, not `HEAD` (#14).

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
