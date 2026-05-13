# survey160r (development version)

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
