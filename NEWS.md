# survey160r 0.6.0

## New features

* Latency analysis pipeline. Replaces the per-wave inline R scripts that the
  analytics team used to maintain by hand with a single algorithm, output
  schema, and YAML config per campaign. New public functions:

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

# survey160r 0.5.0 and earlier

No release notes -- this file was introduced in 0.6.0.
