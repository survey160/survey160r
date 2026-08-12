# survey160r

[![R-CMD-check](https://github.com/survey160/survey160r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/survey160/survey160r/actions/workflows/R-CMD-check.yaml)
[![R-universe](https://survey160.r-universe.dev/badges/survey160r)](https://survey160.r-universe.dev/survey160r)

R package for accessing Survey160 campaign data -- read results from Google Cloud Storage, trigger fresh exports via the API, and compute per-campaign recipient-latency reports as in-memory R objects. Fleet orchestration and Parquet persistence live in downstream consumer projects.

## Installation

```r
# From R-universe
install.packages("survey160r", repos = "https://survey160.r-universe.dev")

# From GitHub
install.packages("pak")  # if not already installed
pak::pkg_install("survey160/survey160r")
```

## Usage

```r
library(survey160r)

# Authenticate and set bucket (opens browser on first run)
s160_gcs_init(bucket = "campaign_results")

# List available campaigns
campaigns <- s160_gcs_campaign_results_list()
campaign_id <- campaigns[1]

# Read a campaign's results into a data frame
df <- s160_gcs_campaign_results_read(campaign_id)

# List files in a campaign folder
files <- s160_gcs_campaign_results_files(campaign_id)
```

## API usage

The API functions let you trigger a fresh campaign results export and
download the data in one step. This is useful when you need the latest
data rather than whatever was last exported to GCS.

Requires both GCS auth (`s160_gcs_init`) and API auth (`s160_api_auth`).
`s160_api_auth(env)` authenticates to a named environment -- `"prod"`
(default) or `"staging"` -- and returns a *connection*. Addressing by
name resolves the API URL, the campaign-results bucket, and the API key
together, so they can't be mismatched. On first run it prompts for any
missing credentials and saves them to `~/.Renviron`.

```r
library(survey160r)

# 1. Authenticate to GCS once -- one Google sign-in reads every bucket
s160_gcs_init(bucket = "campaign_results")

# 2. Authenticate to the Survey160 API (defaults to prod; prompts on first run)
s160_api_auth()

# 3. Export and download -- triggers a fresh export, polls until ready,
#    and returns the results as a data frame
df <- s160_api_campaign_results(campaign_id)

# Exclude open/uncontacted conversations
df <- s160_api_campaign_results(campaign_id, filter_open = TRUE)

# Increase timeout for large campaigns (default 300s)
df <- s160_api_campaign_results(campaign_id, timeout = 600)

# Save the CSV locally instead of using a temp file
df <- s160_api_campaign_results(campaign_id, destdir = ".")
```

### Comparing production and staging

`s160_api_auth(env)` returns a connection you can capture and pass as
`conn =`, so prod and staging stay live in the same session -- e.g. to
A/B compare the same campaign. Each connection carries its own paired
bucket, so the export trigger, poll, and read all target the right
environment.

```r
s160_gcs_init(bucket = "campaign_results")   # one GCS auth covers all buckets

prod <- s160_api_auth("prod")
stg  <- s160_api_auth("staging")

df_prod <- s160_api_campaign_results(campaign_id, conn = prod)
df_stg  <- s160_api_campaign_results(campaign_id, conn = stg)
```

A conn-less call uses the most recent `s160_api_auth()`, so
single-environment use needs no `conn =`.

### Check export status

You can check the last export timestamp without triggering a new export:

```r
status <- s160_gcs_campaign_results_status(campaign_id)
status$updated  # last export time
status$size     # file size
```

## Latency analysis

Compute a per-campaign recipient-latency report from a raw campaign CSV and return it as an in-memory R object. Replaces the per-wave inline scripts that the analytics team used to maintain by hand: one algorithm, one output schema, one config shape per campaign.

This package is **algorithm-only and source-agnostic**. `latency_report(data, config)` is the pure function -- no I/O or mutable globals, and fully deterministic when passed a `run_at` (otherwise `run_at_utc` comes from `Sys.time()`) -- and is the recommended entry point for tests and ad-hoc analysis. `latency_run(campaign_id, data, ...)` composes `latency_build_config()` + `latency_report()` over a caller-supplied data frame; pair it with `s160_gcs_pull_csv()` for the GCS source path, or read the CSV yourself for any other source. Persisting outputs as Parquet, walking the fleet, and scheduling all live in downstream consumer projects.

### Happy path -- GCS source

```r
library(survey160r)
s160_gcs_init(bucket = "campaign_results")

data   <- s160_gcs_pull_csv(1234)
result <- latency_run(1234, data, field_timezone = "America/New_York")
head(result$consolidated)
result$meta$source_csv_hash    # sha256 of the source CSV
result$meta$source_csv_path    # canonical gs:// path
```

### Backfill -- archived CSV on disk / Dropbox

```r
data   <- s160_read_csv("~/Dropbox/archive/campaign_500.csv")
result <- latency_run(500, data, field_timezone = "America/New_York")
result$meta$source_csv_hash    # sha256 of the local file
result$meta$source_csv_path    # the path you passed
```

`s160_read_csv()` is the local-source sibling of `s160_gcs_pull_csv()`
-- both produce a data frame with `source_csv_hash` and
`source_csv_path` attributes set, which `latency_run()` then surfaces
on `result$meta`. Pick the reader that matches where the CSV lives;
the algorithm call is identical.

For ad-hoc invocations with a hand-built data frame (synthetic /
testing), pass `data` to `latency_run()` directly -- `result$meta`
provenance will be `NA`, which is correct for that case.

### Pure function

```r
data <- s160_gcs_pull_csv(campaign_id = 1234)
config <- latency_build_config(1234, data)
result <- latency_report(data, config)

result$consolidated     # one row per (campaign_id, date, hour_local, segment, threshold_min)
result$latency_frame    # one row per (respondent, segment) with na_reason classification
result$diagnostics      # parse failures, NA-by-reason counts, clamped negatives, respondent summary
result$meta             # algorithm_version, schema_version, config_hash, run_at_utc, source_csv_hash, source_csv_path
```

### Result shape

`result$consolidated` (the data frame this package returns) is also the column shape of the Parquet a consumer project writes. Each row stands on its own without sidecar manifests:

| Column | Purpose |
|---|---|
| `campaign_id`, `project_id` | Wave identity |
| `date`, `hour_local` | Bucket grain (hour_local NA for day buckets) |
| `segment`, `segment_index` | Pair of consecutive flow questions, e.g. `intro->q1` |
| `threshold_min` | Universal fleet threshold (1, 3, 5, or 10 min) |
| `n`, `pct_le` | Per-segment in-window dispatch metrics |
| `n_respondents`, `pct_resp_hit_gt`, `pct_resp_worst_gt` | Respondent-cascade metrics |
| `algorithm_version`, `config_hash`, `source_csv_hash`, `run_at_utc`, `run_by` | Provenance |

### Config

`latency_build_config(campaign_id, data, ...)` assembles the config from the CSV header alone -- pure function, no I/O. Override defaults via named args:

```r
config <- latency_build_config(
  campaign_id = 1234,
  data = s160_gcs_pull_csv(1234),
  field_timezone = "America/New_York",
  project_id = 9999,
  date_filter = "2026-01-26",
  respondent_id_column = NULL    # `userid` is agent login, not per-respondent
)
```

`latency_validate_config()` runs fail-fast checks: required columns present, flow order matches the data, no unknown keys, no terminal states (`refusal`, `ineligible`) in `flow.questions`.

## Disposition screening

Screen a phone sample against the **disposition dataset** -- one row per `(phone, campaign_id)`, contacted-only, produced upstream by the pipeline -- to answer "which of these numbers have we contacted / completed / refused before?" and clean a sample list. The readers stay in the `disposition_*` family (bare-named) because they read a survey160r-*derived* projection, not a raw source.

The Survey-Manager path is pull once, then screen a sample in place:

```r
library(survey160r)
s160_gcs_init(bucket = "s160_disposition_prod")   # one-time browser OAuth

# Download the projection to a local (cached) path; refresh = TRUE forces a fresh copy
dataset <- disposition_pull()                       # env = "prod" (default) or "dev"

# Annotate a sample data frame in place -- original rows/columns preserved,
# disposition screening columns appended 1:1
cleaned <- disposition_screen(my_sample, dataset, phone_col = "phone")
# drop already-finished numbers; blank/unparseable phones come back all-NA and
# are kept (so they surface for correction rather than vanishing)
subset(cleaned, !(ever_complete %in% TRUE | ever_terminated %in% TRUE))
```

Ad-hoc query surface over the same projection:

```r
# One row per phone: cross-campaign screening flags + latest_disposition
disposition_summary(dataset, phones = c("5551234567", "5559876543"))

# The raw rows beneath the per-phone rollup: one per (phone, campaign_id)
disposition_records(dataset, campaign_ids = 1234)

# Roll an in-memory disposition frame up to one row per phone (pure, no I/O) --
# read a projection once and screen several samples against the in-memory frame
records <- disposition_records(dataset)
disposition_rollup(records, phones = my_sample$phone)
```

Building the per-respondent disposition frame from a raw campaign CSV (the producer side, pure and source-agnostic like `latency_run()`) is `disposition_run(campaign_id, data)`; `disposition_input_columns()` gives the column-projection set, mirroring `latency_input_columns()`.

## First-time setup

### GCS (`s160_gcs_init`)

On the first call to `s160_gcs_init()`, you'll be walked through two
one-time steps:

1. **Client secret** -- you'll be prompted to paste the OAuth client
   secret (get it from your team lead). It's saved to `~/.Renviron`
   so you won't be asked again.
2. **Google sign-in** -- a browser window opens for you to sign in
   with your Google account. The token is cached locally so subsequent
   runs authenticate automatically.

You may also be asked to allow OAuth token caching (say yes) and to
install the `httpuv` package for a smoother auth experience (say yes).

Your Google account needs **Storage Object Viewer** permission on the
campaign-results source bucket. Persisting latency outputs (via the
consumer project's fleet runner) additionally needs **Storage Object
Creator** on the destination analytics bucket. Contact a sysadmin if
you get 403 errors after authenticating.

### API (`s160_api_auth`)

Credentials live in `~/.Renviron` and are read per environment:

1. **User ID** -- `S160_API_USERID` (shared across environments).
2. **API key** -- a per-environment variable: `S160_PROD_API_KEY` for
   prod (falling back to the legacy `S160_API_KEY` if unset), and
   `S160_STAGING_API_KEY` for staging.

Any missing value is prompted on the first `s160_api_auth(env)` call for
that environment and saved to `~/.Renviron`, so you won't be asked
again. Get these from your survey manager.

## End-to-end testing

Runs against the staging environment with real GCS and API calls.
Requires a cached OAuth token and `S160_API_USERID` +
`S160_STAGING_API_KEY` in `~/.Renviron`.

```bash
make e2e
```

## Troubleshooting

Reset credentials (edit `~/.Renviron`, remove the relevant line, restart R):

```r
file.edit("~/.Renviron")
# S160_GCS_CLIENT_SECRET  -- GCS OAuth client secret
# S160_API_USERID         -- API user ID (all environments)
# S160_PROD_API_KEY       -- prod API key (or legacy S160_API_KEY)
# S160_STAGING_API_KEY    -- staging API key
```

Clear cached OAuth tokens:

```r
gargle::gargle_oauth_sitrep()  # list cached tokens and their location
# delete the cache directory shown above, then restart R
```

## Development

- Changelog: `NEWS.md` (or `news(package = "survey160r")` after install).
- Cutting a release: see [`RELEASING.md`](RELEASING.md).
- Project conventions and agent context: see `CLAUDE.md`.

## License

MIT
