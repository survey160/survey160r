# survey160r

[![R-CMD-check](https://github.com/survey160/survey160r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/survey160/survey160r/actions/workflows/R-CMD-check.yaml)

R package for accessing Survey160 campaign data -- read results from Google Cloud Storage, trigger fresh exports via the API, and compute per-campaign recipient-latency reports written as Parquet to GCS.

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
On first run, `s160_api_auth()` prompts for your user ID and API key
(get these from your survey manager) and saves them to `~/.Renviron`.

```r
library(survey160r)

# 1. Authenticate to GCS (same as above)
s160_gcs_init(bucket = "campaign_results")

# 2. Authenticate to the Survey160 API (prompts on first run)
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

### Check export status

You can check the last export timestamp without triggering a new export:

```r
status <- s160_gcs_campaign_results_status(campaign_id)
status$updated  # last export time
status$size     # file size
```

## Latency analysis

Compute a per-campaign recipient-latency report from a raw campaign CSV and write it as a single Parquet file to a dedicated analytics bucket. Replaces the per-wave inline scripts that the analytics team used to maintain by hand: one algorithm, one output schema, one YAML config per campaign.

The pipeline has three layers. `latency_report(data, config)` is the pure function -- deterministic, no I/O, no globals -- and is the recommended entry point for tests and ad-hoc analysis. `pull_csv_from_gcs()` / `write_to_gcs()` are thin I/O wrappers. `run_latency()` glues them together for the manual happy path.

### Happy path

```r
library(survey160r)
s160_gcs_init(bucket = "campaign_results")

# config_path points to a per-wave YAML (see "Config" below)
run_latency(
  campaign_id = 1234,
  config_path = "configs/wave_w1_20260126.yaml",
  bucket = "s160_analytics",
  run_by = "lshimokawa"
)
# -> writes gs://s160_analytics/latency/1234_latency.parquet
```

### Pure function

```r
data <- pull_csv_from_gcs(campaign_id = 1234)
config <- read_config("configs/wave_w1_20260126.yaml")
result <- latency_report(data, config)

result$consolidated     # one row per (campaign_id, date, hour_local, segment, threshold_min)
result$latency_frame    # one row per (respondent, segment) with na_reason classification
result$diagnostics      # parse failures, NA-by-reason counts, clamped negatives, respondent summary
result$meta             # algorithm_version, schema_version, config_hash, run_at_utc
```

### Output schema

The Parquet at `gs://<bucket>/latency/<campaign_id>_latency.parquet` carries provenance columns alongside the metrics, so each row stands on its own without sidecar manifests:

| Column | Purpose |
|---|---|
| `campaign_id`, `project_id` | Wave identity |
| `date`, `hour_local` | Bucket grain (hour_local NA for day buckets) |
| `segment`, `segment_index` | Pair of consecutive flow questions, e.g. `intro->q1` |
| `threshold_min` | Universal fleet threshold (1, 3, 5, or 10 min) |
| `n`, `pct_le` | Per-segment in-window dispatch metrics |
| `n_respondents`, `pct_resp_hit_gt`, `pct_resp_worst_gt` | Respondent-cascade metrics |
| `algorithm_version`, `config_hash`, `source_csv_hash`, `run_at_utc`, `run_by` | Provenance |

### Reading results back

```r
view <- read_latency(bucket = "s160_analytics")
DBI::dbGetQuery(view$con, "SELECT campaign_id, segment, pct_le FROM latency WHERE threshold_min = 5")
DBI::dbDisconnect(view$con, shutdown = TRUE)
```

### Config

A YAML config tells `latency_report()` which columns to use, the survey-flow order, the field timezone, the texting windows, and which respondents to keep. Minimal example:

```yaml
project_id: 1234
project_name: "Wave 1 -- January 2026"
campaign_id: 1234
field_timezone: "America/New_York"

flow:
  questions: [intro, q1, q2, q3, close]

filters:
  population: 'id.intro.finalText == "Yes"'
  campaign_id_column: campaignid
  respondent_id_column: userid
  date_filter: ["2026-01-26"]

texting_windows:
  - { date: "2026-01-26", start_hour: 16, end_hour: 24 }

reports:
  time_bucket: day
```

`validate_config()` runs fail-fast checks: required columns present, flow order matches the data, texting windows cover survey dates, no unknown keys, no terminal states (`refusal`, `ineligible`) in `flow.questions`.

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
target bucket. Contact a sysadmin if you get 403 errors after
authenticating.

### API (`s160_api_auth`)

On the first call to `s160_api_auth()`, you'll be prompted for:

1. **User ID** -- your Survey160 API user ID.
2. **API key** -- your Survey160 API key.

Both are saved to `~/.Renviron` so you won't be asked again. Get
these from your survey manager.

## End-to-end testing

Runs against the QA environment with real GCS and API calls. Requires
a cached OAuth token and API credentials in `~/.Renviron`.

```bash
make e2e
```

## Troubleshooting

Reset credentials (edit `~/.Renviron`, remove the relevant line, restart R):

```r
file.edit("~/.Renviron")
# S160_GCS_CLIENT_SECRET  -- GCS OAuth client secret
# S160_API_USERID         -- API user ID
# S160_API_KEY            -- API key
```

Clear cached OAuth tokens:

```r
gargle::gargle_oauth_sitrep()  # list cached tokens and their location
# delete the cache directory shown above, then restart R
```

## License

MIT
