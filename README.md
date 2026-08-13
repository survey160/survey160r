# survey160r

[![R-CMD-check](https://github.com/survey160/survey160r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/survey160/survey160r/actions/workflows/R-CMD-check.yaml)
[![R-universe](https://survey160.r-universe.dev/badges/survey160r)](https://survey160.r-universe.dev/survey160r)

R package for accessing Survey160 campaign data, in three layers:

- **Disposition screening** -- screen a phone sample against every recipient Survey160 has ever contacted, to answer "which of these numbers have we contacted / completed / refused before?" and clean a sample list before you field it. The Survey-Manager surface. Guide: `vignette("disposition")`.
- **Latency analysis** -- compute a per-campaign recipient-latency report from a raw campaign CSV, as an in-memory R object. Guide: `vignette("latency")`.
- **Raw data access** -- read campaign results from Google Cloud Storage (`s160_gcs_*`) and trigger fresh exports via the API (`s160_api_*`). Documented [below](#raw-data-access).

Both analyst surfaces return plain in-memory data frames; fleet orchestration and Parquet persistence live in downstream consumer projects. See `?survey160r` for an overview from the R console.

## Installation

```r
# From R-universe
install.packages("survey160r", repos = "https://survey160.r-universe.dev")

# From GitHub
install.packages("pak")  # if not already installed
pak::pkg_install("survey160/survey160r")
```

## Quick start -- disposition screening

The most common task: pull the shared disposition dataset once, then screen a sample in place. Full walkthrough -- columns, ad-hoc queries, caveats -- in `vignette("disposition")`.

```r
library(survey160r)
s160_gcs_init(bucket = "s160_disposition_prod")   # one-time browser sign-in (cached)

dataset <- disposition_pull()                       # download the projection (cached)
cleaned <- disposition_screen(my_sample, dataset)   # screening columns appended 1:1
subset(cleaned, !ever_complete & !ever_terminated)  # drop already-completed / refused
```

## Raw data access

Two ways in, both returning a campaign's results as a data frame. Authenticate once with `s160_gcs_init()` (see [First-time setup](#first-time-setup)); the API path additionally needs `s160_api_auth()`.

### Read the last export from GCS

```r
library(survey160r)
s160_gcs_init(bucket = "campaign_results")   # browser sign-in on first run, then cached

campaigns   <- s160_gcs_campaign_results_list()             # available campaign ids
campaign_id <- campaigns[1]
df          <- s160_gcs_campaign_results_read(campaign_id)  # results -> data frame
files       <- s160_gcs_campaign_results_files(campaign_id) # files in the campaign folder

# Last export time / size, without downloading:
status <- s160_gcs_campaign_results_status(campaign_id)
status$updated
status$size
```

### Trigger a fresh export via the API

When you need data newer than the last GCS export, the API triggers a fresh export, polls until it's ready, and downloads it in one step. `s160_api_auth(env)` authenticates to `"prod"` (default) or `"staging"` and returns a *connection* that pairs the API URL, bucket, and key so they can't be mismatched; on first run it prompts for any missing credentials and saves them to `~/.Renviron`.

```r
s160_api_auth()                                  # defaults to prod; prompts on first run
df <- s160_api_campaign_results(campaign_id)     # fresh export -> data frame

df <- s160_api_campaign_results(campaign_id, filter_open = TRUE)  # drop open/uncontacted
df <- s160_api_campaign_results(campaign_id, timeout = 600)       # large campaigns (default 300s)
df <- s160_api_campaign_results(campaign_id, destdir = ".")       # keep the CSV, not a temp file
```

Capture the connection to keep prod and staging live in one session -- e.g. to A/B compare the same campaign; each carries its own paired bucket:

```r
prod <- s160_api_auth("prod")
stg  <- s160_api_auth("staging")
df_prod <- s160_api_campaign_results(campaign_id, conn = prod)
df_stg  <- s160_api_campaign_results(campaign_id, conn = stg)
```

A conn-less call uses the most recent `s160_api_auth()`.

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
source bucket (the campaign-results bucket, or `s160_disposition_prod`
for disposition screening). Persisting latency outputs (via the consumer
project's fleet runner) additionally needs **Storage Object Creator** on
the destination analytics bucket. Contact a sysadmin if you get 403
errors after authenticating.

### API (`s160_api_auth`)

Credentials live in `~/.Renviron` and are read per environment:

1. **User ID** -- `S160_API_USERID` (shared across environments).
2. **API key** -- a per-environment variable: `S160_PROD_API_KEY` for
   prod (falling back to the legacy `S160_API_KEY` if unset), and
   `S160_STAGING_API_KEY` for staging.

Any missing value is prompted on the first `s160_api_auth(env)` call for
that environment and saved to `~/.Renviron`, so you won't be asked
again. Get these from your survey manager.

## Documentation

- **Guides (articles):** `vignette("disposition")` and `vignette("latency")` -- also rendered as Articles on the [package site](https://survey160.r-universe.dev/survey160r).
- **Function reference:** `?survey160r`, or `help(package = "survey160r")`.
- **Changelog:** `NEWS.md` (or `news(package = "survey160r")` after install). Cutting a release: [`RELEASING.md`](RELEASING.md). Project conventions and agent context: `CLAUDE.md`.

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

## License

MIT
