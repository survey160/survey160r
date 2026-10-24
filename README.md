# survey160r

[![R-CMD-check](https://github.com/survey160/survey160r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/survey160/survey160r/actions/workflows/R-CMD-check.yaml)
[![R-universe](https://survey160.r-universe.dev/badges/survey160r)](https://survey160.r-universe.dev/survey160r)

R package for accessing Survey160 campaign data, across these areas:

- **[Raw data access](#raw-data-access)** -- read campaign results from Google Cloud Storage (`s160_gcs_*`) and trigger fresh exports via the API (`s160_api_*`).
- **[Latency analysis](vignettes/latency.Rmd)** -- compute a per-campaign recipient-latency report from a raw campaign CSV, as an in-memory R object.
- **[Disposition screening](vignettes/disposition.Rmd)** -- screen a phone sample against every recipient Survey160 has contacted, dropping numbers already completed, refused, or screened out before you field.
- **[Opt-out screening](vignettes/opt-out.Rmd)** -- screen a phone sample against the opt-out list, flagging numbers that have opted out before you field.
- **[Campaign metrics](vignettes/campaign-metrics.Rmd)** -- download the consolidated per-campaign metrics + recipient-latency projection (`campaign_all.parquet`) with `campaign_metrics_pull()`, read it into analysis-ready cells with `campaign_metrics_records()`, and roll those up to funnel counts and rates by any project dimension with `campaign_metrics_summary()`.

**New here?** To screen a sample before fielding, jump to [Disposition screening](#disposition-screening). First time on this machine, start with [First-time setup](#first-time-setup) -- you need a credential and a bucket grant before any data call works.

## Which function do I need?

| I want to... | Call | Guide |
|---|---|---|
| Drop numbers that already completed, refused, or screened out before I field | `disposition_pull()` then `disposition_screen(my_sample, dataset)` | [Disposition screening](#disposition-screening) |
| Drop numbers that opted out | `opt_out_pull()` then `opt_out_screen(my_sample, optout)` | [Opt-out screening](#opt-out-screening) |
| Look up a few numbers' contact history | `disposition_summary(dataset, phones = )` (one row per phone) / `disposition_records(dataset, phones = )` (one row per campaign) | [Disposition screening](#disposition-screening) |
| Funnel rates by carrier, mode, topic, or campaign | `disposition_funnel(dataset, by = "carrier")` | [Disposition screening](#disposition-screening) |
| Follow what each number did campaign after campaign | `disposition_paths()` (one path per phone) / `disposition_steps()` (one row per campaign) | `vignette("disposition")` |
| Funnel rates across the fleet by registration, brand, client, or mode | `campaign_metrics_pull()` then `campaign_metrics_summary(path, by = )` | [Campaign metrics](#campaign-metrics) |
| See how fast each message followed the last one in a campaign | `latency_run(campaign_id, data)` | [Latency analysis](#latency-analysis) |
| How many recipients reached each question | `question_funnel(data, c("intro", "q1", "close"))` | `vignette("latency")` |
| Read a campaign's last export | `s160_gcs_campaign_results_read(campaign_id)` | [Raw data access](#raw-data-access) |
| Get an export fresher than the last one on GCS | `s160_api_auth()` then `s160_api_campaign_results(campaign_id)` | [Raw data access](#raw-data-access) |

Every screen appends columns to your sample 1:1 and never drops a row -- you filter on your own rules. Every `*_pull()` downloads once and reuses a local cache (`refresh = TRUE` re-downloads). `?survey160r` lists every function by task from the R console.

## Installation

```r
# From R-universe
install.packages("survey160r", repos = "https://survey160.r-universe.dev")

# From GitHub
install.packages("pak")  # if not already installed
pak::pkg_install("survey160/survey160r")

# Recommended: low-memory, fast disposition screening (used automatically when installed)
install.packages("duckdb")
```

With `duckdb` installed, `disposition_screen()` reads only your sample's rows from the projection, a campaign- or date-filtered `disposition_records()` / `disposition_summary()` reads only the matching rows, and `disposition_funnel()` groups inside the scan: each takes seconds and a GB or two of RAM at most, so it runs on a laptop. Without it the readers fall back to loading the whole projection, which needs tens of GB. `disposition_steps()` / `disposition_paths()` require `duckdb`, `DBI` and `dbplyr` (no fallback).

> R-universe rebuilds from `main` and can lag a fresh release by up to ~30 minutes. If a newly added function (for example `disposition_pull()`) is not found after installing from R-universe, get the latest straight from GitHub with the `pak` line above and restart R.

## Try it without credentials

The pure functions run on an in-memory data frame, so you can see the package work before setting up any access. This rolls a two-campaign history up to one row per phone:

```r
library(survey160r)
records <- data.frame(
  phone = c("5551234567", "5551234567", "5559876543"),
  campaign_id = c(101L, 102L, 101L),
  engaged = c(1L, 1L, 0L), opted_in = c(1L, 0L, 0L), completed = c(1L, 0L, 0L),
  web_complete = c(0L, 0L, 0L), refused = c(0L, 1L, 0L), ineligible = c(0L, 0L, 0L),
  terminated = c(0L, 1L, 0L),
  disposition_date = as.Date(c("2026-01-10", "2026-01-20", "2026-01-15"))
)
disposition_summary(records, phones = c("5551234567", "5550000000"))
```

Everything else -- reading GCS, pulling the disposition dataset, the API -- needs the [access below](#first-time-setup).

## Raw data access

Two ways in, both returning a campaign's results as a data frame. Authenticate once with `s160_gcs_init()` (see [First-time setup](#first-time-setup)); the API path additionally needs `s160_api_auth()`.

The `s160_gcs_campaign_results_*` functions below are the general-purpose readers (with `destdir` / `filename` control). To feed a [latency report](vignettes/latency.Rmd), read with `hash = TRUE` (or use `s160_read_csv()` for a local file) -- that stamps the `source_csv_hash` / `source_csv_path` provenance that `latency_run()` records.

### Read the last export from GCS

```r
library(survey160r)
s160_gcs_init()   # browser sign-in on first run, then cached

campaigns   <- s160_gcs_campaign_results_list()             # available campaign ids
campaign_id <- campaigns[1]
df          <- s160_gcs_campaign_results_read(campaign_id)  # results -> data frame
files       <- s160_gcs_campaign_results_files(campaign_id) # files in the campaign folder

# Last export time / size, without downloading:
status <- s160_gcs_campaign_results_status(campaign_id)
status$updated
status$size
```

Every `s160_gcs_campaign_results_*` reader takes `env = "prod"` (default), `"staging"`, or `"dev"`; `s160_datasets()` lists which environments each dataset is available in.

### Trigger a fresh export via the API

When you need data newer than the last GCS export, the API triggers a fresh export, polls until it's ready, and downloads it in one step. `s160_api_auth(env)` authenticates to `"prod"` (default), `"staging"`, or `"dev"` and returns a *connection* that pairs the API URL, bucket, and key so they can't be mismatched; on first run it prompts for any missing credentials and saves them to `~/.Renviron`. Run `s160_datasets()` to see which environments each dataset is available in.

```r
s160_api_auth()                                  # defaults to prod; prompts on first run
df <- s160_api_campaign_results(campaign_id)     # fresh export -> data frame

df <- s160_api_campaign_results(campaign_id, filter_open = TRUE)  # drop open/uncontacted
df <- s160_api_campaign_results(campaign_id, timeout = 600)       # large campaigns (default 300s)
df <- s160_api_campaign_results(campaign_id, destdir = ".")       # keep the CSV, not a temp file
```

Capture the connection to keep prod and staging live in one session -- e.g. to A/B compare the same campaign; each carries its own paired bucket:

```r
prod <- s160_api_auth(env = "prod")
stg  <- s160_api_auth(env = "staging")
df_prod <- s160_api_campaign_results(campaign_id, conn = prod)
df_stg  <- s160_api_campaign_results(campaign_id, conn = stg)
```

A conn-less call uses the most recent `s160_api_auth()`.

## Latency analysis

Compute a per-campaign recipient-latency report from a raw campaign CSV, returned as an in-memory R object -- `latency_run()` (the one-campaign runner) or the pure `latency_report()`. Reduce the report to the funnel counts with `latency_funnel()`, or get the per-question reached funnel straight from the export with `question_funnel()`. Full walkthrough -- inputs, result schema, config, validation, and the funnel views -- in the **[latency guide](vignettes/latency.Rmd)** (`vignette("latency")` once installed).

## Disposition screening

Screen a phone sample against every recipient Survey160 has contacted, dropping numbers already completed, refused, or screened out before you field. Pull the shared dataset once, then screen in place:

```r
library(survey160r)
s160_gcs_init()   # one-time browser sign-in (cached)

my_sample <- data.frame(phone = c("2015550101", "2015550102"))  # your list; extra columns are kept
dataset   <- disposition_pull()                     # downloads a few hundred MB the first time, then cached
cleaned   <- disposition_screen(my_sample, dataset) # screening columns appended 1:1
# drop already-completed (SMS or web) and every hard stop; blank-phone rows come
# back all-NA and are kept
subset(cleaned, !((n_completed > 0 | n_web_complete > 0) %in% TRUE |
                    (n_terminated > 0 | n_refused > 0 | n_ineligible > 0) %in% TRUE))
```

Full walkthrough -- the appended columns, ad-hoc queries, the `refused` / `ineligible` split, and the read-once tip -- in the **[disposition guide](vignettes/disposition.Rmd)** (`vignette("disposition")` once installed).

## Opt-out screening

Screen a phone sample against the opt-out list, flagging numbers that have opted out before you field. Pull the shared list once, then screen in place:

```r
library(survey160r)
s160_gcs_init()   # one-time browser sign-in (cached)

my_sample <- data.frame(phone = c("2015550101", "2015550102"))  # your list; extra columns are kept
optout    <- opt_out_pull()                       # small download, then cached
cleaned   <- opt_out_screen(my_sample, optout)    # opted_out + opt_out_date appended 1:1
# drop opted-out numbers; blank-phone rows screen to NA and are kept
subset(cleaned, !(opted_out %in% TRUE))
```

The opt-out list lives in the same `s160_disposition_prod` bucket as the disposition dataset, so the same access covers both, and the two screens chain (annotate for opt-out and prior contact, then filter once). Full walkthrough in the **[opt-out guide](vignettes/opt-out.Rmd)** (`vignette("opt-out")` once installed).

## Campaign metrics

Measure the funnel -- sent, engaged, opted in, completed -- across the whole fleet, by registration, brand, client, fielding location, or mode. Pull the consolidated projection once, then roll it up:

```r
library(survey160r)
s160_gcs_init()   # one-time browser sign-in (cached)

path <- campaign_metrics_pull()                        # cached after the first download
campaign_metrics_summary(path, by = "registration_id")
campaign_metrics_summary(path, by = c("brand", "mode"), percent = TRUE)
```

The projection holds counts, not phone numbers, so there is no screen here. To scope a rollup (a date window, a set of campaigns), read the cells with `campaign_metrics_records()`, filter, and pass the frame to `campaign_metrics_summary()`; never sum the raw Parquet yourself, it repeats each count across latency rows. Full walkthrough in the **[campaign-metrics guide](vignettes/campaign-metrics.Rmd)** (`vignette("campaign-metrics")` once installed).

## First-time setup

Before any data call works you need, in order:

1. The **OAuth client secret** -- ask your team lead; you paste it in once (below).
2. **Access to the bucket you are reading** -- Storage Object Viewer on `campaign_results` (raw data / latency) or `s160_disposition_prod` (disposition screening), granted via the `gcp-campaign-readers` group. Without it, sign-in still succeeds but the first read returns a **403** -- ask a sysadmin to add you.
3. For the API path only: your **API user ID and key** -- also from your team lead.

The rest of this section walks through each.

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
2. **API key** -- a per-environment variable, `S160_<ENV>_API_KEY`:
   `S160_PROD_API_KEY` for prod (falling back to the legacy `S160_API_KEY`
   if unset), `S160_STAGING_API_KEY` for staging, and `S160_DEV_API_KEY`
   for dev.

Any missing value is prompted on the first `s160_api_auth(env)` call for
that environment and saved to `~/.Renviron`, so you won't be asked
again. Get these from your team lead.

## Documentation

- **Guides (articles):** [Latency analysis](vignettes/latency.Rmd), [Disposition screening](vignettes/disposition.Rmd), [Opt-out screening](vignettes/opt-out.Rmd), and [Campaign metrics](vignettes/campaign-metrics.Rmd) -- also rendered as Articles on the [package site](https://survey160.r-universe.dev/survey160r). In R: `vignette("latency")` / `vignette("disposition")` / `vignette("opt-out")` / `vignette("campaign-metrics")`.
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

Common symptoms and fixes:

| Symptom | Likely cause | Fix |
|---|---|---|
| `GCS not initialized. Run s160_gcs_init() first.` | A GCS, disposition, or opt-out reader was called before authenticating | Run `s160_gcs_init()` first |
| A **403** after a successful Google sign-in | Your account lacks Storage Object Viewer on that bucket | Ask a sysadmin to add you to `gcp-campaign-readers` for the bucket (see [First-time setup](#first-time-setup)) |
| `could not find function "disposition_pull"` | R-universe has not rebuilt the latest release yet | Install from GitHub (`pak::pkg_install("survey160/survey160r")`), then restart R |
| `disposition_pull()` returns data you know is out of date | It reused a cached copy | Re-download with `disposition_pull(refresh = TRUE)` |
| `argument(s) not accepted by the CSV reader (data.table::fread): filter_open` | The argument belongs to a different function (e.g. `filter_open` is on `s160_api_campaign_results()`, not `s160_gcs_campaign_results_read()`) | Move it to the right function, or drop it |
| A `date_from` / `date_to` filter drops more rows than expected | A row whose `disposition_date` is `NA` (never populated for that campaign) is dropped by any date bound | Expected -- only dated rows match a date filter; omit the bound to keep undated rows |

Every error names the function, what it received, and the next step; a script
can catch one by class (`s160_not_initialized`, `s160_not_found`, ...) -- see
the "Errors and messages" section of `?survey160r`.

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
