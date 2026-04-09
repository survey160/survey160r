# survey160r

[![R-CMD-check](https://github.com/survey160/survey160r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/survey160/survey160r/actions/workflows/R-CMD-check.yaml)

R package for accessing Survey160 campaign data -- read results from Google Cloud Storage and trigger fresh exports via the API.

## Installation

```r
# From R-universe
install.packages("survey160r", repos = "https://survey160.r-universe.dev")

# From GitHub
remotes::install_github("survey160/survey160r")
```

## Usage

```r
library(survey160r)

# Authenticate and set bucket (opens browser on first run)
s160_gcs_init(bucket = "campaign_results")

# Read a campaign's results into a data frame
df <- s160_gcs_campaign_results_read(1980)

# List available campaigns and files
campaigns <- s160_gcs_campaign_results_list()
files <- s160_gcs_campaign_results_files(1980)
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
df <- s160_api_campaign_results(1980)

# Exclude open/uncontacted conversations
df <- s160_api_campaign_results(1980, filter_open = TRUE)

# Increase timeout for large campaigns (default 300s)
df <- s160_api_campaign_results(1980, timeout = 600)

# Save the CSV locally instead of using a temp file
df <- s160_api_campaign_results(1980, destdir = ".")
```

### Check export status

You can check the last export timestamp without triggering a new export:

```r
status <- s160_gcs_campaign_results_status(1980)
status$updated  # last export time
status$size     # file size
```

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
