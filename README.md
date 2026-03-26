# survey160r

[![R-CMD-check](https://github.com/survey160/survey160r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/survey160/survey160r/actions/workflows/R-CMD-check.yaml)

R package for reading Survey160 campaign results from Google Cloud Storage.

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
df <- s160_gcs_results_read(1980)

# List available campaigns and files
campaigns <- s160_gcs_results_list()
files <- s160_gcs_results_files(1980)
```

## First-time setup

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

## Troubleshooting

Reset the client secret:

```r
file.edit("~/.Renviron")  # remove the S160_GCS_CLIENT_SECRET line, save, restart R
```

Clear cached OAuth tokens:

```r
gargle::gargle_oauth_sitrep()  # list cached tokens and their location
# delete the cache directory shown above, then restart R
```

## License

MIT
