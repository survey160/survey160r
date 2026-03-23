# survey160r

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

## Authentication

On first run, `s160_gcs_init()` prompts for the OAuth client secret
(get it from your team lead) and saves it to `~/.Renviron`. It also opens a
browser for Google sign-in. Both are one-time steps -- subsequent runs
authenticate automatically.

Your Google account needs **Storage Object Viewer** permission on the
target bucket. Contact a sysadmin if you get 403 errors after
authenticating.

To clear cached OAuth tokens: `gargle::gargle_oauth_sitrep()` to list
them, then delete from `~/.config/gargle/`.

## License

MIT
