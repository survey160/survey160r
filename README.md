# survey160r

R package for reading Survey160 campaign results from Google Cloud Storage.

## Installation

```r
# From R-universe (once published)
install.packages("survey160r", repos = "https://survey160.r-universe.dev")

# From GitHub
remotes::install_github("survey160/survey160r")
```

## Setup

Set these environment variables in your `~/.Renviron` or project `.Renviron`:

```
S160_GCS_CLIENT_ID=your-client-id
S160_GCS_CLIENT_SECRET=your-client-secret
S160_RESULTS_BUCKET=campaign_results_qa
```

## Usage

```r
library(survey160r)

# Authenticate and set bucket (opens browser on first run)
s160_gcs_init()

# Read a campaign's results into a data frame
df <- s160_gcs_results_read(12345)

# Use a specific bucket
s160_gcs_init(bucket = "campaign_results")
df <- s160_gcs_results_read(12345)

# List available campaigns and files
campaigns <- s160_gcs_results_list()
files <- s160_gcs_results_files(12345)
```

## License

MIT
