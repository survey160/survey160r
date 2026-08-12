# Pure helpers for latency computation.
# Consolidates the four legacy primitives (timestamp_diff, texting_hour_by_date,
# percent_below_thresholds_data, latency_indicator_vars) into testable units.
# All datetime math runs in UTC; localization happens only at window
# construction and day-label derivation (spec invariant I5).

# Accepted timestamp orders (lubridate format tokens).
# Source CSV format: "2026-01-26 17:30:16.853688Z" (UTC, microseconds, Z suffix).
# We strip a trailing Z before parsing and then assume UTC -- lubridate's
# format tokens don't include a literal "Z" matcher.
.timestamp_orders <- c(
  "Y-m-d H:M:OS",
  "Y-m-d H:M:S",
  "YmdHMS"
)

# Strip a trailing 'Z' (UTC marker) from a character vector. NA-safe.
.strip_z <- function(x) {
  out <- x
  has_z <- !is.na(x) & grepl("Z$", x)
  out[has_z] <- sub("Z$", "", x[has_z])
  out
}

# Parse a character vector of Survey160 CSV timestamp strings to POSIXct (UTC).
# Strips a trailing 'Z' first, then tries `.timestamp_orders` in turn. Blank
# and NA inputs return NA; unparseable non-blank inputs also return NA --
# callers that want a parse-failure mask should compare `nzchar(strip_z(...))`
# against `is.na(result)`. Used by parse_timestamps() (per-column with
# diagnostics) and the config validators (one-shot parsing, no diagnostics).
parse_s160_timestamps_chr <- function(chr) {
  suppressWarnings(lubridate::parse_date_time(
    .strip_z(as.character(chr)),
    orders = .timestamp_orders,
    tz = "UTC",
    quiet = TRUE
  ))
}

# Replace empty strings with NA on character columns. Mirrors the legacy
# `na_if(., "")` step so downstream parsers see NA, not "".
na_if_blank <- function(data) {
  char_cols <- vapply(data, is.character, logical(1))
  for (col in names(data)[char_cols]) {
    blank <- !is.na(data[[col]]) & data[[col]] == ""
    if (any(blank)) data[[col]][blank] <- NA_character_
  }
  data
}

# Parse a set of timestamp columns to POSIXct (UTC). Returns:
#   - data: data with parsed columns substituted in place
#   - parse_failures: named integer count per column of non-blank inputs that
#     failed to parse (column-level diagnostic).
#   - parse_failed_mask: named list of logical vectors per column, TRUE where
#     the input was non-blank but failed to parse. Used by build_latency_frame
#     to classify segment NAs as parse_failure vs missing_endpoint.
# NA / blank inputs are treated as absent, not failures.
parse_timestamps <- function(data, cols) {
  failures <- integer(length(cols))
  names(failures) <- cols
  fail_mask <- vector("list", length(cols))
  names(fail_mask) <- cols
  n <- nrow(data)
  for (col in cols) {
    if (!col %in% names(data)) {
      stop_not_found("timestamp column", col)
    }
    raw <- data[[col]]
    if (inherits(raw, "POSIXct")) {
      # Already parsed; normalize to UTC. No parse failures possible.
      attr(raw, "tzone") <- "UTC"
      data[[col]] <- raw
      fail_mask[[col]] <- rep(FALSE, n)
      next
    }
    raw_chr <- .strip_z(as.character(raw))
    nonblank <- !is.na(raw_chr) & nzchar(raw_chr)
    parsed <- rep(as.POSIXct(NA), length(raw_chr))
    if (any(nonblank)) {
      parsed[nonblank] <- parse_s160_timestamps_chr(raw_chr[nonblank])
    }
    col_fail <- nonblank & is.na(parsed)
    failures[[col]] <- sum(col_fail)
    fail_mask[[col]] <- col_fail
    data[[col]] <- parsed
  }
  list(data = data, parse_failures = failures, parse_failed_mask = fail_mask)
}

# Row-subset a (data, parse_failed_mask) pair in lockstep. Used by
# latency_report() after dedupe and date_filter so the per-segment mask
# stays aligned with `data` row-for-row. Pure: returns a new pair, does
# not mutate.
subset_parsed_input <- function(data, parse_failed_mask, keep_idx) {
  list(
    data = data[keep_idx, , drop = FALSE],
    parse_failed_mask = lapply(parse_failed_mask, function(m) m[keep_idx])
  )
}

# Δ in minutes between batch_prior and script_next. Negative values clamped to
# 0 (spec I2). NA where either endpoint is NA. Returns the count of clamped
# negatives so the caller can roll up diagnostics.
compute_segment_delta <- function(batch_prior, script_next) {
  if (length(batch_prior) != length(script_next)) {
    stop("`batch_prior` and `script_next` must have the same length.", call. = FALSE)
  }
  raw <- as.numeric(difftime(script_next, batch_prior, units = "mins"))
  clamped <- !is.na(raw) & raw < 0
  raw[clamped] <- 0
  list(delta = raw, n_clamped = sum(clamped))
}

# Apply chain-validity (spec I4): a segment Δ is NA if any prior batchDate in
# the chain is NA. `chain_priors` is a list of batchDate vectors for all
# segments preceding (and including) the current one; this segment's Δ is set
# to NA wherever any element of those vectors is NA.
apply_chain_validity <- function(delta, chain_priors) {
  if (length(chain_priors) == 0) return(delta)
  any_na <- Reduce(`|`, lapply(chain_priors, is.na))
  delta[any_na] <- NA_real_
  delta
}
