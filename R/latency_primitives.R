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

#' Parse Survey160 campaign-export timestamps
#'
#' Parses the timestamp strings found in a Survey160 campaign export (e.g.
#' \code{id.intro.scriptDate}) to \code{POSIXct} in UTC. The export encodes
#' timestamps as \code{"2026-01-26 17:30:16.853688Z"} (UTC, microseconds,
#' trailing \code{Z}); this strips the trailing \code{Z} and parses the
#' date-time. It is a pure decoder of that specific format, not a general
#' timestamp parser.
#'
#' Blank and \code{NA} inputs return \code{NA}; non-blank inputs that do not
#' match the export format also return \code{NA} (no error, no warning), so a
#' parse-failure mask is \code{!is.na(raw) & nzchar(raw) & is.na(result)}.
#'
#' Used internally by the latency pipeline (per-column parsing with
#' diagnostics) and the config validators; exported so report authors can
#' decode an export column directly.
#'
#' @param x A character vector of export timestamp strings. A \code{POSIXct}
#'   input is returned in UTC with its instant preserved (not re-parsed).
#' @return A \code{POSIXct} vector in UTC, the same length as \code{x}.
#' @examples
#' parse_campaign_timestamps(c("2026-01-26 17:30:16.853688Z", "", NA))
#' @export
parse_campaign_timestamps <- function(x) {
  # An already-parsed POSIXct is returned in UTC with its instant preserved;
  # deriving it via as.character() would shift a non-UTC value.
  if (inherits(x, "POSIXct")) {
    return(lubridate::with_tz(x, "UTC"))
  }
  x <- as.character(x)
  # Fast path: the single-order parser. It runs the same C routine as
  # parse_date_time() for the export's "Y-m-d H:M:OS" order (so the values are
  # bit-identical), accepts the trailing Z and an ISO "T" separator itself, and
  # skips the per-call order training/guessing of the multi-order parser --
  # ~70x faster on an export-sized column. A non-blank string it cannot parse
  # is retried through the lenient multi-order parser below, so the lenient
  # orders ("YmdHMS", slash separators, ...) keep working.
  out <- lubridate::parse_date_time2(x, orders = "Y-m-d H:M:OS", tz = "UTC")
  retry <- !is.na(x) & nzchar(x) & is.na(out)
  if (any(retry)) {
    out[retry] <- suppressWarnings(lubridate::parse_date_time(
      .strip_z(x[retry]),
      orders = .timestamp_orders,
      tz = "UTC",
      quiet = TRUE
    ))
  }
  out
}

# Resolve one export column name to parsed UTC timestamps, null-safe: an absent
# column yields an all-NA POSIXct of the right length. The shared core behind
# question_timestamps() (its strict, validated wrapper), .question_timestamp()
# (which coalesces a set of opener columns), and the disposition
# ineligible/refusal resolvers -- all three encode the same id.<q>.<field>
# resolve-and-parse.
.column_timestamps <- function(data, col) {
  if (col %in% names(data)) {
    parse_campaign_timestamps(data[[col]])
  } else {
    rep(as.POSIXct(NA, tz = "UTC"), nrow(data))
  }
}

# Local calendar date and hour of a UTC POSIXct vector in `tz`, from ONE
# as.POSIXlt() conversion: as.Date() of the broken-down time and its $hour
# field. The previous as.Date(format(x, tz)) / as.integer(format(x, "%H", tz))
# pair rendered every instant to a string twice and parsed the date back; on a
# wide campaign that ran once per segment and dominated frame construction.
# NA in -> NA out for both. Pure.
#
# The conversion is done per distinct UTC MINUTE, not per instant: a zone's
# UTC offset is a whole number of minutes (every standard and DST offset in the
# IANA database is; only pre-1900 local-mean-time offsets carry seconds), so
# every instant inside one UTC minute shares its local date and hour. A
# campaign spans a few thousand distinct minutes versus hundreds of thousands
# of instants, and as.POSIXlt() in a named zone is the expensive step (a
# per-element localtime lookup) -- bucketing makes it ~20x cheaper on an
# export-sized column with identical results. NA instants bucket to NA.
.local_date_hour <- function(x, tz) {
  minute <- floor(as.numeric(x) / 60)
  distinct <- unique(minute)
  lt <- as.POSIXlt(.POSIXct(distinct * 60, tz = "UTC"), tz = tz)
  idx <- match(minute, distinct)
  list(date = as.Date(lt)[idx], hour = lt$hour[idx])
}

# Replace empty strings with NA on character columns. Mirrors the legacy
# `na_if(., "")` step so downstream parsers see NA, not "".
na_if_blank <- function(data) {
  char_cols <- vapply(data, is.character, logical(1))
  for (col in names(data)[char_cols]) {
    # which() drops the NA comparisons, so one `== ""` pass per column is the
    # whole test (no separate !is.na() mask), and only a column that has a
    # blank is rewritten.
    blank <- which(data[[col]] == "")
    if (length(blank) > 0L) data[[col]][blank] <- NA_character_
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
    raw_chr <- as.character(raw)
    nonblank <- !is.na(raw_chr) & nzchar(raw_chr)
    # parse_campaign_timestamps() maps blank / NA to NA itself, so the whole
    # column is parsed in one call (no subset-and-reassign round trip).
    parsed <- parse_campaign_timestamps(raw_chr)
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
  # difftime(units = "mins") is exactly (unclass(t1) - unclass(t2)) / 60; done
  # inline to skip its tz / units dispatch on an export-sized vector.
  raw <- (as.numeric(script_next) - as.numeric(batch_prior)) / 60
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

# The frame builders' incremental form of apply_chain_validity(): `prior_na` is
# the running OR of is.na() over the strictly-prior batchDates (NULL before the
# first segment), so each segment costs one is.na() instead of re-scanning the
# whole chain (O(segments) rather than O(segments^2) over the respondents).
.chain_break_mask <- function(prior_na, batch_prior) {
  if (is.null(prior_na)) is.na(batch_prior) else prior_na | is.na(batch_prior)
}
