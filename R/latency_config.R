# Latency report configuration: load, validate, hash.
# Implements the YAML schema from latency_scripts.md §4 and the fail-fast
# validation rules from §2.4.

# Allowed top-level config keys. Anything else aborts (spec I10).
.config_keys <- c(
  "project_id", "project_name", "campaign_id", "wave_run",
  "input", "field_timezone", "display_timezone", "flow",
  "filters", "texting_windows", "reports", "output"
)

.input_keys <- c("source", "gcs_path")
.flow_keys <- c("questions")
.filter_keys <- c("population", "campaign_id_column", "respondent_id_column", "date_filter")
.report_keys <- c("time_bucket", "extra_grouping_columns")
.output_keys <- c("bucket", "format")
.window_keys <- c("date", "start_hour", "end_hour")

# Terminal flow states that must not appear in `questions`.
.terminal_states <- c("refusal", "ineligible")

# Default filter expression matches legacy scripts.
.default_population <- "id.intro.finalText == \"Yes\""

#' Load a latency report config from a YAML file
#'
#' @param path Path to a YAML file matching the schema in
#'   \code{latency_scripts.md} §4.
#' @return A list with config values; defaults applied for omitted keys.
#' @export
read_config <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("Config file not found: %s", path), call. = FALSE)
  }
  raw <- yaml::read_yaml(path)
  apply_config_defaults(raw)
}

# Fill in defaults for omitted optional keys. Mutates and returns the list.
apply_config_defaults <- function(config) {
  if (is.null(config$filters)) config$filters <- list()
  if (is.null(config$filters$population)) {
    config$filters$population <- .default_population
  }
  if (is.null(config$filters$campaign_id_column)) {
    config$filters$campaign_id_column <- "campaignid"
  }
  if (is.null(config$reports)) config$reports <- list()
  if (is.null(config$reports$time_bucket)) {
    config$reports$time_bucket <- "day"
  }
  if (is.null(config$reports$extra_grouping_columns)) {
    config$reports$extra_grouping_columns <- character(0)
  }
  if (is.null(config$display_timezone)) {
    config$display_timezone <- config$field_timezone
  }
  config
}

#' Validate a latency config against a data frame
#'
#' Implements the fail-fast checks from spec §2.4. Aborts with a named error
#' on the first failing rule.
#'
#' @param config The config list (typically from \code{read_config}).
#' @param data The data frame the report will run against.
#' @return Invisible \code{TRUE} on success; otherwise stops with an error.
#' @export
validate_config <- function(config, data) {
  unknown <- setdiff(names(config), .config_keys)
  if (length(unknown) > 0) {
    stop(sprintf("Unknown config keys: %s", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  if (is.null(config$project_id)) stop("config: 'project_id' is required.", call. = FALSE)
  if (is.null(config$campaign_id)) stop("config: 'campaign_id' is required.", call. = FALSE)
  if (is.null(config$field_timezone)) stop("config: 'field_timezone' is required.", call. = FALSE)
  if (!is.null(config$reports$thresholds)) {
    stop(paste(
      "config: 'reports.thresholds' is no longer configurable --",
      "thresholds are fleet-locked at c(1, 3, 5, 10). Remove this key."
    ), call. = FALSE)
  }
  validate_questions(config$flow$questions)
  validate_time_bucket(config$reports$time_bucket)
  validate_columns_present(config, data)
  validate_flow_order(config, data)
  validate_windows_cover(config, data)
  invisible(TRUE)
}

validate_questions <- function(questions) {
  if (is.null(questions) || length(questions) == 0) {
    stop("config: 'flow.questions' is required and must be non-empty.", call. = FALSE)
  }
  if (length(questions) < 2) {
    stop("config: 'flow.questions' must contain at least two questions.", call. = FALSE)
  }
  if (anyDuplicated(questions)) {
    stop("config: 'flow.questions' contains duplicates.", call. = FALSE)
  }
  bad <- intersect(questions, .terminal_states)
  if (length(bad) > 0) {
    stop(sprintf("config: 'flow.questions' must not include terminal states: %s",
                 paste(bad, collapse = ", ")), call. = FALSE)
  }
}

validate_time_bucket <- function(bucket) {
  if (!bucket %in% c("day", "hour")) {
    stop("config: 'reports.time_bucket' must be 'day' or 'hour'.", call. = FALSE)
  }
}

# Build the list of required CSV columns implied by the question flow and
# verify all are present in `data`.
required_timestamp_columns <- function(questions) {
  n <- length(questions)
  cols <- character(0)
  # scriptDate for every question.
  cols <- c(cols, sprintf("id.%s.scriptDate", questions))
  # batchDate for every question except the last (terminal/close).
  cols <- c(cols, sprintf("id.%s.batchDate", questions[-n]))
  cols
}

validate_columns_present <- function(config, data) {
  required <- required_timestamp_columns(config$flow$questions)
  population_intro <- "id.intro.finalText"
  required <- c(required, population_intro)
  campaign_col <- config$filters$campaign_id_column
  required <- c(required, campaign_col)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required columns missing from data: %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
}

# Verify scriptDate(qᵢ₊₁) >= batchDate(qᵢ) for ≥90% of rows. Drift below that
# typically indicates the question flow is mis-ordered in config.
validate_flow_order <- function(config, data) {
  questions <- config$flow$questions
  if (length(questions) < 2) return(invisible(TRUE))
  ok_total <- 0L
  cmp_total <- 0L
  for (i in seq_len(length(questions) - 1)) {
    prior <- sprintf("id.%s.batchDate", questions[i])
    nxt <- sprintf("id.%s.scriptDate", questions[i + 1])
    bp <- suppressWarnings(lubridate::parse_date_time(
      .strip_z(as.character(data[[prior]])), orders = .timestamp_orders,
      tz = "UTC", quiet = TRUE
    ))
    sn <- suppressWarnings(lubridate::parse_date_time(
      .strip_z(as.character(data[[nxt]])), orders = .timestamp_orders,
      tz = "UTC", quiet = TRUE
    ))
    valid <- !is.na(bp) & !is.na(sn)
    if (!any(valid)) next
    cmp_total <- cmp_total + sum(valid)
    ok_total <- ok_total + sum(sn[valid] >= bp[valid])
  }
  if (cmp_total == 0) return(invisible(TRUE))
  ratio <- ok_total / cmp_total
  if (ratio < 0.9) {
    stop(sprintf("Flow order check failed: only %.1f%% of rows have scriptDate(next) >= batchDate(prior). Likely mis-ordered 'flow.questions'.", # nolint
                 100 * ratio), call. = FALSE)
  }
  invisible(TRUE)
}

# Survey dates that will actually be processed must be covered by some
# texting_window, OR texting_windows must be empty (all-in-window mode).
# Honors `filters.date_filter`: dates outside the filter are not required
# to be covered.
validate_windows_cover <- function(config, data) {
  windows <- config$texting_windows
  if (is.null(windows) || length(windows) == 0) return(invisible(TRUE))
  intro_script <- "id.intro.scriptDate"
  if (!intro_script %in% names(data)) return(invisible(TRUE))
  parsed <- suppressWarnings(lubridate::parse_date_time(
    .strip_z(as.character(data[[intro_script]])),
    orders = .timestamp_orders, tz = "UTC", quiet = TRUE
  ))
  parsed <- parsed[!is.na(parsed)]
  if (length(parsed) == 0) return(invisible(TRUE))
  field_tz <- config$field_timezone
  local_dates <- as.Date(format(parsed, tz = field_tz))
  date_filter <- config$filters$date_filter
  if (!is.null(date_filter)) {
    local_dates <- local_dates[local_dates %in% as.Date(date_filter)]
    if (length(local_dates) == 0) return(invisible(TRUE))
  }
  window_dates <- as.Date(vapply(windows, function(w) as.character(w$date), character(1)))
  missing_dates <- setdiff(unique(local_dates), window_dates)
  if (length(missing_dates) > 0) {
    stop(sprintf("texting_windows do not cover survey dates: %s",
                 paste(as.character(as.Date(missing_dates, origin = "1970-01-01")), collapse = ", ")),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Stable hash of a config
#'
#' Hashes a canonical form of the config so the same logical config always
#' produces the same hash even across different YAML serializations.
#'
#' @param config The config list.
#' @return A hex sha256 string.
#' @export
config_hash <- function(config) {
  canonical <- canonicalize_config(config)
  digest::digest(canonical, algo = "sha256", serialize = TRUE)
}

# Sort named list elements recursively so digest() is order-stable.
canonicalize_config <- function(x) {
  if (is.list(x)) {
    nm <- names(x)
    if (!is.null(nm) && length(nm) == length(x)) {
      x <- x[order(nm)]
    }
    lapply(x, canonicalize_config)
  } else {
    x
  }
}
