# Latency report configuration: defaults, validation, hash.
# Configs are built programmatically via latency_build_config() or as
# hand-written lists with the same shape. The historical YAML schema and
# the per-wave API metadata layer have both been retired; only the fields
# latency_report() actually consults are kept.

# Allowed top-level config keys. latency_validate_config() rejects anything
# else so typos in caller-supplied lists fail loud. The set matches exactly
# what latency_report() reads -- no provenance or YAML-only slots.
.config_keys <- c(
  "project_id", "campaign_id", "field_timezone", "flow",
  "filters", "texting_windows"
)

# Terminal flow states that must not appear in `questions`.
.terminal_states <- c("refusal", "ineligible")

# Default filter expression matches legacy scripts.
.default_population <- "id.intro.finalText == \"Yes\""

#' Discover the question flow from CSV column names
#'
#' Scans the column names of an in-memory CSV data frame (as returned by
#' \code{read.csv} / \code{pull_csv_from_gcs}) for \code{id.<q>.scriptDate}
#' columns and returns the question ids in their original column order.
#' Terminal flow states (\code{refusal}, \code{ineligible}) are dropped so
#' the result is usable directly as \code{config$flow$questions}.
#'
#' Survey160 v2 CSV headers are emitted as \code{id[<q>]scriptDate} on disk;
#' \code{read.csv} converts the brackets to dots. Both forms are accepted so
#' callers can pass either a data frame or a character vector of raw header
#' tokens.
#'
#' @param data A data frame or character vector of column names.
#' @return A character vector of question ids in flow order.
#' @export
latency_discover_questions <- function(data) {
  cols <- if (is.data.frame(data)) names(data) else as.character(data)
  # Match either bracket form (raw header) or dot form (post read.csv).
  m_dot <- regmatches(cols, regexec("^id\\.([A-Za-z0-9_]+)\\.scriptDate$", cols))
  m_brk <- regmatches(cols, regexec("^id\\[([A-Za-z0-9_]+)\\]scriptDate$", cols))
  qs_dot <- vapply(m_dot, function(x) if (length(x) == 2) x[2] else NA_character_,
                   character(1))
  qs_brk <- vapply(m_brk, function(x) if (length(x) == 2) x[2] else NA_character_,
                   character(1))
  qs <- ifelse(!is.na(qs_dot), qs_dot, qs_brk)
  qs <- qs[!is.na(qs)]
  qs <- qs[!qs %in% .terminal_states]
  unique(qs)
}

#' Build a latency config from a campaign id and its CSV
#'
#' Pure function. Derives \code{flow.questions} from the CSV column names
#' via \code{latency_discover_questions()} and assembles the rest of the
#' config from the named arguments. No I/O, no API call, no auth precondition.
#'
#' @param campaign_id Campaign id (numeric or character).
#' @param data A data frame of CSV results (or a character vector of column
#'   names) used to discover the question flow.
#' @param field_timezone Tz used to bucket the Parquet \code{date} and
#'   \code{hour_local} columns. Default \code{"UTC"}.
#' @param project_id Optional Survey160 project id; defaults to the
#'   campaign id as a placeholder.
#' @param texting_windows Optional list of \code{{date, start_hour, end_hour}}
#'   windows. Default \code{list()} = all-in-window.
#' @param date_filter Optional character/Date vector restricting which
#'   survey dates are processed (interpreted in \code{field_timezone}).
#' @param respondent_id_column Optional column name used to dedupe rows by
#'   respondent. Default \code{NULL} (no dedupe).
#' @return A validated config list ready to pass to \code{latency_report()}.
#' @export
latency_build_config <- function(campaign_id, data,
                         field_timezone = "UTC",
                         project_id = NULL,
                         texting_windows = list(),
                         date_filter = NULL,
                         respondent_id_column = NULL) {
  questions <- latency_discover_questions(data)
  if (length(questions) < 2L) {
    stop(paste(
      "Could not discover at least two questions from CSV columns;",
      "expected id.<q>.scriptDate columns. Found:",
      paste(head(questions, 5L), collapse = ", ")
    ), call. = FALSE)
  }

  list(
    project_id = as.integer(project_id %||% campaign_id),
    campaign_id = as.integer(campaign_id),
    field_timezone = field_timezone,
    flow = list(questions = questions),
    filters = list(
      population = .default_population,
      campaign_id_column = "campaignid",
      respondent_id_column = respondent_id_column,
      date_filter = date_filter
    ),
    texting_windows = texting_windows
  )
}

#' Validate a latency config against a data frame
#'
#' Implements the fail-fast checks from spec §2.4. Aborts with a named error
#' on the first failing rule.
#'
#' @param config The config list (typically from
#'   \code{latency_build_config}).
#' @param data The data frame the report will run against.
#' @return Invisible \code{TRUE} on success; otherwise stops with an error.
#' @export
latency_validate_config <- function(config, data) {
  unknown <- setdiff(names(config), .config_keys)
  if (length(unknown) > 0) {
    stop(sprintf("Unknown config keys: %s", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  if (is.null(config$project_id)) stop("config: 'project_id' is required.", call. = FALSE)
  if (is.null(config$campaign_id)) stop("config: 'campaign_id' is required.", call. = FALSE)
  if (is.null(config$field_timezone)) stop("config: 'field_timezone' is required.", call. = FALSE)
  validate_questions(config$flow$questions)
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
latency_config_hash <- function(config) {
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
