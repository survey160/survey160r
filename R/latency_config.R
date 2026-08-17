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
  "filters"
)

# Terminal flow states that must not appear in `questions`.
.terminal_states <- c("refusal", "ineligible")

# Default filter expression matches legacy scripts.
.default_population <- "id.intro.finalText == \"Yes\""

#' Discover the question flow from CSV column names
#'
#' Scans the column names of an in-memory CSV data frame (as returned by
#' \code{read.csv} / \code{s160_gcs_campaign_results_read}) for \code{id.<q>.scriptDate}
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
#' @examples
#' latency_discover_questions(
#'   c("campaignid", "id.intro.scriptDate", "id.intro.batchDate",
#'     "id.q1.scriptDate", "id.q1.batchDate", "id.close.scriptDate")
#' )
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

# The opening question(s) of the flow: the intro-family (grep "^intro(_|$)") of
# the discovered `questions` -- a routed campaign has several (intro + intro_sp /
# intro_latinos, via v2 initialconditionals) -- else the single first question
# (e.g. "FIRSTNET"), else "intro". The contacted/replied/consent signals key on
# this SET, not a hardcoded "intro", so a non-intro or bilingual campaign is
# measured, not dropped. Mirrors the disposition transform's opener resolution
# (kept in sync so the two views align).
.opening_questions <- function(questions) {
  intro_family <- grep("^intro(_|$)", questions, value = TRUE)
  if (length(intro_family) > 0L) return(intro_family)
  if (length(questions) == 0L) "intro" else questions[[1L]]
}

# The per-recipient opener send/reply timestamp: parse each opener's
# id.<q>.<field> column null-safely (absent -> all-NA, length nrow) and coalesce
# across the set. Each recipient hit exactly one opener, so coalesce yields that
# recipient's timestamp; it preserves POSIXct/UTC (all inputs are UTC, matching
# parse_s160_timestamps_chr). `field` is "scriptDate" (send) or "batchDate" (reply).
.opener_timestamp <- function(data, openers, field) {
  n <- nrow(data)
  ts_list <- lapply(openers, function(q) {
    col <- sprintf("id.%s.%s", q, field)
    if (col %in% names(data)) {
      parse_s160_timestamps_chr(data[[col]])
    } else {
      rep(as.POSIXct(NA, tz = "UTC"), n)
    }
  })
  do.call(dplyr::coalesce, ts_list)
}

# Default opt-in population: the opening question set's accepted answer is "Yes"
# -- a disjunction over the openers' finalText columns, restricted to those
# PRESENT in `available` so an absent routed branch doesn't trip
# validate_columns_present() or the population eval. For a pure-intro campaign
# this is exactly `.default_population`.
.opener_population <- function(openers, available) {
  cols <- sprintf("id.%s.finalText", openers)
  present <- cols[cols %in% available]
  if (length(present) == 0L) present <- cols[1L]
  paste(sprintf("%s == \"Yes\"", present), collapse = " | ")
}

# v2 CSV headers arrive dot-form (id.<q>.field, as the readers munge them via
# make.names) OR raw bracket-form (id[<q>]field). latency_discover_questions()
# accepts both, so normalize a raw header to dot-form before .opener_population()
# resolves finalText columns -- otherwise a raw bilingual header matches no
# dot-form finalText column and the population collapses to the first opener,
# silently dropping later branches. Dot-form names pass through unchanged.
.dot_form_headers <- function(cols) {
  sub("^id\\[([A-Za-z0-9_]+)\\]([A-Za-z0-9_]+)$", "id.\\1.\\2", cols)
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
#' @param date_filter Optional character/Date vector restricting which
#'   survey dates are processed (interpreted in \code{field_timezone}).
#' @param respondent_id_column Optional column name used to dedupe rows by
#'   respondent. Default \code{NULL} (no dedupe).
#' @return A config list ready to pass to \code{latency_report()}, which
#'   calls \code{latency_validate_config()} before consuming it.
#' @examples
#' config <- latency_build_config(
#'   campaign_id = 1234,
#'   data = c("id.intro.scriptDate", "id.intro.batchDate",
#'            "id.q1.scriptDate", "id.q1.batchDate", "id.close.scriptDate"),
#'   field_timezone = "America/New_York"
#' )
#' config$flow$questions
#' @export
latency_build_config <- function(campaign_id, data,
                         field_timezone = "UTC",
                         project_id = NULL,
                         date_filter = NULL,
                         respondent_id_column = NULL) {
  questions <- latency_discover_questions(data)
  if (length(questions) < 2L) {
    stop_s160(paste(
      "Could not discover at least two questions from CSV columns;",
      "expected id.<q>.scriptDate columns. Found:",
      paste(head(questions, 5L), collapse = ", ")
    ), fn = "latency_build_config")
  }
  # Population (opt-in/consent) keys on the opening question SET, not a hardcoded
  # "intro", so a non-intro (FIRSTNET) / bilingual campaign is measured. Pure
  # intro -> identical to `.default_population`. `data` may be a data frame or a
  # character header vector (same as latency_discover_questions), so resolve the
  # available columns accordingly -- names() is NULL for a character header.
  available <- if (is.data.frame(data)) names(data) else as.character(data)
  available <- .dot_form_headers(available)
  population <- .opener_population(.opening_questions(questions), available)

  list(
    project_id = as.integer(project_id %||% campaign_id),
    campaign_id = as.integer(campaign_id),
    field_timezone = field_timezone,
    flow = list(questions = questions),
    filters = list(
      population = population,
      campaign_id_column = "campaignid",
      respondent_id_column = respondent_id_column,
      date_filter = date_filter
    )
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
#' @examples
#' data <- data.frame(
#'   campaignid = 1L,
#'   id.intro.finalText = "Yes",
#'   id.intro.scriptDate = "2026-01-26 21:00:00Z",
#'   id.intro.batchDate  = "2026-01-26 21:00:30Z",
#'   id.q1.scriptDate    = "2026-01-26 21:01:00Z",
#'   id.q1.batchDate     = "2026-01-26 21:01:20Z",
#'   id.close.scriptDate = "2026-01-26 21:02:00Z",
#'   check.names = FALSE, stringsAsFactors = FALSE
#' )
#' config <- latency_build_config(1L, data, field_timezone = "America/New_York")
#' latency_validate_config(config, data)
#' @export
latency_validate_config <- function(config, data) {
  unknown <- setdiff(names(config), .config_keys)
  if (length(unknown) > 0) {
    stop(sprintf("config: unknown keys: %s", paste(unknown, collapse = ", ")),
         call. = FALSE)
  }
  if (is.null(config$project_id)) stop("config: `project_id` is required.", call. = FALSE)
  if (is.null(config$campaign_id)) stop("config: `campaign_id` is required.", call. = FALSE)
  if (is.null(config$field_timezone)) stop("config: `field_timezone` is required.", call. = FALSE)
  validate_questions(config$flow$questions)
  validate_columns_present(config, data)
  validate_flow_order(config, data)
  invisible(TRUE)
}

validate_questions <- function(questions) {
  if (is.null(questions) || length(questions) == 0) {
    stop("config: `flow.questions` is required and must be non-empty.", call. = FALSE)
  }
  if (length(questions) < 2) {
    stop("config: `flow.questions` must contain at least two questions.", call. = FALSE)
  }
  if (anyDuplicated(questions)) {
    stop("config: `flow.questions` contains duplicates.", call. = FALSE)
  }
  bad <- intersect(questions, .terminal_states)
  if (length(bad) > 0) {
    stop(sprintf("config: `flow.questions` must not include terminal states: %s",
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
  # Require the population's referenced columns (the opener-family finalText,
  # discovered per campaign), not a hardcoded id.intro.finalText -- otherwise a
  # non-intro (FIRSTNET) campaign is rejected here despite having its own opener.
  pop <- config$filters$population
  if (!is.null(pop) && nzchar(pop)) {
    # A malformed population is surfaced later by population_filter_mask() with a
    # "filters.population" error; don't let all.vars(parse()) raise here first.
    pop_vars <- tryCatch(all.vars(parse(text = pop)), error = function(e) character(0))
    required <- c(required, pop_vars)
  }
  campaign_col <- config$filters$campaign_id_column
  required <- c(required, campaign_col)
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop(sprintf("Required columns missing from data: %s",
                 paste(missing_cols, collapse = ", ")), call. = FALSE)
  }
}

# Non-flow columns latency_report() reads in addition to the per-question
# scriptDate/batchDate set. KEEP IN SYNC with the data[[...]] reads elsewhere:
#   web_complete              -- detect_survey_mode() + t2w completion (summary_aggregate.R)
#   id.ineligible.scriptDate  -- build_ineligible_frame() (a terminal state, so it
#                                is NOT in config$flow$questions and would be missed
#                                by required_timestamp_columns alone)
# The opener finalText column(s) are NOT listed here: they are the population's
# referenced columns, and latency_input_columns() already retains all.vars(pop),
# so the opener-family finalText is projected per campaign (not hardcoded intro).
# Dropping any of these silently changes output (e.g. a t2w campaign would
# misclassify as "sms"), so they are always retained by latency_input_columns().
# The projection parity test (test-latency_input_columns.R) guards against drift.
.report_support_columns <- c(
  "web_complete",
  "id.ineligible.scriptDate"
)

# Non-flow columns whose names are NOT fixed -- detect_survey_mode()'s
# has_personalized_close_link() greps the close-message Text columns
# (id.close<...>.scriptText / .batchText) to classify t2w_external vs sms.
# These can only be matched against the actual header, so latency_input_columns()
# retains them when the caller passes `available`. KEEP IN SYNC with
# has_personalized_close_link() in summary_aggregate.R.
.report_support_patterns <- "^id\\.close[A-Za-z0-9_]*\\.(script|batch)Text$"

#' CSV columns the latency report reads for a given config
#'
#' Returns the (dot-form) column names \code{latency_report()} touches for a
#' given \code{config}: the per-question \code{scriptDate}/\code{batchDate}
#' set, the population-filter columns (extracted from
#' \code{config$filters$population}), the campaign-id and optional
#' respondent-id columns, plus the fixed non-flow support columns
#' (\code{web_complete}, \code{id.ineligible.scriptDate}). The opener
#' \code{finalText} is not fixed here: it is one of the population-filter
#' columns above, so it is retained per campaign only when the population
#' references it.
#'
#' Some columns the report reads have data-dependent names -- the close-message
#' Text columns (\code{id.close*.scriptText}/\code{batchText}) that
#' \code{detect_survey_mode()} greps to tell \code{t2w_external} from
#' \code{sms}. Pass \code{available} (e.g. the result of \code{s160_csv_header()})
#' so these are matched against the real header and retained; omitting it risks
#' projecting them away and misclassifying a \code{t2w_external} campaign as
#' \code{sms}.
#'
#' Pass the result as \code{columns =} to \code{s160_read_csv()} /
#' \code{s160_gcs_campaign_results_read()} to parse only the columns the algorithm needs --
#' the projection yields output identical to a full read.
#'
#' @param config A config list from \code{latency_build_config()} (or one with
#'   the same shape). Build it from a header-only peek
#'   (\code{s160_csv_header()}) to avoid reading the file twice.
#' @param available Optional character vector of the actual (dot-form) column
#'   names present in the file (e.g. from \code{s160_csv_header()}). When
#'   supplied, columns matching the report's data-dependent patterns (the
#'   close-message Text columns) are retained. Strongly recommended.
#' @return A character vector of unique dot-form column names.
#' @examples
#' \dontrun{
#' header <- s160_csv_header(path)
#' config <- latency_build_config(1980, header, field_timezone = "America/New_York")
#' data   <- s160_read_csv(path, columns = latency_input_columns(config, header))
#' }
#' @export
latency_input_columns <- function(config, available = NULL) {
  cols <- required_timestamp_columns(config$flow$questions)
  # build_summary_frame()/build_ineligible_frame() read every opener's batchDate
  # via .opener_timestamp(). required_timestamp_columns() drops the batchDate of
  # the LAST flow question (terminal/close), so an intro-family opener that is
  # itself the last question would be projected away, undercounting n_engaged on
  # a projected read. Retain each opener's batchDate regardless of flow position
  # (a no-op for the usual openers-first flow, deduped by unique() below).
  cols <- c(cols, sprintf("id.%s.batchDate",
                          .opening_questions(config$flow$questions)))
  cols <- c(cols, .report_support_columns)
  pop <- config$filters$population
  if (!is.null(pop) && nzchar(pop)) {
    cols <- c(cols, all.vars(parse(text = pop)))
  }
  cols <- c(cols, config$filters$campaign_id_column)
  rid <- config$filters$respondent_id_column
  if (!is.null(rid)) cols <- c(cols, rid)
  if (!is.null(available)) {
    for (pat in .report_support_patterns) {
      cols <- c(cols, grep(pat, available, value = TRUE))
    }
  }
  unique(cols)
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
    bp <- parse_s160_timestamps_chr(data[[prior]])
    sn <- parse_s160_timestamps_chr(data[[nxt]])
    valid <- !is.na(bp) & !is.na(sn)
    if (!any(valid)) next
    cmp_total <- cmp_total + sum(valid)
    ok_total <- ok_total + sum(sn[valid] >= bp[valid])
  }
  if (cmp_total == 0) return(invisible(TRUE))
  ratio <- ok_total / cmp_total
  if (ratio < 0.9) {
    stop(sprintf("Flow order check failed: only %.1f%% of rows have scriptDate(next) >= batchDate(prior). Likely mis-ordered `flow.questions`.", # nolint
                 100 * ratio), call. = FALSE)
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
#' @examples
#' config <- latency_build_config(
#'   1234,
#'   c("id.intro.scriptDate", "id.intro.batchDate", "id.q1.scriptDate")
#' )
#' latency_config_hash(config)
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
