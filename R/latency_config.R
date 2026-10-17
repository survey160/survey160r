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


#' Discover the question flow from export column names
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
  # The FLOW questions are the full enumerated set (.all_questions, opener.R --
  # one regex shared with the terminal disposition masks) minus the terminal
  # states, so a screen-out never counts as a body step. .all_questions already
  # dedupes and preserves column (flow) order.
  qs <- .all_questions(data)
  qs[!qs %in% .terminal_states]
}

#' Build a latency config from a campaign export
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
    cols <- if (is.data.frame(data)) names(data) else as.character(data)
    stop_s160(sprintf(paste0(
      "could not discover at least two questions from the export's columns ",
      "(need id.<question>.scriptDate columns; found %s). `data` has %d ",
      "column(s), e.g. %s. Read the export with s160_read_csv() or ",
      "s160_gcs_campaign_results_read() so its headers are in dot form."),
      if (length(questions)) paste(questions, collapse = ", ") else "none",
      length(cols), .preview_values(cols)),
      fn = "latency_build_config")
  }
  # Opt-in / consent is routing-based by default (the funnel masks derive it from
  # reaching a continuation step -- phrasing- and language-agnostic), so no
  # population filter is generated: `filters.population = NULL`. A config author
  # may still set a custom consent filter (e.g. a specific finalText value), which
  # then overrides the routing default.
  list(
    project_id = as.integer(project_id %||% campaign_id),
    campaign_id = as.integer(campaign_id),
    field_timezone = field_timezone,
    flow = list(questions = questions),
    filters = list(
      population = NULL,
      campaign_id_column = "campaignid",
      respondent_id_column = respondent_id_column,
      date_filter = date_filter
    )
  )
}

#' Validate a latency config against a campaign export
#'
#' Implements the fail-fast checks from spec section 2.4. Aborts with a named error
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
  .validate_config_shape(config)
  validate_columns_present(config, data)
  validate_flow_order(config, data)
  invisible(TRUE)
}

# The data-independent half of latency_validate_config(): key set, required
# scalars, and the question list. latency_report() runs this and the column
# check before it parses, and the flow-order check on the parsed columns after
# -- the same three checks in the same order, without decoding every timestamp
# a second time for the flow-order ratio.
.validate_config_shape <- function(config) {
  if (!is.list(config)) {
    stop_config(sprintf("must be a list (from latency_build_config())%s.",
                        .not_got(config)))
  }
  unknown <- setdiff(names(config), .config_keys)
  if (length(unknown) > 0) {
    stop_config(sprintf("unknown key(s): %s (known keys: %s).",
                        paste(unknown, collapse = ", "),
                        paste(.config_keys, collapse = ", ")))
  }
  required <- c("project_id", "campaign_id", "field_timezone")
  absent <- required[vapply(config[required], is.null, logical(1))]
  if (length(absent) > 0) {
    stop_config(sprintf("`%s` %s required.", paste(absent, collapse = "`, `"),
                        if (length(absent) == 1L) "is" else "are"))
  }
  validate_questions(config$flow$questions)
  invisible(TRUE)
}

# Raise a config error: the "config:" domain prefix (not a function name, see
# conditions.R) and the `s160_config_error` class, so a caller validating many
# hand-written configs can catch exactly these.
stop_config <- function(msg) {
  stop_s160(paste0("config: ", msg), subclass = "s160_config_error")
}

validate_questions <- function(questions) {
  if (is.null(questions) || length(questions) == 0) {
    stop_config("`flow.questions` is required and must be non-empty.")
  }
  if (length(questions) < 2) {
    stop_config(sprintf("`flow.questions` must contain at least two questions%s.",
                        .not_got(questions)))
  }
  dup <- unique(questions[duplicated(questions)])
  if (length(dup) > 0) {
    stop_config(sprintf("`flow.questions` contains duplicate(s): %s.",
                        paste(dup, collapse = ", ")))
  }
  bad <- intersect(questions, .terminal_states)
  if (length(bad) > 0) {
    stop_config(sprintf(paste0(
      "`flow.questions` must not include terminal state(s): %s. ",
      "latency_discover_questions() excludes them; drop them from the flow."),
      paste(bad, collapse = ", ")))
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
    stop_config(sprintf(paste0(
      "required column(s) missing from `data`: %s. `flow.questions` is %s; ",
      "`data` has %d column(s), e.g. %s. Check the flow against ",
      "s160_csv_header() / names(data)."),
      paste(missing_cols, collapse = ", "),
      paste(config$flow$questions, collapse = ", "),
      ncol(data), .preview_values(names(data))))
  }
}

# Non-flow columns latency_report() reads in addition to the per-question
# scriptDate/batchDate set. KEEP IN SYNC with the data[[...]] reads elsewhere:
#   web_complete              -- detect_survey_mode() + t2w completion (summary_aggregate.R)
#   id.ineligible.scriptDate  -- build_ineligible_frame() (a terminal state, so it
#                                is NOT in config$flow$questions and would be missed
#                                by required_timestamp_columns alone)
#   id.refusal.scriptDate     -- build_refusal_frame() (the refusal-terminal sibling,
#                                likewise not in config$flow$questions)
# The opener finalText column(s) are NOT listed here: they are the population's
# referenced columns, and latency_input_columns() already retains all.vars(pop),
# so the opener-family finalText is projected per campaign (not hardcoded intro).
# Dropping any of these silently changes output (e.g. a t2w campaign would
# misclassify as "sms"), so they are always retained by latency_input_columns().
# The projection parity test (test-latency_input_columns.R) guards against drift.
.report_support_columns <- c(
  "web_complete",
  "id.ineligible.scriptDate",
  "id.refusal.scriptDate"
)

# Non-flow columns whose names are NOT fixed -- detect_survey_mode()'s
# has_personalized_close_link() greps the close-message Text columns
# (id.close<...>.scriptText / .batchText) to classify t2w_external vs sms.
# These can only be matched against the actual header, so latency_input_columns()
# retains them when the caller passes `available`. KEEP IN SYNC with
# has_personalized_close_link() in summary_aggregate.R.
.report_support_patterns <- "^id\\.close[A-Za-z0-9_]*\\.(script|batch)Text$"

#' List the export columns the latency report reads
#'
#' Returns the (dot-form) column names \code{latency_report()} touches for a
#' given \code{config}: the per-question \code{scriptDate}/\code{batchDate}
#' set, the population-filter columns (extracted from
#' \code{config$filters$population}), the campaign-id and optional
#' respondent-id columns, plus the fixed non-flow support columns
#' (\code{web_complete}, \code{id.ineligible.scriptDate},
#' \code{id.refusal.scriptDate}). The opener
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
  # via .question_timestamp(). required_timestamp_columns() drops the batchDate of
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
  n_seg <- length(questions) - 1L
  seg_ok <- integer(n_seg)
  seg_cmp <- integer(n_seg)
  for (i in seq_len(n_seg)) {
    prior <- sprintf("id.%s.batchDate", questions[i])
    nxt <- sprintf("id.%s.scriptDate", questions[i + 1])
    bp <- parse_campaign_timestamps(data[[prior]])
    sn <- parse_campaign_timestamps(data[[nxt]])
    valid <- !is.na(bp) & !is.na(sn)
    if (!any(valid)) next
    seg_cmp[i] <- sum(valid)
    seg_ok[i] <- sum(sn[valid] >= bp[valid])
  }
  cmp_total <- sum(seg_cmp)
  if (cmp_total == 0) return(invisible(TRUE))
  ratio <- sum(seg_ok) / cmp_total
  if (ratio < 0.9) {
    # Name the segment(s) that drag the ratio down, so a mis-ordered flow is
    # located at once rather than hunted through every question pair.
    seg_ratio <- ifelse(seg_cmp > 0L, seg_ok / seg_cmp, NA_real_)
    worst <- which(!is.na(seg_ratio) & seg_ratio < 0.9)
    worst <- worst[order(seg_ratio[worst])]
    worst <- worst[seq_len(min(3L, length(worst)))]
    segments <- sprintf("%s -> %s (%.1f%% of %d rows)",
                        questions[worst], questions[worst + 1L],
                        100 * seg_ratio[worst], seg_cmp[worst])
    stop_config(sprintf(paste0(
      "flow order check failed: only %.1f%% of rows have scriptDate(next) ",
      ">= batchDate(prior); out of order at %s. `flow.questions` (%s) is ",
      "likely mis-ordered."),
      100 * ratio, paste(segments, collapse = ", "),
      paste(questions, collapse = ", ")))
  }
  invisible(TRUE)
}

#' Hash a latency config
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
