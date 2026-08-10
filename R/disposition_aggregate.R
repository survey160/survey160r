# Per-respondent disposition frame for the Disposition Table.
#
# Turns one campaign's per-respondent results CSV into the disposition frame:
# one row per phone carrying 0/1 funnel flags plus the campaign's survey mode.
# This is the algorithm-only half; GCS reads, the Parquet schema, enrichment,
# and persistence live in the consumer project, mirroring the latency split
# (algorithm here, persistence downstream).
#
# Grain: one row per (phone, campaign_id). Phone is unique within a campaign
# export (verified across production campaigns), so disposition_run() enforces
# it with a hard guard rather than silently collapsing rows.
#
# The per-respondent masks below are the same signals build_summary_frame()
# (summary_aggregate.R) computes before aggregating to (date, hour); they are
# factored here as standalone helpers so a later refactor can share one
# definition between the two views.

# Parse a timestamp column to POSIXct, tolerating an absent column (returns an
# all-NA vector of length nrow(data)). Mirrors build_summary_frame()'s
# null-safe reads so a minimal export missing an optional column yields a clean
# 0/NA flag rather than an error.
.disposition_timestamp <- function(data, col) {
  if (col %in% names(data)) {
    parse_s160_timestamps_chr(data[[col]])
  } else {
    rep(as.POSIXct(NA), nrow(data))
  }
}

# started: the intro text was dispatched (id.intro.batchDate non-NA). Same
# signal as build_summary_frame()'s `texted`.
.mask_started <- function(data) {
  !is.na(.disposition_timestamp(data, "id.intro.batchDate"))
}

# engaged: the respondent replied at the intro at all (id.intro.finalValue
# present and non-empty). An absent column means the signal is not in the
# export -> nobody engaged (all FALSE), matching the null-safe convention for
# optional signal columns.
.mask_engaged <- function(data) {
  fv <- data[["id.intro.finalValue"]]
  if (is.null(fv)) {
    return(rep(FALSE, nrow(data)))
  }
  !is.na(fv) & nzchar(trimws(as.character(fv)))
}

# opt_in: passed the population filter (default id.intro.finalText == "Yes")
# AND was texted. Reuses population_filter_mask() so consent is defined exactly
# as the latency view defines n_consented. Null-safe like the other masks: if a
# column the (parseable) filter references is absent from the export, nobody
# opted in (all FALSE) rather than erroring. A filter that will not parse falls
# through to population_filter_mask(), which raises the "not valid R" error.
.mask_opt_in <- function(data, population, started) {
  vars <- tryCatch(all.vars(parse(text = population)), error = function(e) NULL)
  if (!is.null(vars)) {
    # A referenced symbol is a genuinely-absent data column only if it is
    # neither in `data` nor resolvable in the eval environment.
    # population_filter_mask() binds columns with parent = baseenv(), so base
    # symbols (T/F/pi/Inf) and function names still resolve -- treating those
    # as "absent" would wrongly zero a valid filter such as `col == T`.
    missing <- setdiff(vars, names(data))
    missing <- missing[!vapply(missing, exists, logical(1),
                               envir = baseenv(), inherits = TRUE)]
    if (length(missing) > 0L) {
      return(rep(FALSE, nrow(data)))
    }
  }
  population_filter_mask(data, population) & started
}

# web_complete: the raw web_complete callback == 1. Null-safe (absent -> FALSE).
.mask_web_complete <- function(data) {
  wc <- data[["web_complete"]]
  if (is.null(wc)) {
    return(rep(FALSE, nrow(data)))
  }
  wc_int <- suppressWarnings(as.integer(as.character(wc)))
  !is.na(wc_int) & wc_int == 1L
}

# complete: survey-mode dependent.
#   t2w          -> the web_complete callback
#   sms          -> reaching id.close.scriptDate
#   t2w_external -> not computable (external platform, no webhook) -> NA
# Non-external modes require `started`: a completion presupposes a send.
.mask_complete <- function(data, survey_mode, started) {
  if (identical(survey_mode, "t2w_external")) {
    return(rep(NA, nrow(data)))
  }
  if (identical(survey_mode, "t2w")) {
    return(.mask_web_complete(data) & started)
  }
  !is.na(.disposition_timestamp(data, "id.close.scriptDate")) & started
}

# terminated: any hard stop -- screened out (ineligible) or refused. Either
# terminal-state scriptDate being non-NA marks the row terminated.
.mask_terminated <- function(data) {
  inelig <- !is.na(.disposition_timestamp(data, "id.ineligible.scriptDate"))
  refusal <- !is.na(.disposition_timestamp(data, "id.refusal.scriptDate"))
  inelig | refusal
}

# Empty (0-row) disposition frame with the pinned column set + types. Lets
# callers handle a campaign whose export has no rows without special-casing.
empty_disposition_frame <- function() {
  data.frame(
    phone = character(0),
    campaign_id = integer(0),
    started = integer(0),
    engaged = integer(0),
    opt_in = integer(0),
    complete = integer(0),
    web_complete = integer(0),
    terminated = integer(0),
    mode = character(0),
    stringsAsFactors = FALSE
  )
}

# Source provenance carried on the disposition result's `meta`, mirroring what
# latency_report() surfaces from the source data's attributes (set by
# s160_gcs_pull_csv() / s160_read_csv()). NA when the data carries no attrs.
.disposition_meta <- function(data) {
  list(
    source_csv_hash = attr(data, "source_csv_hash") %||% NA_character_,
    source_csv_path = attr(data, "source_csv_path") %||% NA_character_
  )
}

# CSV columns disposition_run() reads directly, regardless of population or mode.
# KEEP IN SYNC with the column reads in the masks + disposition_run():
#   phone                     -- row key + dedup guard (disposition_run)
#   id.intro.batchDate        -- .mask_started
#   id.intro.finalValue       -- .mask_engaged
#   web_complete              -- .mask_web_complete + detect_survey_mode
#   id.close.scriptDate       -- .mask_complete (sms branch)
#   id.ineligible.scriptDate  -- .mask_terminated
#   id.refusal.scriptDate     -- .mask_terminated
# Deliberately NOT `campaignid` (campaign_id is stamped from the argument) and
# NOT a respondent-id column (disposition dedups by phone). The population
# columns and the data-dependent close-message Text columns are added in
# disposition_input_columns(). The projection-parity test guards drift.
.disposition_input_columns <- c(
  "phone",
  "id.intro.batchDate",
  "id.intro.finalValue",
  "web_complete",
  "id.close.scriptDate",
  "id.ineligible.scriptDate",
  "id.refusal.scriptDate"
)

#' CSV columns disposition_run() reads for a given population
#'
#' Returns the (dot-form) column names \code{disposition_run()} touches, so a
#' caller can project a wide export down to just those columns and get output
#' identical to a full read. This is the disposition analogue of
#' \code{latency_input_columns()} (latency), with two deliberate differences:
#' disposition is decoupled from the question flow (no \code{config} argument),
#' it reads \code{phone} (the row key), and it does NOT read \code{campaignid}
#' -- the \code{campaign_id} is stamped from the \code{disposition_run()}
#' argument, not the data.
#'
#' Some columns are data-dependent: the close-message Text columns that
#' \code{detect_survey_mode()} greps to tell \code{t2w_external} from
#' \code{sms}. Pass \code{available} (e.g. the result of \code{s160_csv_header()})
#' so those are matched against the real header and retained; omitting it risks
#' projecting them away and misclassifying a \code{t2w_external} campaign as
#' \code{sms}.
#'
#' @param available Optional character vector of the actual (dot-form) column
#'   names present in the file (e.g. from \code{s160_csv_header()}). When
#'   supplied, the close-message Text columns are retained. Strongly recommended.
#' @param population Optional population-filter expression defining
#'   \code{opt_in}. \code{NULL} (default) uses the package default
#'   \code{id.intro.finalText == "Yes"}. Its columns are added so a custom
#'   population's inputs are not projected away.
#' @return A character vector of unique dot-form column names, including
#'   \code{phone}. Pass it as \code{columns =} to \code{s160_read_csv()} /
#'   \code{s160_gcs_pull_csv()}.
#' @examples
#' \dontrun{
#' header <- s160_csv_header(path)
#' data <- s160_read_csv(path, columns = disposition_input_columns(header))
#' disposition <- disposition_run(1234, data)$consolidated
#' }
#' @export
disposition_input_columns <- function(available = NULL, population = NULL) {
  population <- population %||% .default_population
  # `.report_support_patterns` is the close-message Text pattern shared with
  # latency_input_columns(); detect_survey_mode() greps the same columns.
  cols <- c(.disposition_input_columns, all.vars(parse(text = population)))
  if (!is.null(available)) {
    cols <- c(cols, grep(.report_support_patterns, available, value = TRUE))
  }
  unique(cols)
}

#' Build the per-respondent disposition frame for one campaign
#'
#' Turns an in-memory campaign results CSV (one row per respondent) into a list
#' carrying the per-respondent disposition frame in \code{consolidated} (one row
#' per contacted phone, with 0/1 funnel flags \code{started}, \code{engaged},
#' \code{opt_in}, \code{complete}, \code{web_complete}, \code{terminated} and the
#' campaign's \code{mode}) plus source provenance in \code{meta}. Pure
#' function, no I/O -- pair with \code{s160_gcs_pull_csv()} for the GCS source.
#' Persisting the frame (any enrichment, provenance, and Parquet output) is
#' handled by consumer projects.
#'
#' By default (\code{contacted_only = TRUE}) the frame holds only records that
#' were actually contacted -- rows where an intro was dispatched
#' (\code{started == 1}). A never-attempted record has no disposition to report,
#' so it is excluded; non-responses (contacted but no reply) are kept. Pass
#' \code{contacted_only = FALSE} to emit one row per input respondent instead.
#'
#' Grain: one row per \code{(phone, campaign_id)}. Phone is unique within a
#' campaign export, so the function stops if it finds a duplicate phone rather
#' than silently collapsing rows. The uniqueness guard and survey-mode
#' classification always run on the full data, so the \code{contacted_only}
#' filter never changes \code{mode} or masks a duplicate.
#'
#' The \code{complete} flag is survey-mode dependent: for a \code{t2w} campaign
#' it is the \code{web_complete} callback; for \code{sms} it is reaching
#' \code{id.close.scriptDate}; for \code{t2w_external} it is not computable and
#' is \code{NA} for every row. \code{mode} is classified per campaign from the
#' data.
#'
#' @param campaign_id Campaign id (numeric or character). Stamped on every row
#'   as an integer.
#' @param data In-memory campaign results CSV as a data frame (one row per
#'   respondent). Must contain a \code{phone} column.
#' @param population Optional population-filter expression defining
#'   \code{opt_in}. \code{NULL} (default) uses the package default
#'   \code{id.intro.finalText == "Yes"} -- the same expression the latency view
#'   uses for \code{n_consented}.
#' @param contacted_only A single logical. When \code{TRUE} (default), return
#'   only contacted records (rows where \code{started == 1}). When \code{FALSE},
#'   return one row per input respondent.
#' @return A list mirroring \code{latency_run()}'s shape: \code{consolidated} (a
#'   data frame, one row per (contacted) respondent, with columns \code{phone}
#'   (character), \code{campaign_id} (integer), the 0/1 integer flags
#'   \code{started}, \code{engaged}, \code{opt_in}, \code{complete},
#'   \code{web_complete}, \code{terminated} -- \code{complete} is \code{NA} under
#'   \code{t2w_external} -- and \code{mode} (character); under the default
#'   \code{started} is \code{1} for every row) and \code{meta} (the source
#'   \code{source_csv_hash} / \code{source_csv_path}, or \code{NA}). A zero-row
#'   input, or a campaign where nobody was contacted, yields a zero-row
#'   \code{consolidated} frame.
#' @examples
#' \dontrun{
#' data <- s160_gcs_pull_csv(1234)
#' res  <- disposition_run(1234, data)
#' res$consolidated  # the disposition frame; res$meta carries source provenance
#' }
#' @export
disposition_run <- function(campaign_id, data, population = NULL,
                            contacted_only = TRUE) {
  if (!is.data.frame(data)) {
    stop("disposition_run: `data` must be a data frame.", call. = FALSE)
  }
  if (!"phone" %in% names(data)) {
    stop("disposition_run: `data` must contain a `phone` column.", call. = FALSE)
  }
  if (length(campaign_id) != 1L) {
    # A vector id would recycle into the frame and multiply rows past the
    # dedup guard (which runs on the input phone), silently breaking the grain.
    stop("disposition_run: `campaign_id` must be a single value.", call. = FALSE)
  }
  if (!is.logical(contacted_only) || length(contacted_only) != 1L ||
        is.na(contacted_only)) {
    stop("disposition_run: `contacted_only` must be a single TRUE or FALSE.",
         call. = FALSE)
  }
  if (nrow(data) == 0L) {
    return(list(consolidated = empty_disposition_frame(),
                meta = .disposition_meta(data)))
  }

  phone <- as.character(data[["phone"]])
  dup_idx <- anyDuplicated(phone)
  if (dup_idx > 0L) {
    n_dup <- sum(duplicated(phone))
    stop(sprintf(paste0(
      "disposition_run: campaign %s has %d duplicate phone value(s) (first ",
      "duplicate at row %d). The disposition grain is one row per (phone, ",
      "campaign_id); a duplicate means the export or an upstream merge ",
      "violated it."),
      as.character(campaign_id), n_dup, dup_idx), call. = FALSE)
  }

  population <- population %||% .default_population
  survey_mode <- detect_survey_mode(data)
  started <- .mask_started(data)

  out <- data.frame(
    phone = phone,
    # via as.character() so a factor id stamps its label, not its level code.
    campaign_id = rep(as.integer(as.character(campaign_id)), length(phone)),
    started = as.integer(started),
    engaged = as.integer(.mask_engaged(data)),
    opt_in = as.integer(.mask_opt_in(data, population, started)),
    complete = as.integer(.mask_complete(data, survey_mode, started)),
    web_complete = as.integer(.mask_web_complete(data)),
    terminated = as.integer(.mask_terminated(data)),
    mode = rep(survey_mode, length(phone)),
    stringsAsFactors = FALSE
  )

  if (contacted_only) {
    # `contacted_only` is validated as a single non-NA logical above, and
    # `started` is a non-NA logical mask, so this cannot introduce phantom
    # NA-indexed rows. Filter the OUTPUT (mode + dedup already ran on full data).
    out <- out[started, , drop = FALSE]
    rownames(out) <- NULL
  }
  list(consolidated = out, meta = .disposition_meta(data))
}
