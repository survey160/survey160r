# Per-respondent disposition frame for the Disposition Table.
#
# Turns one campaign's per-respondent results CSV into the disposition frame:
# one row per phone carrying 0/1 funnel flags plus the
# campaign's survey mode. This is the algorithm-only half of the disposition
# pipeline. GCS reads, the pinned Parquet schema, the Tracker-sourced
# (loi/topic) and NA-filled (error/date_closed_on) columns, and the writer all
# live in survey160-shiny (disposition/pipeline.R) -- the same split the
# latency pipeline uses (algorithm here, Parquet I/O in shiny).
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
.disp_timestamp <- function(data, col) {
  if (col %in% names(data)) {
    parse_s160_timestamps_chr(data[[col]])
  } else {
    rep(as.POSIXct(NA), nrow(data))
  }
}

# started: the intro text was dispatched (id.intro.batchDate non-NA). Same
# signal as build_summary_frame()'s `texted`.
.mask_started <- function(data) {
  !is.na(.disp_timestamp(data, "id.intro.batchDate"))
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
.mask_optin <- function(data, population, started) {
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
  !is.na(.disp_timestamp(data, "id.close.scriptDate")) & started
}

# terminated: any hard stop -- screened out (ineligible) or refused. Either
# terminal-state scriptDate being non-NA marks the row terminated.
.mask_terminated <- function(data) {
  inelig <- !is.na(.disp_timestamp(data, "id.ineligible.scriptDate"))
  refusal <- !is.na(.disp_timestamp(data, "id.refusal.scriptDate"))
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

#' Build the per-respondent disposition frame for one campaign
#'
#' Turns an in-memory campaign results CSV (one row per respondent) into the
#' per-respondent disposition frame: one row per phone,
#' with 0/1 funnel flags (\code{started}, \code{engaged}, \code{opt_in},
#' \code{complete}, \code{web_complete}, \code{terminated}) and the campaign's
#' \code{mode}. Pure function, no I/O -- pair with \code{s160_gcs_pull_csv()}
#' for the GCS source, and hand the result to the survey160-shiny writer, which
#' adds the Tracker-sourced (\code{loi}, \code{topic}) and NA-filled
#' (\code{error}, \code{date_closed_on}) columns plus provenance before writing
#' the Parquet.
#'
#' Grain: one row per \code{(phone, campaign_id)}. Phone is unique within a
#' campaign export, so the function stops if it finds a duplicate phone rather
#' than silently collapsing rows.
#'
#' The \code{complete} flag is survey-mode dependent: for a
#' \code{t2w} campaign it is the \code{web_complete} callback; for \code{sms} it
#' is reaching \code{id.close.scriptDate}; for \code{t2w_external} it is not
#' computable and is \code{NA} for every row. Mode is classified per campaign by
#' \code{detect_survey_mode()}.
#'
#' @param campaign_id Campaign id (numeric or character). Stamped on every row
#'   as an integer.
#' @param data In-memory campaign results CSV as a data frame (one row per
#'   respondent). Must contain a \code{phone} column.
#' @param population Optional population-filter expression defining
#'   \code{opt_in}. \code{NULL} (default) uses the package default
#'   \code{id.intro.finalText == "Yes"} -- the same expression the latency view
#'   uses for \code{n_consented}.
#' @return A data frame with one row per respondent and columns \code{phone}
#'   (character), \code{campaign_id} (integer), the 0/1 integer flags
#'   \code{started}, \code{engaged}, \code{opt_in}, \code{complete},
#'   \code{web_complete}, \code{terminated} (\code{complete} is \code{NA} under
#'   \code{t2w_external}), and \code{mode} (character). A zero-row input yields a
#'   zero-row frame with the same columns.
#' @examples
#' \dontrun{
#' data <- s160_gcs_pull_csv(1234)
#' disp <- disposition_run(1234, data)
#' }
#' @export
disposition_run <- function(campaign_id, data, population = NULL) {
  if (!is.data.frame(data)) {
    stop("disposition_run: `data` must be a data frame.", call. = FALSE)
  }
  if (!"phone" %in% names(data)) {
    stop("disposition_run: `data` must contain a `phone` column.", call. = FALSE)
  }
  if (nrow(data) == 0L) {
    return(empty_disposition_frame())
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

  data.frame(
    phone = phone,
    campaign_id = rep(as.integer(campaign_id), length(phone)),
    started = as.integer(started),
    engaged = as.integer(.mask_engaged(data)),
    opt_in = as.integer(.mask_optin(data, population, started)),
    complete = as.integer(.mask_complete(data, survey_mode, started)),
    web_complete = as.integer(.mask_web_complete(data)),
    terminated = as.integer(.mask_terminated(data)),
    mode = rep(survey_mode, length(phone)),
    stringsAsFactors = FALSE
  )
}
