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
# The per-respondent masks below mirror the signals build_summary_frame()
# (summary_aggregate.R) computes before aggregating to (date, hour). NOTE:
# `started`/`engaged`/`opt_in` key on the OPENING question SET (every intro-family
# question, or a single discovered opener) resolved per campaign by the shared
# .discover_openers() (opener.R), NOT a hardcoded "intro" -- so a campaign whose
# opener is named "FIRSTNET" / "intro_sp", or a bilingual campaign routing some
# recipients to intro and others to intro_sp / intro_latinos, is measured on every
# branch instead of being silently dropped (it was: routed recipients' flags came
# up 0). The latency build_summary_frame() view resolves the opener set from the
# SAME shared helpers and keys `texted` on the send (scriptDate) with `engaged`
# gated on it, so the two views measure the same name-agnostic funnel.

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

# Default opt-in population for a disposition run: the opener set's accepted
# answer is "Yes". Delegates to the shared .opener_population() so consent is
# defined exactly as the latency view defines n_consented; .dot_form_headers()
# normalizes a raw bracket-form header so a routed campaign keeps every branch.
.disposition_default_population <- function(x) {
  available <- if (is.data.frame(x)) names(x) else as.character(x)
  .opener_population(.discover_openers(x), .dot_form_headers(available))
}

# started (contacted): the recipient received ANY opening send (id.<opener>.
# scriptDate). NOT batchDate: that is the inbound REPLY, used by `engaged`.
.mask_started <- function(data) {
  .opener_events(data, .discover_openers(data), "scriptDate")
}

# engaged: the recipient REPLIED to an opening question (id.<opener>.batchDate)
# AND was started. Gating on `started` matches the latency view (n_engaged is
# `!is.na(opener.batchDate) & texted`): a reply presupposes a send, so a stray
# reply with no recorded send is not counted as engaged by either view. Distinct
# from opt_in, which additionally requires an accepted "Yes" answer.
.mask_engaged <- function(data, started) {
  .opener_events(data, .discover_openers(data), "batchDate") & started
}

# opt_in: passed the population filter (default id.<opener>.finalText == "Yes")
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
# s160_gcs_campaign_results_read(hash = TRUE) / s160_read_csv()). NA when the data carries no attrs.
.disposition_meta <- function(data) {
  list(
    source_csv_hash = attr(data, "source_csv_hash") %||% NA_character_,
    source_csv_path = attr(data, "source_csv_path") %||% NA_character_
  )
}

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
#'   \code{opt_in}. \code{NULL} (default) uses the opening question set's accepted
#'   answer -- \code{id.intro.finalText == "Yes"} for a normal campaign, a
#'   disjunction over the intro-family openers for a routed one (opener set
#'   discovered from \code{available}). Its columns are added so a custom
#'   population's inputs are not projected away.
#' @return A character vector of unique dot-form column names, including
#'   \code{phone}. Pass it as \code{columns =} to \code{s160_read_csv()} /
#'   \code{s160_gcs_campaign_results_read()}.
#' @seealso \code{\link{latency_input_columns}}, the latency analogue. It leads
#'   with its \emph{required} \code{config}; here \code{available} leads because
#'   it is the argument you almost always pass (see the example), and there is no
#'   required argument -- a deliberate difference, not an inconsistency.
#' @examples
#' \dontrun{
#' header <- s160_csv_header(path)
#' data <- s160_read_csv(path, columns = disposition_input_columns(header))
#' disposition <- disposition_run(1234, data)$consolidated
#' }
#' @export
disposition_input_columns <- function(available = NULL, population = NULL) {
  # The opener's name varies per campaign (and a routed campaign has several), so
  # resolve the opening set from `available` and lead with its columns: a
  # projection preserves column order, and for a single non-intro opener the mask
  # reads flow order from that order -- a later question (e.g. close) must not
  # precede the opener and shadow it. With `available` NULL the set degrades to
  # {"intro"} (the default set) and the population default matches the historical
  # id.intro.finalText.
  openers <- .discover_openers(available)
  population <- population %||% .disposition_default_population(available)
  # `.report_support_patterns` is the close-message Text pattern shared with
  # latency_input_columns(); detect_survey_mode() greps the same columns.
  cols <- c(
    "phone",
    sprintf("id.%s.scriptDate", openers),
    sprintf("id.%s.batchDate", openers),
    all.vars(parse(text = population)),
    "web_complete",
    "id.close.scriptDate",
    "id.ineligible.scriptDate",
    "id.refusal.scriptDate"
  )
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
#' function, no I/O -- pair with \code{s160_gcs_campaign_results_read(hash = TRUE)} for the GCS source.
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
#'   \code{opt_in}. \code{NULL} (default) uses the opening question set's accepted
#'   answer -- \code{id.intro.finalText == "Yes"} for a normal campaign, a
#'   disjunction over the intro-family openers for a routed one -- resolved per
#'   campaign from the data. The latency view resolves \code{n_consented} from
#'   the same opener set, so the two views agree for a non-\code{intro} campaign.
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
#' data <- s160_gcs_campaign_results_read(1234, hash = TRUE)
#' res  <- disposition_run(1234, data)
#' res$consolidated  # the disposition frame; res$meta carries source provenance
#' }
#' @export
disposition_run <- function(campaign_id, data, population = NULL,
                            contacted_only = TRUE) {
  check_data_frame(data, "data", fn = "disposition_run")
  if (!"phone" %in% names(data)) {
    stop_s160("`data` must contain a `phone` column.", fn = "disposition_run")
  }
  if (length(campaign_id) != 1L) {
    # A vector id would recycle into the frame and multiply rows past the
    # dedup guard (which runs on the input phone), silently breaking the grain.
    stop_s160("`campaign_id` must be a single value.", fn = "disposition_run")
  }
  if (!is.logical(contacted_only) || length(contacted_only) != 1L ||
        is.na(contacted_only)) {
    stop_s160("`contacted_only` must be a single TRUE or FALSE.",
              fn = "disposition_run")
  }
  if (nrow(data) == 0L) {
    return(list(consolidated = empty_disposition_frame(),
                meta = .disposition_meta(data)))
  }

  phone <- as.character(data[["phone"]])
  dup_idx <- anyDuplicated(phone)
  if (dup_idx > 0L) {
    n_dup <- sum(duplicated(phone))
    stop_s160(sprintf(paste0(
      "campaign %s has %d duplicate phone value(s) (first ",
      "duplicate at row %d). The disposition grain is one row per (phone, ",
      "campaign_id); a duplicate means the export or an upstream merge ",
      "violated it."),
      as.character(campaign_id), n_dup, dup_idx), fn = "disposition_run")
  }

  population <- population %||% .disposition_default_population(data)
  survey_mode <- detect_survey_mode(data)
  started <- .mask_started(data)

  out <- data.frame(
    phone = phone,
    # via as.character() so a factor id stamps its label, not its level code.
    campaign_id = rep(as.integer(as.character(campaign_id)), length(phone)),
    started = as.integer(started),
    engaged = as.integer(.mask_engaged(data, started)),
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
