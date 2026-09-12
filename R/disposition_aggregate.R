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
# `sent`/`engaged`/`opted_in` key on the OPENING question SET (every intro-family
# question, or a single discovered opener) resolved per campaign by the shared
# .discover_openers() (opener.R), NOT a hardcoded "intro" -- so a campaign whose
# opener is named "FIRSTNET" / "intro_sp", or a bilingual campaign routing some
# recipients to intro and others to intro_sp / intro_latinos, is measured on every
# branch instead of being silently dropped (it was: routed recipients' flags came
# up 0). The latency build_summary_frame() view resolves the opener set from the
# SAME shared helpers and keys `texted` on the send (scriptDate) with `engaged`
# gated on it, so the two views measure the same name-agnostic funnel.

# sent (contacted), engaged (replied & sent), and opted_in (reached a
# continuation step & sent -- or an explicit population filter & sent) are the
# shared per-recipient funnel masks -- .funnel_masks() (opener.R) computes them
# identically for the latency summary, so the two views measure the same funnel.
# disposition_run() calls .funnel_masks() once (below) rather than a per-flag
# mask here.

# web_complete: the raw web_complete callback == 1. Null-safe (absent -> FALSE).
.mask_web_complete <- function(data) {
  wc <- data[["web_complete"]]
  if (is.null(wc)) {
    return(rep(FALSE, nrow(data)))
  }
  wc_int <- suppressWarnings(as.integer(as.character(wc)))
  !is.na(wc_int) & wc_int == 1L
}

# completed: survey-mode dependent.
#   t2w          -> the web_complete callback
#   sms          -> reaching the close (any close-family scriptDate: close /
#                   close_sp / close_latinos, via .reached_close) so a bilingual
#                   campaign's Spanish completers are counted, matching latency.
#   t2w_external -> not computable (external platform, no webhook) -> NA
# Non-external modes require `sent`: a completion presupposes a send.
.mask_completed <- function(data, survey_mode, sent) {
  if (identical(survey_mode, "t2w_external")) {
    return(rep(NA, nrow(data)))
  }
  if (identical(survey_mode, "t2w")) {
    return(.mask_web_complete(data) & sent)
  }
  .reached_close(data, latency_discover_questions(data)) & sent
}

# terminated: any hard stop -- screened out (ineligible) or refused. Either
# terminal-state scriptDate being non-NA marks the row terminated.
.mask_terminated <- function(data) {
  inelig <- !is.na(.column_timestamps(data, "id.ineligible.scriptDate"))
  refusal <- !is.na(.column_timestamps(data, "id.refusal.scriptDate"))
  inelig | refusal
}

# error: the carrier delivery-error code for this record, as a string. The
# export carries phonelist.error_code, written only on a send/delivery failure
# (a 4-digit Bandwidth code; a clean send leaves it NULL, which the export
# renders as ""). It is a delivery-quality attribute orthogonal to the funnel --
# an errored record is almost always a non-response whose message never landed,
# and it co-occurs with, rather than replaces, a funnel outcome. The code is
# carried through as a string, never interpreted or bucketed here (a
# human-readable category, if ever wanted, is a read-time concern). NOTE the CSV
# reader (fread) infers this column's type per file: codes+blanks come back
# integer, an all-blank column (a campaign with no errors -- the common case)
# logical, only a column carrying "None"/alpha stays character -- so as.character()
# re-renders it. Real 4-5 digit carrier codes have no leading zeros, so the
# string is stable. Blank / whitespace / "None" / a reader-supplied NA all
# normalize to NA, so `error` is NA whenever the export carries no usable code.
# Null-safe (column absent -> all NA), mirroring the masks.
.disposition_error <- function(data) {
  ec <- data[["error_code"]]
  if (is.null(ec)) {
    return(rep(NA_character_, nrow(data)))
  }
  ec <- trimws(as.character(ec))
  # is.na() leads so the logical index never itself contains NA (a NA index
  # would be a silent no-op here, correct but fragile); a reader NA stays NA.
  ec[is.na(ec) | ec == "" | ec == "None"] <- NA_character_
  ec
}

# disposition_date: the per-respondent disposition day -- the CSV analogue of the
# DB producer's `lastsms::date`. Each script step's send timestamp lives in an
# `id.<step>.scriptDate` column, so the ROW-WISE MAX over every scriptDate is the
# phone's LAST outbound message (its terminal activity), and it is bucketed to the
# field timezone exactly as the latency view buckets a send
# (`as.Date(format(send, tz))`) and the DB path buckets lastsms
# (`lastsms AT TIME ZONE 'UTC' AT TIME ZONE tz`). parse_campaign_timestamps
# returns UTC, so `format(..., tz)` performs the naive-UTC -> field-tz shift. A row
# with no send timestamp -- or an input projected down to columns that carry no
# scriptDate -- yields NA (the historical CSV-only campaigns keep NA where no send
# time survives). Pure.
.disposition_dates <- function(data, field_timezone) {
  cols <- grep("^id\\..+\\.scriptDate$", names(data), value = TRUE)
  if (length(cols) == 0L) {
    return(rep(as.Date(NA), nrow(data)))
  }
  # numeric UTC epoch seconds per scriptDate column; pmax(na.rm) is the NA-safe
  # row-wise max (all-NA row -> NA, not -Inf), reduced across the columns.
  secs <- lapply(cols, function(col) as.numeric(parse_campaign_timestamps(data[[col]])))
  mx <- Reduce(function(a, b) pmax(a, b, na.rm = TRUE), secs)
  as.Date(format(as.POSIXct(mx, origin = "1970-01-01", tz = "UTC"),
                 tz = field_timezone))
}

# Empty (0-row) disposition frame with the pinned column set + types. Lets
# callers handle a campaign whose export has no rows without special-casing.
empty_disposition_frame <- function() {
  data.frame(
    phone = character(0),
    campaign_id = integer(0),
    sent = integer(0),
    engaged = integer(0),
    opted_in = integer(0),
    completed = integer(0),
    web_complete = integer(0),
    terminated = integer(0),
    mode = character(0),
    error = character(0),
    disposition_date = as.Date(character(0)),
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
#' Some columns are data-dependent, so \code{available} (e.g. the result of
#' \code{s160_csv_header()}) is effectively required for a faithful projection:
#' \enumerate{
#'   \item the close-message Text columns \code{detect_survey_mode()} greps to
#'     tell \code{t2w_external} from \code{sms} -- omit them and a
#'     \code{t2w_external} campaign is misclassified as \code{sms};
#'   \item every survey-body \code{scriptDate}, which the routing-based
#'     \code{opted_in} keys on. Without \code{available} the set cannot enumerate
#'     the body questions (it degrades to \code{intro} + the fallback \code{close}
#'     only), so a projected read of a multi-question campaign would DROP the body
#'     timestamps and silently undercount \code{opted_in} (a recipient who reached
#'     a body question but not the close). Pass \code{available} (or read the full
#'     file) whenever a campaign has body questions.
#' }
#'
#' @param available Optional character vector of the actual (dot-form) column
#'   names present in the file (e.g. from \code{s160_csv_header()}). When
#'   supplied, the close-message Text columns and every survey-body
#'   \code{scriptDate} are retained. Effectively required: omitting it yields a
#'   lossy projection that can undercount \code{opted_in} on a multi-question
#'   campaign (see Details).
#' @param population Optional population-filter expression defining
#'   \code{opted_in}. \code{NULL} (default) is routing-based opt-in -- the
#'   recipient reached a continuation step (any non-opener, non-terminal
#'   \code{scriptDate}), which is language- and phrasing-agnostic and reads no
#'   answer text. A caller may instead pass an explicit filter (e.g.
#'   \code{id.intro.finalText == "Yes"}); its columns are added so a custom
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
  # {"intro"} (the default set); the routing-based opt-in then keys on the close
  # family (the continuation fallback), so no finalText column is read.
  qs <- latency_discover_questions(available)
  openers <- .opening_questions(qs)
  closers <- .closing_questions(qs)
  # Default opt-in is routing-based (reached a continuation step): its scriptDate
  # columns are the continuation + close family already required below, so only a
  # caller-supplied `population` filter contributes extra columns.
  pop_cols <- if (is.null(population)) character(0) else all.vars(parse(text = population))
  # `.report_support_patterns` is the close-message Text pattern shared with
  # latency_input_columns(); detect_survey_mode() greps the same columns.
  cols <- c(
    "phone",
    sprintf("id.%s.scriptDate", openers),
    sprintf("id.%s.batchDate", openers),
    sprintf("id.%s.scriptDate", .continuation_questions(qs)),
    pop_cols,
    "web_complete",
    "error_code",                           # raw carrier delivery-error code (-> `error`)
    sprintf("id.%s.scriptDate", closers),   # close family (close / close_sp / ...)
    "id.ineligible.scriptDate",
    "id.refusal.scriptDate"
  )
  if (!is.null(available)) {
    cols <- c(
      cols,
      grep(.report_support_patterns, available, value = TRUE),
      # every script-step send timestamp, so disposition_run() can take the
      # row-wise max(scriptDate) for `disposition_date` (not just the opener /
      # closer / terminal sends already listed above).
      grep("^id\\..+\\.scriptDate$", available, value = TRUE))
  }
  unique(cols)
}

#' Build the per-respondent disposition frame for one campaign
#'
#' Turns an in-memory campaign results CSV (one row per respondent) into a list
#' carrying the per-respondent disposition frame in \code{consolidated} (one row
#' per contacted phone, with 0/1 funnel flags \code{sent}, \code{engaged},
#' \code{opted_in}, \code{completed}, \code{web_complete}, \code{terminated}, the
#' campaign's \code{mode}, the raw carrier delivery-error code \code{error}, and
#' the \code{disposition_date} (the last-send day, \code{max(scriptDate)}))
#' plus source provenance in \code{meta}. Pure
#' function, no I/O -- pair with \code{s160_gcs_campaign_results_read(hash = TRUE)} for the GCS source.
#' Persisting the frame (any enrichment, provenance, and Parquet output) is
#' handled by consumer projects.
#'
#' By default (\code{contacted_only = TRUE}) the frame holds only records that
#' were actually contacted -- rows where an intro was dispatched
#' (\code{sent == 1}). A never-attempted record has no disposition to report,
#' so it is excluded; non-responses (contacted but no reply) are kept. Pass
#' \code{contacted_only = FALSE} to emit one row per input respondent instead.
#'
#' Grain: one row per \code{(phone, campaign_id)}. Phone is unique within a
#' campaign export, so the function stops if it finds a duplicate phone rather
#' than silently collapsing rows. The uniqueness guard and survey-mode
#' classification always run on the full data, so the \code{contacted_only}
#' filter never changes \code{mode} or masks a duplicate.
#'
#' The \code{completed} flag is survey-mode dependent: for a \code{t2w} campaign
#' it is the \code{web_complete} callback; for \code{sms} it is reaching the
#' close -- any close-family \code{scriptDate} (\code{id.close.scriptDate} /
#' \code{id.close_sp.scriptDate} / ...), so a bilingual campaign's Spanish
#' completers count; for \code{t2w_external} it is not computable and
#' is \code{NA} for every row. \code{mode} is classified per campaign from the
#' data.
#'
#' @param campaign_id Campaign id (numeric or character). Stamped on every row
#'   as an integer.
#' @param data In-memory campaign results CSV as a data frame (one row per
#'   respondent). Must contain a \code{phone} column.
#' @param population Optional population-filter expression defining
#'   \code{opted_in}. \code{NULL} (default) is routing-based: a recipient opted in
#'   when the opener routed them FORWARD -- i.e. they reached a continuation step
#'   (any non-opener, non-terminal \code{scriptDate}: a survey-body question or
#'   the close family). This is language- and phrasing-agnostic (it reads no
#'   answer text), so a Spanish or non-\dQuote{Yes} opt-in still counts, where the
#'   legacy \code{id.intro.finalText == "Yes"} match silently read 0. Pass an
#'   explicit filter to decide \code{opted_in} by a specific answer instead. A
#'   completion is also treated as an opt-in (\code{opted_in} is OR-ed with
#'   \code{completed}), so \code{opted_in >= completed} in every mode -- this
#'   matters for \code{t2w} campaigns whose web link sits in the intro, where a
#'   web completion is the only opt-in evidence. The latency view resolves
#'   \code{n_opted_in} from the same routing signal and the same fold, so the two
#'   views agree.
#' @param contacted_only A single logical. When \code{TRUE} (default), return
#'   only contacted records (rows where \code{sent == 1}). When \code{FALSE},
#'   return one row per input respondent.
#' @param field_timezone IANA timezone (a name in \code{OlsonNames()}; default
#'   \code{"America/New_York"}) the \code{disposition_date} is bucketed to; an
#'   unknown zone is rejected rather than silently mis-bucketed. Each
#'   \code{id.<step>.scriptDate}
#'   send timestamp is stored naive-UTC; the row-wise max is converted to this
#'   zone before its calendar date is taken -- matching the latency view's
#'   send-date bucketing and the live DB producer's \code{lastsms::date}.
#' @return A list mirroring \code{latency_run()}'s shape: \code{consolidated} (a
#'   data frame, one row per (contacted) respondent, with columns \code{phone}
#'   (character), \code{campaign_id} (integer), the 0/1 integer flags
#'   \code{sent}, \code{engaged}, \code{opted_in}, \code{completed},
#'   \code{web_complete}, \code{terminated} -- \code{completed} is \code{NA} under
#'   \code{t2w_external} -- \code{mode} (character), \code{error} (character;
#'   the raw carrier delivery-error code, \code{NA} when the export carries no
#'   usable error code), and \code{disposition_date} (a \code{Date}: the row-wise
#'   max \code{id.<step>.scriptDate} bucketed to \code{field_timezone} -- the last
#'   outbound send -- or \code{NA} when no send time survives); under the default
#'   \code{sent} is \code{1} for every row) and
#'   \code{meta} (the source
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
                            contacted_only = TRUE,
                            field_timezone = "America/New_York") {
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
  if (!is.character(field_timezone) || length(field_timezone) != 1L ||
        is.na(field_timezone) || !nzchar(field_timezone)) {
    stop_s160("`field_timezone` must be a single non-empty string.",
              fn = "disposition_run")
  }
  # An unknown zone is silently ignored by `format(..., tz =)` on some platforms
  # (falling back to UTC / local), which would assign a wrong disposition_date --
  # so reject anything not in the IANA database rather than bucket dates wrong.
  if (!field_timezone %in% OlsonNames()) {
    stop_s160(sprintf(
      "`field_timezone` (\"%s\") is not a known IANA timezone (see OlsonNames()).",
      field_timezone), fn = "disposition_run")
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

  survey_mode <- detect_survey_mode(data)
  questions <- latency_discover_questions(data)
  # `population` NULL (the default) -> .funnel_masks derives opt-in from routing
  # (reached a continuation step); a caller-supplied filter overrides it.
  masks <- .funnel_masks(data, .opening_questions(questions), questions, population)
  sent <- masks$sent
  completed <- .mask_completed(data, survey_mode, sent)
  # A completion IS an opt-in: fold it into opted_in so the funnel stays monotone
  # (completed <= opted_in) in every mode. For sms this is a no-op (the close is a
  # continuation step); for t2w it recovers web-completers whose link sat in the
  # intro (no downstream close scriptDate). t2w_external completed is NA (%in% is
  # NA-safe), and `completed` is already gated on `sent`. KEEP IN SYNC with
  # build_summary_frame() (summary_aggregate.R), which folds it identically.
  opted_in <- masks$opted_in | (completed %in% TRUE)

  # Output columns use the canonical funnel vocabulary (sent / engaged /
  # opted_in / completed), matching the latency signals and .funnel_masks().
  out <- data.frame(
    phone = phone,
    # via as.character() so a factor id stamps its label, not its level code.
    campaign_id = rep(as.integer(as.character(campaign_id)), length(phone)),
    sent = as.integer(sent),
    engaged = as.integer(masks$engaged),
    opted_in = as.integer(opted_in),
    completed = as.integer(completed),
    web_complete = as.integer(.mask_web_complete(data)),
    terminated = as.integer(.mask_terminated(data)),
    mode = rep(survey_mode, length(phone)),
    error = .disposition_error(data),
    disposition_date = .disposition_dates(data, field_timezone),
    stringsAsFactors = FALSE
  )

  if (contacted_only) {
    # `contacted_only` is validated as a single non-NA logical above, and
    # `sent` is a non-NA logical mask, so this cannot introduce phantom
    # NA-indexed rows. Filter the OUTPUT (mode + dedup already ran on full data).
    out <- out[sent, , drop = FALSE]
    rownames(out) <- NULL
  }
  list(consolidated = out, meta = .disposition_meta(data))
}
