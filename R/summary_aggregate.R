# Summary metrics aggregation (spec §4). Computed on the pre-filter data
# frame so the sent/opted_in/completed denominators reflect the full
# campaign population, not just the funnel survivors.
#
# Orchestrator (called by latency_report() before the population filter):
#   build_summary_frame(data, config)     -> per (campaign, date, hour) counts
#   build_ineligible_frame(data, config)  -> per (campaign, date, hour, segment_index) counts
#   collapse_summary_to_day(frame)        -> hourly -> day rollup (hour_local = NA)
#
# These return small data frames that aggregate_consolidated() left-joins
# onto the latency cells. Denormalising into the same parquet keeps the
# Shiny consumer on one read; the doc decided in favour of that over a
# sidecar parquet (campaign_scripts.md §4.3, decision: one parquet).

# Classify a campaign's survey mode from the source data, per this
# rule:
#   * web completes present                  -> "t2w"           (web survey,
#                                               completion = web_complete)
#   * a personalized survey link but no web   -> "t2w_external"  (web survey on
#                                               an external platform with no
#                                               webhook; completion not
#                                               computable -> n_completed = NA)
#   * no web completes and no survey link     -> "sms"           (live SMS)
#
# A "survey link" is a personalized URL in the close message -- one that varies
# per respondent (carries a per-recipient token). A single static link shared
# by all respondents (e.g. a youtube/temu stimulus) is NOT a survey link.
# Platforms use different token params (__userid / rid / uid / ...), so a fixed
# token-name list is brittle; we test personalization directly via the distinct
# close-URL count instead. The authoritative flag (campaigns.use_web_completes
# in the v2 DB) is not in the CSV export, hence this data-only heuristic.
detect_survey_mode <- function(data) {
  wc <- data[["web_complete"]]
  if (!is.null(wc)) {
    wc_int <- suppressWarnings(as.integer(as.character(wc)))
    if (any(!is.na(wc_int) & wc_int == 1L)) return("t2w")
  }
  if (has_personalized_close_link(data)) "t2w_external" else "sms"
}

# TRUE when the close message carries a personalized (per-respondent) survey
# URL, detected as more than one distinct URL across respondents. A single
# shared static URL (stimulus link) yields one distinct URL and is ignored.
has_personalized_close_link <- function(data) {
  close_cols <- grep("^id\\.close[A-Za-z0-9_]*\\.(script|batch)Text$",
                     names(data), value = TRUE)
  if (length(close_cols) == 0L) return(FALSE)
  urls <- character(0)
  for (col in close_cols) {
    txt <- as.character(data[[col]])
    hit <- regmatches(txt, regexpr("https?://\\S+", txt))
    urls <- c(urls, hit[nzchar(hit)])
  }
  length(unique(urls)) > 1L
}

# Build the per-(campaign_id, date, hour_local) summary frame at hourly
# grain. Output columns: campaign_id, date (Date), hour_local (int 0..23),
# n_sent, n_engaged, n_opted_in, n_completed (all int32-safe). Returns a
# zero-row frame with the correct schema if `data` has no respondents --
# callers rbind multiple of these for day rollups without special-casing
# empties.
#
# The funnel is send-anchored: `n_sent` counts recipients the platform
# SENT the intro to (id.intro.scriptDate, the outbound scripted send);
# `n_engaged` is the subset that REPLIED (id.intro.batchDate, the inbound
# reply). Keying n_sent on scriptDate matches disposition_run()'s
# `sent`/`engaged` split (disposition_aggregate.R) -- an earlier version keyed
# it on batchDate, which counted repliers, not sends. n_opted_in and
# n_completed are subsets of the sent cohort.
#
# `survey_mode` selects the completion signal: "sms" (default)
# completes on id.close.scriptDate; "t2w" on the web_complete callback;
# "t2w_external" is not computable (n_completed nulled to NA downstream, since
# the SMS close is just the external survey link sent to every consenter).
build_summary_frame <- function(data, config, survey_mode = "sms") {
  if (nrow(data) == 0L) return(empty_summary_frame())
  campaign_col <- config$filters$campaign_id_column
  field_tz <- config$field_timezone

  # Parse the timestamps summary needs, keyed on the OPENING question set (not a
  # hardcoded "intro") -- .question_timestamp coalesces each recipient's opener
  # send/reply across the intro-family / discovered opener, so a non-intro
  # (FIRSTNET) or bilingual (intro + intro_sp) campaign is measured, not dropped.
  # Null-safe per column, so this also replaces the old unguarded read that
  # crashed on an absent id.intro.scriptDate.
  openers <- .opening_questions(config$flow$questions)
  # sent / engaged / opted_in / completed are the shared per-recipient funnel
  # masks -- computed identically here and in the disposition transform
  # (.funnel_masks in opener.R), so the two views measure the same funnel. `send`
  # (the coalesced opener scriptDate) is retained to bucket the summary by send
  # date/hour; the masks are summed into the n_sent / n_engaged / n_opted_in /
  # n_completed counts at the summarise() below (schema-version 6).
  masks <- .funnel_masks(data, openers, config$filters$population)
  send <- masks$send
  sent <- masks$sent
  engaged <- masks$engaged
  opted_in <- masks$opted_in
  # Completion signal is survey-mode dependent:
  #   t2w          -> the web_complete callback
  #   t2w_external -> not computable; n_completed is nulled to NA in
  #                   assemble_consolidated, so the count here is a placeholder
  #   sms          -> reaching the close (any close-family scriptDate: close /
  #                   close_sp / close_latinos), so a bilingual campaign's
  #                   Spanish completers are counted, not dropped.
  completed <- if (identical(survey_mode, "t2w")) {
    # Null-safe: detect_survey_mode only returns "t2w" when web_complete
    # exists, but build_summary_frame shouldn't assume the caller paired
    # the mode with the column -- a missing column means zero completions.
    wc_raw <- data[["web_complete"]]
    if (is.null(wc_raw)) {
      rep(FALSE, nrow(data))
    } else {
      wc <- suppressWarnings(as.integer(as.character(wc_raw)))
      !is.na(wc) & wc == 1L & sent
    }
  } else if (identical(survey_mode, "t2w_external")) {
    rep(FALSE, nrow(data))
  } else {
    .reached_close(data, config$flow$questions) & sent
  }

  campaign_id <- as.integer(data[[campaign_col]])
  # Bucket by intro.scriptDate (the send) in field timezone -- the
  # send-time cohort ("of recipients we sent to at hour H, how many replied /
  # opted-in / completed?"). A recipient with no scriptDate was never sent
  # to: sent is FALSE and every mask is gated on sent, so the row is
  # all-zero and dropped below; the NA bucket key it gets never survives the
  # keep filter. (This is why the funnel must bucket on the send, not the
  # reply: a sent-but-never-replied recipient has no batchDate to bucket on.)
  seg_date <- as.Date(format(send, tz = field_tz))
  hour_local <- as.integer(format(send, format = "%H", tz = field_tz))

  long <- data.frame(
    campaign_id = campaign_id,
    date = seg_date,
    hour_local = hour_local,
    sent = as.integer(sent),
    engaged = as.integer(engaged),
    opted_in = as.integer(opted_in),
    completed = as.integer(completed),
    stringsAsFactors = FALSE
  )
  # Drop rows where every flag is zero -- they only happened to share a
  # row with the data but contribute nothing. Avoids carrying NA-keyed
  # zero rows through group_by.
  keep <- long$sent > 0L | long$engaged > 0L |
    long$opted_in > 0L | long$completed > 0L
  long <- long[keep, , drop = FALSE]
  if (nrow(long) == 0L) return(empty_summary_frame())

  # Sum each per-recipient mask into its count column: n_<signal> is the count
  # of that signal (n_sent / n_engaged / n_opted_in / n_completed). These are the
  # public schema-version 6 columns the dashboards read.
  agg <- dplyr::summarise(
    dplyr::group_by(long, .data$campaign_id, .data$date, .data$hour_local),
    n_sent = sum(.data$sent),
    n_engaged = sum(.data$engaged),
    n_opted_in = sum(.data$opted_in),
    n_completed = sum(.data$completed),
    .groups = "drop"
  )
  data.frame(
    campaign_id = as.integer(agg$campaign_id),
    date = agg$date,
    hour_local = as.integer(agg$hour_local),
    n_sent = as.integer(agg$n_sent),
    n_engaged = as.integer(agg$n_engaged),
    n_opted_in = as.integer(agg$n_opted_in),
    n_completed = as.integer(agg$n_completed),
    stringsAsFactors = FALSE
  )
}

# Build the per-(campaign_id, date, hour_local, segment_index) ineligible
# frame. A respondent is "ineligible at segment_index k" when:
#   - id.ineligible.scriptDate is non-NA (terminal screen-out), AND
#   - the last question in config$flow$questions they reached
#     (had scriptDate on) is questions[k+1] -- i.e. they reached the
#     end of segment k (intro->q1 = segment 1, etc.).
# Returns zero-row frame with correct schema when no ineligibles exist.
build_ineligible_frame <- function(data, config) {
  if (nrow(data) == 0L) return(empty_ineligible_frame())
  campaign_col <- config$filters$campaign_id_column
  field_tz <- config$field_timezone
  questions <- config$flow$questions

  inelig_col <- "id.ineligible.scriptDate"
  if (!inelig_col %in% names(data)) return(empty_ineligible_frame())
  inelig_ts <- parse_s160_timestamps_chr(data[[inelig_col]])
  # Anchor on the OPENING question set's reply (coalesced), not a hardcoded
  # id.intro.batchDate, so a bilingual campaign's routed cohort is bucketed.
  intro_batch <- .question_timestamp(data, .opening_questions(questions), "batchDate")

  is_ineligible <- !is.na(inelig_ts) & !is.na(intro_batch)
  if (!any(is_ineligible)) return(empty_ineligible_frame())

  # Pre-parse the scriptDate columns the last-reached computation needs;
  # store on a copy so we don't mutate the caller's data. POSIXct
  # already-parsed columns pass through cleanly.
  for (q in questions) {
    col <- sprintf("id.%s.scriptDate", q)
    if (col %in% names(data) && !inherits(data[[col]], "POSIXct")) {
      data[[col]] <- parse_s160_timestamps_chr(data[[col]])
    }
  }
  last_idx <- last_reached_question_index(data, questions)

  # last_idx of 1 = only intro reached. Edge: no preceding segment, so
  # no segment_index applies; drop these from the ineligible count.
  valid <- is_ineligible & !is.na(last_idx) & last_idx >= 2L
  if (!any(valid)) return(empty_ineligible_frame())

  campaign_id <- as.integer(data[[campaign_col]])
  seg_date <- as.Date(format(intro_batch, tz = field_tz))
  hour_local <- as.integer(format(intro_batch, format = "%H", tz = field_tz))
  # segment_index ending at q_k = k - 1 (segment 1 ends at questions[2]).
  segment_index <- last_idx - 1L

  long <- data.frame(
    campaign_id = campaign_id[valid],
    date = seg_date[valid],
    hour_local = hour_local[valid],
    segment_index = as.integer(segment_index[valid]),
    stringsAsFactors = FALSE
  )
  agg <- dplyr::summarise(
    dplyr::group_by(long, .data$campaign_id, .data$date, .data$hour_local,
                    .data$segment_index),
    n_ineligible = dplyr::n(),
    .groups = "drop"
  )
  data.frame(
    campaign_id = as.integer(agg$campaign_id),
    date = agg$date,
    hour_local = as.integer(agg$hour_local),
    segment_index = as.integer(agg$segment_index),
    n_ineligible = as.integer(agg$n_ineligible),
    stringsAsFactors = FALSE
  )
}

# Collapse an hourly summary frame to day grain (hour_local = NA). The
# day rollup is the sum of the hourly counts for each (campaign_id, date)
# pair. Latency cells use the same null-hour_local convention for day
# rows; this keeps the two views consistent for the assemble step's
# left-join.
collapse_summary_to_day <- function(summary_frame) {
  if (nrow(summary_frame) == 0L) return(empty_summary_frame())
  agg <- dplyr::summarise(
    dplyr::group_by(summary_frame, .data$campaign_id, .data$date),
    n_sent = sum(.data$n_sent),
    n_engaged = sum(.data$n_engaged),
    n_opted_in = sum(.data$n_opted_in),
    n_completed = sum(.data$n_completed),
    .groups = "drop"
  )
  data.frame(
    campaign_id = as.integer(agg$campaign_id),
    date = agg$date,
    hour_local = NA_integer_,
    n_sent = as.integer(agg$n_sent),
    n_engaged = as.integer(agg$n_engaged),
    n_opted_in = as.integer(agg$n_opted_in),
    n_completed = as.integer(agg$n_completed),
    stringsAsFactors = FALSE
  )
}

collapse_ineligible_to_day <- function(ineligible_frame) {
  if (nrow(ineligible_frame) == 0L) return(empty_ineligible_frame())
  agg <- dplyr::summarise(
    dplyr::group_by(ineligible_frame, .data$campaign_id, .data$date,
                    .data$segment_index),
    n_ineligible = sum(.data$n_ineligible),
    .groups = "drop"
  )
  data.frame(
    campaign_id = as.integer(agg$campaign_id),
    date = agg$date,
    hour_local = NA_integer_,
    segment_index = as.integer(agg$segment_index),
    n_ineligible = as.integer(agg$n_ineligible),
    stringsAsFactors = FALSE
  )
}

empty_summary_frame <- function() {
  data.frame(
    campaign_id = integer(0),
    date = as.Date(character(0)),
    hour_local = integer(0),
    n_sent = integer(0),
    n_engaged = integer(0),
    n_opted_in = integer(0),
    n_completed = integer(0),
    stringsAsFactors = FALSE
  )
}

empty_ineligible_frame <- function() {
  data.frame(
    campaign_id = integer(0),
    date = as.Date(character(0)),
    hour_local = integer(0),
    segment_index = integer(0),
    n_ineligible = integer(0),
    stringsAsFactors = FALSE
  )
}
