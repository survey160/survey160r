# Summary metrics aggregation (spec §4). Computed on the pre-filter data
# frame so the texted/consented/completed denominators reflect the full
# campaign population, not just the funnel survivors.
#
# Orchestrator (called by campaign_report() before the population filter):
#   build_summary_frame(data, config)     -> per (campaign, date, hour) counts
#   build_ineligible_frame(data, config)  -> per (campaign, date, hour, segment_index) counts
#   collapse_summary_to_day(frame)        -> hourly -> day rollup (hour_local = NA)
#
# These return small data frames that aggregate_consolidated() left-joins
# onto the latency cells. Denormalising into the same parquet keeps the
# Shiny consumer on one read; the doc decided in favour of that over a
# sidecar parquet (campaign_scripts.md §4.3, decision: one parquet).

# Detect a campaign's survey mode from the source data. Text-to-Web ("t2w")
# when any web_complete == 1 is present; otherwise "sms". The authoritative
# campaign-level flag (campaigns.use_web_completes in the v2 DB) is not carried
# in the CSV export, so this presence heuristic is the best signal from the
# data alone. Documented limitation: a Text-to-Web campaign with zero web
# completions classifies as "sms" (and completes on close.scriptDate).
detect_survey_mode <- function(data) {
  wc <- data[["web_complete"]]
  if (is.null(wc)) return("sms")
  wc_int <- suppressWarnings(as.integer(as.character(wc)))
  if (any(!is.na(wc_int) & wc_int == 1L)) "t2w" else "sms"
}

# Build the per-(campaign_id, date, hour_local) summary frame at hourly
# grain. Output columns: campaign_id, date (Date), hour_local (int 0..23),
# n_texted, n_consented, n_completed (all int32-safe). Returns a zero-row
# frame with the correct schema if `data` has no respondents -- callers
# rbind multiple of these for day rollups without special-casing empties.
#
# `survey_mode` selects the completion signal: "sms" (default) completes on
# id.close.scriptDate (reaching the close question); "t2w" completes on the
# web_complete callback column (the SMS close is just the link, sent to every
# consenter, so close.scriptDate would overstate completion -- see SUR-1368).
build_summary_frame <- function(data, config, survey_mode = "sms") {
  if (nrow(data) == 0L) return(empty_summary_frame())
  campaign_col <- config$filters$campaign_id_column
  field_tz <- config$field_timezone

  # Parse the timestamps summary needs. Idempotent on POSIXct (the main
  # orchestrator may parse them again later); cheap on raw character.
  intro_batch <- parse_s160_timestamps_chr(data[["id.intro.batchDate"]])
  close_script_col <- "id.close.scriptDate"
  close_script <- if (close_script_col %in% names(data)) {
    parse_s160_timestamps_chr(data[[close_script_col]])
  } else {
    rep(as.POSIXct(NA), nrow(data))
  }

  texted <- !is.na(intro_batch)
  # n_consented uses the configured population filter, not a hardcoded
  # finalValue == 1 check. Different platform variants emit consent in
  # different fields ("Yes"/"No" labels vs integer codes); the filter is
  # already the project-owned expression for "who's in the funnel" so we
  # reuse it as the canonical consent definition.
  consented <- population_filter_mask(data, config$filters$population)
  consented <- consented & texted
  # Completion signal is survey-mode dependent (SUR-1368): t2w campaigns
  # complete via the web_complete callback, sms via reaching close.
  completed <- if (identical(survey_mode, "t2w")) {
    # Null-safe: detect_survey_mode only returns "t2w" when web_complete
    # exists, but build_summary_frame shouldn't assume the caller paired
    # the mode with the column -- a missing column means zero completions.
    wc_raw <- data[["web_complete"]]
    if (is.null(wc_raw)) {
      rep(FALSE, nrow(data))
    } else {
      wc <- suppressWarnings(as.integer(as.character(wc_raw)))
      !is.na(wc) & wc == 1L & texted
    }
  } else {
    !is.na(close_script) & texted
  }

  campaign_id <- as.integer(data[[campaign_col]])
  # Bucket by intro.batchDate in field timezone. Rows with no batchDate
  # contribute to none of the three counts (handled by the masks above)
  # but still need a non-NA bucket key to keep group_by stable -- assign
  # them date=NA / hour=NA, then drop the all-FALSE rows after summing.
  seg_date <- as.Date(format(intro_batch, tz = field_tz))
  hour_local <- as.integer(format(intro_batch, format = "%H", tz = field_tz))

  long <- data.frame(
    campaign_id = campaign_id,
    date = seg_date,
    hour_local = hour_local,
    texted = as.integer(texted),
    consented = as.integer(consented),
    completed = as.integer(completed),
    stringsAsFactors = FALSE
  )
  # Drop rows where every flag is zero -- they only happened to share a
  # row with the data but contribute nothing. Avoids carrying NA-keyed
  # zero rows through group_by.
  keep <- long$texted > 0L | long$consented > 0L | long$completed > 0L
  long <- long[keep, , drop = FALSE]
  if (nrow(long) == 0L) return(empty_summary_frame())

  agg <- dplyr::summarise(
    dplyr::group_by(long, .data$campaign_id, .data$date, .data$hour_local),
    n_texted = sum(.data$texted),
    n_consented = sum(.data$consented),
    n_completed = sum(.data$completed),
    .groups = "drop"
  )
  data.frame(
    campaign_id = as.integer(agg$campaign_id),
    date = agg$date,
    hour_local = as.integer(agg$hour_local),
    n_texted = as.integer(agg$n_texted),
    n_consented = as.integer(agg$n_consented),
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
  intro_batch <- parse_s160_timestamps_chr(data[["id.intro.batchDate"]])

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
    n_texted = sum(.data$n_texted),
    n_consented = sum(.data$n_consented),
    n_completed = sum(.data$n_completed),
    .groups = "drop"
  )
  data.frame(
    campaign_id = as.integer(agg$campaign_id),
    date = agg$date,
    hour_local = NA_integer_,
    n_texted = as.integer(agg$n_texted),
    n_consented = as.integer(agg$n_consented),
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
    n_texted = integer(0),
    n_consented = integer(0),
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
