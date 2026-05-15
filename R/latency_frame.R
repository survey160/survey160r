# Per-respondent x per-segment frame construction (spec §2.2).
# Pure functions, no I/O. Inputs are filtered+parsed data plus the config
# and texting-window frame; output is the long latency_frame consumed by
# aggregate_consolidated() and build_diagnostics().

# Build the long (respondent x segment) data.frame: one row per
# (respondent_index, segment) with delta, in_window flag, segment_date_local,
# hour_local, campaign_id, and na_reason (NA when delta_min is valid;
# otherwise "parse_failure" | "missing_endpoint" | "chain_break").
#
# Classification precedence (most actionable first):
#   parse_failure   -- at least one endpoint cell was non-blank but the
#                      timestamp string was unparseable. Data quality issue.
#   missing_endpoint-- at least one endpoint cell was blank (legitimately
#                      absent), no parse failures on this segment's endpoints.
#                      Reflects respondent drop-off mid-flow.
#   chain_break     -- both endpoints parsed cleanly, but a prior batchDate
#                      in the chain was NA so apply_chain_validity invalidated
#                      this segment.
build_latency_frame <- function(data, config, windows_df,
                                parse_failed_mask = NULL) {
  questions <- config$flow$questions
  field_tz <- config$field_timezone
  campaign_col <- config$filters$campaign_id_column
  n <- nrow(data)
  if (n == 0) {
    return(empty_latency_frame())
  }

  campaign_id <- data[[campaign_col]]
  resp_idx <- seq_len(n)

  segments <- vector("list", length(questions) - 1)
  chain_priors <- list()
  total_clamped <- 0L
  for (i in seq_len(length(questions) - 1)) {
    q_prior <- questions[i]
    q_next <- questions[i + 1]
    batch_prior_col <- sprintf("id.%s.batchDate", q_prior)
    script_next_col <- sprintf("id.%s.scriptDate", q_next)
    batch_prior <- data[[batch_prior_col]]
    script_next <- data[[script_next_col]]

    cs <- compute_segment_delta(batch_prior, script_next)
    delta_pre <- cs$delta
    total_clamped <- total_clamped + cs$n_clamped

    # Apply chain validity using only *strictly prior* batchDates -- the
    # current segment's own batch_prior NA is already reflected in delta_pre
    # by compute_segment_delta(), so including it here would be redundant
    # work and would muddy the chain_break vs missing_endpoint diagnostic
    # classification below.
    delta <- apply_chain_validity(delta_pre, chain_priors)
    chain_priors <- c(chain_priors, list(batch_prior))

    in_window <- in_window_flag(batch_prior, windows_df, field_tz)
    in_window[is.na(batch_prior)] <- 0L

    seg_date_local <- as.Date(format(batch_prior, tz = field_tz))
    hour_local <- as.integer(format(batch_prior, format = "%H", tz = field_tz))

    parse_fail_row <- segment_parse_fail_mask(
      parse_failed_mask, batch_prior_col, script_next_col, n
    )
    na_reason <- classify_na_reason(delta, delta_pre, parse_fail_row)

    segments[[i]] <- data.frame(
      respondent_index = resp_idx,
      campaign_id = campaign_id,
      segment = sprintf("%s\u2192%s", q_prior, q_next),
      segment_index = i,
      delta_min = delta,
      in_window = in_window,
      segment_date_local = seg_date_local,
      hour_local = hour_local,
      na_reason = na_reason,
      stringsAsFactors = FALSE
    )
  }
  frame <- do.call(rbind, segments)
  attr(frame, "n_clamped") <- total_clamped
  frame
}

# Classify why a segment's Δ is NA. Precedence (most actionable first):
#   parse_failure   -- an endpoint string was non-blank but unparseable.
#   missing_endpoint-- an endpoint was blank/NA before chain validity ran
#                      (i.e. delta_pre is already NA from compute_segment_delta).
#   chain_break     -- this segment's own endpoints parsed cleanly, but a
#                      strictly-prior batchDate was NA, so apply_chain_validity
#                      invalidated the segment.
# Returns NA_character_ on rows where delta is valid.
classify_na_reason <- function(delta, delta_pre, parse_fail_row) {
  is_na_post <- is.na(delta)
  out <- rep(NA_character_, length(delta))
  out[is_na_post & parse_fail_row] <- "parse_failure"
  out[is_na_post & !parse_fail_row & is.na(delta_pre)] <- "missing_endpoint"
  out[is_na_post & !parse_fail_row & !is.na(delta_pre)] <- "chain_break"
  out
}

# OR-combine the parse-fail masks for a segment's two endpoint columns.
# Returns a length-n logical. Tolerant of a NULL mask (test code paths that
# call build_latency_frame directly) -- treats absence as "no parse failures."
segment_parse_fail_mask <- function(parse_failed_mask, batch_col,
                                    script_col, n) {
  if (is.null(parse_failed_mask)) return(rep(FALSE, n))
  bp <- parse_failed_mask[[batch_col]]
  sn <- parse_failed_mask[[script_col]]
  if (is.null(bp)) bp <- rep(FALSE, n)
  if (is.null(sn)) sn <- rep(FALSE, n)
  bp | sn
}

empty_latency_frame <- function() {
  out <- data.frame(
    respondent_index = integer(0),
    campaign_id = integer(0),
    segment = character(0),
    segment_index = integer(0),
    delta_min = numeric(0),
    in_window = integer(0),
    segment_date_local = as.Date(character(0)),
    hour_local = integer(0),
    na_reason = character(0),
    stringsAsFactors = FALSE
  )
  attr(out, "n_clamped") <- 0L
  out
}
