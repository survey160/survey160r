# Per-respondent x per-segment frame construction (spec §2.2).
# Pure functions, no I/O. Inputs are filtered+parsed data plus the config;
# output is the long latency_frame consumed by aggregate_consolidated() and
# build_diagnostics().

# Build the long (respondent x segment) data.frame: one row per
# (respondent_index, segment) with delta, segment_date_local, hour_local,
# campaign_id, and na_reason (NA when delta_min is valid; otherwise
# "parse_failure" | "missing_endpoint" | "chain_break").
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
build_latency_frame <- function(data, config, parse_failed_mask = NULL) {
  questions <- config$flow$questions
  field_tz <- config$field_timezone
  campaign_col <- config$filters$campaign_id_column
  n <- nrow(data)
  if (n == 0) {
    return(empty_latency_frame())
  }

  campaign_id <- data[[campaign_col]]
  resp_idx <- seq_len(n)
  n_seg <- length(questions) - 1L
  n_out <- n * n_seg

  # The long frame is segment-major: segment 1's n rows, then segment 2's, ...
  # Each column is allocated once at its final length and filled slice by
  # slice, so the frame is never held twice (a per-segment list of sub-frames
  # plus the bound result peaked at 2x the frame). The segment-constant
  # columns (campaign_id, segment label, segment_index) are built by
  # repetition below rather than filled per segment.
  delta_out <- numeric(n_out)
  date_out <- numeric(n_out)
  hour_out <- integer(n_out)
  reason_out <- character(n_out)
  prior_na <- NULL
  total_clamped <- 0L
  for (i in seq_len(n_seg)) {
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
    # classification below. `prior_na` is the running OR of the prior
    # batchDates' NA masks (the incremental form of apply_chain_validity()).
    delta <- delta_pre
    if (!is.null(prior_na)) delta[prior_na] <- NA_real_
    prior_na <- .chain_break_mask(prior_na, batch_prior)

    local <- .local_date_hour(batch_prior, field_tz)

    parse_fail_row <- segment_parse_fail_mask(
      parse_failed_mask, batch_prior_col, script_next_col, n
    )

    slice <- (i - 1L) * n + resp_idx
    delta_out[slice] <- delta
    date_out[slice] <- unclass(local$date)
    hour_out[slice] <- local$hour
    reason_out[slice] <- classify_na_reason(delta, delta_pre, parse_fail_row)
  }
  segment_labels <- sprintf("%s\u2192%s", questions[-length(questions)],
                            questions[-1L])
  frame <- data.frame(
    respondent_index = rep.int(resp_idx, n_seg),
    campaign_id = rep.int(campaign_id, n_seg),
    segment = rep(segment_labels, each = n),
    segment_index = rep(seq_len(n_seg), each = n),
    delta_min = delta_out,
    segment_date_local = .Date(date_out),
    hour_local = hour_out,
    na_reason = reason_out,
    stringsAsFactors = FALSE
  )
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
  out <- rep(NA_character_, length(delta))
  # Work on the NA rows' indices only: the three class masks are then
  # evaluated over that subset rather than over the whole column three times.
  na_idx <- which(is.na(delta))
  if (length(na_idx) == 0L) return(out)
  parse_fail <- parse_fail_row[na_idx]
  pre_na <- is.na(delta_pre[na_idx])
  out[na_idx[parse_fail]] <- "parse_failure"
  out[na_idx[!parse_fail & pre_na]] <- "missing_endpoint"
  out[na_idx[!parse_fail & !pre_na]] <- "chain_break"
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
    segment_date_local = as.Date(character(0)),
    hour_local = integer(0),
    na_reason = character(0),
    stringsAsFactors = FALSE
  )
  attr(out, "n_clamped") <- 0L
  out
}
