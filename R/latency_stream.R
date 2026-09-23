# Compact (streaming) latency path -- computes the identical consolidated table
# as the full-frame path in latency_report() WITHOUT materialising the
# (n_questions - 1) x N_respondents long frame.
#
# Why: on wide, high-volume campaigns the long frame is ~99.9% NA drop-off --
# one row per respondent for every configured question, even the deep ones no
# respondent reached. Those NA rows split cleanly:
#   * real-date NA rows (segment_date_local not NA: chain_break, or an endpoint
#     missing/unparseable but the segment's batch_prior present) -- KEPT, because
#     they feed real-bucket totals and n_na_* exactly like valid rows.
#   * NA-date rows (segment_date_local NA: batch_prior blank/unparseable) -- these
#     land ONLY in the (date=NA, hour=NA) day-rollup bucket, whose output rows
#     carry only n_na_missing / n_na_parse per segment (n = 0, everything else
#     0/NA; .total_resp there is never an output column and pct_resp_hit_gt is NA
#     when n = 0). So they need not be materialised -- just counted per segment.
#
# Keeping only real-date rows bounds peak memory by real engagement, not by
# (n_questions - 1) x N; the dropped NA-date bucket is rebuilt exactly by
# .na_date_day_rows(). Parity with the full-frame path is enforced in tests.

# Empty per-(campaign, segment) NA-date count frame.
.empty_na_date <- function() {
  data.frame(campaign_id = integer(0), na_parse = integer(0),
             na_missing = integer(0), segment_index = integer(0),
             stringsAsFactors = FALSE)
}

# Streaming build_latency_frame(): same per-(respondent, segment) computation,
# segment by segment, but KEEP only real-date rows and return per-(campaign,
# segment) counts of the dropped NA-date rows by reason. Returns
# list(kept, na_date, n_clamped).
.stream_latency_frame <- function(data, config, parse_failed_mask = NULL) {
  questions <- config$flow$questions
  field_tz <- config$field_timezone
  campaign_col <- config$filters$campaign_id_column
  n <- nrow(data)
  n_seg <- length(questions) - 1L
  if (n == 0L || n_seg <= 0L) {
    return(list(kept = empty_latency_frame(), na_date = .empty_na_date(),
                n_clamped = 0L))
  }

  campaign_id <- data[[campaign_col]]
  resp_idx <- seq_len(n)
  kept <- vector("list", n_seg)
  na_list <- vector("list", n_seg)
  chain_priors <- list()
  total_clamped <- 0L
  for (i in seq_len(n_seg)) {
    q_prior <- questions[i]
    q_next <- questions[i + 1L]
    batch_prior_col <- sprintf("id.%s.batchDate", q_prior)
    script_next_col <- sprintf("id.%s.scriptDate", q_next)
    batch_prior <- data[[batch_prior_col]]
    script_next <- data[[script_next_col]]

    cs <- compute_segment_delta(batch_prior, script_next)
    delta_pre <- cs$delta
    total_clamped <- total_clamped + cs$n_clamped
    delta <- apply_chain_validity(delta_pre, chain_priors)
    chain_priors <- c(chain_priors, list(batch_prior))

    seg_date_local <- as.Date(format(batch_prior, tz = field_tz))
    hour_local <- as.integer(format(batch_prior, format = "%H", tz = field_tz))
    parse_fail_row <- segment_parse_fail_mask(parse_failed_mask,
                                              batch_prior_col, script_next_col, n)
    na_reason <- classify_na_reason(delta, delta_pre, parse_fail_row)

    real <- !is.na(seg_date_local)
    if (any(real)) {
      kept[[i]] <- data.frame(
        respondent_index = resp_idx[real],
        campaign_id = campaign_id[real],
        segment = sprintf("%s→%s", q_prior, q_next),
        segment_index = i,
        delta_min = delta[real],
        segment_date_local = seg_date_local[real],
        hour_local = hour_local[real],
        na_reason = na_reason[real],
        stringsAsFactors = FALSE
      )
    }
    # NA-date rows -> per-(campaign) counts by reason. chain_break can never be
    # NA-date (it requires a present batch_prior), so only parse/missing occur.
    nd <- !real
    if (any(nd)) {
      cid <- campaign_id[nd]
      reason <- na_reason[nd]
      ucid <- unique(cid)
      pf <- vapply(ucid, function(cc) sum(cid == cc & reason == "parse_failure"),
                   integer(1))
      me <- vapply(ucid, function(cc) sum(cid == cc & reason == "missing_endpoint"),
                   integer(1))
      na_list[[i]] <- data.frame(
        campaign_id = ucid,
        na_parse = pf,
        na_missing = me,
        segment_index = i,
        stringsAsFactors = FALSE
      )
    }
  }
  kept_frame <- data.table::setDF(data.table::rbindlist(kept))
  if (nrow(kept_frame) == 0L) kept_frame <- empty_latency_frame()
  attr(kept_frame, "n_clamped") <- total_clamped
  na_date <- data.table::setDF(data.table::rbindlist(na_list))
  if (nrow(na_date) == 0L) na_date <- .empty_na_date()
  list(kept = kept_frame, na_date = na_date, n_clamped = total_clamped)
}

# Rebuild the (date=NA, hour=NA) day-rollup rows that the dropped NA-date rows
# would have produced in the full-frame day pass. Reuses assemble_consolidated()
# so column set, types, fills and sort match exactly. Returns a zero-row
# consolidated frame when there were no NA-date rows.
.na_date_day_rows <- function(na_date, config, cfg_hash, run_at, src_csv_hash,
                              summary_day, ineligible_day, survey_mode) {
  project_id <- as.integer(config$project_id)
  if (nrow(na_date) == 0L) {
    return(empty_consolidated(project_id, cfg_hash, run_at))
  }
  questions <- config$flow$questions
  thresholds <- UNIVERSAL_THRESHOLDS_MIN
  n_seg <- length(questions) - 1L
  seg_label <- sprintf("%s→%s", questions[-length(questions)],
                       questions[-1])

  # Scaffold: (campaign, date=NA, hour=NA) x all segments x thresholds -- built
  # directly (no summary union) so only the NA-date bucket is emitted here.
  buckets <- data.frame(campaign_id = as.integer(unique(na_date$campaign_id)),
                        date = as.Date(NA), hour_local = NA_integer_,
                        stringsAsFactors = FALSE)
  segs <- data.frame(segment = seg_label, segment_index = seq_len(n_seg),
                     stringsAsFactors = FALSE)
  thr <- data.frame(threshold_min = as.integer(thresholds))
  scaffold <- merge(merge(buckets, segs, by = NULL), thr, by = NULL)

  # Cells shaped exactly like aggregate_segment_cells(): one row per (segment
  # with NA-date rows) x threshold; n=0 so distribution cols are NA, only the
  # n_na_* counts are populated. Segments with no NA-date rows are absent here
  # and filled to 0 by assemble_consolidated()'s scaffold seeding.
  cells_base <- data.frame(
    campaign_id = as.integer(na_date$campaign_id),
    date = as.Date(NA),
    hour_local = NA_integer_,
    segment = seg_label[na_date$segment_index],
    segment_index = as.integer(na_date$segment_index),
    n = 0L, n_le = 0L, n_resp_over = 0L,
    mean_delta_min = NA_real_, p50_delta_min = NA_real_,
    p90_delta_min = NA_real_, p95_delta_min = NA_real_,
    n_na_parse = as.integer(na_date$na_parse),
    n_na_missing = as.integer(na_date$na_missing),
    n_na_chain = 0L,
    stringsAsFactors = FALSE
  )
  cells <- do.call(rbind, lapply(thresholds, function(t) {
    x <- cells_base
    x$threshold_min <- as.integer(t)
    x$pct_le <- NA_real_
    x
  }))

  # totals empty -> .total_resp joins as NA (irrelevant: pct_resp_hit_gt is NA
  # when n = 0). cascade empty -> n_respondents / pct_resp_worst_gt join as NA.
  totals <- data.frame(campaign_id = integer(0), date = as.Date(character(0)),
                       hour_local = integer(0), .total_resp = integer(0),
                       stringsAsFactors = FALSE, check.names = FALSE)
  cascade <- data.frame(campaign_id = integer(0), date = as.Date(character(0)),
                        hour_local = integer(0), threshold_min = integer(0),
                        n_respondents = integer(0),
                        pct_resp_worst_gt = numeric(0), stringsAsFactors = FALSE)

  assemble_consolidated(scaffold, cells, totals, cascade,
                        project_id = project_id, cfg_hash = cfg_hash,
                        run_at = run_at, src_csv_hash = src_csv_hash,
                        summary_frame = summary_day,
                        ineligible_frame = ineligible_day,
                        survey_mode = survey_mode)
}

# Diagnostics for the compact path -- byte-identical to build_diagnostics() run
# on the full frame, computed from the kept (real-date) frame plus the per-
# segment NA-date counts. `n_frame` is nrow of the (post-filter) data the frame
# was built from (= distinct respondents in the full frame); `n_in` is the
# post-population count carried through as n_respondents_in.
.build_diagnostics_streamed <- function(kept, na_date, n_in, n_frame, n_seg,
                                        parse_failures, config_hash) {
  n_clamped <- attr(kept, "n_clamped") %||% 0L
  # Full frame is empty iff no segments or no respondents.
  if (n_seg <= 0L || n_frame == 0L) {
    return(list(
      n_respondents_in = n_in,
      n_respondents_used = 0L,
      n_respondents_no_valid_segment = n_in,
      n_segments_total = 0L,
      n_segments_na = 0L,
      n_segments_na_by_reason = list(parse_failure = 0L,
                                     missing_endpoint = 0L,
                                     chain_break = 0L),
      n_negative_latencies_clamped = n_clamped,
      parse_failures_per_column = parse_failures,
      config_hash = config_hash,
      algorithm_version = .algorithm_version,
      respondent_summary = list(n_respondents = 0L,
                                pct_clean_at_5min = NA_real_,
                                pct_worst_in_5_to_10 = NA_real_,
                                pct_worst_over_10 = NA_real_)
    ))
  }
  valid <- !is.na(kept$delta_min)
  used <- length(unique(kept$respondent_index[valid]))
  n_valid <- sum(valid)
  total_segments <- n_seg * n_frame
  na_segments <- total_segments - n_valid
  by_reason <- list(
    parse_failure = sum(na_date$na_parse) +
      sum(kept$na_reason == "parse_failure", na.rm = TRUE),
    missing_endpoint = sum(na_date$na_missing) +
      sum(kept$na_reason == "missing_endpoint", na.rm = TRUE),
    chain_break = sum(kept$na_reason == "chain_break", na.rm = TRUE)
  )
  if (used > 0L) {
    r <- kept$respondent_index[valid]
    d <- kept$delta_min[valid]
    worst <- vapply(split(d, r), max, numeric(1))
    pct_clean <- 100 * mean(worst <= 5)
    pct_5_10 <- 100 * mean(worst > 5 & worst <= 10)
    pct_over_10 <- 100 * mean(worst > 10)
  } else {
    pct_clean <- NA_real_
    pct_5_10 <- NA_real_
    pct_over_10 <- NA_real_
  }
  list(
    n_respondents_in = n_in,
    n_respondents_used = used,
    n_respondents_no_valid_segment = n_frame - used,
    n_segments_total = total_segments,
    n_segments_na = na_segments,
    n_segments_na_by_reason = by_reason,
    n_negative_latencies_clamped = n_clamped,
    parse_failures_per_column = parse_failures,
    config_hash = config_hash,
    algorithm_version = .algorithm_version,
    respondent_summary = list(
      n_respondents = used,
      pct_clean_at_5min = pct_clean,
      pct_worst_in_5_to_10 = pct_5_10,
      pct_worst_over_10 = pct_over_10
    )
  )
}
