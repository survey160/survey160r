# Aggregation of the per-respondent x per-segment frame to the consolidated
# Parquet table. Cells are always at the hour grain:
# (campaign_id, date, hour_local, segment, threshold_min). Downstream
# consumers (and latency_report()'s day-rollup pass) get day-grain rows by
# nulling hour_local on the input frame before calling.
#
# Orchestrator: aggregate_consolidated()
# Pieces:
#   aggregate_totals()         -- per-bucket distinct respondent count
#   aggregate_worst_cascade()  -- per-threshold respondent worst-Δ cascade
#   aggregate_segment_cells()  -- per-(bucket, segment, threshold) cell rows
#   assemble_consolidated()    -- joins, schema-shaped data.frame, sort

# na_reason enum, in the order of the n_na_* cell columns (parse, missing,
# chain). aggregate_consolidated() codes the column to this index.
.na_reason_levels <- c("parse_failure", "missing_endpoint", "chain_break")

# Grouping keys used by every aggregation in this file. Kept as a single
# vector so a future column addition (e.g. operator_id) only edits one place.
.bucket_keys <- c("campaign_id", "date", "hour_local")

# 100 * num / denom, or NA when denom is missing or non-positive. The pattern
# shows up everywhere a "percent of X" column is built; centralised so the
# zero-denominator NA convention can't drift between cells, cascade, and
# diagnostics.
safe_pct <- function(num, denom) {
  ifelse(!is.na(denom) & denom > 0, 100 * num / denom, NA_real_)
}

aggregate_consolidated <- function(frame, config, cfg_hash, run_at,
                                   src_csv_hash = NA_character_,
                                   summary_frame = NULL,
                                   ineligible_frame = NULL,
                                   refusal_frame = NULL,
                                   survey_mode = "sms") {
  project_id <- as.integer(config$project_id)
  if (is.null(summary_frame)) summary_frame <- empty_summary_frame()
  if (is.null(ineligible_frame)) ineligible_frame <- empty_ineligible_frame()
  if (is.null(refusal_frame)) refusal_frame <- empty_refusal_frame()
  if (nrow(frame) == 0L && nrow(summary_frame) == 0L) {
    return(empty_consolidated(project_id, cfg_hash, run_at))
  }

  thresholds <- UNIVERSAL_THRESHOLDS_MIN
  bucketed <- frame
  # Always materialise `date` so downstream group_by()s don't trip on a
  # missing column when the latency frame is empty (summary-only path).
  bucketed$date <- if (nrow(bucketed) > 0L) bucketed$segment_date_local else
    as.Date(character(0))

  # The per-bucket aggregations run on data.table. On the (n_questions-1) x N
  # long frame the dplyr group_by()s did not scale -- a ~47M-row frame took
  # >15 min and >11 GB of RSS; data.table does the identical work in seconds at
  # a fraction of the peak memory. Convert once here; build_consolidated_scaffold
  # and assemble_consolidated below keep operating on the data.frame `bucketed`
  # and the small joined frames.
  # setDT() converts the local shell in place instead of as.data.table()'s deep
  # copy of every column: `bucketed` is already a fresh list shell (the `$date`
  # assignment above), so the caller's `frame` is untouched, and every
  # data.table call below is read-only.
  bucketed_data_table <- data.table::setDT(bucketed)
  # Integer code for na_reason (1 parse_failure, 2 missing_endpoint, 3
  # chain_break, NA when the delta is valid) so the per-cell counts are one
  # tabulate() over the group instead of three string comparisons. Added to
  # the local shell only; `frame` is untouched.
  data.table::set(bucketed_data_table, j = "na_code",
                  value = match(bucketed_data_table[["na_reason"]],
                                .na_reason_levels))
  totals <- aggregate_totals(bucketed_data_table)
  cascade <- aggregate_worst_cascade(bucketed_data_table, thresholds)
  cells <- aggregate_segment_cells(bucketed_data_table, thresholds)

  # Scaffold: union of bucket keys from latency frame and summary frame.
  # Without this, hours where every respondent was filtered out (e.g.
  # 100 sent, 0 opted_in) lose their n_sent denominator because
  # the latency frame has no rows for those buckets. CodeRabbit
  # (PR #26) flagged the original cells-only seeding as defeating the
  # pre-filter summary contract. Scaffolding from the union preserves
  # the denominator while still emitting one row per
  # (bucket, segment, threshold) for query uniformity.
  scaffold <- build_consolidated_scaffold(bucketed_data_table, summary_frame,
                                          config, thresholds)

  assemble_consolidated(scaffold, cells, totals, cascade,
                        project_id = project_id,
                        cfg_hash = cfg_hash,
                        run_at = run_at,
                        src_csv_hash = src_csv_hash,
                        summary_frame = summary_frame,
                        ineligible_frame = ineligible_frame,
                        refusal_frame = refusal_frame,
                        survey_mode = survey_mode)
}

# Build the (bucket × segment × threshold) scaffold the assemble step
# left-joins everything onto. Buckets are the union of those present in
# the latency frame and the summary frame; segments come from the
# configured question flow; thresholds are universal. The scaffold
# guarantees that summary-only buckets (texted but no consents) appear
# in the output even though they have no latency rows.
build_consolidated_scaffold <- function(bucketed, summary_frame, config,
                                        thresholds) {
  latency_buckets <- if (nrow(bucketed) > 0L) {
    # `bucketed` is the data.table built by aggregate_consolidated(). unique()
    # on the key columns runs in data.table's radix path; unique.data.frame on
    # the full long frame pasted every row to a string (it was ~30% of
    # latency_report() wall time and a large transient allocation).
    keys <- if (data.table::is.data.table(bucketed)) {
      bucketed[, .bucket_keys, with = FALSE]
    } else {
      data.table::as.data.table(bucketed[, .bucket_keys])
    }
    data.table::setDF(unique(keys))
  } else {
    data.frame(campaign_id = integer(0),
               date = as.Date(character(0)),
               hour_local = integer(0),
               stringsAsFactors = FALSE)
  }
  summary_buckets <- summary_frame[, c("campaign_id", "date", "hour_local"),
                                   drop = FALSE]
  all_buckets <- unique(rbind(latency_buckets, summary_buckets))
  # Force types so merge() in the cross-join below treats bucket keys
  # identically across the two source frames (latency hour_local can
  # arrive as numeric from build_latency_frame).
  all_buckets$campaign_id <- as.integer(all_buckets$campaign_id)
  all_buckets$hour_local <- as.integer(all_buckets$hour_local)

  questions <- config$flow$questions
  segments_df <- data.frame(
    segment = sprintf("%s\u2192%s", questions[-length(questions)],
                      questions[-1]),
    segment_index = seq_len(length(questions) - 1L),
    stringsAsFactors = FALSE
  )
  thresholds_df <- data.frame(
    threshold_min = as.integer(thresholds),
    stringsAsFactors = FALSE
  )
  # merge(by = NULL) is the cross join. Three-way cross produces
  # buckets × segments × thresholds with no duplicates.
  merge(merge(all_buckets, segments_df, by = NULL),
        thresholds_df, by = NULL)
}

# Total respondents per bucket -- the denominator for pct_resp_hit_gt.
# Defined as distinct respondent_index appearing in *any* segment for the
# bucket key (matches "all in-wave respondents" per spec).
aggregate_totals <- function(bucketed) {
  # Bare column names below are data.table (j/by) references, not free
  # variables; NULL-bind them so R CMD check / lintr do not flag them.
  campaign_id <- date <- hour_local <- respondent_index <- NULL
  as.data.frame(bucketed[
    , list(.total_resp = data.table::uniqueN(respondent_index)),
    by = list(campaign_id, date, hour_local)
  ])
}

# Per-bucket respondent latency cascade: for each threshold, how many
# respondents had at least one valid Δ in the bucket and what fraction of
# those had a *worst* Δ exceeding the threshold. This is the wave-level
# view the historical reports show.
aggregate_worst_cascade <- function(bucketed, thresholds) {
  campaign_id <- date <- hour_local <- respondent_index <- delta_min <-
    worst_delta <- NULL
  # Per-respondent worst delta within each bucket. Filtering !is.na(delta_min)
  # in `i` means max() sees only valid deltas, so it is finite for every
  # (non-empty) group -- no na.rm and no -Inf warning. The bare max() is
  # GForce-optimised (computed in C across all groups at once); wrapping it,
  # e.g. in suppressWarnings(), would force a per-group R evaluation over the
  # ~(buckets x respondents) groups -- ~20x slower on a campaign-sized frame.
  # With no valid delta at all the grouped form would evaluate j once on the
  # empty table to probe its type -- max(numeric(0)) warns -- so that case
  # builds the typed empty result directly. The is.finite() filter keeps the
  # empty-group edge belt-and-suspenders.
  valid <- !is.na(bucketed[["delta_min"]])
  worst <- if (any(valid)) {
    bucketed[
      valid,
      list(worst_delta = max(delta_min)),
      by = list(campaign_id, date, hour_local, respondent_index)
    ]
  } else {
    empty <- bucketed[0L, c(.bucket_keys, "respondent_index"), with = FALSE]
    data.table::set(empty, j = "worst_delta", value = numeric(0))
  }
  worst <- worst[is.finite(worst_delta)]

  chunks <- lapply(thresholds, function(t) cascade_chunk(worst, t))
  do.call(rbind, chunks)
}

# Single-threshold cascade row builder. Extracted so aggregate_worst_cascade
# is purely the lapply skeleton + rbind.
cascade_chunk <- function(worst, t) {
  campaign_id <- date <- hour_local <- worst_delta <- NULL
  chunk <- worst[
    , list(n_respondents = length(worst_delta),
           n_worst_over = sum(worst_delta > t)),
    by = list(campaign_id, date, hour_local)
  ]
  # set() rather than `:=` so the data.table assign stays namespace-qualified
  # (no "no visible global function ':='" note under R CMD check).
  data.table::set(chunk, j = "threshold_min", value = as.integer(t))
  data.table::set(chunk, j = "pct_resp_worst_gt",
                  value = safe_pct(chunk$n_worst_over, chunk$n_respondents))
  as.data.frame(chunk[, c(.bucket_keys, "threshold_min", "n_respondents",
                          "pct_resp_worst_gt"), with = FALSE])
}

# Per-(bucket, segment, threshold) cell rows. n is the valid-Δ count for the
# cell; pct_le and n_resp_over are derived from the same set.
aggregate_segment_cells <- function(bucketed, thresholds) {
  # mean_delta_min and the p50/p90/p95 quantiles are threshold-independent,
  # so the values emitted here are identical across the four threshold rows
  # of the same (campaign_id, date, hour_local, segment) cell. Kept inline
  # rather than split into a sidecar table so the Shiny query model stays
  # a single wide consolidated.
  #
  # n_na_parse / n_na_missing / n_na_chain count rows in this group with
  # the matching value of `na_reason` set by classify_na_reason() in
  # R/latency_frame.R. The enum strings stay longer ("parse_failure" etc.)
  # for debugging the latency_frame; cell-column names use the n_na_*
  # prefix family so they group together in column listings and tooltips.
  #
  # ONE grouped pass computes the threshold-independent stats once and the two
  # threshold-dependent counts (n_le, n_resp_over) for every threshold as wide
  # columns; segment_cells_long() then fans those out to the per-threshold
  # rows. The previous one-pass-per-threshold form grouped the long frame four
  # times and recomputed the quantiles each time.
  segment <- segment_index <- delta_min <- respondent_index <- na_code <-
    NULL
  thresholds <- as.integer(thresholds)
  wide <- bucketed[, {
    valid <- !is.na(delta_min)
    vals <- delta_min[valid]
    resp <- respondent_index[valid]
    n_valid <- length(vals)
    q <- if (n_valid > 0L) {
      stats::quantile(vals, probs = c(0.50, 0.90, 0.95), names = FALSE)
    } else {
      c(NA_real_, NA_real_, NA_real_)
    }
    na_counts <- tabulate(na_code, nbins = 3L)
    out <- list(
      n = n_valid,
      mean_delta_min = if (n_valid > 0L) mean(vals) else NA_real_,
      p50_delta_min = q[[1L]],
      p90_delta_min = q[[2L]],
      p95_delta_min = q[[3L]],
      n_na_parse = na_counts[[1L]],
      n_na_missing = na_counts[[2L]],
      n_na_chain = na_counts[[3L]]
    )
    for (k in seq_along(thresholds)) {
      over <- vals > thresholds[[k]]
      out[[sprintf(".n_le_%d", k)]] <- n_valid - sum(over)
      out[[sprintf(".n_resp_over_%d", k)]] <- length(unique(resp[over]))
    }
    out
  }, by = c(.bucket_keys, "segment", "segment_index")]
  segment_cells_long(wide, thresholds)
}

# Fan the wide per-cell table out to one row per (cell, threshold): the shared
# columns repeat, the threshold-specific pair is picked from the `.n_le_<k>` /
# `.n_resp_over_<k>` columns, and pct_le is derived.
segment_cells_long <- function(wide, thresholds) {
  shared <- c(.bucket_keys, "segment", "segment_index", "n",
              "mean_delta_min", "p50_delta_min", "p90_delta_min",
              "p95_delta_min", "n_na_parse", "n_na_missing", "n_na_chain")
  base <- data.table::setDF(wide[, shared, with = FALSE])
  chunks <- lapply(seq_along(thresholds), function(k) {
    cells <- base
    cells$n_le <- wide[[sprintf(".n_le_%d", k)]]
    cells$n_resp_over <- wide[[sprintf(".n_resp_over_%d", k)]]
    cells$threshold_min <- rep(thresholds[[k]], nrow(cells))
    cells$pct_le <- safe_pct(cells$n_le, cells$n)
    cells
  })
  do.call(rbind, chunks)
}

# Left-join every aggregation onto the (bucket × segment × threshold)
# scaffold, stamp provenance, coerce to the final schema order, and
# sort. Scaffold-first seeding ensures summary-only buckets (texted but
# zero consents) appear in the output even though they have no latency
# rows. Returns the data.frame written to Parquet.
assemble_consolidated <- function(scaffold, cells, totals, cascade,
                                  project_id, cfg_hash, run_at,
                                  src_csv_hash,
                                  summary_frame, ineligible_frame,
                                  refusal_frame,
                                  survey_mode = "sms") {
  # Latency cell stats. NA on scaffold rows whose bucket has no latency
  # frame entries -- the new summary-only path.
  joined <- dplyr::left_join(scaffold, cells,
                             by = c(.bucket_keys, "segment",
                                    "segment_index", "threshold_min"))
  joined <- dplyr::left_join(joined, totals, by = .bucket_keys)
  # pct_resp_hit_gt is gated on `n > 0` (the cell has at least one valid
  # Δ). safe_pct() handles the denominator side; the explicit `n > 0`
  # mask layered on top covers scaffold rows that came in with NA `n`
  # (summary-only buckets) -- those become NA pct_resp_hit_gt, not 0.
  joined$pct_resp_hit_gt <- ifelse(
    !is.na(joined$n) & joined$n > 0,
    safe_pct(joined$n_resp_over, joined$.total_resp),
    NA_real_
  )
  # Cascade left-join can produce NA n_respondents / pct_resp_worst_gt for
  # a bucket where *every* segment was NA (parse failure, chain break, or
  # missing endpoint) so the worst-by-respondent step has no row for it.
  # The Arrow schema permits int32 NA so this round-trips cleanly;
  # downstream consumers should treat NA cascade columns as "no respondent
  # had any valid Δ in this bucket."
  joined <- dplyr::left_join(joined, cascade,
                             by = c(.bucket_keys, "threshold_min"))

  # Summary metrics join. n_sent/n_engaged/n_opted_in/n_completed
  # denormalise across every (segment, threshold) row sharing the bucket key
  # (one value per bucket repeats across N-1 segments × 4 thresholds).
  joined <- dplyr::left_join(joined, summary_frame, by = .bucket_keys)
  # Ineligible join is per (bucket, segment_index). n_ineligible
  # denormalises across the 4 threshold rows of the same
  # (bucket, segment_index): one value per (bucket, segment) repeats
  # across the threshold ladder. Consumers SUM-ming across threshold
  # rows will quadruple-count. To get the correct total:
  #   - Per (campaign, date, hour, segment): filter to ONE threshold_min
  #     (any value works since the row is the same across thresholds).
  #   - Per (campaign, date, segment) day rollup: filter
  #     `hour_local IS NULL AND threshold_min = 1` (or any one threshold).
  # The MAX-across-thresholds aggregate also works but composes less
  # cleanly with the day/hour grain split.
  joined <- dplyr::left_join(joined, ineligible_frame,
                             by = c(.bucket_keys, "segment_index"))
  # Refusal join is per (bucket, segment_index), exactly as ineligible above:
  # n_refused denormalises across the 4 threshold rows of the same
  # (bucket, segment_index). The same de-duplication caveat applies -- filter to
  # ONE threshold_min before summing across segments to avoid quadruple-counting.
  joined <- dplyr::left_join(joined, refusal_frame,
                             by = c(.bucket_keys, "segment_index"))

  # Scaffold-only rows (bucket × segment × threshold combinations with
  # no matching latency cell or summary row) get 0 for every COUNT
  # column and NA for distribution columns (mean / quantile over zero
  # observations is genuinely undefined). All four summary counts plus
  # ineligible / refused are filled symmetrically: a bucket with no summary-frame
  # row means "no respondents in this bucket" -> 0, not "unknown".
  count_cols <- c("n", "n_le", "n_resp_over",
                  "n_na_parse", "n_na_missing", "n_na_chain",
                  "n_sent", "n_engaged", "n_opted_in", "n_completed",
                  "n_ineligible", "n_refused")
  for (col in count_cols) {
    if (col %in% names(joined)) {
      joined[[col]][is.na(joined[[col]])] <- 0L
    }
  }

  out <- data.frame(
    campaign_id = as.integer(joined$campaign_id),
    project_id = rep(project_id, nrow(joined)),
    survey_mode = rep(survey_mode, nrow(joined)),
    date = joined$date,
    hour_local = joined$hour_local,
    segment = joined$segment,
    segment_index = as.integer(joined$segment_index),
    threshold_min = as.integer(joined$threshold_min),
    n = as.integer(joined$n),
    pct_le = as.numeric(joined$pct_le),
    pct_resp_hit_gt = joined$pct_resp_hit_gt,
    n_respondents = as.integer(joined$n_respondents),
    pct_resp_worst_gt = as.numeric(joined$pct_resp_worst_gt),
    mean_delta_min = as.numeric(joined$mean_delta_min),
    p50_delta_min = as.numeric(joined$p50_delta_min),
    p90_delta_min = as.numeric(joined$p90_delta_min),
    p95_delta_min = as.numeric(joined$p95_delta_min),
    n_na_parse = as.integer(joined$n_na_parse),
    n_na_missing = as.integer(joined$n_na_missing),
    n_na_chain = as.integer(joined$n_na_chain),
    n_sent = as.integer(joined$n_sent),
    n_engaged = as.integer(joined$n_engaged),
    n_opted_in = as.integer(joined$n_opted_in),
    n_completed = as.integer(joined$n_completed),
    n_ineligible = as.integer(joined$n_ineligible),
    n_refused = as.integer(joined$n_refused),
    algorithm_version = .algorithm_version,
    config_hash = cfg_hash,
    source_csv_hash = src_csv_hash %||% NA_character_,
    run_at_utc = run_at,
    run_by = NA_character_,
    stringsAsFactors = FALSE
  )
  # Text-to-Web on an external platform with no webhook: completion
  # cannot be computed from the export, so n_completed is NA (not 0/consent) --
  # n_sent / n_opted_in remain valid.
  if (identical(survey_mode, "t2w_external")) {
    out$n_completed <- NA_integer_
  }
  out <- out[order(out$campaign_id, out$date, out$hour_local,
                   out$segment_index, out$threshold_min), , drop = FALSE]
  rownames(out) <- NULL
  out
}

empty_consolidated <- function(project_id, cfg_hash, run_at) {
  data.frame(
    campaign_id = integer(0),
    project_id = integer(0),
    survey_mode = character(0),
    date = as.Date(character(0)),
    hour_local = integer(0),
    segment = character(0),
    segment_index = integer(0),
    threshold_min = integer(0),
    n = integer(0),
    pct_le = numeric(0),
    pct_resp_hit_gt = numeric(0),
    n_respondents = integer(0),
    pct_resp_worst_gt = numeric(0),
    mean_delta_min = numeric(0),
    p50_delta_min = numeric(0),
    p90_delta_min = numeric(0),
    p95_delta_min = numeric(0),
    n_na_parse = integer(0),
    n_na_missing = integer(0),
    n_na_chain = integer(0),
    n_sent = integer(0),
    n_engaged = integer(0),
    n_opted_in = integer(0),
    n_completed = integer(0),
    n_ineligible = integer(0),
    n_refused = integer(0),
    algorithm_version = character(0),
    config_hash = character(0),
    source_csv_hash = character(0),
    run_at_utc = as.POSIXct(character(0), tz = "UTC"),
    run_by = character(0),
    stringsAsFactors = FALSE
  )
}
