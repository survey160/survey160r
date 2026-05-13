# Pure latency_report() function. Spec §2.2.
# Deterministic: same (data, config) produces identical output, including
# config_hash. No I/O, no globals (spec invariant I8).

# Algorithm + schema versions stamped onto every result.
.algorithm_version <- "2.0.0"
.schema_version <- "2"

# Fleet-locked thresholds (minutes). Per spec §8.1 O2, every campaign uses the
# same set so cross-campaign analytics is uniform and the respondent-cascade
# buckets are universal. Changing this set is a major algorithm_version bump.
UNIVERSAL_THRESHOLDS_MIN <- c(1L, 3L, 5L, 10L)

#' Compute a latency report for one campaign
#'
#' Pure function: same \code{(data, config)} always yields identical output.
#' Implements the algorithm in \code{latency_scripts.md} §2.
#'
#' @param data A data frame with one row per respondent and the per-question
#'   timestamp columns named \code{id.<q>.scriptDate} / \code{id.<q>.batchDate}
#'   for each question in \code{config$flow$questions}, plus the population
#'   column \code{id.intro.finalText} and the campaign id column.
#' @param config Config list from \code{read_config()} (or constructed
#'   directly).
#' @return A list with \code{consolidated} (one row per
#'   (campaign_id, date, hour_local, segment, threshold_min)),
#'   \code{latency_frame} (one row per respondent x segment),
#'   \code{diagnostics} (counts and breakdowns per spec §3.3), and
#'   \code{meta} (algorithm_version, config_hash, run_at_utc).
#' @export
latency_report <- function(data, config) {
  validate_config(config, data)

  cfg_hash <- config_hash(config)
  run_at <- Sys.time()
  attr(run_at, "tzone") <- "UTC"

  questions <- config$flow$questions
  field_tz <- config$field_timezone
  campaign_col <- config$filters$campaign_id_column
  resp_id_col <- config$filters$respondent_id_column

  # Stash source_csv_hash from the input attribute before any subsetting (R
  # drops custom attributes on `[`). pull_csv_from_gcs() sets this; manual
  # callers can attach it themselves. Falls back to NA so write_to_gcs's
  # override path still works for ad-hoc invocations.
  src_csv_hash <- attr(data, "source_csv_hash") %||% NA_character_

  # Step 1: population filter.
  data <- apply_population_filter(data, config$filters$population)
  n_in <- nrow(data)

  # Step 2: blanks -> NA, parse timestamps.
  data <- na_if_blank(data)
  ts_cols <- required_timestamp_columns(questions)
  parsed <- parse_timestamps(data, ts_cols)
  data <- parsed$data
  parse_failures <- parsed$parse_failures
  parse_failed_mask <- parsed$parse_failed_mask

  # Step 3: dedupe by respondent_id (earliest intro.scriptDate wins).
  # Dedupe drops rows from `data`; we must drop the same rows from each
  # parse_failed_mask vector so segment-NA classification later lines up
  # row-for-row with `data`.
  if (!is.null(resp_id_col)) {
    keep_idx <- dedupe_keep_rows(data, resp_id_col)
    data <- data[keep_idx, , drop = FALSE]
    parse_failed_mask <- lapply(parse_failed_mask, function(m) m[keep_idx])
  }

  # Step 4: optional date_filter.
  if (!is.null(config$filters$date_filter)) {
    keep_idx <- date_filter_keep_rows(data, config$filters$date_filter, field_tz)
    data <- data[keep_idx, , drop = FALSE]
    parse_failed_mask <- lapply(parse_failed_mask, function(m) m[keep_idx])
  }

  # Step 5: build the per-(respondent, segment) frame.
  windows_df <- normalize_windows(config$texting_windows)
  frame <- build_latency_frame(data, config, windows_df, parse_failed_mask)

  # Step 6: aggregate to consolidated.
  consolidated <- aggregate_consolidated(frame, config, cfg_hash, run_at,
                                         src_csv_hash)

  # Step 7: diagnostics.
  diagnostics <- build_diagnostics(
    frame = frame,
    n_respondents_in = n_in,
    parse_failures = parse_failures,
    windows_df = windows_df,
    field_tz = field_tz,
    config_hash = cfg_hash
  )

  meta <- list(
    algorithm_version = .algorithm_version,
    schema_version = .schema_version,
    config_hash = cfg_hash,
    run_at_utc = run_at
  )

  list(
    consolidated = consolidated,
    latency_frame = frame,
    diagnostics = diagnostics,
    meta = meta
  )
}

# Evaluate a population-filter expression against the data. The expression is
# whatever string the analyst placed in `filters.population` (e.g.
# 'id.intro.finalText == "Yes"'). Evaluated in an empty-parent env so only
# columns of `data` are accessible.
apply_population_filter <- function(data, expr) {
  if (is.null(expr) || !nzchar(expr)) return(data)
  parsed <- tryCatch(parse(text = expr),
                     error = function(e) {
                       stop(sprintf("filters.population is not valid R: %s", expr),
                            call. = FALSE)
                     })
  env <- list2env(as.list(data), parent = baseenv())
  keep <- tryCatch(eval(parsed, envir = env),
                   error = function(e) {
                     stop(sprintf("filters.population evaluation failed: %s",
                                  conditionMessage(e)), call. = FALSE)
                   })
  if (!is.logical(keep) || length(keep) != nrow(data)) {
    stop("filters.population must evaluate to a logical vector matching nrow(data).",
         call. = FALSE)
  }
  keep[is.na(keep)] <- FALSE
  data[keep, , drop = FALSE]
}

# Return the row indices to keep when deduping by respondent_id, choosing the
# row with the earliest id.intro.scriptDate per id. Rows where the id is NA
# pass through (they are unidentifiable and cannot be deduped). Indices are
# in original row order so callers can apply them to parallel per-row masks.
dedupe_keep_rows <- function(data, resp_id_col) {
  if (!resp_id_col %in% names(data)) {
    stop(sprintf("respondent_id_column not found: %s", resp_id_col), call. = FALSE)
  }
  n <- nrow(data)
  intro <- data[["id.intro.scriptDate"]]
  if (is.null(intro)) return(seq_len(n))
  rid <- data[[resp_id_col]]
  ord <- order(rid, intro, na.last = TRUE)
  rid_sorted <- rid[ord]
  has_id_sorted <- !is.na(rid_sorted) & nzchar(as.character(rid_sorted))
  is_dup_sorted <- has_id_sorted & duplicated(rid_sorted)
  sort(ord[!is_dup_sorted])
}

# Return row indices whose intro.scriptDate (in field_tz) falls in date_filter.
date_filter_keep_rows <- function(data, date_filter, field_tz) {
  intro <- data[["id.intro.scriptDate"]]
  if (is.null(intro)) return(seq_len(nrow(data)))
  local_dates <- as.Date(format(intro, tz = field_tz))
  target <- as.Date(date_filter)
  which(!is.na(local_dates) & local_dates %in% target)
}

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

    chain_priors <- c(chain_priors, list(batch_prior))
    delta <- apply_chain_validity(delta_pre, chain_priors)

    in_window <- in_window_flag(batch_prior, windows_df, field_tz)
    in_window[is.na(batch_prior)] <- 0L

    seg_date_local <- as.Date(format(batch_prior, tz = field_tz))
    hour_local <- as.integer(format(batch_prior, format = "%H", tz = field_tz))

    parse_fail_row <- segment_parse_fail_mask(
      parse_failed_mask, batch_prior_col, script_next_col, n
    )
    is_na_post <- is.na(delta)
    na_reason <- rep(NA_character_, n)
    na_reason[is_na_post & parse_fail_row] <- "parse_failure"
    na_reason[is_na_post & !parse_fail_row & is.na(delta_pre)] <-
      "missing_endpoint"
    na_reason[is_na_post & !parse_fail_row & !is.na(delta_pre)] <-
      "chain_break"

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

# Aggregate latency_frame to the consolidated table (spec §3.1). Cells are
# (campaign_id, date, hour_local, segment, threshold_min). For day buckets
# hour_local is NA on every row.
aggregate_consolidated <- function(frame, config, cfg_hash, run_at,
                                   src_csv_hash = NA_character_) {
  thresholds <- UNIVERSAL_THRESHOLDS_MIN
  bucket <- config$reports$time_bucket
  project_id <- as.integer(config$project_id)

  if (nrow(frame) == 0) {
    return(empty_consolidated(project_id, cfg_hash, run_at))
  }

  bucketed <- frame
  if (bucket == "day") {
    bucketed$hour_local <- NA_integer_
  }
  bucketed$date <- bucketed$segment_date_local

  # Total respondents per (campaign_id, date, hour_local) -- denominator for
  # pct_resp_hit_gt. Defined as distinct respondent_index appearing in any
  # segment for that bucket key (matches "all in-wave respondents" per spec).
  totals <- dplyr::summarise(
    dplyr::group_by(bucketed, .data$campaign_id, .data$date, .data$hour_local),
    .total_resp = dplyr::n_distinct(.data$respondent_index),
    .groups = "drop"
  )

  # Per-respondent worst Δ across all segments, per (campaign, date, hour).
  # Drives the n_respondents and pct_resp_worst_gt columns -- the wave-level
  # "respondent latency cascade" the historical reports show.
  worst_by_respondent <- dplyr::summarise(
    dplyr::group_by(
      dplyr::filter(bucketed, !is.na(.data$delta_min)),
      .data$campaign_id, .data$date, .data$hour_local,
      .data$respondent_index
    ),
    worst_delta = suppressWarnings(max(.data$delta_min, na.rm = TRUE)),
    .groups = "drop"
  )
  # Drop respondents whose worst is non-finite (no valid Δ at all in this
  # bucket -- shouldn't happen given the filter above, but guards against
  # edge cases where dplyr emits -Inf).
  worst_by_respondent <- worst_by_respondent[
    is.finite(worst_by_respondent$worst_delta), , drop = FALSE]
  cascade_rows <- list()
  for (t in thresholds) {
    chunk <- dplyr::summarise(
      dplyr::group_by(worst_by_respondent,
                      .data$campaign_id, .data$date, .data$hour_local),
      n_respondents = dplyr::n(),
      n_worst_over = sum(.data$worst_delta > t),
      .groups = "drop"
    )
    chunk$threshold_min <- as.integer(t)
    chunk$pct_resp_worst_gt <- ifelse(
      chunk$n_respondents > 0,
      100 * chunk$n_worst_over / chunk$n_respondents,
      NA_real_
    )
    cascade_rows[[length(cascade_rows) + 1L]] <-
      chunk[, c("campaign_id", "date", "hour_local", "threshold_min",
                "n_respondents", "pct_resp_worst_gt")]
  }
  cascade <- do.call(rbind, cascade_rows)

  rows <- list()
  for (t in thresholds) {
    cells <- dplyr::summarise(
      dplyr::group_by(
        bucketed,
        .data$campaign_id, .data$date, .data$hour_local,
        .data$segment, .data$segment_index
      ),
      n = sum(!is.na(.data$delta_min) & .data$in_window == 1L),
      pct_le = ifelse(
        sum(!is.na(.data$delta_min) & .data$in_window == 1L) > 0,
        100 * mean(.data$delta_min[!is.na(.data$delta_min) & .data$in_window == 1L] <= t),
        NA_real_
      ),
      n_resp_over = dplyr::n_distinct(
        .data$respondent_index[!is.na(.data$delta_min) & .data$delta_min > t]
      ),
      .groups = "drop"
    )
    cells$threshold_min <- as.integer(t)
    rows[[length(rows) + 1L]] <- cells
  }
  combined <- do.call(rbind, rows)
  joined <- dplyr::left_join(
    combined, totals,
    by = c("campaign_id", "date", "hour_local")
  )
  joined$pct_resp_hit_gt <- ifelse(
    !is.na(joined$.total_resp) & joined$.total_resp > 0 & joined$n > 0,
    100 * joined$n_resp_over / joined$.total_resp,
    NA_real_
  )
  joined <- dplyr::left_join(
    joined, cascade,
    by = c("campaign_id", "date", "hour_local", "threshold_min")
  )

  out <- data.frame(
    campaign_id = as.integer(joined$campaign_id),
    project_id = rep(project_id, nrow(joined)),
    date = joined$date,
    hour_local = joined$hour_local,
    segment = joined$segment,
    segment_index = as.integer(joined$segment_index),
    threshold_min = as.integer(joined$threshold_min),
    n = as.integer(joined$n),
    pct_le = joined$pct_le,
    pct_resp_hit_gt = joined$pct_resp_hit_gt,
    n_respondents = as.integer(joined$n_respondents),
    pct_resp_worst_gt = as.numeric(joined$pct_resp_worst_gt),
    algorithm_version = .algorithm_version,
    config_hash = cfg_hash,
    source_csv_hash = src_csv_hash %||% NA_character_,
    run_at_utc = run_at,
    run_by = NA_character_,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$campaign_id, out$date, out$hour_local,
                   out$segment_index, out$threshold_min), , drop = FALSE]
  rownames(out) <- NULL
  out
}

empty_consolidated <- function(project_id, cfg_hash, run_at) {
  data.frame(
    campaign_id = integer(0),
    project_id = integer(0),
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
    algorithm_version = character(0),
    config_hash = character(0),
    source_csv_hash = character(0),
    run_at_utc = as.POSIXct(character(0), tz = "UTC"),
    run_by = character(0),
    stringsAsFactors = FALSE
  )
}

# Build the diagnostics list per spec §3.3.
build_diagnostics <- function(frame, n_respondents_in, parse_failures,
                              windows_df, field_tz, config_hash) {
  n_clamped <- attr(frame, "n_clamped") %||% 0L
  if (nrow(frame) == 0) {
    return(list(
      n_respondents_in = n_respondents_in,
      n_respondents_used = 0L,
      n_respondents_no_valid_segment = n_respondents_in,
      n_segments_total = 0L,
      n_segments_na = 0L,
      n_segments_na_by_reason = list(parse_failure = 0L,
                                     missing_endpoint = 0L,
                                     chain_break = 0L),
      n_negative_latencies_clamped = n_clamped,
      n_out_of_window_dropped = 0L,
      parse_failures_per_column = parse_failures,
      windows_normalized_utc = windows_to_utc(windows_df, field_tz),
      config_hash = config_hash,
      algorithm_version = .algorithm_version,
      respondent_summary = list(
        n_respondents = 0L,
        pct_clean_at_5min = NA_real_,
        pct_worst_in_5_to_10 = NA_real_,
        pct_worst_over_10 = NA_real_
      )
    ))
  }
  by_resp <- dplyr::summarise(
    dplyr::group_by(frame, .data$respondent_index),
    has_valid = any(!is.na(.data$delta_min)),
    max_delta = suppressWarnings(max(.data$delta_min, na.rm = TRUE)),
    .groups = "drop"
  )
  used <- sum(by_resp$has_valid)
  total_resp_observed <- nrow(by_resp)
  no_valid <- total_resp_observed - used
  total_segments <- nrow(frame)
  na_segments <- sum(is.na(frame$delta_min))
  out_of_window <- sum(!is.na(frame$delta_min) & frame$in_window == 0L)

  worst <- by_resp$max_delta
  worst[!is.finite(worst)] <- NA_real_
  pct_clean <- 100 * mean(!is.na(worst) & worst <= 5)
  pct_5_10 <- 100 * mean(!is.na(worst) & worst > 5 & worst <= 10)
  pct_over_10 <- 100 * mean(!is.na(worst) & worst > 10)

  list(
    n_respondents_in = n_respondents_in,
    n_respondents_used = used,
    n_respondents_no_valid_segment = no_valid,
    n_segments_total = total_segments,
    n_segments_na = na_segments,
    n_segments_na_by_reason = list(
      parse_failure = sum(frame$na_reason == "parse_failure", na.rm = TRUE),
      missing_endpoint = sum(frame$na_reason == "missing_endpoint",
                             na.rm = TRUE),
      chain_break = sum(frame$na_reason == "chain_break", na.rm = TRUE)
    ),
    n_negative_latencies_clamped = n_clamped,
    n_out_of_window_dropped = out_of_window,
    parse_failures_per_column = parse_failures,
    windows_normalized_utc = windows_to_utc(windows_df, field_tz),
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

windows_to_utc <- function(windows_df, field_tz) {
  if (is.null(windows_df) || nrow(windows_df) == 0) {
    return(data.frame(start_utc = as.POSIXct(character(0), tz = "UTC"),
                      end_utc = as.POSIXct(character(0), tz = "UTC")))
  }
  midnights <- as.POSIXct(format(windows_df$date), tz = field_tz)
  start_local <- midnights + windows_df$start_hour * 3600
  end_local <- midnights + windows_df$end_hour * 3600
  attr(start_local, "tzone") <- "UTC"
  attr(end_local, "tzone") <- "UTC"
  data.frame(start_utc = start_local, end_utc = end_local)
}

`%||%` <- function(a, b) if (is.null(a)) b else a
