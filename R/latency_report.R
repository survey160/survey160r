# Pure latency_report() function. Spec §2.2.
# Deterministic: same (data, config) produces identical output, including
# config_hash. No I/O, no globals (spec invariant I8).
#
# The orchestrator lives here; the per-pass implementations live in:
#   latency_filter.R       -- population/respondent/date filters
#   latency_frame.R        -- per-respondent x per-segment frame builder
#   latency_aggregate.R    -- consolidated table (Parquet payload)
#   latency_diagnostics.R  -- diagnostics list per spec §3.3

# Algorithm + schema versions stamped onto every result. Used by
# latency_aggregate.R and latency_diagnostics.R; package-namespace visible.
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
#' @param config Config list from \code{latency_build_config()} (or
#'   a hand-built list with the same shape).
#' @param run_at Optional \code{POSIXct} timestamp to stamp on every row's
#'   \code{run_at_utc} column. \code{NULL} (default) uses \code{Sys.time()}.
#'   Bulk runners (\code{run_latency_all}) pass a single timestamp here so
#'   every campaign in one fleet pass shares the same \code{run_at_utc},
#'   making "last fleet run" queries trivial.
#' @return A list with \code{consolidated} (one row per
#'   (campaign_id, date, hour_local, segment, threshold_min)),
#'   \code{latency_frame} (one row per respondent x segment),
#'   \code{diagnostics} (counts and breakdowns per spec §3.3), and
#'   \code{meta} (algorithm_version, config_hash, run_at_utc).
#' @export
latency_report <- function(data, config, run_at = NULL) {
  latency_validate_config(config, data)

  cfg_hash <- latency_config_hash(config)
  if (is.null(run_at)) run_at <- Sys.time()
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
