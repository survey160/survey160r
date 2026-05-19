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
#'   Fleet runners pass a single timestamp here so every campaign in one
#'   fleet pass shares the same \code{run_at_utc}, making "last fleet
#'   run" queries trivial.
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
  resp_id_col <- config$filters$respondent_id_column

  # Stash source_csv_hash from the input attribute before any subsetting (R
  # drops custom attributes on `[`). s160_gcs_pull_csv() sets this; manual
  # callers can attach it themselves. Falls back to NA so downstream writers
  # can still override it at persist time for ad-hoc invocations.
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

  # Steps 3 & 4 drop rows from `data`; the per-segment parse_failed_mask
  # vectors must shrink in lockstep so segment-NA classification later lines
  # up row-for-row. subset_parsed_input() does both at once -- adding a
  # third filter step in the future cannot forget the reindex.

  # Step 3: dedupe by respondent_id (earliest intro.scriptDate wins).
  if (!is.null(resp_id_col)) {
    pair <- subset_parsed_input(data, parse_failed_mask,
                                dedupe_keep_rows(data, resp_id_col))
    data <- pair$data
    parse_failed_mask <- pair$parse_failed_mask
  }

  # Step 4: optional date_filter.
  if (!is.null(config$filters$date_filter)) {
    pair <- subset_parsed_input(
      data, parse_failed_mask,
      date_filter_keep_rows(data, config$filters$date_filter, field_tz)
    )
    data <- pair$data
    parse_failed_mask <- pair$parse_failed_mask
  }

  # Step 5: build the per-(respondent, segment) frame.
  frame <- build_latency_frame(data, config, parse_failed_mask)

  # Step 6: aggregate to consolidated at TWO grains in the same frame.
  # Hour rows (hour_local 0-23) for time-of-day analysis; day-rollup rows
  # (hour_local = NA) carrying correct day-grain cascade metrics that can't
  # be reconstructed by simple aggregation of hour rows (a respondent
  # spanning two hours is counted in both hours' denominators). Downstream
  # consumers filter on `hour_local IS NULL` for day rollups,
  # `hour_local IS NOT NULL` for time-of-day.
  hour_grain <- aggregate_consolidated(frame, config, cfg_hash, run_at,
                                       src_csv_hash)
  day_frame <- frame
  if (nrow(day_frame) > 0L) day_frame$hour_local <- NA_integer_
  day_grain <- aggregate_consolidated(day_frame, config, cfg_hash, run_at,
                                      src_csv_hash)
  consolidated <- rbind(hour_grain, day_grain)

  # Step 7: diagnostics.
  diagnostics <- build_diagnostics(
    frame = frame,
    n_respondents_in = n_in,
    parse_failures = parse_failures,
    config_hash = cfg_hash
  )

  # Surface CSV-level provenance on `meta` as well as on every row of
  # `consolidated`. Meta survives data-frame subsetting and is the
  # documented contract for downstream persistence layers (e.g.
  # survey160-shiny's writer reads result$meta$source_csv_hash); the
  # per-row column is what ends up in the persisted Parquet.
  meta <- list(
    algorithm_version = .algorithm_version,
    schema_version = .schema_version,
    config_hash = cfg_hash,
    run_at_utc = run_at,
    source_csv_hash = src_csv_hash,
    source_csv_path = attr(data, "source_csv_path") %||% NA_character_
  )

  list(
    consolidated = consolidated,
    latency_frame = frame,
    diagnostics = diagnostics,
    meta = meta
  )
}
