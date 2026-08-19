# Pure latency_report() function. Spec §2.2.
# Deterministic: same (data, config) produces identical output, including
# config_hash. No I/O, no globals (spec invariant I8).
#
# The orchestrator lives here; the per-pass implementations live in:
#   latency_filter.R       -- population/respondent/date filters
#   latency_frame.R        -- per-respondent x per-segment frame builder
#   latency_aggregate.R    -- consolidated table (Parquet payload)
#   latency_diagnostics.R  -- diagnostics list per spec §3.3
#   summary_primitives.R   -- pre-filter mask + last-reached helpers
#   summary_aggregate.R    -- per-bucket sent/engaged/opted_in/completed/inelig

# Algorithm + schema versions stamped onto every result. Used by
# latency_aggregate.R and latency_diagnostics.R; package-namespace visible.
# 2.2.0: summary n_texted corrected to key on the send (id.intro.scriptDate)
# instead of the reply, and a new n_engaged (reply) count added -- schema 5.
# Schema 6: summary count columns renamed to the canonical funnel vocabulary --
# n_texted -> n_sent and n_consented -> n_opted_in (n_engaged / n_completed /
# n_ineligible unchanged), so the counts share the disposition flags' stems.
# Values identical, a rename only, so algorithm_version stays 2.2.0.
.algorithm_version <- "2.2.0"
.schema_version <- "6"

# Fleet-locked thresholds (minutes). Per spec §8.1 O2, every campaign uses the
# same set so cross-campaign analytics is uniform and the respondent-cascade
# buckets are universal. Changing this set is a major algorithm_version bump.
UNIVERSAL_THRESHOLDS_MIN <- c(1L, 3L, 5L, 10L)

#' Compute a latency report for one campaign
#'
#' Pure function of \code{(data, config, run_at)}: no I/O, no mutable globals.
#' With a supplied \code{run_at} the output is fully deterministic; left
#' \code{NULL} it stamps \code{run_at_utc} from \code{Sys.time()}.
#' Implements the algorithm in \code{campaign_scripts.md} §2.
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
#' @examples
#' data <- data.frame(
#'   campaignid = c(1L, 1L),
#'   id.intro.finalText = c("Yes", "Yes"),
#'   id.intro.scriptDate = c("2026-01-26 21:00:00Z", "2026-01-26 21:05:00Z"),
#'   id.intro.batchDate  = c("2026-01-26 21:00:30Z", "2026-01-26 21:05:20Z"),
#'   id.q1.scriptDate    = c("2026-01-26 21:01:00Z", "2026-01-26 21:06:00Z"),
#'   id.q1.batchDate     = c("2026-01-26 21:01:20Z", "2026-01-26 21:06:15Z"),
#'   id.close.scriptDate = c("2026-01-26 21:02:00Z", "2026-01-26 21:07:00Z"),
#'   check.names = FALSE, stringsAsFactors = FALSE
#' )
#' config <- latency_build_config(1L, data, field_timezone = "America/New_York")
#' result <- latency_report(data, config, run_at = as.POSIXct("2026-01-01", tz = "UTC"))
#' head(result$consolidated)
#' @export
latency_report <- function(data, config, run_at = NULL) {
  latency_validate_config(config, data)

  cfg_hash <- latency_config_hash(config)
  if (is.null(run_at)) run_at <- Sys.time()
  attr(run_at, "tzone") <- "UTC"

  questions <- config$flow$questions
  field_tz <- config$field_timezone
  resp_id_col <- config$filters$respondent_id_column

  # Stash the source_csv_* provenance pair from the input attributes up front,
  # before the transforms below, so both survive regardless of a step that
  # rebuilds `data` and drops custom attributes.
  # s160_gcs_campaign_results_read(hash = TRUE) / s160_read_csv() set these;
  # manual callers can attach them. Falls back to NA so downstream writers can
  # still override at persist time for ad-hoc invocations.
  src_csv_hash <- attr(data, "source_csv_hash") %||% NA_character_
  src_csv_path <- attr(data, "source_csv_path") %||% NA_character_

  # Step 0: na_if_blank up-front so both the summary computation (pre-filter)
  # and the latency parse step (post-filter) see NA where the CSV had "".
  data <- na_if_blank(data)

  # Survey mode (sms vs t2w) is detected once from the pre-filter data. It
  # drives the completion signal in build_summary_frame and is stamped on
  # every consolidated row so downstream consumers (Shiny) can filter.
  survey_mode <- detect_survey_mode(data)

  # Step 1: pre-filter summary metrics (spec §4). Counts sent /
  # opted_in / completed at the (campaign, date, hour_local) grain,
  # plus per-segment ineligible counts. Computed on the full pre-filter
  # population so the denominators reflect every respondent the platform
  # dispatched the intro to, not just those who consented.
  summary_hour <- build_summary_frame(data, config, survey_mode)
  ineligible_hour <- build_ineligible_frame(data, config)
  # date_filter, when set, restricts both views to the listed dates --
  # not just latency. The user's intent ("show me this date's data") is
  # symmetric across summary and latency.
  if (!is.null(config$filters$date_filter)) {
    target_dates <- as.Date(config$filters$date_filter)
    summary_hour <- summary_hour[summary_hour$date %in% target_dates,
                                 , drop = FALSE]
    ineligible_hour <- ineligible_hour[ineligible_hour$date %in% target_dates,
                                       , drop = FALSE]
  }
  summary_day <- collapse_summary_to_day(summary_hour)
  ineligible_day <- collapse_ineligible_to_day(ineligible_hour)

  # Step 2: population filter.
  data <- apply_population_filter(data, config$filters$population)
  n_in <- nrow(data)

  # Step 3: parse timestamps. (Blanks were already replaced in step 0.)
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
                                       src_csv_hash,
                                       summary_frame = summary_hour,
                                       ineligible_frame = ineligible_hour,
                                       survey_mode = survey_mode)
  # A segment whose prior-question timestamp is blank/unparseable has an NA
  # segment_date_local, and hour_local (derived from the same batch_prior) is
  # NA too -- so it already lands at (date=NA, hour_local=NA) in the hour pass.
  # The day pass re-emits that exact key (it forces hour_local=NA over the same
  # frame and the day-collapsed summary keeps NA-date groups), so keeping the
  # hour-pass NA-hour rows would duplicate the unknown-time bucket after rbind.
  # The (hour=NULL) unknown bucket belongs to the day partition only.
  hour_grain <- hour_grain[!is.na(hour_grain$hour_local), , drop = FALSE]
  day_frame <- frame
  if (nrow(day_frame) > 0L) day_frame$hour_local <- NA_integer_
  day_grain <- aggregate_consolidated(day_frame, config, cfg_hash, run_at,
                                      src_csv_hash,
                                      summary_frame = summary_day,
                                      ineligible_frame = ineligible_day,
                                      survey_mode = survey_mode)
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
  # a consumer project's writer reads result$meta$source_csv_hash); the
  # per-row column is what ends up in the persisted Parquet.
  meta <- list(
    algorithm_version = .algorithm_version,
    schema_version = .schema_version,
    config_hash = cfg_hash,
    run_at_utc = run_at,
    source_csv_hash = src_csv_hash,
    source_csv_path = src_csv_path
  )

  list(
    consolidated = consolidated,
    latency_frame = frame,
    diagnostics = diagnostics,
    meta = meta
  )
}
