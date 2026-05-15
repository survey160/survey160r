# Equivalence test: the day rollup rows that latency_report emits alongside
# the hour rows match what a weighted-aggregation rollup of the hour rows
# would produce, for `n` and `pct_le`. Day-grain cascade columns are NOT
# reconstructible from the hour rows by simple aggregation -- the day rows
# carry the correct values, computed from the per-respondent frame.
#
# Uses inline synthetic data with two respondents whose segments cross the
# hour boundary so the naive-rollup over-count is visible.

# ---- Inline synthetic fixture -------------------------------------------

# 4 respondents on 2026-01-26 in UTC. r3 spans hours 20:55 -> 21:01 (UTC),
# which is 15:55 -> 16:01 in field_tz = America/New_York -- crossing the
# hour boundary. r4 spans 22:58 -> 23:02 UTC = 17:58 -> 18:02 ET, also
# crossing. r1 and r2 stay within a single hour each.
.cross_hour_data <- function() {
  data.frame(
    campaignid = c(1L, 1L, 1L, 1L),
    userid = c("r1", "r2", "r3", "r4"),
    id.intro.finalText = c("Yes", "Yes", "Yes", "Yes"),
    id.intro.scriptDate = c(
      "2026-01-26 21:30:00.000000Z",
      "2026-01-26 22:30:00.000000Z",
      "2026-01-26 20:55:00.000000Z",
      "2026-01-26 22:58:00.000000Z"
    ),
    id.intro.batchDate = c(
      "2026-01-26 21:30:30.000000Z",  # r1 ET 16:30
      "2026-01-26 22:30:30.000000Z",  # r2 ET 17:30
      "2026-01-26 20:55:30.000000Z",  # r3 ET 15:55 (out of window 16-24)
      "2026-01-26 22:58:30.000000Z"   # r4 ET 17:58
    ),
    id.q1.scriptDate = c(
      "2026-01-26 21:30:40.000000Z",
      "2026-01-26 22:31:00.000000Z",
      "2026-01-26 20:55:45.000000Z",
      "2026-01-26 22:58:45.000000Z"
    ),
    id.q1.batchDate = c(
      "2026-01-26 21:31:00.000000Z",  # r1 ET 16:31 (same hour as intro)
      "2026-01-26 22:32:00.000000Z",  # r2 ET 17:32 (same hour)
      "2026-01-26 21:01:30.000000Z",  # r3 ET 16:01 (CROSSES hour 15 -> 16)
      "2026-01-26 23:02:00.000000Z"   # r4 ET 18:02 (CROSSES hour 17 -> 18)
    ),
    id.q2.scriptDate = c(
      "2026-01-26 21:31:30.000000Z",
      "2026-01-26 22:35:00.000000Z",
      "2026-01-26 21:02:00.000000Z",
      "2026-01-26 23:03:00.000000Z"
    ),
    id.q2.batchDate = c(
      "2026-01-26 21:32:00.000000Z",
      "2026-01-26 22:36:00.000000Z",
      "2026-01-26 21:02:30.000000Z",
      "2026-01-26 23:04:00.000000Z"
    ),
    id.close.scriptDate = c(
      "2026-01-26 21:32:30.000000Z",
      "2026-01-26 22:37:00.000000Z",
      "2026-01-26 21:03:00.000000Z",
      "2026-01-26 23:05:00.000000Z"
    ),
    stringsAsFactors = FALSE
  )
}

.cross_hour_config <- function() {
  list(
    project_id = 1L,
    campaign_id = 1L,
    field_timezone = "America/New_York",
    flow = list(questions = c("intro", "q1", "q2", "close")),
    filters = list(
      population = 'id.intro.finalText == "Yes"',
      campaign_id_column = "campaignid",
      respondent_id_column = NULL
    ),
    texting_windows = list(
      list(date = "2026-01-26", start_hour = 16, end_hour = 24)
    )
  )
}

# ---- Helpers ------------------------------------------------------------

.weighted_pct_le <- function(pct, n) {
  if (sum(n, na.rm = TRUE) == 0) return(NA_real_)
  sum(pct * n, na.rm = TRUE) / sum(n, na.rm = TRUE)
}

.split_grains <- function(cons) {
  list(
    hour = cons[!is.na(cons$hour_local), ],
    day  = cons[is.na(cons$hour_local), ]
  )
}

# ---- Tests --------------------------------------------------------------

test_that("the fixture actually crosses hour boundaries", {
  data <- .cross_hour_data()
  config <- .cross_hour_config()
  result <- latency_report(data, config)
  # Two respondents (r3 and r4) have segments in two different hours.
  per_resp <- table(unique(result$latency_frame[,
    c("respondent_index", "hour_local")])$respondent_index)
  expect_true(any(per_resp > 1L),
              info = "fixture must include at least one cross-hour respondent")
})

test_that("the day rollup rows match a weighted-mean rollup of the hour rows for n and pct_le", {
  data <- .cross_hour_data()
  config <- .cross_hour_config()
  result <- latency_report(data, config)
  g <- .split_grains(result$consolidated)

  rolled <- dplyr::summarise(
    dplyr::group_by(g$hour,
                    .data$campaign_id, .data$project_id, .data$date,
                    .data$segment, .data$segment_index, .data$threshold_min),
    pct_le_rolled = .weighted_pct_le(.data$pct_le, .data$n),
    n_rolled = sum(.data$n, na.rm = TRUE),
    .groups = "drop"
  )
  merged <- merge(
    g$day[, c("campaign_id", "project_id", "date", "segment",
              "segment_index", "threshold_min", "n", "pct_le")],
    rolled,
    by = c("campaign_id", "project_id", "date", "segment", "segment_index",
           "threshold_min")
  )
  expect_equal(nrow(rolled), nrow(g$day))
  expect_equal(merged$n, merged$n_rolled, tolerance = 0)
  expect_equal(merged$pct_le, merged$pct_le_rolled, tolerance = 1e-9)
})

test_that("day rollup rows carry distinct-respondent cascade matching re-derivation from frame", {
  data <- .cross_hour_data()
  config <- .cross_hour_config()
  result <- latency_report(data, config)
  day_grain <- .split_grains(result$consolidated)$day

  thresholds <- c(1L, 3L, 5L, 10L)
  worst_by_resp <- result$latency_frame |>
    dplyr::filter(!is.na(.data$delta_min)) |>
    dplyr::group_by(.data$campaign_id, .data$segment_date_local,
                    .data$respondent_index) |>
    dplyr::summarise(worst = max(.data$delta_min), .groups = "drop") |>
    dplyr::filter(is.finite(.data$worst))

  derived <- do.call(rbind, lapply(thresholds, function(t) {
    by_day <- dplyr::summarise(
      dplyr::group_by(worst_by_resp, .data$campaign_id,
                      .data$segment_date_local),
      n_respondents_derived = dplyr::n(),
      pct_resp_worst_gt_derived = 100 * sum(.data$worst > t) / dplyr::n(),
      .groups = "drop"
    )
    by_day$threshold_min <- as.integer(t)
    by_day
  }))
  names(derived)[names(derived) == "segment_date_local"] <- "date"

  day_casc <- unique(day_grain[, c("date", "threshold_min", "n_respondents",
                                   "pct_resp_worst_gt")])
  merged <- merge(day_casc, derived, by = c("date", "threshold_min"))
  expect_equal(as.integer(merged$n_respondents),
               as.integer(merged$n_respondents_derived), tolerance = 0)
  expect_equal(merged$pct_resp_worst_gt,
               merged$pct_resp_worst_gt_derived, tolerance = 1e-9)
})

test_that("naive SUM(n_respondents) over the hour rows over-counts cross-hour respondents", {
  data <- .cross_hour_data()
  config <- .cross_hour_config()
  result <- latency_report(data, config)
  g <- .split_grains(result$consolidated)

  naive <- dplyr::summarise(
    dplyr::group_by(
      unique(g$hour[,
        c("date", "threshold_min", "hour_local", "n_respondents")]),
      .data$date, .data$threshold_min
    ),
    naive_n = sum(.data$n_respondents, na.rm = TRUE),
    .groups = "drop"
  )
  day_casc <- unique(g$day[, c("date", "threshold_min", "n_respondents")])
  cmp <- merge(day_casc, naive, by = c("date", "threshold_min"))
  # At least one (date, threshold) shows the over-count -- this is precisely
  # why day rollup rows exist as a first-class output rather than letting
  # downstream tools attempt to recompute cascade from the hour rows.
  expect_true(any(cmp$naive_n > cmp$n_respondents),
              info = "fixture should make naive hour-rollup over-count")
})
