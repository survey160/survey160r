# Coverage for R/summary_primitives.R and R/summary_aggregate.R.
# Hand-computed counts against the synthetic fixtures + ineligible
# scenarios constructed inline.

test_that("build_summary_frame: send-anchored counts on the synthetic fixture", {
  d <- load_synthetic_data()
  cfg <- synthetic_config()
  frame <- build_summary_frame(d, cfg)
  day <- collapse_summary_to_day(frame)

  # All 4 rows were SENT the intro (id.intro.scriptDate) -> texted = 4:
  # r3 sent @h15, r1 @h16, r2 @h17, r4 @h17. r1/r2/r3 replied (batchDate),
  # said "Yes", and reached close. r4 was texted but never replied and said
  # "No" -- the texted-but-no-reply recipient the old batchDate key dropped.
  expect_setequal(frame$hour_local, c(15L, 16L, 17L))
  expect_equal(unique(frame$campaign_id), 1L)
  expect_equal(unique(as.character(frame$date)), "2026-01-26")
  expect_equal(day$n_sent, 4L)
  expect_equal(day$n_engaged, 3L)
  expect_equal(day$n_opted_in, 3L)
  expect_equal(day$n_completed, 3L)
  # Hour 17 holds two sends (r2 @22:00Z, r4 @22:30Z) but only r2 replied.
  h17 <- frame[frame$hour_local == 17L, ]
  expect_equal(h17$n_sent, 2L)
  expect_equal(h17$n_engaged, 1L)
  # Schema-6 no-dual-emit contract: the renamed-away legacy names are gone from
  # both grains. (n_completed is a KEPT column, not a legacy alias.)
  expect_false(any(c("n_texted", "n_consented") %in% names(frame)))
  expect_false(any(c("n_texted", "n_consented") %in% names(day)))
})

test_that("build_summary_frame: a replying 'No' respondent is engaged but not consented", {
  d <- load_synthetic_data(mutate = function(d) {
    # r4 now replies (gets a batchDate) but its finalText stays "No", so the
    # population filter rejects it: engaged but not consented.
    d$id.intro.batchDate[4] <- "2026-01-26 22:30:30.000000Z"
    d
  })
  cfg <- synthetic_config()
  day <- collapse_summary_to_day(build_summary_frame(d, cfg))

  expect_equal(day$n_sent, 4L)        # all 4 were sent the intro
  expect_equal(day$n_engaged, 4L)       # all 4 now replied
  expect_equal(day$n_opted_in, 3L)     # 3 said "Yes", 1 said "No"
  expect_equal(day$n_completed, 3L)     # 3 reached close (No-respondent didn't)
})

test_that("build_summary_frame: zero rows returns empty schema", {
  cfg <- synthetic_config()
  d <- minimal_synthetic_data(with_rows = FALSE)
  frame <- build_summary_frame(d, cfg)
  expect_equal(nrow(frame), 0L)
  expect_named(frame, c("campaign_id", "date", "hour_local",
                        "n_sent", "n_engaged", "n_opted_in", "n_completed"))
})

test_that("build_summary_frame: data without close.scriptDate column treats n_completed as zero", {
  cfg <- synthetic_config()
  d <- load_synthetic_data(mutate = function(d) {
    d$id.close.scriptDate <- NULL
    d
  })
  day <- collapse_summary_to_day(build_summary_frame(d, cfg))
  expect_equal(day$n_completed, 0L)
  expect_equal(day$n_sent, 4L)        # all 4 sent
  expect_equal(day$n_engaged, 3L)       # r1/r2/r3 replied
})

test_that("build_summary_frame: all-zero rows yield empty frame", {
  cfg <- synthetic_config()
  d <- load_synthetic_data(mutate = function(d) {
    # Strip the send timestamp the funnel keys off -- no row was texted, so
    # nothing contributes (every mask is gated on texted).
    d$id.intro.scriptDate <- ""
    d
  })
  frame <- build_summary_frame(d, cfg)
  expect_equal(nrow(frame), 0L)
})

test_that("collapse_summary_to_day: sums hourly counts per (campaign, date)", {
  hourly <- data.frame(
    campaign_id = c(1L, 1L, 1L),
    date = as.Date(c("2026-01-26", "2026-01-26", "2026-01-26")),
    hour_local = c(15L, 16L, 17L),
    n_sent = c(2L, 3L, 5L),
    n_engaged = c(2L, 2L, 4L),
    n_opted_in = c(1L, 2L, 4L),
    n_completed = c(1L, 1L, 3L),
    stringsAsFactors = FALSE
  )
  day <- collapse_summary_to_day(hourly)
  expect_equal(nrow(day), 1L)
  expect_true(is.na(day$hour_local))
  expect_equal(day$n_sent, 10L)
  expect_equal(day$n_engaged, 8L)
  expect_equal(day$n_opted_in, 7L)
  expect_equal(day$n_completed, 5L)
})

test_that("collapse_summary_to_day: empty frame in -> empty frame out", {
  expect_equal(nrow(collapse_summary_to_day(empty_summary_frame())), 0L)
})

test_that("build_ineligible_frame: per-segment counts", {
  cfg <- synthetic_config()
  # Inline 3-respondent frame: r1 ineligible after q1, r2 ineligible after q2,
  # r3 not ineligible. Bucketed by intro.batchDate at hour 16 EST.
  d <- data.frame(
    campaignid = c(1L, 1L, 1L),
    userid = c("r1", "r2", "r3"),
    id.intro.finalText = c("Yes", "Yes", "Yes"),
    id.intro.scriptDate = rep("2026-01-26 21:00:00.000000Z", 3),
    id.intro.batchDate = rep("2026-01-26 21:00:30.000000Z", 3),
    id.q1.scriptDate = rep("2026-01-26 21:01:00.000000Z", 3),
    id.q1.batchDate = c("",                                       # r1 stops here
                       "2026-01-26 21:01:30.000000Z",
                       "2026-01-26 21:01:30.000000Z"),
    id.q2.scriptDate = c("",
                         "2026-01-26 21:02:00.000000Z",
                         "2026-01-26 21:02:00.000000Z"),
    id.q2.batchDate = c("", "", "2026-01-26 21:02:30.000000Z"),
    id.close.scriptDate = c("", "", "2026-01-26 21:03:00.000000Z"),
    id.ineligible.scriptDate = c("2026-01-26 21:01:05.000000Z",
                                 "2026-01-26 21:02:05.000000Z",
                                 ""),
    stringsAsFactors = FALSE
  )
  inelig <- build_ineligible_frame(d, cfg)
  # r1 last reached = q1 (index 2 in questions), segment_index = 1 (intro->q1)
  # r2 last reached = q2 (index 3), segment_index = 2 (q1->q2)
  expect_setequal(inelig$segment_index, c(1L, 2L))
  expect_equal(inelig$n_ineligible[inelig$segment_index == 1L], 1L)
  expect_equal(inelig$n_ineligible[inelig$segment_index == 2L], 1L)
})

test_that("build_ineligible_frame: zero rows / no inelig column / no inelig values", {
  cfg <- synthetic_config()
  # zero rows
  expect_equal(nrow(build_ineligible_frame(
    minimal_synthetic_data(with_rows = FALSE), cfg
  )), 0L)
  # column missing entirely
  d_no_col <- load_synthetic_data(mutate = function(d) {
    d$id.ineligible.scriptDate <- NULL
    d
  })
  expect_equal(nrow(build_ineligible_frame(d_no_col, cfg)), 0L)
  # column present but all blank
  d_all_blank <- load_synthetic_data(mutate = function(d) {
    d$id.ineligible.scriptDate <- ""
    d
  })
  expect_equal(nrow(build_ineligible_frame(d_all_blank, cfg)), 0L)
})

test_that("build_ineligible_frame: respondent who only reached intro is dropped", {
  cfg <- synthetic_config()
  d <- data.frame(
    campaignid = 1L,
    userid = "r1",
    id.intro.finalText = "Yes",
    id.intro.scriptDate = "2026-01-26 21:00:00.000000Z",
    id.intro.batchDate = "2026-01-26 21:00:30.000000Z",
    id.q1.scriptDate = "",  # never reached q1
    id.q1.batchDate = "",
    id.q2.scriptDate = "",
    id.q2.batchDate = "",
    id.close.scriptDate = "",
    id.ineligible.scriptDate = "2026-01-26 21:00:45.000000Z",
    stringsAsFactors = FALSE
  )
  expect_equal(nrow(build_ineligible_frame(d, cfg)), 0L)
})

test_that("collapse_ineligible_to_day: sums hourly per (campaign, date, segment)", {
  hourly <- data.frame(
    campaign_id = c(1L, 1L, 1L),
    date = as.Date(c("2026-01-26", "2026-01-26", "2026-01-26")),
    hour_local = c(15L, 16L, 16L),
    segment_index = c(1L, 1L, 2L),
    n_ineligible = c(2L, 3L, 5L),
    stringsAsFactors = FALSE
  )
  day <- collapse_ineligible_to_day(hourly)
  expect_setequal(day$segment_index, c(1L, 2L))
  expect_equal(day$n_ineligible[day$segment_index == 1L], 5L)
  expect_equal(day$n_ineligible[day$segment_index == 2L], 5L)
  expect_true(all(is.na(day$hour_local)))
})

test_that("collapse_ineligible_to_day: empty frame in -> empty frame out", {
  expect_equal(nrow(collapse_ineligible_to_day(empty_ineligible_frame())), 0L)
})

test_that("population_filter_mask: NULL / empty expr returns all-TRUE", {
  d <- load_synthetic_data()
  expect_true(all(population_filter_mask(d, NULL)))
  expect_true(all(population_filter_mask(d, "")))
})

test_that("population_filter_mask: invalid expression aborts", {
  d <- load_synthetic_data()
  expect_error(population_filter_mask(d, "this is not R"),
               "`filters.population` is not valid R")
  expect_error(population_filter_mask(d, "no_such_column == 1"),
               "`filters.population` evaluation failed")
  # Expression that returns a non-logical / wrong length
  expect_error(population_filter_mask(d, "42"),
               "must evaluate to a logical vector matching nrow")
})

test_that("population_filter_mask: NA in expression becomes FALSE", {
  d <- load_synthetic_data(mutate = function(d) {
    d$id.intro.finalText[1] <- NA_character_
    d
  })
  mask <- population_filter_mask(d, 'id.intro.finalText == "Yes"')
  # Row 1 has NA finalText: NA == "Yes" is NA, which we coerce to FALSE.
  expect_false(mask[1])
})

test_that("last_reached_question_index: tracks max k with non-NA scriptDate", {
  d <- data.frame(
    id.intro.scriptDate = as.POSIXct(c("2026-01-26 21:00", NA,
                                        "2026-01-26 21:00"),
                                      tz = "UTC"),
    id.q1.scriptDate = as.POSIXct(c("2026-01-26 21:01", NA, NA),
                                    tz = "UTC"),
    id.q2.scriptDate = as.POSIXct(c("2026-01-26 21:02", NA, NA),
                                    tz = "UTC"),
    stringsAsFactors = FALSE
  )
  idx <- last_reached_question_index(d, c("intro", "q1", "q2"))
  expect_equal(idx, c(3L, NA, 1L))
})

test_that("last_reached_question_index: missing columns skipped gracefully", {
  d <- data.frame(
    id.intro.scriptDate = as.POSIXct(c("2026-01-26 21:00"), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  # questions vector includes ones not in the frame; those are skipped.
  idx <- last_reached_question_index(d, c("intro", "q_missing"))
  expect_equal(idx, 1L)
})

test_that("latency_report integrates summary columns into consolidated", {
  d <- load_synthetic_data()
  cfg <- synthetic_config()
  result <- latency_report(d, cfg, run_at = as.POSIXct("2026-05-21", tz = "UTC"))
  cons <- result$consolidated

  # Day rollup row: 4 texted (all sent); 3 engaged/consented/completed.
  day <- cons[is.na(cons$hour_local), ]
  expect_true(all(day$n_sent == 4L))
  expect_true(all(day$n_engaged == 3L))
  expect_true(all(day$n_opted_in == 3L))
  expect_true(all(day$n_completed == 3L))
  expect_true(all(day$n_ineligible == 0L))

  # Hour rows: h15/h16 each have 1 texted; h17 has 2 sends (r2 + the
  # texted-but-no-reply r4) with 1 engaged/consented/completed.
  hr <- cons[!is.na(cons$hour_local), ]
  for (h in c(15L, 16L)) {
    cell <- hr[hr$hour_local == h, ]
    expect_true(all(cell$n_sent == 1L))
    expect_true(all(cell$n_engaged == 1L))
    expect_true(all(cell$n_opted_in == 1L))
    expect_true(all(cell$n_completed == 1L))
  }
  h17 <- hr[hr$hour_local == 17L, ]
  expect_true(all(h17$n_sent == 2L))
  expect_true(all(h17$n_engaged == 1L))
  expect_true(all(h17$n_opted_in == 1L))
  expect_true(all(h17$n_completed == 1L))
})

test_that("build_consolidated_scaffold: latency-only buckets produce full grid", {
  cfg <- synthetic_config()
  thresholds <- c(1L, 3L, 5L, 10L)
  bucketed <- data.frame(
    campaign_id = c(1L, 1L),
    date = as.Date(c("2026-01-26", "2026-01-26")),
    hour_local = c(15L, 16L),
    stringsAsFactors = FALSE
  )
  scaffold <- build_consolidated_scaffold(bucketed, empty_summary_frame(),
                                          cfg, thresholds)
  # 2 hours × 3 segments × 4 thresholds = 24 rows
  expect_equal(nrow(scaffold), 24L)
  expect_setequal(scaffold$segment_index, c(1L, 2L, 3L))
  expect_setequal(scaffold$threshold_min, thresholds)
  expect_setequal(scaffold$hour_local, c(15L, 16L))
})

test_that("build_consolidated_scaffold: summary-only buckets included in scaffold", {
  cfg <- synthetic_config()
  thresholds <- c(1L, 3L, 5L, 10L)
  empty_bucketed <- data.frame(campaign_id = integer(0),
                               date = as.Date(character(0)),
                               hour_local = integer(0),
                               stringsAsFactors = FALSE)
  summary_only <- data.frame(
    campaign_id = c(1L, 1L),
    date = as.Date(c("2026-01-26", "2026-01-26")),
    hour_local = c(20L, 21L),
    n_sent = c(50L, 75L),
    n_opted_in = c(0L, 0L),
    n_completed = c(0L, 0L),
    stringsAsFactors = FALSE
  )
  scaffold <- build_consolidated_scaffold(empty_bucketed, summary_only,
                                          cfg, thresholds)
  # Latency empty + 2 summary hours -> 2 × 3 × 4 = 24
  expect_equal(nrow(scaffold), 24L)
  expect_setequal(scaffold$hour_local, c(20L, 21L))
})

test_that("build_consolidated_scaffold: union of latency + summary buckets, deduped", {
  cfg <- synthetic_config()
  thresholds <- c(1L, 3L, 5L, 10L)
  bucketed <- data.frame(
    campaign_id = c(1L, 1L),
    date = as.Date(c("2026-01-26", "2026-01-26")),
    hour_local = c(15L, 16L),  # overlap with summary on 16, latency-only on 15
    stringsAsFactors = FALSE
  )
  summary <- data.frame(
    campaign_id = c(1L, 1L),
    date = as.Date(c("2026-01-26", "2026-01-26")),
    hour_local = c(16L, 17L),  # overlap on 16, summary-only on 17
    n_sent = c(5L, 5L),
    n_opted_in = c(5L, 5L),
    n_completed = c(5L, 5L),
    stringsAsFactors = FALSE
  )
  scaffold <- build_consolidated_scaffold(bucketed, summary, cfg, thresholds)
  # Union {15, 16, 17} × 3 segments × 4 thresholds = 36 rows; hour 16
  # MUST NOT appear twice (rbind+unique semantics).
  expect_equal(nrow(scaffold), 36L)
  expect_setequal(scaffold$hour_local, c(15L, 16L, 17L))
})

test_that("detect_survey_mode: web completes -> t2w (3-way classifier)", {
  d <- load_synthetic_data()
  expect_equal(detect_survey_mode(d), "sms")        # no web, no link
  d$web_complete <- rep("0", nrow(d))
  expect_equal(detect_survey_mode(d), "sms")        # web col all zero, no link
  d$web_complete[1] <- "1"
  expect_equal(detect_survey_mode(d), "t2w")        # at least one callback
})

test_that("detect_survey_mode: personalized close link + no web -> t2w_external", {
  d <- load_synthetic_data()
  d$web_complete <- rep("0", nrow(d))
  # Distinct per-respondent survey URLs (personalized -> t2w_external).
  d$id.close.scriptText <- sprintf(
    "Finish here https://survey.example.org/s?uid=%s", seq_len(nrow(d)))
  expect_equal(detect_survey_mode(d), "t2w_external")
})

test_that("detect_survey_mode: single static stimulus link -> sms", {
  d <- load_synthetic_data()
  d$web_complete <- rep("0", nrow(d))
  # Same URL for everyone (a stimulus video, not a personalized survey link).
  d$id.close.scriptText <- "Watch https://youtube.com/shorts/abc123"
  expect_equal(detect_survey_mode(d), "sms")
})

test_that("build_summary_frame: t2w completion counts web_complete, not close", {
  d <- load_synthetic_data(mutate = function(d) {
    # All three consenters reach `close` (fixture sets close.scriptDate), but
    # only r1 returned a web-completion callback. sms would count 3, t2w 1.
    d$web_complete <- rep("0", nrow(d))
    d$web_complete[1] <- "1"
    d
  })
  cfg <- synthetic_config()
  day_sms <- collapse_summary_to_day(build_summary_frame(d, cfg, "sms"))
  day_t2w <- collapse_summary_to_day(build_summary_frame(d, cfg, "t2w"))

  expect_equal(day_sms$n_completed, 3L)   # reached close
  expect_equal(day_t2w$n_completed, 1L)   # only r1 web_complete==1
  expect_equal(day_t2w$n_sent, 4L)      # texted/consented unaffected by mode
  expect_equal(day_t2w$n_opted_in, 3L)
})

test_that("build_summary_frame: t2w with no web_complete column -> 0 completed", {
  # Defensive path: if a caller passes survey_mode="t2w" but the data has no
  # web_complete column, completion is zero (not an error). detect_survey_mode
  # never produces this pairing, but build_summary_frame must not assume it.
  d <- load_synthetic_data()  # synthetic fixture has no web_complete column
  cfg <- synthetic_config()
  day <- collapse_summary_to_day(build_summary_frame(d, cfg, "t2w"))
  expect_equal(day$n_completed, 0L)
  expect_equal(day$n_sent, 4L)
})

test_that("latency_report stamps survey_mode on every consolidated row", {
  cfg <- synthetic_config()
  run_at <- as.POSIXct("2026-05-21", tz = "UTC")

  # sms (default): no web_complete column.
  cons_sms <- latency_report(load_synthetic_data(), cfg, run_at)$consolidated
  expect_true("survey_mode" %in% names(cons_sms))
  expect_equal(unique(cons_sms$survey_mode), "sms")
  expect_true(all(cons_sms[is.na(cons_sms$hour_local), ]$n_completed == 3L))

  # t2w: web_complete present with a callback; completion drops to web count.
  d_t2w <- load_synthetic_data(mutate = function(d) {
    d$web_complete <- rep("0", nrow(d))
    d$web_complete[1] <- "1"
    d
  })
  cons_t2w <- latency_report(d_t2w, cfg, run_at)$consolidated
  expect_equal(unique(cons_t2w$survey_mode), "t2w")
  expect_true(all(cons_t2w[is.na(cons_t2w$hour_local), ]$n_completed == 1L))
})

test_that("latency_report: t2w_external nulls n_completed to NA, keeps texted", {
  cfg <- synthetic_config()
  run_at <- as.POSIXct("2026-05-21", tz = "UTC")
  d <- load_synthetic_data(mutate = function(d) {
    d$web_complete <- rep("0", nrow(d))   # external platform, no webhook
    d$id.close.scriptText <- sprintf(
      "Finish here https://survey.example.org/s?uid=%s", seq_len(nrow(d)))
    d
  })
  cons <- latency_report(d, cfg, run_at)$consolidated
  expect_equal(unique(cons$survey_mode), "t2w_external")
  expect_true(all(is.na(cons$n_completed)))          # completion not computable
  day <- cons[is.na(cons$hour_local) & cons$threshold_min == 1L, ]
  expect_true(all(day$n_sent == 4L))               # texted/consented still valid
  expect_true(all(day$n_opted_in == 3L))
})

test_that("build_consolidated_scaffold: NA hour_local dedups (day-rollup grain)", {
  cfg <- synthetic_config()
  thresholds <- c(1L, 3L, 5L, 10L)
  # Both frames carry hour_local = NA (day rollup). unique.data.frame
  # must collapse these to a single row -- otherwise the day rollup
  # double-counts.
  bucketed <- data.frame(
    campaign_id = 1L,
    date = as.Date("2026-01-26"),
    hour_local = NA_integer_,
    stringsAsFactors = FALSE
  )
  summary <- data.frame(
    campaign_id = 1L,
    date = as.Date("2026-01-26"),
    hour_local = NA_integer_,
    n_sent = 10L, n_opted_in = 8L, n_completed = 6L,
    stringsAsFactors = FALSE
  )
  scaffold <- build_consolidated_scaffold(bucketed, summary, cfg, thresholds)
  # 1 bucket × 3 segments × 4 thresholds = 12, not 24
  expect_equal(nrow(scaffold), 12L)
  expect_true(all(is.na(scaffold$hour_local)))
})

test_that("aggregate_consolidated tolerates NULL summary/ineligible (defensive default)", {
  # Synthetic config + a single-respondent frame to exercise the
  # `is.null(summary_frame)` defaulting path. The frame still needs the
  # full latency-frame columns build_latency_frame() produces.
  d <- load_synthetic_data()
  cfg <- synthetic_config()
  data <- na_if_blank(d)
  data <- apply_population_filter(data, cfg$filters$population)
  parsed <- parse_timestamps(data, required_timestamp_columns(cfg$flow$questions))
  frame <- build_latency_frame(parsed$data, cfg, parsed$parse_failed_mask)
  cons <- aggregate_consolidated(frame, cfg, cfg_hash = "h",
                                 run_at = as.POSIXct("2026-05-21", tz = "UTC"))
  # NULL inputs default to empty frames; every summary count is then
  # filled with 0 in assemble_consolidated -- symmetric with n_ineligible.
  # "No summary row for this bucket" semantically means "no respondents
  # in this bucket" -> 0, not "unknown".
  expect_true(all(cons$n_sent == 0L))
  expect_true(all(cons$n_engaged == 0L))
  expect_true(all(cons$n_opted_in == 0L))
  expect_true(all(cons$n_completed == 0L))
  expect_true(all(cons$n_ineligible == 0L))
})
