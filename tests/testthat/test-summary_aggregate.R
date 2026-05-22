# Coverage for R/summary_primitives.R and R/summary_aggregate.R.
# Hand-computed counts against the synthetic fixtures + ineligible
# scenarios constructed inline.

test_that("build_summary_frame: counts on the synthetic fixture", {
  d <- load_synthetic_data()
  cfg <- synthetic_config()
  frame <- build_summary_frame(d, cfg)

  # Synthetic: r1@h16, r2@h17, r3@h15 all have intro.batchDate AND say "Yes"
  # AND reach close. r4 has no intro.batchDate (so it counts for nothing).
  expect_setequal(frame$hour_local, c(15L, 16L, 17L))
  expect_equal(unique(frame$campaign_id), 1L)
  expect_equal(unique(as.character(frame$date)), "2026-01-26")
  expect_true(all(frame$n_texted == 1L))
  expect_true(all(frame$n_consented == 1L))
  expect_true(all(frame$n_completed == 1L))
})

test_that("build_summary_frame: n_consented diverges from n_texted on 'No'", {
  d <- load_synthetic_data(mutate = function(d) {
    # Give r4 a batchDate so it counts as texted, but finalText stays "No"
    # so the population filter rejects it.
    d$id.intro.batchDate[4] <- "2026-01-26 22:30:00.000000Z"
    d
  })
  cfg <- synthetic_config()
  frame <- build_summary_frame(d, cfg)
  day <- collapse_summary_to_day(frame)

  expect_equal(day$n_texted, 4L)        # all 4 rows now have batchDate
  expect_equal(day$n_consented, 3L)     # 3 said "Yes", 1 said "No"
  expect_equal(day$n_completed, 3L)     # 3 reached close (No-respondent didn't)
})

test_that("build_summary_frame: zero rows returns empty schema", {
  cfg <- synthetic_config()
  d <- minimal_synthetic_data(with_rows = FALSE)
  frame <- build_summary_frame(d, cfg)
  expect_equal(nrow(frame), 0L)
  expect_named(frame, c("campaign_id", "date", "hour_local",
                        "n_texted", "n_consented", "n_completed"))
})

test_that("build_summary_frame: data without close.scriptDate column treats n_completed as zero", {
  cfg <- synthetic_config()
  d <- load_synthetic_data(mutate = function(d) {
    d$id.close.scriptDate <- NULL
    d
  })
  day <- collapse_summary_to_day(build_summary_frame(d, cfg))
  expect_equal(day$n_completed, 0L)
  expect_equal(day$n_texted, 3L)
})

test_that("build_summary_frame: all-zero rows yield empty frame", {
  cfg <- synthetic_config()
  d <- load_synthetic_data(mutate = function(d) {
    # Strip every column the summary keys off -- no row contributes.
    d$id.intro.batchDate <- ""
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
    n_texted = c(2L, 3L, 5L),
    n_consented = c(1L, 2L, 4L),
    n_completed = c(1L, 1L, 3L),
    stringsAsFactors = FALSE
  )
  day <- collapse_summary_to_day(hourly)
  expect_equal(nrow(day), 1L)
  expect_true(is.na(day$hour_local))
  expect_equal(day$n_texted, 10L)
  expect_equal(day$n_consented, 7L)
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
               "filters.population is not valid R")
  expect_error(population_filter_mask(d, "no_such_column == 1"),
               "filters.population evaluation failed")
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

test_that("campaign_report integrates summary columns into consolidated", {
  d <- load_synthetic_data()
  cfg <- synthetic_config()
  result <- campaign_report(d, cfg, run_at = as.POSIXct("2026-05-21", tz = "UTC"))
  cons <- result$consolidated

  # Day rollup row: 3 texted = 3 consented = 3 completed (synthetic.csv).
  day <- cons[is.na(cons$hour_local), ]
  expect_true(all(day$n_texted == 3L))
  expect_true(all(day$n_consented == 3L))
  expect_true(all(day$n_completed == 3L))
  expect_true(all(day$n_ineligible == 0L))

  # Hour rows: each of h15/h16/h17 has 1 texted/consented/completed.
  hr <- cons[!is.na(cons$hour_local), ]
  for (h in c(15L, 16L, 17L)) {
    cell <- hr[hr$hour_local == h, ]
    expect_true(all(cell$n_texted == 1L))
    expect_true(all(cell$n_consented == 1L))
    expect_true(all(cell$n_completed == 1L))
  }
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
  # NULL inputs default to empty frames; n_texted/etc become NA
  # via the left_join. n_ineligible is filled to 0 in assemble_consolidated.
  expect_true(all(is.na(cons$n_texted)))
  expect_true(all(cons$n_ineligible == 0L))
})
