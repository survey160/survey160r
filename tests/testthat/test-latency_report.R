# Coverage for R/latency_report.R: the pure latency_report() function.
# Uses the synthetic fixture under tests/testthat/fixtures/.

.load_synthetic <- function() {
  csv_path <- test_path("fixtures/synthetic.csv")
  data <- read.csv(csv_path, stringsAsFactors = FALSE, na.strings = c(""))
  # read.csv translates NA to NA, but we want empty strings in the original
  # cells to test na_if_blank end-to-end. Reread without na.strings.
  data <- read.csv(csv_path, stringsAsFactors = FALSE)
  list(data = data, config = synthetic_config())
}

test_that("latency_report honors an explicit run_at override", {
  fx <- .load_synthetic()
  fixed <- as.POSIXct("2026-01-01 12:00:00", tz = "UTC")
  out <- latency_report(fx$data, fx$config, run_at = fixed)
  expect_equal(out$meta$run_at_utc, fixed)
  expect_true(all(out$consolidated$run_at_utc == fixed))
})

test_that("latency_report tolerates an all-NA wave batchDate column", {
  # Degenerate input: every respondent's id.intro.batchDate is NA. The
  # chain-validity check cascades, so every downstream segment loses every
  # respondent. The report must still return a well-formed result frame
  # (correct schema, all-NA percentages) instead of crashing.
  fx <- .load_synthetic()
  fx$data$id.intro.batchDate <- NA_character_
  out <- latency_report(fx$data, fx$config)
  cons <- out$consolidated

  # The schema is intact and the per-(segment, threshold) skeleton still
  # populates day-rollup rows -- they're just all empty.
  day_rows <- cons[is.na(cons$hour_local), ]
  expect_true(nrow(day_rows) > 0L)
  expect_true(all(day_rows$n == 0L))
  expect_true(all(is.na(day_rows$pct_le) | is.nan(day_rows$pct_le)))
})

test_that("latency_report is deterministic on identical inputs", {
  fx <- .load_synthetic()
  r1 <- latency_report(fx$data, fx$config)
  r2 <- latency_report(fx$data, fx$config)
  # Strip run_at_utc (clock-dependent) before comparison.
  r1$consolidated$run_at_utc <- NULL
  r2$consolidated$run_at_utc <- NULL
  r1$meta$run_at_utc <- NULL
  r2$meta$run_at_utc <- NULL
  expect_identical(r1$consolidated, r2$consolidated)
  expect_identical(r1$meta$config_hash, r2$meta$config_hash)
  expect_identical(r1$diagnostics$config_hash, r2$diagnostics$config_hash)
})

test_that("latency_report drops respondents excluded by population_filter", {
  fx <- .load_synthetic()
  result <- latency_report(fx$data, fx$config)
  # 4 rows in CSV; 1 has finalText="No". Should yield 3 respondents in.
  expect_equal(result$diagnostics$n_respondents_in, 3L)
})

test_that("latency_report consolidated has hour and day rollup rows", {
  fx <- .load_synthetic()
  result <- latency_report(fx$data, fx$config)
  cons <- result$consolidated

  # Fixture: 3 respondents (r1, r2, r3) with intro.batchDate at distinct
  # UTC hours that map to 3 distinct ET hours (the configured field_tz).
  # Hour-grain rows: 3 hours x 3 segments x 4 thresholds = 36.
  # Day-rollup rows: 1 date x 3 segments x 4 thresholds = 12.
  # r3 (UTC 20:00 -> ET 15:00) is out of texting window 16-24.
  hour_rows <- cons[!is.na(cons$hour_local), ]
  day_rows  <- cons[is.na(cons$hour_local), ]
  expect_equal(nrow(hour_rows), 36L)
  expect_equal(nrow(day_rows), 12L)

  expect_setequal(unique(cons$segment),
                  c("intro→q1", "q1→q2", "q2→close"))
  expect_setequal(unique(cons$threshold_min), c(1L, 3L, 5L, 10L))
  expect_setequal(unique(hour_rows$hour_local), c(15L, 16L, 17L))
  expect_equal(unique(as.character(cons$date)), "2026-01-26")
  expect_equal(unique(cons$campaign_id), 1L)
  expect_equal(unique(cons$project_id), 1L)
  expect_equal(unique(cons$algorithm_version), "2.1.0")

  # n per hour cell is 1 (the single respondent in that hour). All three
  # respondents now count -- there's no texting-window exclusion.
  expect_true(all(hour_rows$n == 1L))

  # pct_le per single-respondent hour cell.
  r2_q1q2_t1 <- hour_rows[hour_rows$segment == "q1→q2" &
                            hour_rows$threshold_min == 1L &
                            hour_rows$hour_local == 17L, ]
  expect_equal(r2_q1q2_t1$pct_le, 0)         # r2's q1->q2 is 4 min > 1 min
  r2_q1q2_t5 <- hour_rows[hour_rows$segment == "q1→q2" &
                            hour_rows$threshold_min == 5L &
                            hour_rows$hour_local == 17L, ]
  expect_equal(r2_q1q2_t5$pct_le, 100)       # 4 min <= 5 min

  # Day rollup row: n is sum of hour cells (3 = r1 + r2 + r3). pct_le for
  # q1->q2 at threshold 1 is 2/3 (r1 0.5 min, r3 ~ms, r2 4 min).
  day_q1q2_t1 <- day_rows[day_rows$segment == "q1→q2" &
                            day_rows$threshold_min == 1L, ]
  expect_equal(day_q1q2_t1$n, 3L)
  expect_equal(round(day_q1q2_t1$pct_le, 4), round(200 / 3, 4))
})

test_that("hour-rollup of pct_le matches the day rows in the same output", {
  fx <- .load_synthetic()
  result <- latency_report(fx$data, fx$config)
  cons <- result$consolidated
  hour_rows <- cons[!is.na(cons$hour_local), ]
  day_rows  <- cons[is.na(cons$hour_local), ]

  rolled <- dplyr::summarise(
    dplyr::group_by(hour_rows, .data$segment, .data$threshold_min),
    pct_le_rolled = ifelse(sum(.data$n) > 0,
                           sum(.data$pct_le * .data$n, na.rm = TRUE) /
                             sum(.data$n),
                           NA_real_),
    n_rolled = sum(.data$n),
    .groups = "drop"
  )
  merged <- merge(
    day_rows[, c("segment", "threshold_min", "n", "pct_le")],
    rolled,
    by = c("segment", "threshold_min")
  )
  expect_equal(merged$n, merged$n_rolled, tolerance = 0)
  expect_equal(merged$pct_le, merged$pct_le_rolled, tolerance = 1e-9)
})

test_that("cascade columns are present and per-hour-respondent", {
  fx <- .load_synthetic()
  result <- latency_report(fx$data, fx$config)
  cons <- result$consolidated
  hour_rows <- cons[!is.na(cons$hour_local), ]
  day_rows  <- cons[is.na(cons$hour_local), ]

  # Hour grain: each of the 3 in-fixture respondents occupies a distinct
  # hour, so each hour bucket has exactly one respondent.
  expect_equal(unique(hour_rows$n_respondents), 1L)
  # Day rollup carries the wave-level distinct count: all 3 respondents.
  expect_equal(unique(day_rows$n_respondents), 3L)

  # Hour 17 (r2, worst delta = 4 min): pct_resp_worst_gt > 0 for
  # thresholds 1 and 3; 0 for 5 and 10.
  h17 <- unique(hour_rows[hour_rows$hour_local == 17L,
                     c("threshold_min", "pct_resp_worst_gt")])
  h17 <- h17[order(h17$threshold_min), ]
  expect_equal(h17$pct_resp_worst_gt, c(100, 100, 0, 0))

  # Hour 15 (r3): r3's worst delta is 0.5 min, so pct_resp_worst_gt is 0
  # at every threshold.
  h15 <- unique(hour_rows[hour_rows$hour_local == 15L,
                     c("threshold_min", "pct_resp_worst_gt")])
  expect_true(all(h15$pct_resp_worst_gt == 0))
})

test_that("latency_report dedupes by respondent_id keeping earliest intro", {
  fx <- .load_synthetic()
  # Add a duplicate of r1 with a later intro.scriptDate (should be dropped).
  data <- fx$data
  dup <- data[data$userid == "r1", ]
  dup$id.intro.scriptDate <- "2026-01-26 23:00:00.000000Z"
  dup$id.intro.batchDate <- "2026-01-26 23:00:30.000000Z"
  dup$id.q1.scriptDate <- "2026-01-26 23:01:00.000000Z"
  data <- rbind(data, dup)
  result <- latency_report(data, fx$config)
  expect_equal(result$diagnostics$n_respondents_used, 3L)
})

test_that("latency_report preserves summary metrics when no respondents pass filter", {
  fx <- .load_synthetic()
  data <- fx$data
  data$id.intro.finalText <- "No"
  result <- latency_report(data, fx$config)
  cons <- result$consolidated
  # Scaffold-first seeding (post-CodeRabbit-PR26): summary-only buckets
  # still appear in the output so the "we texted N but nobody consented"
  # denominator isn't lost. Latency cell counts are 0 across the board.
  expect_gt(nrow(cons), 0L)
  expect_true(all(cons$n_consented == 0L))
  expect_true(any(cons$n_texted >= 1L))
  expect_true(all(cons$n == 0L))
  expect_true(all(is.na(cons$pct_le)))
  # SUR-1365: an all-NA pct_le must stay a double, not collapse to logical.
  # The fleet writer casts this column to a float64 Arrow schema; a logical
  # NA vector fails that cast ("Invalid: cannot convert") and drops the
  # campaign's Parquet output.
  expect_type(cons$pct_le, "double")
  expect_equal(result$diagnostics$n_respondents_in, 0L)
})

test_that("latency_report emits both hour rows (0-23) and a day rollup row (NA) per cell", {
  fx <- .load_synthetic()
  cons <- latency_report(fx$data, fx$config)$consolidated
  expect_true(any(!is.na(cons$hour_local)),
              info = "expect at least one hour row")
  expect_true(any(is.na(cons$hour_local)),
              info = "expect at least one day rollup row")
  # Hour rows carry integer 0-23; day rows carry NA.
  expect_true(all(cons$hour_local[!is.na(cons$hour_local)] %in% 0:23))
})

test_that("latency_report's date_filter restricts to listed dates", {
  fx <- .load_synthetic()
  cfg <- fx$config
  cfg$filters$date_filter <- as.Date("2099-01-01")
  result <- latency_report(fx$data, cfg)
  expect_equal(nrow(result$consolidated), 0L)
})

test_that("latency_report errors on invalid population_filter", {
  fx <- .load_synthetic()
  cfg <- fx$config
  cfg$filters$population <- "this is not valid R syntax !!!"
  expect_error(latency_report(fx$data, cfg), "filters.population")
})

test_that("latency_report threads source_csv_hash from input attribute to consolidated and meta", {
  fx <- .load_synthetic()
  data <- fx$data
  attr(data, "source_csv_hash") <- "sha256:fixture-from-attr"
  attr(data, "source_csv_path") <- "gs://b/1/1_raw_data_download.csv"
  result <- latency_report(data, fx$config)
  expect_equal(unique(result$consolidated$source_csv_hash),
               "sha256:fixture-from-attr")
  expect_equal(result$meta$source_csv_hash, "sha256:fixture-from-attr")
  expect_equal(result$meta$source_csv_path,
               "gs://b/1/1_raw_data_download.csv")
})

test_that("latency_report leaves source_csv_hash NA when no input attribute", {
  fx <- .load_synthetic()
  result <- latency_report(fx$data, fx$config)
  expect_true(all(is.na(result$consolidated$source_csv_hash)))
  expect_true(is.na(result$meta$source_csv_hash))
  expect_true(is.na(result$meta$source_csv_path))
})

test_that("diagnostics na_by_reason: parse_failure counted on garbage timestamps", {
  fx <- .load_synthetic()
  data <- fx$data
  # Corrupt one batchDate cell with non-blank garbage on r1's q1 segment.
  data$id.q1.batchDate[data$userid == "r1"] <- "not-a-date"
  result <- latency_report(data, fx$config)
  reasons <- result$diagnostics$n_segments_na_by_reason
  # The q1->q2 segment for r1 should be classified parse_failure.
  expect_gte(reasons$parse_failure, 1L)
  # Conservation: sums of all reasons == total NA segments.
  total <- reasons$parse_failure + reasons$missing_endpoint + reasons$chain_break
  expect_equal(total, result$diagnostics$n_segments_na)
})

test_that("diagnostics na_by_reason: missing_endpoint counted on blank batchDate", {
  fx <- .load_synthetic()
  data <- fx$data
  # Blank out r2's q1.batchDate. Both endpoint cells were originally valid, so
  # this is a legitimate "respondent didn't advance" miss, not a parse failure.
  data$id.q1.batchDate[data$userid == "r2"] <- ""
  result <- latency_report(data, fx$config)
  reasons <- result$diagnostics$n_segments_na_by_reason
  expect_gte(reasons$missing_endpoint, 1L)
  expect_equal(reasons$parse_failure, 0L)
  total <- reasons$parse_failure + reasons$missing_endpoint + reasons$chain_break
  expect_equal(total, result$diagnostics$n_segments_na)
})

test_that("diagnostics na_by_reason: chain_break counted when a prior batchDate is NA", {
  fx <- .load_synthetic()
  data <- fx$data
  # Blank intro.batchDate for r1. Subsequent segments q1->q2 and q2->close keep
  # both endpoints valid, but chain validity nukes them because a prior
  # batchDate in the chain is NA. The intro->q1 segment itself is
  # missing_endpoint (its batch_prior is intro.batchDate which is now NA).
  data$id.intro.batchDate[data$userid == "r1"] <- ""
  result <- latency_report(data, fx$config)
  reasons <- result$diagnostics$n_segments_na_by_reason
  expect_gte(reasons$chain_break, 1L)
  expect_gte(reasons$missing_endpoint, 1L)
  expect_equal(reasons$parse_failure, 0L)
  total <- reasons$parse_failure + reasons$missing_endpoint + reasons$chain_break
  expect_equal(total, result$diagnostics$n_segments_na)
})

test_that("latency_report negative-clamp counter is exposed in diagnostics", {
  fx <- .load_synthetic()
  data <- fx$data
  # Force a negative on r1's q1->q2: set q2.scriptDate before q1.batchDate.
  # Scale carefully so flow_order check still passes (we only need <10% bad).
  data <- rbind(data, data[1, ])
  data$userid[nrow(data)] <- "r5"
  data$id.q2.scriptDate[nrow(data)] <- "2026-01-26 21:00:50.000000Z"
  result <- latency_report(data, fx$config)
  expect_gte(result$diagnostics$n_negative_latencies_clamped, 1L)
})

test_that("latency_report consolidated carries the new distribution and NA-reason columns", {
  fx <- .load_synthetic()
  cons <- latency_report(fx$data, fx$config)$consolidated
  expect_true(all(c("mean_delta_min", "p50_delta_min", "p90_delta_min",
                    "p95_delta_min", "n_na_parse", "n_na_missing",
                    "n_na_chain") %in% names(cons)))
  expect_type(cons$mean_delta_min, "double")
  expect_type(cons$p50_delta_min, "double")
  expect_type(cons$p90_delta_min, "double")
  expect_type(cons$p95_delta_min, "double")
  expect_type(cons$n_na_parse, "integer")
  expect_type(cons$n_na_missing, "integer")
  expect_type(cons$n_na_chain, "integer")
})

test_that("distribution columns are threshold-independent within a cell", {
  # The four threshold rows of the same (date, hour_local, segment) cell
  # must carry identical mean/p50/p90/p95 values -- they are properties of
  # the Δ vector, not of any threshold. Guards against a future refactor
  # accidentally folding the quantile call inside a threshold-dependent
  # filter.
  fx <- .load_synthetic()
  cons <- latency_report(fx$data, fx$config)$consolidated
  per_cell <- dplyr::summarise(
    dplyr::group_by(cons, .data$date, .data$hour_local, .data$segment),
    distinct_p50 = dplyr::n_distinct(.data$p50_delta_min),
    distinct_p95 = dplyr::n_distinct(.data$p95_delta_min),
    distinct_mean = dplyr::n_distinct(.data$mean_delta_min),
    .groups = "drop"
  )
  expect_true(all(per_cell$distinct_p50 == 1L))
  expect_true(all(per_cell$distinct_p95 == 1L))
  expect_true(all(per_cell$distinct_mean == 1L))
})

test_that("single-respondent cell has p50 == p95 == mean equal to the Δ", {
  # In the clean synthetic fixture each hour cell has exactly one
  # respondent, so p50 / p90 / p95 / mean must all collapse to the same
  # value -- the respondent's Δ for that segment. Hour 21 ET corresponds
  # to r1's intro->q1 batch_prior; intro.batchDate=21:00:30 -> q1.scriptDate
  # =21:00:40, Δ = 10s = 1/6 min.
  fx <- .load_synthetic()
  cons <- latency_report(fx$data, fx$config)$consolidated
  r1_intro_q1 <- cons[!is.na(cons$hour_local) &
                        cons$hour_local == 16L &
                        cons$segment == "intro→q1" &
                        cons$threshold_min == 1L, ]
  expect_equal(nrow(r1_intro_q1), 1L)
  expected <- 10 / 60
  expect_equal(r1_intro_q1$mean_delta_min, expected, tolerance = 1e-9)
  expect_equal(r1_intro_q1$p50_delta_min, expected, tolerance = 1e-9)
  expect_equal(r1_intro_q1$p95_delta_min, expected, tolerance = 1e-9)
})

test_that("day rollup quantiles span the full Δ distribution across hours", {
  # The day rollup row aggregates every respondent's segment Δs for the
  # day, so p95 should be the largest Δ in the segment (or close to it).
  # For q1->q2: r1=0.5min, r3≈0.5min, r2=4min. p95 should be ~4min.
  fx <- .load_synthetic()
  cons <- latency_report(fx$data, fx$config)$consolidated
  day_q1q2 <- cons[is.na(cons$hour_local) &
                     cons$segment == "q1→q2" &
                     cons$threshold_min == 1L, ]
  expect_equal(nrow(day_q1q2), 1L)
  expect_equal(day_q1q2$p95_delta_min, 4, tolerance = 0.5)
  # Mean over the three Δs (~0.5, ~0.5, 4) is between p50 and p95.
  expect_gt(day_q1q2$mean_delta_min, day_q1q2$p50_delta_min)
  expect_lte(day_q1q2$mean_delta_min, day_q1q2$p95_delta_min)
})

test_that("n_na_parse counts parse failures in the matching cell only", {
  # Inject a garbage timestamp on r1's q1.batchDate. q1.batchDate is the
  # batch_prior endpoint for the q1->q2 segment, so that segment's
  # hour_local becomes NA (the hour is derived from batch_prior, which is
  # unparseable). The parse_failure row therefore lands in the
  # hour_local IS NA partition of q1->q2 -- the day-rollup row (the hour
  # pass drops its own NA-hour rows so the unknown-time bucket is not
  # double-counted; see the grain-uniqueness test below).
  fx <- .load_synthetic()
  data <- fx$data
  data$id.q1.batchDate[data$userid == "r1"] <- "not-a-date"
  cons <- latency_report(data, fx$config)$consolidated
  q1q2_t1 <- cons[cons$segment == "q1→q2" & cons$threshold_min == 1L, ]
  expect_gte(sum(q1q2_t1$n_na_parse), 1L)
  expect_equal(sum(q1q2_t1$n_na_missing), 0L)
  expect_equal(sum(q1q2_t1$n_na_chain), 0L)
  # The unaffected hour cells for other respondents still record zero
  # parse failures.
  r2_cell <- cons[!is.na(cons$hour_local) &
                    cons$hour_local == 17L &
                    cons$segment == "q1→q2" &
                    cons$threshold_min == 1L, ]
  expect_equal(nrow(r2_cell), 1L)
  expect_equal(r2_cell$n_na_parse, 0L)
})

test_that("n_na_missing counts blank-endpoint NAs in the matching cell", {
  # Blank r2's q1.batchDate. Affected segment: q1->q2. The blanked
  # batch_prior nukes hour_local for that segment row, so the
  # missing_endpoint count lands in the hour_local IS NA partition.
  fx <- .load_synthetic()
  data <- fx$data
  data$id.q1.batchDate[data$userid == "r2"] <- ""
  cons <- latency_report(data, fx$config)$consolidated
  q1q2_t1 <- cons[cons$segment == "q1→q2" & cons$threshold_min == 1L, ]
  expect_gte(sum(q1q2_t1$n_na_missing), 1L)
  expect_equal(sum(q1q2_t1$n_na_parse), 0L)
  expect_equal(sum(q1q2_t1$n_na_chain), 0L)
  # r1's cell (hour 16 ET) is unaffected.
  r1_cell <- cons[!is.na(cons$hour_local) &
                    cons$hour_local == 16L &
                    cons$segment == "q1→q2" &
                    cons$threshold_min == 1L, ]
  expect_equal(nrow(r1_cell), 1L)
  expect_equal(r1_cell$n_na_missing, 0L)
})

test_that("n_na_chain counts chain-break NAs on segments after an NA prior batchDate", {
  # Blank r1's intro.batchDate. The intro->q1 segment itself becomes
  # missing_endpoint (its own batch_prior endpoint is NA), and the
  # downstream q1->q2 / q2->close segments become chain_break (their own
  # endpoints are clean but a strictly-prior batchDate is NA). Check the
  # chain_break cell for q1->q2.
  fx <- .load_synthetic()
  data <- fx$data
  data$id.intro.batchDate[data$userid == "r1"] <- ""
  cons <- latency_report(data, fx$config)$consolidated
  cell <- cons[!is.na(cons$hour_local) &
                 cons$hour_local == 16L &
                 cons$segment == "q1→q2" &
                 cons$threshold_min == 1L, ]
  expect_equal(nrow(cell), 1L)
  expect_gte(cell$n_na_chain, 1L)
  expect_equal(cell$n_na_parse, 0L)
})

test_that("consolidated grain is unique when a segment drops off mid-flow", {
  # Regression for the duplicated day-rollup grain (C2). A blank/unparseable
  # mid-flow batchDate makes a segment's segment_date_local -- and hence its
  # hour_local, derived from the same batch_prior -- NA. Both the hour pass
  # and the day pass then emit the identical
  # (campaign, date=NA, hour_local=NA, segment, threshold) key, so a naive
  # rbind double-counts it. The (hour=NULL) unknown-time bucket belongs to the
  # day partition only; the consolidated grain must stay unique.
  fx <- .load_synthetic()
  data <- fx$data
  data$id.q1.batchDate[data$userid == "r2"] <- ""  # r2 drops off before q1
  cons <- latency_report(data, fx$config)$consolidated

  key <- cons[, c("campaign_id", "project_id", "date", "hour_local",
                  "segment", "threshold_min")]
  expect_identical(anyDuplicated(key), 0L)

  # The affected q1->q2 segment's unknown-time drop-off still lands in the
  # day-rollup (hour NA) partition exactly once per threshold -- neither
  # dropped by the NA-hour filter nor duplicated across the two passes.
  day_rows <- cons[is.na(cons$hour_local), ]
  expect_gt(nrow(day_rows), 0L)
  unknown_q1q2 <- day_rows[is.na(day_rows$date) &
                             day_rows$segment == "q1→q2", , drop = FALSE]
  expect_equal(nrow(unknown_q1q2), 4L)
  expect_setequal(unknown_q1q2$threshold_min, c(1L, 3L, 5L, 10L))
})

test_that("consolidated declares the new columns with the right types when latency is empty", {
  fx <- .load_synthetic()
  data <- fx$data
  data$id.intro.finalText <- "No"
  cons <- latency_report(data, fx$config)$consolidated
  # Scaffold path: rows come from summary buckets, latency cols are
  # 0 (counts) / NA (means + quantiles). Schema must still declare every
  # documented column with its declared type.
  expect_gt(nrow(cons), 0L)
  expect_true(all(c("mean_delta_min", "p50_delta_min", "p90_delta_min",
                    "p95_delta_min", "n_na_parse", "n_na_missing",
                    "n_na_chain", "n_texted", "n_consented", "n_completed",
                    "n_ineligible") %in% names(cons)))
  expect_type(cons$p95_delta_min, "double")
  expect_type(cons$n_na_chain, "integer")
  expect_type(cons$n_texted, "integer")
})
