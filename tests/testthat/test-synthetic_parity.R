# Property / parity checks on a generated campaign (helper-synthetic.R): the
# fast paths the hot-path tuning introduced must give the same answer as the
# straightforward forms they replaced, on data big enough to exercise
# drop-off, blank replies, parse failures and a routed (bilingual) opener.

run_at <- as.POSIXct("2026-01-01", tz = "UTC")

test_that("full and compact latency paths are byte-identical on a synthetic campaign", {
  export <- synthetic_export(n = 1500L, n_questions = 7L, bilingual = TRUE)
  full <- latency_run(1980, export, field_timezone = "America/New_York",
                      respondent_id_column = "phone", run_at = run_at)
  compact <- latency_run(1980, export, field_timezone = "America/New_York",
                         respondent_id_column = "phone", run_at = run_at, compact = TRUE)
  expect_identical(full$consolidated, compact$consolidated)
  expect_identical(full$diagnostics, compact$diagnostics)
  expect_identical(full$meta, compact$meta)
  # The compact frame is the full frame's real-date rows, in the same order.
  real <- full$latency_frame[!is.na(full$latency_frame$segment_date_local), , drop = FALSE]
  rownames(real) <- NULL
  attr(real, "n_clamped") <- attr(full$latency_frame, "n_clamped")
  expect_identical(compact$latency_frame, real)
  # Something actually happened: parse failures were counted, drop-off produced
  # NA segments, and the routed opener's cohort was measured.
  expect_gt(full$diagnostics$n_segments_na_by_reason$parse_failure, 0L)
  expect_gt(full$diagnostics$n_segments_na_by_reason$missing_endpoint, 0L)
  expect_gt(sum(full$consolidated$n_sent[is.na(full$consolidated$hour_local)]), 0L)
})

test_that("string and POSIXct timestamp inputs give identical latency and disposition results", {
  export <- synthetic_export(n = 800L, n_questions = 5L)
  parsed <- synthetic_parse_timestamps(export)
  expect_s3_class(parsed$id.intro.scriptDate, "POSIXct")
  expect_type(export$id.intro.scriptDate, "character")
  a <- latency_run(1980, export, field_timezone = "America/Chicago",
                   respondent_id_column = "phone", run_at = run_at)
  b <- latency_run(1980, parsed, field_timezone = "America/Chicago",
                   respondent_id_column = "phone", run_at = run_at)
  expect_identical(a, b)
  expect_identical(disposition_run(1980, export), disposition_run(1980, parsed))
  expect_identical(question_funnel(export, c("intro", "q1", "close")),
                   question_funnel(parsed, c("intro", "q1", "close")))
})

test_that("a population filter and a date filter give the same answer on both latency paths", {
  export <- synthetic_export(n = 1000L, n_questions = 5L)
  config <- latency_build_config(1980, export, field_timezone = "America/New_York",
                                 date_filter = c("2026-03-02", "2026-03-03"))
  config$filters$population <- 'id.intro.finalText == "Yes" & id.q1.finalText == "Yes"'
  full <- latency_report(export, config, run_at = run_at)
  compact <- latency_report(export, config, run_at = run_at, compact = TRUE)
  expect_identical(full$consolidated, compact$consolidated)
  expect_identical(full$diagnostics, compact$diagnostics)
  # The date filter keys on the opener SEND date: the summary counts only
  # exist on the filtered dates (a segment's own date can land on a later day,
  # so the latency buckets are not bounded the same way).
  sent_dates <- full$consolidated$date[full$consolidated$n_sent > 0L]
  expect_true(all(sent_dates %in% as.Date(c("2026-03-02", "2026-03-03"))))
  expect_gt(length(sent_dates), 0L)
  expect_lt(full$diagnostics$n_respondents_in, nrow(export))
})

test_that(".local_date_hour matches the format()-based conversion across zones and DST edges", {
  set.seed(3)
  x <- as.POSIXct("2026-01-01", tz = "UTC") + stats::runif(5000, 0, 400 * 86400)
  x[sample(5000, 50)] <- NA
  # Exact DST transition instants for America/New_York (2026-03-08 / 2026-11-01).
  x <- c(x, as.POSIXct(c("2026-03-08 06:59:59.5", "2026-03-08 07:00:00",
                         "2026-11-01 05:59:59.9", "2026-11-01 06:00:00"), tz = "UTC"))
  for (tz in c("America/New_York", "UTC", "Australia/Lord_Howe", "Asia/Kolkata",
               "Pacific/Chatham")) {
    expected <- list(date = as.Date(format(x, tz = tz)),
                     hour = as.integer(format(x, format = "%H", tz = tz)))
    expect_identical(survey160r:::.local_date_hour(x, tz), expected, info = tz)
  }
})

test_that(".chain_break_mask reproduces apply_chain_validity segment by segment", {
  set.seed(9)
  n <- 200L
  priors <- lapply(1:5, function(i) {
    p <- as.POSIXct("2026-03-01", tz = "UTC") + stats::runif(n, 0, 3600)
    p[sample(n, 20L)] <- NA
    p
  })
  delta <- stats::runif(n, 0, 30)
  running <- NULL
  for (i in seq_along(priors)) {
    expected <- survey160r:::apply_chain_validity(delta, priors[seq_len(i - 1L)])
    actual <- delta
    if (!is.null(running)) actual[running] <- NA_real_
    expect_identical(actual, expected, info = i)
    running <- survey160r:::.chain_break_mask(running, priors[[i]])
    expect_identical(running, Reduce(`|`, lapply(priors[seq_len(i)], is.na)))
  }
})

test_that(".worst_delta_by_respondent agrees with a split/max over valid rows", {
  export <- synthetic_export(n = 400L, n_questions = 5L)
  frame <- latency_run(1980, export, field_timezone = "UTC", run_at = run_at)$latency_frame
  valid <- !is.na(frame$delta_min)
  expected <- unname(vapply(split(frame$delta_min[valid], frame$respondent_index[valid]),
                            max, numeric(1)))
  actual <- survey160r:::.worst_delta_by_respondent(frame$respondent_index, frame$delta_min)
  expect_equal(sort(actual), sort(expected))
  expect_identical(survey160r:::.worst_delta_by_respondent(1:3, c(NA, NA, NA)), numeric(0))
})

test_that("the cascade on an all-NA frame is silent and empty", {
  frame <- data.frame(
    respondent_index = 1:4, campaign_id = 1L, segment = "a→b", segment_index = 1L,
    delta_min = NA_real_, segment_date_local = as.Date("2026-03-01"), hour_local = 10L,
    na_reason = "missing_endpoint", stringsAsFactors = FALSE
  )
  frame$date <- frame$segment_date_local
  dt <- data.table::as.data.table(frame)
  expect_no_warning(cascade <- survey160r:::aggregate_worst_cascade(dt, c(1L, 3L, 5L, 10L)))
  expect_equal(nrow(cascade), 0L)
  expect_named(cascade, c("campaign_id", "date", "hour_local", "threshold_min",
                          "n_respondents", "pct_resp_worst_gt"))
})

test_that(".collapse_duplicate_rows drops exact duplicates only, keeping provenance", {
  d <- data.frame(phone = c("1", "2", "2", "3", "3"), v = c("a", "b", "b", "c", "d"),
                  stringsAsFactors = FALSE)
  attr(d, "source_csv_hash") <- "sha256:x"
  attr(d, "source_csv_path") <- "p"
  out <- survey160r:::.collapse_duplicate_rows(d)
  expect_equal(out$phone, c("1", "2", "3", "3"))
  expect_equal(out$v, c("a", "b", "c", "d"))
  expect_equal(attr(out, "source_csv_hash"), "sha256:x")
  expect_equal(attr(out, "source_csv_path"), "p")
  # No recurring phone -> the input is returned as-is (no copy, no attr loss).
  unique_phones <- data.frame(phone = c("1", "2"), v = c("a", "b"), stringsAsFactors = FALSE)
  expect_identical(survey160r:::.collapse_duplicate_rows(unique_phones), unique_phones)
  # Recurring phone whose rows differ -> nothing collapses (the grain guard decides).
  conflict <- data.frame(phone = c("1", "1"), v = c("a", "b"), stringsAsFactors = FALSE)
  expect_identical(survey160r:::.collapse_duplicate_rows(conflict), conflict)
  # NA phones compare equal, as duplicated() treats them.
  na_phone <- data.frame(phone = c(NA, NA), v = c("a", "a"), stringsAsFactors = FALSE)
  expect_equal(nrow(survey160r:::.collapse_duplicate_rows(na_phone)), 1L)
})

test_that("disposition_summary on a synthetic projection matches a per-phone recomputation", {
  d <- synthetic_disposition(n_rows = 3000L, n_phones = 1200L)
  summ <- disposition_summary(d)
  expect_equal(nrow(summ), length(unique(d$phone)))
  expect_equal(summ$phone, sort(unique(d$phone), method = "radix"))
  # Spot-check a handful of phones against a direct computation.
  for (p in summ$phone[c(1L, 7L, 100L, nrow(summ))]) {
    rows <- d[d$phone == p, , drop = FALSE]
    s <- summ[summ$phone == p, ]
    expect_equal(s$n_campaigns, length(unique(rows$campaign_id)), info = p)
    expect_equal(s$campaigns, paste(sort(unique(rows$campaign_id)), collapse = ","), info = p)
    expect_equal(s$n_engaged, sum(rows$engaged == 1L, na.rm = TRUE), info = p)
    expect_equal(s$n_error, sum(!is.na(rows$error)), info = p)
    expect_equal(s$first_disposition_date, min(rows$disposition_date), info = p)
    expect_equal(s$last_disposition_date, max(rows$disposition_date), info = p)
    latest <- rows[order(-as.numeric(rows$disposition_date), -rows$campaign_id), ][1L, ]
    expect_equal(s$latest_campaign_id, as.character(latest$campaign_id), info = p)
  }
})

test_that("the synthetic generators produce a well-formed export and projection", {
  export <- synthetic_export(n = 300L, n_questions = 4L, bilingual = TRUE)
  expect_equal(nrow(export), 300L)
  expect_false(anyDuplicated(export$phone) > 0L)
  expect_true(all(grepl("^[0-9]{10}$", export$phone)))
  expect_setequal(latency_discover_questions(export), c("intro", "q1", "q2", "close", "intro_sp"))
  ts <- parse_campaign_timestamps(export$id.intro.scriptDate)
  expect_equal(sum(is.na(ts)), sum(export$id.intro.scriptDate == ""))
  expect_true(any(export$id.q1.scriptDate == "not a timestamp"))
  expect_identical(synthetic_export(n = 50L), synthetic_export(n = 50L))
  d <- synthetic_disposition(n_rows = 500L, n_phones = 100L)
  expect_equal(nrow(d), 500L)
  expect_lte(length(unique(d$phone)), 100L)
  expect_true(all(is.na(d$completed[d$campaign_id == 1001L])))
})
