# completion_forecast(): one planned survey scored from its sample and design.

fit_fixture_model <- function() {
  p <- write_completion_parquet(completion_records(n_per_campaign = 60L))
  list(path = p, model = completion_model_fit(p, train_until = as.Date("2026-10-01"), min_sent = 10))
}

forecast_columns <- c(
  "optin_rate", "optin_lo50", "optin_hi50", "optin_lo95", "optin_hi95", "optins",
  "optins_lo95", "optins_hi95", "completion_rate", "completion_lo50", "completion_hi50",
  "completion_lo95", "completion_hi95", "completes", "completes_lo95", "completes_hi95",
  "n_phones", "share_with_history", "sample_score", "sample_multiplier",
  "survey_multiplier", "warnings")

test_that("forecast: one row with rate, bands, completes and drivers", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  r <- completion_records()
  phones <- unique(r$phone[r$campaign_id %in% 9:10])
  out <- completion_forecast(f$model, phones, mode = "T2W", topic = "Brand", loi = 12,
                             dataset = f$path, start_date = as.Date("2026-11-15"))
  expect_equal(nrow(out), 1L)
  expect_named(out, forecast_columns)
  expect_equal(out$n_phones, length(phones))
  expect_true(out$completion_rate > 0 && out$completion_rate < 1)
  expect_true(out$optin_rate > 0 && out$optin_rate < 1)
  expect_true(out$completion_lo95 <= out$completion_lo50 && out$completion_lo50 <= out$completion_rate &&
                out$completion_rate <= out$completion_hi50 && out$completion_hi50 <= out$completion_hi95)
  expect_true(out$optin_lo95 < out$optin_rate && out$optin_rate < out$optin_hi95)
  expect_equal(out$completes, round(out$completion_rate * out$n_phones))
  expect_equal(out$completes_lo95, round(out$completion_lo95 * out$n_phones))
  expect_equal(out$optins, round(out$optin_rate * out$n_phones))
  expect_equal(out$optins_hi95, round(out$optin_hi95 * out$n_phones))
  expect_true(out$share_with_history > 0 && out$share_with_history <= 1)
  expect_true(out$sample_multiplier > 0 && out$survey_multiplier > 0)
  expect_equal(out$warnings[[1]], character(0))
  # the odds-scale band: hi95 / rate on the odds scale is exp(1.96 * sd of the mode)
  odds <- function(p) p / (1 - p)
  sd_t2w <- f$model$models$completion$residual_sd[["T2W"]]
  expect_equal(odds(out$completion_hi95) / odds(out$completion_rate), exp(1.96 * sd_t2w))
  expect_equal(odds(out$completion_lo50) / odds(out$completion_rate), exp(-0.674 * sd_t2w))
})

test_that("forecast: accepts a date string and defaults start_date to today", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  ph <- unique(completion_records()$phone)[1:20]
  a <- completion_forecast(f$model, ph, "Live", "Brand", 12, f$path, start_date = "2026-11-15")
  b <- completion_forecast(f$model, ph, "Live", "Brand", 12, f$path)
  expect_equal(a$completion_rate, b$completion_rate)
})

test_that("forecast: never-contacted sample", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  out <- completion_forecast(f$model, c("3015550001", "3015550002", "3015550002"), mode = "Live",
                             topic = "Policy", loi = 8, dataset = f$path,
                             start_date = as.Date("2026-11-15"))
  expect_equal(out$n_phones, 2L)
  expect_equal(out$share_with_history, 0)
  first <- f$model$contexts
  first <- first$p_complete[first$context == "___" & first$year == 2026L]
  expect_equal(out$sample_score, .completion_logit(first))
  expect_false(is.na(out$completion_rate))
  expect_false(is.na(out$optin_rate))
})

test_that("forecast: the sample is scored on the next contact's context", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  # phones with a history: their NEXT context starts with their latest letter
  con <- .disposition_lazy_con()
  src <- .disposition_lazy_source(con, f$path, "test")
  h <- DBI::dbGetQuery(con, .completion_history_sql(con, src, "2015550001"))
  last <- h[h$is_last, ]
  expected_context <- paste0(last$letter, substr(last$context, 1, 2))
  ctx <- f$model$contexts[f$model$contexts$year == 2026L, ]
  p <- ctx$p_complete[ctx$context == expected_context]
  if (length(p) == 0L) p <- ctx$p_complete[ctx$context == "___"]
  out <- completion_forecast(f$model, "2015550001", "Live", "Brand", 12, f$path,
                             start_date = as.Date("2026-11-15"))
  expect_equal(out$share_with_history, 1)
  expect_equal(out$sample_score, .completion_logit(p))
})

test_that("forecast: history is read up to the planned start only", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  r <- completion_records(n_per_campaign = 60L)  # the rows the fixture model was fitted on
  # numbers first contacted by campaign 1 (2024-02-01): on that start date they have no history
  ph <- unique(r$phone[r$campaign_id == 1L])
  before <- completion_forecast(f$model, ph, "Live", "Brand", 12, f$path,
                                start_date = as.Date("2024-02-01"))
  expect_equal(before$share_with_history, 0)
  after <- completion_forecast(f$model, ph, "Live", "Brand", 12, f$path,
                               start_date = as.Date("2024-02-05"))
  expect_equal(after$share_with_history, 1)
})

test_that("forecast: year outside the stored tables", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  ph <- unique(completion_records()$phone)[1:20]
  late <- completion_forecast(f$model, ph, "Live", "Brand", 12, f$path,
                              start_date = as.Date("2031-01-01"))
  # beyond the last year: the all-rows table, no warning
  expect_equal(late$warnings[[1]], character(0))
  all_rows <- completion_forecast(f$model, ph, "Live", "Brand", 12, f$path,
                                  start_date = as.Date("2027-06-01"))
  expect_equal(late$sample_score, all_rows$sample_score)
  early <- completion_forecast(f$model, ph, "Live", "Brand", 12, f$path,
                               start_date = as.Date("2020-01-01"))
  expect_match(early$warnings[[1]], "2025", all = FALSE)
  expect_false(is.na(early$completion_rate))
})

test_that("forecast: unseen levels warn, bad mode errors", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  ph <- unique(completion_records()$phone)[1:20]
  out <- completion_forecast(f$model, ph, "Live", "Weather", loi = 45, dataset = f$path,
                             start_date = as.Date("2026-11-15"))
  expect_true(any(grepl("Weather", out$warnings[[1]])))
  expect_true(any(grepl("45", out$warnings[[1]])))
  out2 <- completion_forecast(f$model, ph, "Live", NULL, NULL, f$path,
                              start_date = as.Date("2026-11-15"))
  expect_true(any(grepl("LOI", out2$warnings[[1]], ignore.case = TRUE)))
  expect_true(any(grepl("topic", out2$warnings[[1]], ignore.case = TRUE)))
  # a missing tracker value (NA of any type) is the same as NULL
  out3 <- completion_forecast(f$model, ph, "Live", NA, NA, f$path, start_date = as.Date("2026-11-15"))
  expect_equal(out3$warnings[[1]], out2$warnings[[1]])
  expect_equal(out3$completion_rate, out2$completion_rate)
  out4 <- completion_forecast(f$model, ph, "Live", NA_character_, NA_real_, f$path,
                              start_date = as.Date("2026-11-15"))
  expect_equal(out4$warnings[[1]], out2$warnings[[1]])
  # the unknown topic level had no training row: say so
  expect_true(any(grepl("no training campaign", out2$warnings[[1]])))
  expect_error(completion_forecast(f$model, ph, "Phone", "Brand", 12, f$path), "mode")
  expect_error(completion_forecast(f$model, ph, c("Live", "T2W"), "Brand", 12, f$path), "mode")
  expect_error(completion_forecast(f$model, ph, "Live", "Brand", "twelve", f$path), "loi")
  expect_error(completion_forecast(f$model, ph, "Live", c("Brand", "Policy"), 12, f$path), "topic")
  expect_error(completion_forecast(f$model, ph, "Live", "Brand", 12, f$path, start_date = "soon"),
               "start_date")
})

test_that("forecast: external web", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  ph <- unique(completion_records()$phone)[1:20]
  out <- completion_forecast(f$model, ph, "T2W external", "Brand", 12, f$path,
                             start_date = as.Date("2026-11-15"))
  expect_false(is.na(out$optin_rate))
  expect_equal(out$n_phones, 20L)
  expect_true(is.na(out$completion_rate) && is.na(out$completes_hi95) && is.na(out$survey_multiplier) &&
                is.na(out$sample_multiplier))
  expect_true(any(grepl("opt-in only", out$warnings[[1]])))
  expect_error(completion_forecast(list(), ph, "Live", "Brand", 12, f$path), "completion_model")
  expect_error(completion_forecast(f$model, character(0), "Live", "Brand", 12, f$path), "phones")
  expect_error(completion_forecast(f$model, c("abc", NA), "Live", "Brand", 12, f$path), "phones")
})

test_that("forecast: small sample warns and bands are NA when the model has no residual sd", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  ph <- unique(completion_records()$phone)[1:5]
  m <- f$model
  m$models$completion$residual_sd[] <- NA_real_
  out <- completion_forecast(m, ph, "Live", "Brand", 12, f$path, start_date = as.Date("2026-11-15"))
  expect_true(is.na(out$completion_lo95) && is.na(out$completes_hi95))
  expect_false(is.na(out$completion_rate))
  expect_false(is.na(out$optin_lo95))
  expect_true(any(grepl("5 phones", out$warnings[[1]])))
  expect_true(any(grepl("no band", out$warnings[[1]])))
})

test_that("forecast: refuses without the DuckDB packages", {
  local_mocked_bindings(.disposition_lazy_missing = function() "duckdb")
  m <- structure(list(), class = "completion_model")
  expect_error(completion_forecast(m, "2015550001", "Live", "Brand", 12, "x.parquet"), "duckdb")
})
