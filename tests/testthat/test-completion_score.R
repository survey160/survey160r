# completion_score(): one row per phone with its history context and the
# context table's P(complete) and P(reply).

score_columns <- c("phone", "context", "n_prior", "prior_completes", "prior_replies",
                   "days_since_contact", "n_context", "p_complete", "p_reply")

test_that("score: one row per input phone, input order, duplicates and formats collapsed", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  ph <- c("3015550001", "(201) 555-0001", "2015550001", "1-201-555-0002")
  out <- completion_score(f$model, ph, f$path, mode = "Live", start_date = as.Date("2026-11-15"))
  expect_s3_class(out, "data.frame")
  expect_identical(names(out), score_columns)
  expect_identical(out$phone, c("3015550001", "2015550001", "2015550002"))
  # never contacted: the first-contact cell, no history
  expect_identical(out$context[1], "___")
  expect_identical(out$n_prior[1], 0L)
  expect_identical(out$prior_completes[1], 0L)
  expect_identical(out$prior_replies[1], 0L)
  expect_true(is.na(out$days_since_contact[1]))
  expect_equal(out$p_complete[1], .completion_context_lookup(f$model$contexts, 2026L, "___", "sms"))
  expect_equal(out$n_context[1], .completion_context_lookup(f$model$contexts, 2026L, "___", "sms", "n"))
  expect_true(all(out$n_prior[-1] > 0L))
})

test_that("score: a phone with history is scored on its next context, mode block or pooled", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  con <- .disposition_lazy_con()
  src <- .disposition_lazy_source(con, f$path, "test")
  h <- DBI::dbGetQuery(con, .completion_history_sql(con, src, "2015550001", before = as.Date("2026-11-15")))
  last <- h[h$is_last, ]
  ctx <- paste0(last$letter, substr(last$context, 1, 2))
  live <- completion_score(f$model, "2015550001", f$path, mode = "Live", start_date = as.Date("2026-11-15"))
  expect_identical(live$context, ctx)
  expect_identical(live$n_prior, last$n_prior + 1L)
  expect_identical(live$prior_completes, last$prior_completes + as.integer(last$letter == "C"))
  expect_identical(live$prior_replies, last$prior_replies + as.integer(last$letter %in% c("E", "O", "R", "C")))
  expect_equal(live$days_since_contact, as.numeric(as.Date("2026-11-15") - as.Date(last$disposition_date)))
  expect_equal(live$p_complete, .completion_context_lookup(f$model$contexts, 2026L, ctx, "sms"))
  expect_equal(live$p_reply, .completion_context_lookup(f$model$contexts, 2026L, ctx, "sms", "p_reply"))
  pooled <- completion_score(f$model, "2015550001", f$path, start_date = as.Date("2026-11-15"))
  expect_equal(pooled$p_complete, .completion_context_lookup(f$model$contexts, 2026L, ctx, NA_character_))
  expect_equal(pooled$n_context, .completion_context_lookup(f$model$contexts, 2026L, ctx, NA_character_, "n"))
})

test_that("score: the estimate's sample_score is the logit of the mean per-phone p_complete", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  ph <- c("3015550001", unique(completion_records()$phone)[1:25])
  s <- completion_score(f$model, ph, f$path, mode = "T2W", start_date = as.Date("2026-11-15"))
  est <- completion_estimate(f$model, ph, "T2W", "Brand", 12, f$path, start_date = as.Date("2026-11-15"))
  expect_equal(est$sample_score, .completion_logit(mean(s$p_complete)))
  expect_equal(est$reply_score, .completion_logit(mean(s$p_reply)))
  expect_equal(est$share_with_history, mean(s$n_prior > 0))
  expect_equal(est$n_phones, nrow(s))
})

test_that("score: history is read up to the planned start only", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  ph <- unique(completion_records()$phone)[1:10]
  before <- completion_score(f$model, ph, f$path, mode = "Live", start_date = as.Date("2025-03-01"))
  after <- completion_score(f$model, ph, f$path, mode = "Live", start_date = as.Date("2026-11-15"))
  expect_true(all(before$n_prior <= after$n_prior))
  expect_true(any(before$n_prior < after$n_prior))
})

test_that("score: text-to-web external has no completion score, reply still scored", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  expect_warning(
    out <- completion_score(f$model, "2015550001", f$path, mode = "T2W external",
                            start_date = as.Date("2026-11-15")),
    "completion is never recorded")
  expect_true(is.na(out$p_complete))
  expect_false(is.na(out$p_reply))
})

test_that("score: a year outside the tables and a context with no row warn", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  expect_warning(
    early <- completion_score(f$model, "3015550001", f$path, mode = "Live", start_date = as.Date("2020-01-01")),
    "before the first context table")
  expect_equal(early$p_complete, .completion_context_lookup(f$model$contexts, min(f$model$contexts$year), "___", "sms"))
  late <- completion_score(f$model, "3015550001", f$path, mode = "Live", start_date = as.Date("2031-01-01"))
  expect_equal(late$p_complete, .completion_context_lookup(f$model$contexts, 2026L, "___", "sms"))
  m <- f$model
  m$contexts <- m$contexts[!(m$contexts$context == "___" & m$contexts$year == 2026L), ]
  expect_warning(
    none <- completion_score(m, c("3015550001", "2015550001"), f$path, mode = "Live",
                             start_date = as.Date("2026-11-15")),
    "1 of 2 numbers have a context with no row")
  expect_true(is.na(none$p_complete[1]))
  expect_false(is.na(none$p_complete[2]))
})

test_that("score: argument errors", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  expect_error(completion_score(list(), "2015550001", f$path), "completion_model")
  expect_error(completion_score(f$model, character(0), f$path), "phones")
  expect_error(completion_score(f$model, c("abc", NA), f$path), "phones")
  expect_error(completion_score(f$model, 2015550001, f$path), "phones")
  expect_error(completion_score(f$model, "2015550001", f$path, mode = "Phone"), "mode")
  expect_error(completion_score(f$model, "2015550001", f$path, start_date = NULL), "start_date")
})
