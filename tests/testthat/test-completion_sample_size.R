# completion_sample_size(): the sends needed for a target number of completes.

test_that("sample size: sends needed at the estimate and at the band's low ends", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  out <- completion_sample_size(f$model, 500, mode = "T2W", topic = "Brand", loi = 12,
                                start_date = as.Date("2026-11-15"))
  expect_equal(nrow(out), 1L)
  expect_named(out, c("target", "rate", "rate_estimate", "rate_lo50", "rate_lo95",
                      "sends_at_estimate", "sends_at_lo50", "sends_at_lo95", "n_phones", "list_covers",
                      "warnings"))
  expect_equal(out$target, 500)
  expect_equal(out$rate, "completion")
  fc <- completion_estimate(f$model, NULL, "T2W", "Brand", 12, start_date = as.Date("2026-11-15"))
  expect_equal(out$rate_estimate, fc$completion_rate)
  expect_equal(out$sends_at_estimate, ceiling(500 / fc$completion_rate))
  expect_equal(out$sends_at_lo50, ceiling(500 / fc$completion_lo50))
  expect_equal(out$sends_at_lo95, ceiling(500 / fc$completion_lo95))
  expect_true(out$sends_at_estimate < out$sends_at_lo50 && out$sends_at_lo50 < out$sends_at_lo95)
  expect_true(is.na(out$n_phones) && is.na(out$list_covers))
  expect_true(any(grepl("no sample", out$warnings[[1]])))
})

test_that("sample size: with a list, says which level the list covers", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  # the fixture's contacted numbers plus 5,000 never-contacted ones: a list big enough to reach a
  # target at every level of the band
  ph <- c(unique(completion_records()$phone), sprintf("30255%05d", seq_len(5000L)))
  fc <- completion_estimate(f$model, ph, "Live", "Brand", 12, f$path, start_date = as.Date("2026-11-15"))
  n <- length(ph)
  # a target the list reaches even at the 95% low end
  t95 <- floor(fc$completion_lo95 * n)
  expect_true(t95 >= 1L)
  out <- completion_sample_size(f$model, t95, "Live", "Brand", 12, phones = ph, dataset = f$path,
                                start_date = as.Date("2026-11-15"))
  expect_equal(out$n_phones, n)
  expect_equal(out$list_covers, "95% band")
  expect_equal(out$rate_estimate, fc$completion_rate)
  # a target the list reaches at the 50% low end but not at the 95% one
  t50 <- floor(fc$completion_lo50 * n)
  expect_true(t50 > fc$completion_lo95 * n)
  out50 <- completion_sample_size(f$model, t50, "Live", "Brand", 12, phones = ph, dataset = f$path,
                                  start_date = as.Date("2026-11-15"))
  expect_equal(out50$list_covers, "50% band")
  # one it reaches at the point estimate only
  tf <- floor(fc$completion_rate * n)
  expect_true(tf > fc$completion_lo50 * n)
  outf <- completion_sample_size(f$model, tf, "Live", "Brand", 12, phones = ph, dataset = f$path,
                                 start_date = as.Date("2026-11-15"))
  expect_equal(outf$list_covers, "estimate")
  # a target beyond the list at the point estimate
  out0 <- completion_sample_size(f$model, ceiling(fc$completion_rate * n) + 1L, "Live", "Brand", 12,
                                 phones = ph, dataset = f$path, start_date = as.Date("2026-11-15"))
  expect_equal(out0$list_covers, "none")
  expect_true(any(grepl("short", out0$warnings[[1]])))
})

test_that("sample size: argument checks", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  expect_error(completion_sample_size(f$model, 0, "Live", "Brand", 12), "target")
  expect_error(completion_sample_size(f$model, 10.5, "Live", "Brand", 12), "target")
  expect_error(completion_sample_size(f$model, c(1, 2), "Live", "Brand", 12), "target")
  expect_error(completion_sample_size(f$model, 100, "T2W external", "Brand", 12), "completion")
  expect_error(completion_sample_size(f$model, 100, "Live", "Brand", 12, rate = "reply"), "rate")
  expect_error(completion_sample_size(f$model, 100, "Phone", "Brand", 12), "mode")
  expect_error(completion_sample_size(f$model, 100, "Live", "Brand", 12, rate = c("optin", "completion")),
               "rate")
  expect_error(completion_sample_size(list(), 100, "Live", "Brand", 12), "completion_model")
  # no band: the band-based sends are NA, the estimate one stands
  m <- f$model
  m$models$completion$residual_sd[] <- NA_real_
  m$models$completion$residual_sd_size$sd[] <- NA_real_
  out <- completion_sample_size(m, 100, "Live", "Brand", 12, start_date = as.Date("2026-11-15"))
  expect_false(is.na(out$sends_at_estimate))
  expect_true(is.na(out$sends_at_lo50) && is.na(out$sends_at_lo95))
})

test_that("sample size: rate sizes on another fitted target", {
  skip_if_no_sequence()
  f <- fit_fixture_model()
  out <- completion_sample_size(f$model, 300, "T2W external", "Brand", 12, rate = "optin",
                                start_date = as.Date("2026-11-15"))
  fc <- completion_estimate(f$model, NULL, "T2W external", "Brand", 12, start_date = as.Date("2026-11-15"))
  expect_equal(out$rate, "optin")
  expect_equal(out$rate_estimate, fc$optin_rate)
  expect_equal(out$sends_at_estimate, ceiling(300 / fc$optin_rate))
  # the external mode has no opt-in sd in the fixture: the band sends are NA
  expect_true(is.na(out$sends_at_lo50) && is.na(out$sends_at_lo95))
  expect_true(any(grepl("completion is not fitted", out$warnings[[1]])))
})

test_that("rate's choices are in the signature and a wrong one is named with them", {
  expect_identical(eval(formals(completion_sample_size)$rate),
                   c("completion", "optin", "reply", "engage"))
  expect_setequal(eval(formals(completion_sample_size)$rate), names(.COMPLETION_MODELS))
  skip_if_no_sequence()
  f <- fit_fixture_model()
  expect_error(completion_sample_size(f$model, 100, mode = "Live", rate = "bogus"),
               "`rate` must be one of 'completion', 'optin', 'reply', 'engage'")
  # the full choice vector reads as the default, like match.arg()
  full <- completion_sample_size(f$model, 100, mode = "Live", topic = "Brand", loi = 12,
                                 rate = c("completion", "optin", "reply", "engage"))
  expect_identical(full$rate, "completion")
})
