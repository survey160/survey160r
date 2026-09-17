test_that("funnel_rates adds the three canonical rates as proportions", {
  df  <- data.frame(n_sent = 1000, n_engaged = 80, n_opted_in = 20, n_completed = 5)
  out <- funnel_rates(df)
  expect_equal(out$engagement_rate, 0.08)
  expect_equal(out$opted_in_engaged_rate, 20 / 80)
  expect_equal(out$completion_rate, 0.005)
})

test_that("funnel_rates percent scales to 0-100", {
  df  <- data.frame(n_sent = 1000, n_engaged = 80, n_opted_in = 20, n_completed = 5)
  out <- funnel_rates(df, percent = TRUE)
  expect_equal(out$engagement_rate, 8)
  expect_equal(out$opted_in_engaged_rate, 25)
  expect_equal(out$completion_rate, 0.5)
})

test_that("funnel_rates returns NA on a zero denominator (not NaN/Inf)", {
  df  <- data.frame(n_sent = 0, n_engaged = 0, n_opted_in = 0, n_completed = 0)
  out <- funnel_rates(df)
  expect_true(is.na(out$engagement_rate))
  expect_true(is.na(out$opted_in_engaged_rate))
  expect_true(is.na(out$completion_rate))
})

test_that("funnel_rates errors on missing count columns", {
  expect_error(funnel_rates(data.frame(n_sent = 1)), "missing count columns")
})
