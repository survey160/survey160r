# Extracted from test-completion_sample_size.R:89

# test -------------------------------------------------------------------------
skip_if_no_sequence()
f <- fit_fixture_model()
out <- completion_sample_size(f$model, 300, "T2W external", "Brand", 12, rate = "optin",
                                start_date = as.Date("2026-11-15"))
fc <- completion_estimate(f$model, NULL, "T2W external", "Brand", 12, start_date = as.Date("2026-11-15"))
expect_equal(out$rate, "optin")
expect_equal(out$rate_estimate, fc$optin_rate)
expect_equal(out$sends_at_estimate, ceiling(300 / fc$optin_rate))
expect_false(any(grepl("completion", out$warnings[[1]])))
