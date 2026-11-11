# Extracted from test-completion_estimate.R:337

# prequel ----------------------------------------------------------------------
estimate_columns <- c(
  "optin_rate", "optin_lo50", "optin_hi50", "optin_lo95", "optin_hi95", "optins",
  "optins_lo95", "optins_hi95", "optins_mean", "completion_rate", "completion_lo50", "completion_hi50",
  "completion_lo95", "completion_hi95", "completes", "completes_lo95", "completes_hi95", "completes_mean",
  "n_phones", "share_with_history", "share_recent_contact", "sample_score", "reply_score",
  "years_since_contact", "prior_optin_score", "sample_multiplier", "survey_multiplier", "band_sd",
  "warnings")

# test -------------------------------------------------------------------------
skip_if_no_sequence()
f <- fit_fixture_model()
out <- completion_estimate(f$model, NULL, mode = "T2W", topic = "Brand", loi = 12,
                             start_date = as.Date("2026-11-15"))
expect_equal(nrow(out), 1L)
na_cols <- c("n_phones", "completes", "completes_hi95", "optins", "completes_mean", "optins_mean",
               "share_with_history", "share_recent_contact", "years_since_contact", "prior_optin_score")
expect_true(all(is.na(unlist(out[na_cols]))))
expect_false(anyNA(unlist(out[c("completion_rate", "optin_rate", "completion_lo95")])))
expect_equal(out$sample_multiplier, 1)
expect_equal(out$sample_score, f$model$models$completion$training_means[["sample_score"]])
