# Extracted from test-completion_model.R:516

# prequel ----------------------------------------------------------------------
mixed_size_records <- function() {
  # odd campaigns keep 60 phones, even ones 40: two size bands around a break of 50
  r <- completion_records(n_per_campaign = 60L)
  keep <- unlist(lapply(split(seq_len(nrow(r)), r$campaign_id), function(i) {
    if (r$campaign_id[i[1L]] %% 2L == 0L) i[1:40] else i
  }))
  r[sort(keep), ]
}

# test -------------------------------------------------------------------------
ct <- data.frame(
    campaign_id = 1:3, first_date = as.Date(c("2025-03-01", "2025-04-01", "2025-05-01")),
    last_date = as.Date(c("2025-03-02", "2025-04-02", "2025-05-02")), year = 2025L, sent = 50L,
    optins = c(3L, 0L, 2L), completes = c(2L, 0L, 1L), replies = c(10L, 4L, 0L), engaged = c(7L, 4L, 0L),
    completion_known = c(TRUE, FALSE, TRUE), mode = c("Live", "T2W external", "T2W"), topic = "Brand",
    loi = 12, sample_score = -4, reply_score = c(-2, NA, -2), share_prior_completers = 0, share_cold = 0,
    stringsAsFactors = FALSE)
args <- list(ct, train_until = as.Date("2026-10-01"), min_sent = 10, settle_days = 7,
               data_end = as.Date("2026-09-04"))
expect_equal(do.call(.completion_fit_rows, c(args, target = "reply"))$campaign_id, 1L)
ct$reply_score[2] <- -2
rows <- do.call(.completion_fit_rows, c(args, target = "reply"))
expect_equal(rows$campaign_id, 1:2)
expect_equal(rows$rate, c(10, 4) / 50)
