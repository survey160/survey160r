# Fixture for the completion estimate (completion_model_fit / completion_estimate):
# a small synthetic projection of 25 campaigns over three years, so the per-year
# context tables, the time holdout and the two fits all have rows. Dates: 2024
# quarterly (ids 1-4), 2025 monthly (5-16), 2026 January to September (17-25).
# Campaign 8 has zero completes (unrecorded completion); campaign 25 is a
# text-to-web external survey (completion never reported). Campaigns never
# overlap in time, so a phone's last row before a later campaign's start is
# the same row the training window sees (the estimate and training paths agree).
.completion_campaign_dates <- function() {
  as.Date(c("2024-02-01", "2024-05-01", "2024-08-01", "2024-11-01",
            sprintf("2025-%02d-01", 1:12), sprintf("2026-%02d-01", 1:9)))
}

completion_records <- function(n_per_campaign = 40L, seed = 1L) {
  set.seed(seed)
  starts <- .completion_campaign_dates()
  n_campaigns <- length(starts)
  pool <- sprintf("201555%04d", seq_len(3L * n_per_campaign))
  modes <- c("Live", "T2W")
  topics <- c("Brand", "Electoral", "Policy")
  # seven LOI values, not six: a 6-cycle would be exactly mode (2) x topic (3)
  # and the LOI bands collinear with the other dummies
  lois <- c(4, 8, 12, 18, 25, NA, 14)
  completed_before <- character(0)
  out <- vector("list", n_campaigns)
  for (i in seq_len(n_campaigns)) {
    phones <- sample(pool, n_per_campaign)
    p_complete <- ifelse(phones %in% completed_before, 0.12, 0.03)
    completed <- as.integer(stats::runif(n_per_campaign) < p_complete)
    opted_in <- pmax(completed, as.integer(stats::runif(n_per_campaign) < 0.12))
    engaged <- pmax(opted_in, as.integer(stats::runif(n_per_campaign) < 0.25))
    # every campaign has at least one opt-in and (unless it is the zero-complete
    # one) at least one complete, whatever the draw
    completed[1L] <- 1L
    opted_in[1L] <- 1L
    engaged[1L] <- 1L
    if (i == 8L) completed[] <- 0L
    error <- ifelse(stats::runif(n_per_campaign) < 0.05, "4720", NA_character_)
    external <- i == n_campaigns
    mode_i <- modes[(i - 1L) %% 2L + 1L]
    dates <- rep(starts[i], n_per_campaign)
    dates[c(2L, 3L)] <- starts[i] + 3L
    out[[i]] <- data.frame(
      phone = phones, campaign_id = i, sent = 1L, engaged = engaged, opted_in = opted_in,
      completed = if (external) NA_integer_ else completed,
      web_complete = 0L, refused = 0L, ineligible = 0L, terminated = 0L, error = error,
      survey_mode = if (external) "t2w_external" else c(Live = "sms", T2W = "t2w")[[mode_i]],
      tracker_mode = if (external) NA_character_ else mode_i,
      tracker_topic = topics[(i - 1L) %% 3L + 1L],
      tracker_loi = lois[(i - 1L) %% 7L + 1L],
      disposition_date = dates, stringsAsFactors = FALSE)
    if (!external) completed_before <- union(completed_before, phones[completed == 1L])
  }
  do.call(rbind, out)
}

write_completion_parquet <- function(rows = completion_records()) write_disposition_parquet(rows)

# The model the estimate tests score with: the 60-phone fixture fitted to October 2026.
fit_fixture_model <- function() {
  p <- write_completion_parquet(completion_records(n_per_campaign = 60L))
  list(path = p, model = completion_model_fit(p, train_until = as.Date("2026-10-01"), min_sent = 10))
}
