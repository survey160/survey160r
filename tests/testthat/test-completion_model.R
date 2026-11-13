# completion_model_fit() and its internals: the per-row history SQL, the
# per-year context tables, the campaign table and the two fits.

test_that("history sql: letters, prior counts and context per row", {
  skip_if_no_sequence()
  p <- write_completion_parquet(rbind(
    .sequence_row("2015550101", 1L, "2024-01-01"),
    .sequence_row("2015550101", 2L, "2024-02-01", engaged = 1L, terminated = 1L),
    .sequence_row("2015550101", 3L, "2024-03-01", engaged = 1L, opted_in = 1L, completed = 1L),
    .sequence_row("2015550101", 4L, "2024-04-01", engaged = 1L, opted_in = 1L,
                  completed = 1L, web_complete = 1L),
    .sequence_row("2015550102", 1L, "2024-01-01", error = "4720")))
  con <- .disposition_lazy_con()
  src <- .disposition_lazy_source(con, p, "test")
  h <- DBI::dbGetQuery(con, .completion_history_sql(con, src))
  h <- h[order(h$phone, h$campaign_id), ]
  expect_equal(h$letter, c("N", "E", "C", "C", "X"))
  expect_equal(h$opted, c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(h$context, c("___", "N__", "EN_", "CEN", "___"))
  expect_equal(h$n_prior, c(0L, 1L, 2L, 3L, 0L))
  expect_equal(h$prior_completes, c(0L, 0L, 0L, 1L, 0L))
  expect_equal(h$prior_replies, c(0L, 0L, 1L, 2L, 0L))
  expect_equal(h$is_last, c(FALSE, FALSE, FALSE, TRUE, TRUE))
  expect_equal(h$prev_campaign, c(NA, 1L, 2L, 3L, NA))
  expect_equal(as.Date(h$prev_date), as.Date(c(NA, "2024-01-01", "2024-02-01", "2024-03-01", NA)))
  expect_true(all(h$completion_known))
  expect_true(all(is.na(h$tracker_loi)))
})

test_that("context tables: the context table is the sum of its prior-completes split", {
  skip_if_no_sequence()
  p <- write_completion_parquet(completion_records())
  con <- .disposition_lazy_con()
  src <- .disposition_lazy_source(con, p, "test")
  tabs <- .completion_context_tables(con, .completion_history_sql(con, src))
  ctx <- tabs$contexts
  sp <- tabs$completes
  expect_identical(names(sp), c("year", "context", "survey_mode", "prior_completes", "n_known",
                                "completes"))
  expect_true(all(sp$prior_completes %in% 0:3))
  expect_true(any(sp$prior_completes > 0L))
  key <- function(x) paste(x$year, x$context, x$survey_mode)
  known <- tapply(sp$n_known, key(sp), sum)
  comp <- tapply(sp$completes, key(sp), sum)
  k <- key(ctx)
  rate <- ifelse(known[k] > 0, comp[k] / known[k], NA_real_)
  expect_equal(as.vector(rate), ctx$p_complete)
  expect_false(anyDuplicated(k) > 0)
  expect_setequal(k, names(known))
})

test_that("context tables: one block per year, estimated on earlier years only", {
  skip_if_no_sequence()
  p <- write_completion_parquet(completion_records())
  con <- .disposition_lazy_con()
  src <- .disposition_lazy_source(con, p, "test")
  ctx <- .completion_context_tables(con, .completion_history_sql(con, src))$contexts
  expect_setequal(unique(ctx$year), 2025:2027)
  first <- ctx[ctx$context == "___" & is.na(ctx$survey_mode), ]
  expect_equal(nrow(first), 3L)
  # the 2027 block sees every known row; the 2025 block only 2024's
  expect_gt(first$n[first$year == 2027], first$n[first$year == 2025])
  known <- !is.na(ctx$p_complete)
  expect_true(all(ctx$p_complete[known] >= 0 & ctx$p_complete[known] <= 1))
  # one block per survey mode plus a pooled block (survey_mode NA); the external
  # block has no known completion, so its p_complete is NA
  expect_setequal(unique(ctx$survey_mode), c("sms", "t2w", "t2w_external", NA))
  expect_true(all(is.na(ctx$p_complete[ctx$survey_mode %in% "t2w_external"])))
  pooled <- ctx[ctx$year == 2027L & ctx$context == "___" & is.na(ctx$survey_mode), ]
  by_mode <- ctx[ctx$year == 2027L & ctx$context == "___" & !is.na(ctx$survey_mode), ]
  expect_equal(nrow(pooled), 1L)
  expect_equal(pooled$n, sum(by_mode$n))
  expect_equal(pooled$p_reply, sum(by_mode$p_reply * by_mode$n) / sum(by_mode$n))
  # p_complete pools the known rows only: the two known blocks weighted by their rows
  kn <- by_mode[!is.na(by_mode$p_complete), ]
  expect_equal(pooled$p_complete, sum(kn$p_complete * kn$n) / sum(kn$n))
  expect_equal(nrow(ctx), nrow(unique(ctx[c("year", "context", "survey_mode")])))
})

test_that("context lookup: own mode, then pooled, then the pooled first contact", {
  ctx <- data.frame(year = 2026L, context = c("N__", "N__", "___", "C__"),
                    survey_mode = c("sms", NA, NA, "t2w"), p_complete = c(0.1, 0.2, 0.3, 0.4),
                    n = 1L, stringsAsFactors = FALSE)
  expect_equal(.completion_context_lookup(ctx, 2026L, c("N__", "N__", "C__", "ZZZ", "N__"),
                                          c("sms", "t2w", "sms", "sms", NA)),
               c(0.1, 0.2, 0.3, 0.3, 0.2))
})

test_that("campaign table: one row per campaign with dimensions and sample features", {
  skip_if_no_sequence()
  p <- write_completion_parquet(completion_records())
  con <- .disposition_lazy_con()
  src <- .disposition_lazy_source(con, p, "test")
  hs <- .completion_history_sql(con, src)
  ct <- .completion_campaign_table(con, hs, .completion_context_tables(con, hs)$contexts)
  expect_equal(nrow(ct), 25L)
  expect_equal(ct$campaign_id, 1:25)
  expect_equal(ct$sent, rep(40L, 25L))
  expect_equal(ct$first_date[1:2], as.Date(c("2024-02-01", "2024-05-01")))
  expect_equal(ct$last_date[1], as.Date("2024-02-04"))
  expect_equal(ct$year[c(1, 5, 25)], c(2024L, 2025L, 2026L))
  expect_false(ct$completion_known[ct$campaign_id == 25L])
  expect_true(all(ct$completion_known[ct$campaign_id < 25L]))
  expect_equal(ct$completes[ct$campaign_id == 8L], 0L)
  expect_true(ct$optins[ct$campaign_id == 25L] > 0L)
  expect_equal(ct$mode[c(1, 2, 25)], c("Live", "T2W", "T2W external"))
  expect_equal(ct$topic[1:3], c("Brand", "Electoral", "Policy"))
  expect_equal(ct$loi[1:6], c(4, 8, 12, 18, 25, NA))
  expect_true(all(is.na(ct$sample_score[ct$year == 2024])))
  expect_true(all(!is.na(ct$sample_score[ct$year >= 2025])))
  expect_true(all(ct$share_prior_completers >= 0 & ct$share_prior_completers <= 1))
  expect_true(all(ct$share_cold >= 0 & ct$share_cold <= 1))
  expect_equal(ct$share_prior_completers[1], 0)
  # the first campaign's sample has no prior contact: the recency and provenance features are NA
  expect_true(is.na(ct$years_since_contact[1]))
  expect_true(is.na(ct$prior_optin_score[1]))
  expect_true(all(!is.na(ct$years_since_contact[-1])))
  expect_true(all(ct$years_since_contact[-1] > 0 & ct$years_since_contact[-1] < 3))
  expect_true(all(!is.na(ct$prior_optin_score[-1])))
  expect_true(all(ct$prior_optin_score[-1] < 0))
  # the sample score is the mean P(complete | context, survey mode) with the pooled fallbacks:
  # the SQL agrees with the R lookup the estimate uses
  con <- .disposition_lazy_con()
  h <- DBI::dbGetQuery(con, paste("SELECT * FROM (", hs, ") WHERE campaign_id = 6"))
  ctx <- .completion_context_tables(con, hs)$contexts
  expect_equal(ct$sample_score[6], .completion_logit(mean(
    .completion_context_lookup(ctx, 2025L, h$context, h$survey_mode))))
  expect_true(any(.completion_context_lookup(ctx, 2025L, h$context, h$survey_mode) !=
                    .completion_context_lookup(ctx, 2025L, h$context, NA)))
})

test_that("campaign table: tracker columns absent", {
  skip_if_no_sequence()
  r <- completion_records()
  r$tracker_mode <- NULL
  r$tracker_topic <- NULL
  r$tracker_loi <- NULL
  p <- write_completion_parquet(r)
  con <- .disposition_lazy_con()
  src <- .disposition_lazy_source(con, p, "test")
  hs <- .completion_history_sql(con, src)
  ct <- .completion_campaign_table(con, hs, .completion_context_tables(con, hs)$contexts)
  expect_equal(ct$mode[ct$campaign_id %in% 1:2], c("Live", "T2W"))
  expect_true(all(ct$topic == "unknown"))
  expect_true(all(is.na(ct$loi)))
})

test_that("campaign table: a previous contact after the campaign started does not count", {
  skip_if_no_sequence()
  # campaign 3 starts 2024-03-01 (phone ...03). Phone ...01's previous row is in
  # overlapping campaign 2, dated after that start: no gap. Phone ...02's is
  # 60 days before it. Phone ...03 is a first contact.
  p <- write_completion_parquet(rbind(
    .sequence_row("2015550101", 1L, "2024-01-01"),
    .sequence_row("2015550102", 1L, "2024-01-01"),
    .sequence_row("2015550101", 2L, "2024-03-05"),
    .sequence_row("2015550103", 3L, "2024-03-01"),
    .sequence_row("2015550102", 3L, "2024-03-02"),
    .sequence_row("2015550101", 3L, "2024-03-10")))
  con <- .disposition_lazy_con()
  src <- .disposition_lazy_source(con, p, "test")
  hs <- .completion_history_sql(con, src)
  ct <- .completion_campaign_table(con, hs, .completion_context_tables(con, hs)$contexts)
  expect_equal(ct$years_since_contact[ct$campaign_id == 3L], 60 / 365.25)
})

test_that("level helpers", {
  expect_equal(.completion_mode(c("Live", NA, NA, "T2W", NA, "T2W"),
                                c("sms", "sms", "t2w", "t2w", "t2w_external", "t2w_external")),
               c("Live", "Live", "T2W", "T2W", "T2W external", "T2W external"))
  expect_true(is.na(.completion_mode(NA, "fax")))
  expect_equal(.completion_topic(c("Brand", "other", NA)), c("Brand", "unknown", "unknown"))
  expect_equal(as.character(.completion_loi_band(c(5, 5.5, 12, 20, 31, 32, 0, NA))),
               c("[0,5]", "(5,10]", "(10,15]", "(15,20]", "(20,31]", "unknown", "unknown", "unknown"))
  expect_equal(levels(.completion_loi_band(12))[1], "(10,15]")
  expect_equal(.completion_logit(c(0, 0.5, 1)), qlogis(c(1e-4, 0.5, 1 - 1e-4)))
})

test_that("fit: returns a completion_model with coefficients, contexts and holdout", {
  skip_if_no_sequence()
  p <- write_completion_parquet(completion_records(n_per_campaign = 60L))
  m <- completion_model_fit(p, train_until = as.Date("2026-10-01"), holdout_months = 6,
                            min_sent = 10)
  expect_s3_class(m, "completion_model")
  expect_equal(m$min_sent, 10)
  expect_equal(m$settle_days, 7)
  expect_named(m$models, c("optin", "completion"))
  expect_true(all(c("(Intercept)", "modeT2W", "sample_score") %in%
                    m$models$completion$coefficients$term))
  expect_true("modeT2W external" %in% m$models$optin$coefficients$term)
  expect_false("modeT2W external" %in% m$models$completion$coefficients$term)
  expect_equal(m$models$optin$formula, "logit ~ mode + sample_score + prior_optin_score")
  expect_equal(m$models$completion$formula, paste(
    "logit ~ mode + topic + loi_band + sample_score + share_prior_completers + share_cold +",
    "years_since_contact"))
  expect_equal(m$campaigns$campaign_id, 1:25)
  expect_equal(m$campaigns$optin_rate[1], sum(completion_records(60L)$opted_in[1:60]) / 60)
  expect_named(m$campaigns, c("campaign_id", "first_date", "optin_rate"))
  expect_named(m$models$completion$residual_sd, c("Live", "T2W"))
  expect_named(m$models$optin$residual_sd, c("Live", "T2W", "T2W external"))
  expect_equal(m$windows$data_end, as.Date("2026-09-04"))
  expect_equal(m$windows$train_until, as.Date("2026-10-01"))
  expect_equal(m$windows$holdout_from, as.Date("2026-04-01"))
  expect_equal(m$windows$train_from, as.Date("2025-01-01"))
  expect_equal(m$counts$campaigns_total, 25L)
  # completion drops 1-4 (2024: no prior-year table), 8 (zero completes), 25 (unknown + settle lag)
  expect_equal(m$models$completion$counts$campaigns_fit, 19L)
  expect_equal(m$models$completion$holdout$n, 5L)  # campaigns 20-24
  expect_equal(m$models$completion$counts$campaigns_holdout_fit, 14L)
  # opt-in keeps 8 (it has opt-ins) and still drops 25 (settle lag)
  expect_equal(m$models$optin$counts$campaigns_fit, 20L)
  expect_false(anyNA(m$models$completion$residual_sd))
  expect_true(all(m$models$completion$residual_sd > 0))
  expect_true(is.na(m$models$optin$residual_sd[["T2W external"]]))
  expect_true(m$models$completion$holdout$r2_logit >= 0 && m$models$completion$holdout$r2_logit <= 1)
  expect_true(m$models$completion$holdout$median_ape >= 0)
  expect_true(m$models$completion$holdout$within_50 >= 0 && m$models$completion$holdout$within_50 <= 1)
  # a level absent from the training rows is still a term, estimate 0, no standard error
  ext <- m$models$optin$coefficients[m$models$optin$coefficients$term == "modeT2W external", ]
  expect_equal(ext$estimate, 0)
  expect_true(is.na(ext$std_error))
  expect_true(all(c("sample_score", "mode.T2W", "topic.Brand", "loi_band.(10,15]",
                    "years_since_contact") %in% names(m$models$completion$training_means)))
  expect_true("prior_optin_score" %in% names(m$models$optin$training_means))
  expect_false(anyNA(m$models$completion$training_means))
  expect_equal(unname(m$models$completion$training_means["mode.Live"] +
                        m$models$completion$training_means["mode.T2W"]), 1)
  expect_equal(nrow(m$contexts), nrow(unique(m$contexts[c("year", "context", "survey_mode")])))
  expect_equal(m$version, as.character(utils::packageVersion("survey160r")))
  expect_output(print(m), "opt-in")
  expect_output(print(m), "2026-04-01")
  s <- summary(m)
  expect_true(all(c("model", "term", "odds_multiplier", "std_error") %in% names(s)))
  expect_equal(s$odds_multiplier[s$model == "completion" & s$term == "(Intercept)"],
               exp(m$models$completion$coefficients$estimate[1]))
})

test_that("fit: filters follow the spec", {
  ct <- data.frame(
    campaign_id = 1:6,
    first_date = as.Date(c("2024-03-01", "2025-03-01", "2025-06-01", "2026-01-01", "2026-08-30",
                           "2026-07-01")),
    last_date = as.Date(c("2024-03-02", "2025-03-02", "2025-06-02", "2026-01-02", "2026-09-03",
                          "2026-07-02")),
    year = c(2024L, 2025L, 2025L, 2026L, 2026L, 2026L), sent = c(50L, 50L, 5L, 50L, 50L, 50L),
    optins = c(3L, 3L, 3L, 3L, 3L, 3L),
    completes = c(2L, 2L, 2L, 0L, 2L, 2L), completion_known = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE),
    mode = "Live", topic = "Brand", loi = 12, sample_score = c(NA, -4, -4, -4, -4, -4),
    share_prior_completers = 0, share_cold = 0, stringsAsFactors = FALSE)
  rows <- .completion_fit_rows(ct, "completion", train_until = as.Date("2026-10-01"), min_sent = 10,
                               settle_days = 7, data_end = as.Date("2026-09-04"))
  # 1: no table; 3: too small; 4: zero completes; 5: within settle lag; 6: unknown
  expect_equal(rows$campaign_id, 2L)
  expect_equal(rows$logit, qlogis(2 / 50))
  rows <- .completion_fit_rows(ct, "optin", train_until = as.Date("2026-10-01"), min_sent = 10,
                               settle_days = 7, data_end = as.Date("2026-09-04"))
  # opt-in keeps zero-complete and unknown-completion campaigns
  expect_equal(rows$campaign_id, c(2L, 4L, 6L))
  expect_equal(rows$logit, rep(qlogis(3 / 50), 3))
  # a train_until inside the data cuts the later campaigns
  rows <- .completion_fit_rows(ct, "optin", train_until = as.Date("2026-01-01"), min_sent = 10,
                               settle_days = 7, data_end = as.Date("2026-09-04"))
  expect_equal(rows$campaign_id, 2L)
  # zero opt-ins drop from the opt-in model
  ct$optins[2] <- 0L
  rows <- .completion_fit_rows(ct, "optin", train_until = as.Date("2026-10-01"), min_sent = 10,
                               settle_days = 7, data_end = as.Date("2026-09-04"))
  expect_equal(rows$campaign_id, c(4L, 6L))
})

test_that("fit: a mode outside the model's levels drops from that fit", {
  ct <- data.frame(
    campaign_id = 1:2, first_date = as.Date(c("2025-03-01", "2025-04-01")),
    last_date = as.Date(c("2025-03-02", "2025-04-02")), year = 2025L, sent = 50L, optins = 3L,
    completes = 2L, completion_known = TRUE, mode = c("Live", "T2W external"), topic = "Brand",
    loi = 12, sample_score = -4, share_prior_completers = 0, share_cold = 0,
    stringsAsFactors = FALSE)
  args <- list(ct, train_until = as.Date("2026-10-01"), min_sent = 10, settle_days = 7,
               data_end = as.Date("2026-09-04"))
  expect_equal(do.call(.completion_fit_rows, c(args, target = "completion"))$campaign_id, 1L)
  expect_equal(do.call(.completion_fit_rows, c(args, target = "optin"))$campaign_id, 1:2)
})

test_that("fit: a live campaign the tracker labels T2W external does not break the fit", {
  skip_if_no_sequence()
  r <- completion_records()
  r$tracker_mode[r$campaign_id == 5L] <- "T2W external"
  p <- write_completion_parquet(r)
  expect_warning(
    m <- completion_model_fit(p, train_until = as.Date("2026-10-01"), holdout_months = 6,
                              min_sent = 10),
    "one per term")
  expect_s3_class(m, "completion_model")
  # the relabelled campaign leaves the completion fit and stays in the opt-in fit
  expect_equal(m$models$completion$counts$campaigns_fit, 18L)
  expect_equal(m$models$optin$counts$campaigns_fit, 20L)
})

test_that("fit: the holdout needs more campaigns to fit on than the model has terms", {
  n <- 7L
  ct <- data.frame(
    campaign_id = seq_len(n), first_date = as.Date("2026-01-01") + 30 * (seq_len(n) - 1),
    last_date = as.Date("2026-01-02") + 30 * (seq_len(n) - 1), year = 2026L, sent = 50L,
    optins = 2:8, completes = 1:7, completion_known = TRUE,
    mode = rep(c("Live", "T2W", "T2W external"), length.out = n), topic = "Brand", loi = 12,
    sample_score = seq(-5, -3, length.out = n), share_prior_completers = 0, share_cold = 0,
    years_since_contact = 1, prior_optin_score = -2, stringsAsFactors = FALSE)
  # 4 campaigns before the holdout start against 5 opt-in terms: no second fit
  res <- .completion_fit_model(ct, "optin", train_until = as.Date("2026-12-01"),
                               holdout_from = as.Date("2026-05-01"), min_sent = 10,
                               settle_days = 0, data_end = as.Date("2026-12-31"), fn = "f")
  expect_equal(res$counts$campaigns_holdout_fit, 4L)
  expect_match(res$holdout$warning, "at least 5")
  expect_true(is.na(res$holdout$r2_logit))
  expect_true(all(is.na(res$residual_sd)))
})

test_that("fit: a projection without disposition rows is refused", {
  skip_if_no_sequence()
  expect_error(completion_model_fit(write_completion_parquet(completion_records()[0, ])),
               "no disposition rows")
  r <- completion_records()
  r$phone <- ""
  expect_error(completion_model_fit(write_completion_parquet(r)), "no disposition rows")
})

test_that("fit: a campaign whose sample has no prior contact takes the training mean", {
  rows <- data.frame(logit = c(-4, -3, -5, -4.5, -3.5, -4),
                     mode = c("Live", "T2W", "Live", "T2W", "Live", "T2W"),
                     topic = "Brand", loi = 12, sample_score = c(-4, -3, -3.5, -2, -5, -4),
                     prior_optin_score = c(NA, -2, -4, -3, -2.5, -3.5), stringsAsFactors = FALSE)
  means <- .completion_training_means(rows, "optin")
  expect_equal(unname(means["prior_optin_score"]), -3)
  filled <- .completion_fill_means(.completion_design(rows, "optin"), means)
  expect_equal(filled$prior_optin_score, c(-3, -2, -4, -3, -2.5, -3.5))
  fit <- .completion_fit_lm(rows, "optin", means)
  expect_false(anyNA(stats::coef(fit)[c("(Intercept)", "sample_score", "prior_optin_score")]))
  expect_equal(length(stats::residuals(fit)), 6L)
})

test_that("fit: train_until cuts the history, so a past-dated fit sees no later row", {
  skip_if_no_sequence()
  p <- write_completion_parquet(completion_records())
  cut <- as.Date("2026-04-01")
  m <- completion_model_fit(p, train_until = cut, holdout_months = 6, min_sent = 10)
  expect_true(m$windows$data_end < cut)
  expect_equal(m$windows$data_end, as.Date("2026-03-04"))
  expect_true(all(m$campaigns$first_date < cut))
  expect_equal(m$counts$campaigns_total, 19L)
  # the context tables stop at the cut too: no table past the cut year, and the
  # 2026 table only sees rows before 2026 either way
  expect_equal(max(m$contexts$year), 2027L)
  full <- completion_model_fit(p, train_until = as.Date("2026-10-01"), holdout_months = 6,
                               min_sent = 10)
  expect_equal(m$contexts[m$contexts$year == 2026L, ], full$contexts[full$contexts$year == 2026L, ],
               ignore_attr = TRUE)
  expect_false(isTRUE(all.equal(m$contexts[m$contexts$year == 2027L, "n"],
                                full$contexts[full$contexts$year == 2027L, "n"])))
})

test_that("fit: thin holdout", {
  skip_if_no_sequence()
  p <- write_completion_parquet(completion_records())
  expect_warning(
    m <- completion_model_fit(p, train_until = as.Date("2026-10-01"), holdout_months = 1,
                              min_sent = 10),
    "holdout")
  expect_true(all(is.na(m$models$completion$residual_sd)))
  expect_true(all(is.na(m$models$optin$residual_sd)))
  expect_true(is.na(m$models$completion$holdout$r2_logit))
  expect_equal(m$models$completion$holdout$n, 0L)
  expect_output(print(m), "NA")
})

test_that("fit: argument checks", {
  skip_if_no_sequence()
  p <- write_completion_parquet(completion_records())
  expect_error(completion_model_fit(p, train_until = "not a date"), "train_until")
  expect_error(completion_model_fit(p, train_until = NULL), "train_until")
  expect_error(completion_model_fit(p, holdout_months = 0), "holdout_months")
  expect_error(completion_model_fit(p, holdout_months = 2.5), "holdout_months")
  expect_error(completion_model_fit(p, min_sent = -1), "min_sent")
  expect_error(completion_model_fit(p, settle_days = 1.5), "settle_days")
  # a cut inside the first year: rows exist but no campaign has a context table
  expect_error(completion_model_fit(p, train_until = as.Date("2024-06-01")), "no campaign")
  expect_error(completion_model_fit(p, train_until = as.Date("2000-01-01")), "no disposition rows")
})

test_that("fit: refuses without the DuckDB packages and a data frame dataset", {
  local_mocked_bindings(.disposition_lazy_missing = function() "duckdb")
  expect_error(completion_model_fit("x.parquet"), "duckdb")
})

test_that("fit: refuses a data frame dataset", {
  skip_if_no_sequence()
  expect_error(completion_model_fit(data.frame(phone = "1")), "Parquet path")
})

test_that("campaign table: share of the sample contacted in the 30 days before the start", {
  skip_if_no_sequence()
  # campaign 3 (2024-03-01): ...02 was contacted 60 days before (not recent), ...03 is a first
  # contact, ...01's previous row is after the start (not recent). Campaign 4 (2024-03-20):
  # ...01 and ...02 both within 30 days, ...04 a first contact.
  p <- write_completion_parquet(rbind(
    .sequence_row("2015550101", 1L, "2024-01-01"),
    .sequence_row("2015550102", 1L, "2024-01-01"),
    .sequence_row("2015550101", 2L, "2024-03-05"),
    .sequence_row("2015550103", 3L, "2024-03-01"),
    .sequence_row("2015550102", 3L, "2024-03-02"),
    .sequence_row("2015550101", 3L, "2024-03-10"),
    .sequence_row("2015550101", 4L, "2024-03-20"),
    .sequence_row("2015550102", 4L, "2024-03-20"),
    .sequence_row("2015550104", 4L, "2024-03-21")))
  con <- .disposition_lazy_con()
  src <- .disposition_lazy_source(con, p, "test")
  hs <- .completion_history_sql(con, src)
  ct <- .completion_campaign_table(con, hs, .completion_context_tables(con, hs)$contexts)
  expect_equal(ct$share_recent_contact[ct$campaign_id == 1L], 0)
  expect_equal(ct$share_recent_contact[ct$campaign_id == 3L], 0)
  expect_equal(ct$share_recent_contact[ct$campaign_id == 4L], 2 / 3)
})

mixed_size_records <- function() {
  # odd campaigns keep 60 phones, even ones 40: two size bands around a break of 50
  r <- completion_records(n_per_campaign = 60L)
  keep <- unlist(lapply(split(seq_len(nrow(r)), r$campaign_id), function(i) {
    if (r$campaign_id[i[1L]] %% 2L == 0L) i[1:40] else i
  }))
  r[sort(keep), ]
}

test_that("fit: residual sd by mode and size band, re-send summary, size_break argument", {
  skip_if_no_sequence()
  p <- write_completion_parquet(mixed_size_records())
  m <- completion_model_fit(p, train_until = as.Date("2026-10-01"), holdout_months = 6,
                            min_sent = 10, size_break = 50)
  expect_equal(m$size_break, 50)
  sz <- m$models$completion$residual_sd_size
  expect_named(sz, c("mode", "size", "sd", "n"))
  expect_setequal(sz$mode, c("Live", "T2W"))
  expect_setequal(sz$size, c("under", "over"))
  expect_equal(nrow(sz), 4L)
  # odd campaigns are Live (60 phones), even ones T2W (40): each mode sits in one band
  expect_true(all(!is.na(sz$sd[(sz$mode == "Live" & sz$size == "over") |
                                 (sz$mode == "T2W" & sz$size == "under")])))
  expect_true(all(is.na(sz$sd[(sz$mode == "Live" & sz$size == "under") |
                                (sz$mode == "T2W" & sz$size == "over")])))
  expect_equal(sz$n[sz$mode == "Live" & sz$size == "under"], 0L)
  # a band with every campaign of its mode has the mode's sd
  expect_equal(sz$sd[sz$mode == "Live" & sz$size == "over"], m$models$completion$residual_sd[["Live"]])
  expect_named(m$models$optin$residual_sd_size, c("mode", "size", "sd", "n"))
  expect_equal(nrow(m$models$optin$residual_sd_size), 6L)
  rs <- m$models$completion$resend
  expect_named(rs, c("threshold", "campaigns", "ratio"))
  expect_equal(rs$threshold, 0.5)
  expect_named(rs$campaigns, c("Live", "T2W"))
  expect_named(rs$ratio, c("Live", "T2W"))
  # the fixture's campaigns never overlap and are a month or more apart: no re-send in training
  expect_equal(unname(rs$campaigns), c(0L, 0L))
  expect_true(all(is.na(rs$ratio)))
  # the default break is 50,000 sends
  m2 <- completion_model_fit(p, train_until = as.Date("2026-10-01"), holdout_months = 6, min_sent = 10)
  expect_equal(m2$size_break, 50000)
  expect_true(all(m2$models$completion$residual_sd_size$n[m2$models$completion$residual_sd_size$size == "over"] == 0L))
  expect_error(completion_model_fit(p, size_break = -1), "size_break")
  expect_error(completion_model_fit(p, size_break = c(1, 2)), "size_break")
  expect_error(completion_model_fit(p, size_break = 0), "size_break")
})

test_that("fit: re-send summary counts training campaigns whose sample was mostly contacted in the prior 30 days", {
  skip_if_no_sequence()
  r <- completion_records(n_per_campaign = 60L)
  # campaign 24 (2026-08-01) re-sends campaign 23's (2026-07-01) sample: move its rows onto
  # campaign 23's phones, dated 20 days after campaign 23
  ph23 <- r$phone[r$campaign_id == 23L]
  i24 <- which(r$campaign_id == 24L)
  r$phone[i24] <- ph23
  r$disposition_date[i24] <- as.Date("2026-07-21")
  p <- write_completion_parquet(r)
  m <- completion_model_fit(p, train_until = as.Date("2026-10-01"), holdout_months = 6, min_sent = 10)
  rs <- m$models$completion$resend
  # campaign 24 is T2W (even id)
  expect_equal(unname(rs$campaigns), c(0L, 1L))
  expect_true(is.na(rs$ratio[["Live"]]))
  expect_true(is.finite(rs$ratio[["T2W"]]) && rs$ratio[["T2W"]] > 0)
})

test_that("registry: every target names its label, count, formula, modes and score", {
  expect_named(.COMPLETION_MODELS, c("optin", "completion", "reply", "engage"))
  for (spec in .COMPLETION_MODELS) {
    expect_true(all(c("label", "count", "formula", "modes", "score") %in% names(spec)))
    expect_true(all(c("context", "term") %in% names(spec$score)))
    expect_true(all(spec$modes %in% .COMPLETION_MODES))
  }
  expect_equal(.COMPLETION_MODELS$completion$require, "completion_known")
  expect_null(.COMPLETION_MODELS$reply$require)
  expect_equal(.COMPLETION_MODELS$reply$score[["context"]], "p_reply")
  expect_equal(.COMPLETION_MODELS$reply$score[["term"]], "reply_score")
  expect_equal(.COMPLETION_MODELS$reply$modes, .COMPLETION_MODES)
  expect_equal(.COMPLETION_DEFAULT_TARGETS, c("optin", "completion"))
})

test_that("context tables and campaign table carry the reply score", {
  skip_if_no_sequence()
  p <- write_completion_parquet(completion_records())
  con <- .disposition_lazy_con()
  src <- .disposition_lazy_source(con, p, "test")
  history <- .completion_history_sql(con, src)
  ctx <- .completion_context_tables(con, history)$contexts
  expect_true(all(c("p_complete", "p_reply") %in% names(ctx)))
  expect_true(all(ctx$p_reply >= 0 & ctx$p_reply <= 1))
  # a reply is at least as common as a complete in every block whose rows all have a known completion
  known <- ctx$survey_mode %in% c("sms", "t2w")
  expect_true(all(ctx$p_reply[known] >= ctx$p_complete[known]))
  # the external campaign's rows (completion unknown) still count for the reply table
  ext <- ctx[ctx$year == 2027L & ctx$survey_mode %in% "t2w_external", ]
  expect_gt(nrow(ext), 0L)
  expect_true(all(is.na(ext$p_complete)))
  ct <- .completion_campaign_table(con, history, ctx)
  expect_true(all(c("replies", "engaged", "reply_score") %in% names(ct)))
  r <- completion_records()
  expect_equal(ct$engaged[1], sum(r$engaged[r$campaign_id == 1] == 1 & r$opted_in[r$campaign_id == 1] == 0 &
                                    is.na(r$error[r$campaign_id == 1])))
  expect_equal(ct$replies[1], sum(r$engaged[r$campaign_id == 1] == 1 & is.na(r$error[r$campaign_id == 1])))
  expect_true(all(ct$replies >= ct$optins))
  expect_false(anyNA(ct$reply_score[ct$year > 2024]))
})

test_that("fit rows: a target's count, filter and modes come from the registry", {
  ct <- data.frame(
    campaign_id = 1:3, first_date = as.Date(c("2025-03-01", "2025-04-01", "2025-05-01")),
    last_date = as.Date(c("2025-03-02", "2025-04-02", "2025-05-02")), year = 2025L, sent = 50L,
    optins = c(3L, 0L, 2L), completes = c(2L, 0L, 1L), replies = c(10L, 4L, 0L), engaged = c(7L, 4L, 0L),
    completion_known = c(TRUE, FALSE, TRUE), mode = c("Live", "T2W external", "T2W"), topic = "Brand",
    loi = 12, sample_score = -4, reply_score = c(-2, NA, -2), share_prior_completers = 0, share_cold = 0,
    stringsAsFactors = FALSE)
  args <- list(ct, train_until = as.Date("2026-10-01"), min_sent = 10, settle_days = 7,
               data_end = as.Date("2026-09-04"))
  # reply keeps the external campaign if its score is known; drops 3 (no replies) and 2 (no score)
  expect_equal(do.call(.completion_fit_rows, c(args, target = "reply"))$campaign_id, 1L)
  ct$reply_score[2] <- -2
  args[[1]] <- ct
  rows <- do.call(.completion_fit_rows, c(args, target = "reply"))
  expect_equal(rows$campaign_id, 1:2)
  expect_equal(rows$rate, c(10, 4) / 50)
  expect_equal(do.call(.completion_fit_rows, c(args, target = "engage"))$campaign_id, 1:2)
  expect_error(do.call(.completion_fit_rows, c(args, target = "refuse")), "arg")
})

test_that("fit: targets selects the models", {
  skip_if_no_sequence()
  p <- write_completion_parquet(completion_records(n_per_campaign = 60L))
  m <- completion_model_fit(p, train_until = as.Date("2026-10-01"), min_sent = 10,
                            targets = c("reply", "engage", "optin"))
  expect_named(m$models, c("reply", "engage", "optin"))
  expect_true("reply_score" %in% m$models$reply$coefficients$term)
  expect_true("modeT2W external" %in% m$models$engage$coefficients$term)
  expect_named(m$models$reply$residual_sd, .COMPLETION_MODES)
  expect_true(m$models$reply$counts$campaigns_fit >= 20L)
  expect_equal(m$windows$train_from, as.Date("2025-01-01"))
  expect_output(print(m), "reply model")
  expect_true(all(c("reply", "engage", "optin") %in% summary(m)$model))
  expect_error(completion_model_fit(p, targets = "refuse"), "targets")
  expect_error(completion_model_fit(p, targets = character(0)), "targets")
  expect_error(completion_model_fit(p, targets = c("optin", "optin")), "targets")
})

test_that("band sd rule: the holdout sd when its cell has enough campaigns, else the larger of the two", {
  in_sample <- c(Live = 0.6, T2W = 0.5, X = 0.4, Y = NA, Z = 0.3)
  holdout <- c(Live = 0.4, T2W = 0.7, X = 0.9, Y = 0.2, Z = NA)
  n <- c(Live = 10L, T2W = 10L, X = 9L, Y = 10L, Z = 0L)
  expect_equal(.completion_band_sd_rule(in_sample, holdout, n),
               c(Live = 0.4, T2W = 0.7, X = 0.9, Y = 0.2, Z = NA))
  expect_equal(.COMPLETION_HOLDOUT_MIN_CAMPAIGNS, 10L)
})

test_that("fit: the band sd rule sees the holdout campaign counts per mode and per size cell", {
  skip_if_no_sequence()
  seen <- new_capture()
  seen$n <- list()
  local_mocked_bindings(.completion_band_sd_rule = function(in_sample, holdout, n_holdout) {
    seen$n <- c(seen$n, list(n_holdout))
    ifelse(is.na(in_sample) | is.na(holdout), NA_real_, pmax(in_sample, holdout))
  })
  m <- completion_model_fit(write_completion_parquet(completion_records(n_per_campaign = 60L)),
                            train_until = as.Date("2026-10-01"), min_sent = 10,
                            targets = "completion")
  expect_length(seen$n, 2L)
  expect_named(seen$n[[1L]], c("Live", "T2W"))
  expect_equal(sum(seen$n[[1L]]), m$models$completion$holdout$n)
  expect_equal(sum(seen$n[[2L]]), m$models$completion$holdout$n)
  expect_length(seen$n[[2L]], 4L)
})

test_that("holdout spread is the root mean square of the residuals, not the centred sd", {
  r <- c(1, 1, 1, -2, 0)
  mode <- c("a", "a", "a", "b", "b")
  expect_equal(.completion_sd_by_mode(r, mode, c("a", "b"), rms = TRUE), c(a = 1, b = sqrt(2)))
  expect_equal(.completion_sd_by_mode(r, mode, c("a", "b")), c(a = 0, b = stats::sd(c(-2, 0))))
  cells <- .completion_sd_by_size(r, mode, c(1, 1, 1, 100, 100), c("a", "b"), 50, rms = TRUE)
  expect_equal(cells$sd[cells$mode == "a" & cells$size == "under"], 1)
  expect_equal(cells$sd[cells$mode == "b" & cells$size == "over"], sqrt(2))
})
