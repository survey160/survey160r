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
  expect_true(all(h$completion_known))
  expect_true(all(is.na(h$tracker_loi)))
})

test_that("context tables: one block per year, estimated on earlier years only", {
  skip_if_no_sequence()
  p <- write_completion_parquet(completion_records())
  con <- .disposition_lazy_con()
  src <- .disposition_lazy_source(con, p, "test")
  ctx <- .completion_context_tables(con, .completion_history_sql(con, src))
  expect_setequal(unique(ctx$year), 2025:2027)
  first <- ctx[ctx$context == "___", ]
  expect_equal(nrow(first), 3L)
  # the 2027 block sees every known row; the 2025 block only 2024's
  expect_gt(first$n[first$year == 2027], first$n[first$year == 2025])
  expect_true(all(ctx$p_complete >= 0 & ctx$p_complete <= 1))
})

test_that("campaign table: one row per campaign with dimensions and sample features", {
  skip_if_no_sequence()
  p <- write_completion_parquet(completion_records())
  con <- .disposition_lazy_con()
  src <- .disposition_lazy_source(con, p, "test")
  hs <- .completion_history_sql(con, src)
  ct <- .completion_campaign_table(con, hs, .completion_context_tables(con, hs))
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
  ct <- .completion_campaign_table(con, hs, .completion_context_tables(con, hs))
  expect_equal(ct$mode[ct$campaign_id %in% 1:2], c("Live", "T2W"))
  expect_true(all(ct$topic == "unknown"))
  expect_true(all(is.na(ct$loi)))
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
