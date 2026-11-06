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
