# disposition_steps(): one row per (phone, campaign) with the phone's history up
# to that campaign, as a lazy dbplyr table on the shared DuckDB connection.
# Fixtures: sequence_records() / write_sequence_parquet() (helper-sequence.R).

test_that("steps order each phone's campaigns and label them", {
  skip_if_no_sequence()
  d <- collect_sorted(disposition_steps(write_sequence_parquet()))
  by_phone <- split(d, d$phone)
  expect_named(by_phone, paste0("201555010", 1:5))
  expect_equal(by_phone[["2015550101"]]$label, c("non_response", "engaged", "completed"))
  # 4720 is a hard rejection (undeliverable); 4740 is not in the default list
  expect_equal(by_phone[["2015550102"]]$label,
               c("undeliverable", "non_response", "non_response", "terminated"))
  # same-date tie broken by campaign_id; a leading-1 stored phone is normalized
  expect_equal(by_phone[["2015550103"]]$campaign_id, c(1L, 2L))
  expect_equal(by_phone[["2015550103"]]$label, c("opted_in", "web_complete"))
  # a missing date sorts last; completed NA (t2w_external) is engaged, completion unknown
  expect_equal(by_phone[["2015550104"]]$campaign_id, c(3L, 1L))
  expect_equal(by_phone[["2015550104"]]$label, c("engaged", "non_response"))
  expect_equal(by_phone[["2015550104"]]$completion_known, c(FALSE, TRUE))
  # stored out of date order: refused (Jan) precedes ineligible (Feb)
  expect_equal(by_phone[["2015550105"]]$label, c("refused", "ineligible"))
  expect_equal(d$step, c(1:3, 1:4, 1:2, 1:2, 1:2))
  expect_equal(d$n_campaigns, rep(c(3, 4, 2, 2, 2), c(3, 4, 2, 2, 2)))
})

test_that("history columns: neighbours, gaps, running counts, run length, path", {
  skip_if_no_sequence()
  d <- collect_sorted(disposition_steps(write_sequence_parquet()))
  a <- d[d$phone == "2015550101", ]
  expect_equal(a$prev_label, c(NA, "non_response", "engaged"))
  expect_equal(a$next_label, c("engaged", "completed", NA))
  expect_equal(a$next_date, as.Date(c("2026-02-01", "2026-03-01", NA)))
  expect_equal(a$gap_prev_days, c(NA, 31L, 28L))
  expect_equal(a$gap_next_days, c(31L, 28L, NA))
  expect_equal(a$completes_so_far, c(0, 0, 1))
  expect_equal(a$replies_so_far, c(0, 1, 2))
  expect_equal(a$path_so_far, c("N", "NE", "NEC"))
  b <- d[d$phone == "2015550102", ]
  expect_equal(b$path_so_far, c("X", "XN", "XNN", "XNNT"))
  expect_equal(b$non_responses_so_far, c(1, 2, 3, 3))
  # the trailing run of THIS step's letter: X, then N N, then T
  expect_equal(b$run_length, c(1L, 1L, 2L, 1L))
  # stored columns ride along after the generated ones
  expect_true(all(c("survey_mode", "error", "engaged", "sent") %in% names(d)))
  expect_identical(names(d)[1:7], c("phone", "campaign_id", "disposition_date", "step",
                                    "n_campaigns", "label", "completion_known"))
})

test_that("`columns` picks the history groups; NULL keeps only identity and label", {
  skip_if_no_sequence()
  p <- write_sequence_parquet()
  base <- collect_sorted(disposition_steps(p, columns = NULL))
  expect_false(any(c("prev_label", "run_length", "path_so_far", "completes_so_far") %in% names(base)))
  only_run <- collect_sorted(disposition_steps(p, columns = "run_length"))
  expect_true("run_length" %in% names(only_run))
  expect_false("path_so_far" %in% names(only_run))
  expect_error(disposition_steps(p, columns = "history"),
               "`columns` must be NULL or any of neighbors, so_far, run_length, path_so_far",
               class = "s160_error")
})

test_that("a pattern flags steps whose history matches; it needs path_so_far", {
  skip_if_no_sequence()
  p <- write_sequence_parquet()
  d <- collect_sorted(disposition_steps(p, pattern = "non_response|undeliverable{2,}$"))
  expect_equal(d$history_matched[d$phone == "2015550102"], c(FALSE, TRUE, TRUE, FALSE))
  expect_false(any(d$history_matched[d$phone != "2015550102"]))
  expect_error(disposition_steps(p, columns = "so_far", pattern = "N"),
               "must include \"path_so_far\"", class = "s160_error")
})

test_that("undeliverable_codes and terminal change the labels", {
  skip_if_no_sequence()
  p <- write_sequence_parquet()
  lab <- function(...) collect_sorted(disposition_steps(p, phones = "2015550102", ...))$label
  expect_equal(lab(undeliverable_codes = NULL),
               c("non_response", "non_response", "non_response", "terminated"))
  expect_equal(lab(undeliverable_codes = c(4720, 4740)),
               c("undeliverable", "undeliverable", "non_response", "terminated"))
  expect_equal(lab(terminal = "engaged"),
               c("undeliverable", "non_response", "non_response", "engaged"))
  # a split terminal (refused / ineligible) is not folded back
  r <- collect_sorted(disposition_steps(p, phones = "2015550105", terminal = "engaged"))
  expect_equal(r$label, c("refused", "ineligible"))
  # an unsplit terminal that opted in folds to opted_in
  t2 <- write_sequence_parquet(.sequence_row("2015550199", 1L, "2026-01-01", engaged = 1L,
                                             opted_in = 1L, terminated = 1L))
  expect_equal(collect_sorted(disposition_steps(t2, terminal = "engaged"))$label, "opted_in")
  expect_error(disposition_steps(p, undeliverable_codes = c("4720", NA)),
               "`undeliverable_codes` must be a character vector", class = "s160_error")
  expect_error(disposition_steps(p, terminal = "drop"), "should be one of")
})

test_that("phones and sample_frac scope whole phones before the history", {
  skip_if_no_sequence()
  p <- write_sequence_parquet()
  d <- collect_sorted(disposition_steps(p, phones = c("(201) 555-0103", "12015550101", NA)))
  expect_equal(unique(d$phone), c("2015550101", "2015550103"))
  expect_equal(nrow(collect_sorted(disposition_steps(p, phones = character(0)))), 0L)
  expect_equal(nrow(collect_sorted(disposition_steps(p, phones = NA))), 0L)
  expect_equal(nrow(collect_sorted(disposition_steps(p, sample_frac = 1))), 13L)
  half <- collect_sorted(disposition_steps(p, sample_frac = 0.5))
  expect_identical(half, collect_sorted(disposition_steps(p, sample_frac = 0.5)))
  # whole phones: every kept phone keeps all of its campaigns
  expect_true(all(table(half$phone) == half$n_campaigns[match(names(table(half$phone)), half$phone)]))
  expect_lt(length(unique(half$phone)), 5L)
  expect_error(disposition_steps(p, sample_frac = 0), "`sample_frac` must be a single number in \\(0, 1\\]",
               class = "s160_error")
  expect_error(disposition_steps(p, sample_frac = c(0.1, 0.2)), "`sample_frac`", class = "s160_error")
})

test_that("campaign and date filters keep rows after the history is computed", {
  skip_if_no_sequence()
  p <- write_sequence_parquet()
  d <- collect_sorted(disposition_steps(p, campaign_ids = 3, phones = "2015550101"))
  expect_equal(d$step, 3L)
  expect_equal(d$path_so_far, "NEC")
  d <- collect_sorted(disposition_steps(p, date_from = "2026-02-15", date_to = as.Date("2026-03-01"),
                                        phones = "2015550102"))
  expect_equal(d$step, 4L)
  expect_equal(d$non_responses_so_far, 3)
  expect_error(disposition_steps(p, campaign_ids = c(1, NA)), "`campaign_ids` must be a vector",
               class = "s160_error")
  expect_error(disposition_steps(p, date_from = "not a date"), "`date_from` must be a single valid date",
               class = "s160_error")
})

test_that("an older projection without the optional columns still reads", {
  skip_if_no_sequence()
  rows <- sequence_records()[, c("phone", "campaign_id", "engaged", "opted_in", "completed",
                                 "disposition_date")]
  d <- collect_sorted(disposition_steps(write_sequence_parquet(rows)))
  expect_setequal(unique(d$label), c("non_response", "engaged", "opted_in", "completed"))
  expect_equal(d$label[d$phone == "2015550102"], rep(c("non_response", "engaged"), c(3, 1)))
})

test_that("dataset problems are reported", {
  skip_if_no_sequence()
  rows <- sequence_records()
  expect_error(disposition_steps(write_sequence_parquet(rows[, setdiff(names(rows), "opted_in")])),
               "`dataset` is missing required column\\(s\\): opted_in", class = "s160_error")
  rows$label <- "x"
  expect_error(disposition_steps(write_sequence_parquet(rows)),
               "named like the sequence columns these readers add: label", class = "s160_error")
  globbed <- file.path(tempdir(), "seq[1].parquet")
  file.copy(write_sequence_parquet(), globbed)
  expect_error(disposition_steps(globbed), "must be a literal file path", class = "s160_error")
  expect_error(disposition_steps(file.path(tempdir(), "absent.parquet")),
               class = "s160_not_found")
})

test_that("a missing dependency is named with the install command", {
  local_mocked_bindings(.disposition_lazy_missing = function() c("dbplyr"))
  expect_error(disposition_steps("x.parquet"),
               "disposition_steps: needs dbplyr installed: install.packages\\(c\\(\"dbplyr\"\\)\\)",
               class = "s160_error")
  expect_error(disposition_paths("x.parquet"), "disposition_paths: needs dbplyr", class = "s160_error")
})

test_that("tables share one session connection, configured from options", {
  skip_if_no_sequence()
  p <- write_sequence_parquet()
  a <- disposition_steps(p)
  b <- disposition_paths(p)
  expect_identical(dbplyr::remote_con(a), dbplyr::remote_con(b))
  con <- dbplyr::remote_con(a)
  withr::local_options(survey160r.duckdb_memory = "1GB")
  disposition_steps(p)
  expect_equal(DBI::dbGetQuery(con, "SELECT current_setting('memory_limit') AS m")$m, "953.6 MiB")
  withr::local_options(survey160r.duckdb_memory = "lots")
  expect_error(disposition_steps(p), "DuckDB rejected the connection settings \\(memory lots",
               class = "s160_error")
  withr::local_options(survey160r.duckdb_memory = 4)
  expect_error(disposition_steps(p), "option `survey160r.duckdb_memory` must be a single non-empty string",
               class = "s160_error")
})

test_that("closing the session connection reopens it on the next call", {
  skip_if_no_sequence()
  p <- write_sequence_parquet()
  old <- dbplyr::remote_con(disposition_steps(p))
  .disposition_lazy_close()
  expect_false(DBI::dbIsValid(old))
  .disposition_lazy_close()   # idempotent
  fresh <- disposition_steps(p)
  expect_true(DBI::dbIsValid(dbplyr::remote_con(fresh)))
  expect_equal(nrow(collect_sorted(fresh)), 13L)
})
