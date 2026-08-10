# Coverage for s160_disposition_query() + its helpers. Synthetic disposition
# Parquet fixtures are written with nanoparquet (no arrow, no network).

# One (phone, campaign) row with the columns the query reads and sensible funnel
# defaults; override via args. (The reader `col_select`s exactly these columns;
# reading them out of the wider 16-column production schema -- incl. an all-NA
# date_closed_on and a timestamp column -- is validated separately.)
.dq_row <- function(phone, campaign_id, started = 1L, engaged = 0L, opt_in = 0L,
                    complete = 0L, web_complete = 0L, terminated = 0L,
                    date_closed_on = as.Date(NA)) {
  data.frame(phone = phone, campaign_id = as.integer(campaign_id),
             started = as.integer(started), engaged = as.integer(engaged),
             opt_in = as.integer(opt_in), complete = as.integer(complete),
             web_complete = as.integer(web_complete),
             terminated = as.integer(terminated),
             date_closed_on = as.Date(date_closed_on), stringsAsFactors = FALSE)
}

# Write per-(phone, campaign) rows to a temp disposition Parquet; return path.
.dq_write <- function(rows) {
  p <- tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(rows, p)
  p
}

# A two-phone fixture reused across tests.
.dq_base <- function() {
  .dq_write(rbind(
    .dq_row("2015550101", 2339, engaged = 1, opt_in = 1, complete = 1,
            date_closed_on = "2026-03-01"),
    .dq_row("2015550101", 2354, engaged = 1, date_closed_on = "2026-04-01"),
    .dq_row("2015550102", 2339, terminated = 1, date_closed_on = "2026-03-01")
  ))
}

test_that("summarizes one row per phone with cross-campaign flags", {
  res <- s160_disposition_query(.dq_base())
  expect_equal(nrow(res), 2L)
  expect_named(res, c("phone", "ever_contacted", "n_campaigns", "ever_engaged",
                      "ever_opted_in", "ever_complete", "ever_terminated",
                      "latest_disposition", "campaigns"))
  r1 <- res[res$phone == "2015550101", ]
  expect_equal(r1$n_campaigns, 2L)
  expect_true(r1$ever_contacted)
  expect_true(r1$ever_complete)      # from 2339
  expect_true(r1$ever_opted_in)
  expect_false(r1$ever_terminated)
  expect_equal(r1$campaigns, "2339,2354")
  expect_equal(r1$latest_disposition, "engaged")   # 2354 is later + only engaged
  r2 <- res[res$phone == "2015550102", ]
  expect_equal(r2$latest_disposition, "terminated")
  expect_true(r2$ever_terminated)
  expect_false(r2$ever_complete)
})

test_that("screens a phone list, normalizing formats and flagging never-contacted", {
  res <- s160_disposition_query(
    .dq_base(),
    phones = c("+1 (201) 555-0101", "2015559999", "()"))  # 11-digit, absent, junk
  expect_setequal(res$phone, c("2015550101", "2015559999"))  # junk -> dropped
  nc <- res[res$phone == "2015559999", ]
  expect_false(nc$ever_contacted)
  expect_equal(nc$latest_disposition, "never_contacted")
  expect_equal(nc$n_campaigns, 0L)
  expect_true(is.na(nc$campaigns))
  # the +1/formatted number matched the stored 10-digit one
  expect_true(res[res$phone == "2015550101", "ever_complete"])
})

test_that("campaign_ids filter scopes the underlying rows before rollup", {
  res <- s160_disposition_query(.dq_base(), campaign_ids = 2339)
  r1 <- res[res$phone == "2015550101", ]
  expect_equal(r1$campaigns, "2339")
  expect_equal(r1$n_campaigns, 1L)
  expect_equal(r1$latest_disposition, "complete")   # 2354 excluded
})

test_that("statuses filter keeps matching latest_disposition; unknown status errors", {
  res <- s160_disposition_query(.dq_base(), statuses = "terminated")
  expect_equal(res$phone, "2015550102")
  expect_error(s160_disposition_query(.dq_base(), statuses = "bogus"),
               "unknown status")
})

test_that("date bounds drop rows outside the range (incl. NA close dates)", {
  # date_from keeps only the 2026-04 row (phone 0101 @ 2354).
  res <- s160_disposition_query(.dq_base(), date_from = "2026-04-01")
  expect_equal(res$phone, "2015550101")
  expect_equal(res$campaigns, "2354")
  expect_equal(res$latest_disposition, "engaged")
  # date_to keeps only the 2026-03 rows.
  res2 <- s160_disposition_query(.dq_base(), date_to = "2026-03-31")
  expect_setequal(res2$campaigns, c("2339", "2339"))
  # a row with an NA close date is dropped by any date bound.
  p <- .dq_write(.dq_row("2015550103", 2400, engaged = 1))  # NA date
  expect_equal(nrow(s160_disposition_query(p, date_from = "2020-01-01")), 0L)
})

test_that("derived disposition follows funnel precedence", {
  res <- s160_disposition_query(.dq_write(rbind(
    .dq_row("1", 1, engaged = 1, opt_in = 1, complete = 1, web_complete = 1),
    .dq_row("2", 1, engaged = 1, opt_in = 1, complete = 1),
    .dq_row("3", 1, engaged = 1, opt_in = 1, terminated = 1),
    .dq_row("4", 1, engaged = 1, opt_in = 1),
    .dq_row("5", 1, engaged = 1),
    .dq_row("6", 1))))
  d <- stats::setNames(res$latest_disposition, res$phone)
  expect_equal(unname(d[c("1", "2", "3", "4", "5", "6")]),
               c("web_complete", "complete", "terminated", "opt_in",
                 "engaged", "non_response"))
})

test_that("t2w_external complete = NA does not become a false complete", {
  res <- s160_disposition_query(
    .dq_write(.dq_row("2015550101", 1, engaged = 1, opt_in = 1,
                      complete = NA_integer_)))
  expect_equal(res$latest_disposition, "opt_in")
  expect_false(res$ever_complete)
})

test_that("pagination slices the phone-ordered result", {
  p <- .dq_write(rbind(.dq_row("1", 1), .dq_row("2", 1), .dq_row("3", 1)))
  expect_equal(nrow(s160_disposition_query(p, page = 1, page_size = 2)), 2L)
  expect_equal(s160_disposition_query(p, page = 2, page_size = 2)$phone, "3")
  expect_equal(nrow(s160_disposition_query(p, page = 5, page_size = 2)), 0L)
  expect_error(s160_disposition_query(p, page = 0), "positive integers")
  expect_error(s160_disposition_query(p, page_size = 1.5), "positive integers")
})

test_that("empty dataset yields an empty result; screened phones come back never-contacted", {
  p0 <- .dq_write(.dq_row("x", 1)[0, , drop = FALSE])
  expect_equal(nrow(s160_disposition_query(p0)), 0L)
  res <- s160_disposition_query(p0, phones = "2015550101")
  expect_equal(res$phone, "2015550101")
  expect_false(res$ever_contacted)
})

test_that("a blank stored phone is dropped, and all-invalid input yields no rows", {
  p <- .dq_write(rbind(.dq_row("2015550101", 1, complete = 1),
                       .dq_row("", 2)))            # blank phone -> dropped on read
  expect_equal(s160_disposition_query(p)$phone, "2015550101")
  expect_equal(nrow(s160_disposition_query(p, phones = "abc")), 0L)
})

test_that("input validation on the dataset path", {
  expect_error(s160_disposition_query(character(0)), "single Parquet path")
  expect_error(s160_disposition_query("/no/such/file.parquet"), "not found")
})
