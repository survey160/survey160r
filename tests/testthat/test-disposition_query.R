# Coverage for disposition_query() + its helpers. Synthetic disposition
# Parquet fixtures are written with nanoparquet (no arrow, no network).

# One (phone, campaign) row with exactly the columns the query reads (`.DISPOSITION_READ_
# COLS`) and sensible funnel defaults; override via args. Writing exactly the read
# set means nanoparquet does a full read, not a `col_select` subset -- which
# segfaults on multi-row nanoparquet-written files (a nanoparquet quirk; the real
# reader subsets *arrow*-written files, validated separately on 2.2M rows).
.disposition_row <- function(phone, campaign_id, engaged = 0L, opt_in = 0L,
                    complete = 0L, web_complete = 0L, terminated = 0L,
                    date_closed_on = as.Date(NA)) {
  data.frame(phone = phone, campaign_id = as.integer(campaign_id),
             engaged = as.integer(engaged),
             opt_in = as.integer(opt_in), complete = as.integer(complete),
             web_complete = as.integer(web_complete),
             terminated = as.integer(terminated),
             date_closed_on = as.Date(date_closed_on), stringsAsFactors = FALSE)
}

# Write per-(phone, campaign) rows to a temp disposition Parquet; return path.
.disposition_write <- function(rows) {
  p <- tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(rows, p)
  p
}

# A two-phone fixture reused across tests.
.disposition_base <- function() {
  .disposition_write(rbind(
    .disposition_row("2015550101", 2339, engaged = 1, opt_in = 1, complete = 1,
            date_closed_on = "2026-03-01"),
    .disposition_row("2015550101", 2354, engaged = 1, date_closed_on = "2026-04-01"),
    .disposition_row("2015550102", 2339, terminated = 1, date_closed_on = "2026-03-01")
  ))
}

test_that("summarizes one row per phone with cross-campaign flags", {
  res <- disposition_query(.disposition_base())
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
  res <- disposition_query(
    .disposition_base(),
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
  res <- disposition_query(.disposition_base(), campaign_ids = 2339)
  r1 <- res[res$phone == "2015550101", ]
  expect_equal(r1$campaigns, "2339")
  expect_equal(r1$n_campaigns, 1L)
  expect_equal(r1$latest_disposition, "complete")   # 2354 excluded
})

test_that("statuses filter keeps matching latest_disposition; unknown status errors", {
  res <- disposition_query(.disposition_base(), statuses = "terminated")
  expect_equal(res$phone, "2015550102")
  expect_error(disposition_query(.disposition_base(), statuses = "bogus"),
               "unknown status")
})

test_that("date bounds drop rows outside the range (incl. NA close dates)", {
  # date_from keeps only the 2026-04 row (phone 0101 @ 2354).
  res <- disposition_query(.disposition_base(), date_from = "2026-04-01")
  expect_equal(res$phone, "2015550101")
  expect_equal(res$campaigns, "2354")
  expect_equal(res$latest_disposition, "engaged")
  # date_to keeps only the 2026-03 rows.
  res2 <- disposition_query(.disposition_base(), date_to = "2026-03-31")
  expect_setequal(res2$campaigns, c("2339", "2339"))
  # a row with an NA close date is dropped by any date bound.
  p <- .disposition_write(.disposition_row("2015550103", 2400, engaged = 1))  # NA date
  expect_equal(nrow(disposition_query(p, date_from = "2020-01-01")), 0L)
})

test_that("each date bound must be a single valid date", {
  d <- .disposition_row("2015550101", 2339, engaged = 1, date_closed_on = "2026-03-01")
  expect_error(disposition_summary(d, date_from = c("2026-01-01", "2026-02-01")),
               "single valid date")
  expect_error(disposition_summary(d, date_to = "not-a-date"), "single valid date")
})

test_that("derived disposition follows funnel precedence", {
  res <- disposition_query(.disposition_write(rbind(
    .disposition_row("1", 1, engaged = 1, opt_in = 1, complete = 1, web_complete = 1),
    .disposition_row("2", 1, engaged = 1, opt_in = 1, complete = 1),
    .disposition_row("3", 1, engaged = 1, opt_in = 1, terminated = 1),
    .disposition_row("4", 1, engaged = 1, opt_in = 1),
    .disposition_row("5", 1, engaged = 1),
    .disposition_row("6", 1))))
  d <- stats::setNames(res$latest_disposition, res$phone)
  expect_equal(unname(d[c("1", "2", "3", "4", "5", "6")]),
               c("web_complete", "complete", "terminated", "opt_in",
                 "engaged", "non_response"))
})

test_that("t2w_external complete = NA does not become a false complete", {
  res <- disposition_query(
    .disposition_write(.disposition_row("2015550101", 1, engaged = 1, opt_in = 1,
                      complete = NA_integer_)))
  expect_equal(res$latest_disposition, "opt_in")
  expect_false(res$ever_complete)
})

test_that("pagination slices the phone-ordered result", {
  p <- .disposition_write(rbind(.disposition_row("1", 1), .disposition_row("2", 1), .disposition_row("3", 1)))
  expect_equal(nrow(disposition_query(p, page = 1, page_size = 2)), 2L)
  expect_equal(disposition_query(p, page = 2, page_size = 2)$phone, "3")
  expect_equal(nrow(disposition_query(p, page = 5, page_size = 2)), 0L)
  expect_error(disposition_query(p, page = 0), "positive integers")
  expect_error(disposition_query(p, page_size = 1.5), "positive integers")
})

test_that("empty dataset yields an empty result; screened phones come back never-contacted", {
  p0 <- .disposition_write(.disposition_row("x", 1)[0, , drop = FALSE])
  expect_equal(nrow(disposition_query(p0)), 0L)
  expect_equal(nrow(disposition_query(p0, page = 1)), 0L)  # page on empty -> no error
  res <- disposition_query(p0, phones = "2015550101")
  expect_equal(res$phone, "2015550101")
  expect_false(res$ever_contacted)
})

test_that("a blank stored phone is dropped, and all-invalid input yields no rows", {
  p <- .disposition_write(rbind(.disposition_row("2015550101", 1, complete = 1),
                       .disposition_row("", 2)))            # blank phone -> dropped on read
  expect_equal(disposition_query(p)$phone, "2015550101")
  expect_equal(nrow(disposition_query(p, phones = "abc")), 0L)
})

test_that("input validation on the dataset path", {
  expect_error(disposition_query(character(0)), "single Parquet path")
  expect_error(disposition_query("/no/such/file.parquet"), "not found")
})

# --- disposition_summary (pure core) ------------------------------------------

test_that("disposition_summary works on an in-memory frame and validates input", {
  d <- rbind(
    .disposition_row("2015550101", 2339, engaged = 1, opt_in = 1, complete = 1,
            date_closed_on = "2026-03-01"),
    .disposition_row("2015550101", 2354, engaged = 1, date_closed_on = "2026-04-01"))
  res <- disposition_summary(d, phones = c("2015550101", "2015559999"))
  expect_setequal(res$phone, c("2015550101", "2015559999"))
  expect_true(res[res$phone == "2015550101", "ever_complete"])
  expect_false(res[res$phone == "2015559999", "ever_contacted"])
  # validation branches (unreachable via the file reader, which always has cols)
  expect_error(disposition_summary(list()), "must be a data frame")
  expect_error(disposition_summary(d[, c("phone", "campaign_id")]),
               "missing column")
})

# --- disposition_screen --------------------------------------------------

test_that("disposition_screen annotates the sample in place, preserving it", {
  sample <- data.frame(
    phone = c("+1 (201) 555-0101", "2015550102", "2015559999"),  # fmt, present, absent
    region = c("NE", "NE", "SW"), quota = c("A", "A", "B"),
    stringsAsFactors = FALSE)
  out <- disposition_screen(sample, .disposition_base())

  expect_equal(out$phone, sample$phone)          # original formatting kept
  expect_equal(out$region, c("NE", "NE", "SW"))  # original columns preserved
  expect_true(all(c("ever_complete", "latest_disposition", "campaigns") %in%
                    names(out)))
  expect_true(out$ever_complete[1])                       # +1/formatted matched
  expect_equal(out$latest_disposition[2], "terminated")
  expect_false(out$ever_contacted[3])                    # absent -> never_contacted
  expect_equal(out$latest_disposition[3], "never_contacted")
})

test_that("disposition_screen validates sample, phone_col, and column clashes", {
  p <- .disposition_base()
  expect_error(disposition_screen(list(), p), "must be a data frame")
  expect_error(disposition_screen(data.frame(x = 1), p),
               "phone column")
  clash <- data.frame(phone = "2015550101", ever_complete = TRUE,
                      stringsAsFactors = FALSE)
  expect_error(disposition_screen(clash, p), "already has")
})
