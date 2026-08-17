# Coverage for disposition_summary() + its helpers. Synthetic disposition
# Parquet fixtures are written with nanoparquet (no arrow, no network).

# One (phone, campaign) row with exactly the columns disposition_summary() reads
# (`.DISPOSITION_READ_COLS`) and sensible funnel defaults; override via args.
# Writing exactly the read set means nanoparquet does a full read, not a
# `col_select` subset -- which segfaults on multi-row nanoparquet-written files (a
# nanoparquet quirk; the real reader subsets *arrow*-written files, validated
# separately on 2.2M rows).
.disposition_row <- function(phone, campaign_id, engaged = 0L, opted_in = 0L,
                    completed = 0L, web_complete = 0L, terminated = 0L,
                    date_closed_on = as.Date(NA)) {
  data.frame(phone = phone, campaign_id = as.integer(campaign_id),
             engaged = as.integer(engaged),
             opted_in = as.integer(opted_in), completed = as.integer(completed),
             web_complete = as.integer(web_complete),
             terminated = as.integer(terminated),
             date_closed_on = as.Date(date_closed_on), stringsAsFactors = FALSE)
}

# A two-phone fixture reused across tests. write_disposition_parquet() (a shared
# helper in helper-stubs.R) writes the rows to a temp Parquet and returns the path.
.disposition_base <- function() {
  write_disposition_parquet(rbind(
    .disposition_row("2015550101", 2339, engaged = 1, opted_in = 1, completed = 1,
            date_closed_on = "2026-03-01"),
    .disposition_row("2015550101", 2354, engaged = 1, date_closed_on = "2026-04-01"),
    .disposition_row("2015550102", 2339, terminated = 1, date_closed_on = "2026-03-01")
  ))
}

test_that("summarizes one row per phone with cross-campaign flags", {
  res <- disposition_summary(.disposition_base())
  expect_equal(nrow(res), 2L)
  expect_named(res, c("phone", "ever_contacted", "n_campaigns", "ever_engaged",
                      "ever_opted_in", "ever_completed", "ever_terminated",
                      "latest_disposition", "campaigns"))
  r1 <- res[res$phone == "2015550101", ]
  expect_equal(r1$n_campaigns, 2L)
  expect_true(r1$ever_contacted)
  expect_true(r1$ever_completed)      # from 2339
  expect_true(r1$ever_opted_in)
  expect_false(r1$ever_terminated)
  expect_equal(r1$campaigns, "2339,2354")
  expect_equal(r1$latest_disposition, "engaged")   # 2354 is later + only engaged
  r2 <- res[res$phone == "2015550102", ]
  expect_equal(r2$latest_disposition, "terminated")
  expect_true(r2$ever_terminated)
  expect_false(r2$ever_completed)
})

test_that("screens a phone list, normalizing formats and flagging never-contacted", {
  res <- disposition_summary(
    .disposition_base(),
    phones = c("+1 (201) 555-0101", "2015559999", "()"))  # 11-digit, absent, junk
  expect_setequal(res$phone, c("2015550101", "2015559999"))  # junk -> dropped
  nc <- res[res$phone == "2015559999", ]
  expect_false(nc$ever_contacted)
  expect_equal(nc$latest_disposition, "never_contacted")
  expect_equal(nc$n_campaigns, 0L)
  expect_true(is.na(nc$campaigns))
  # the +1/formatted number matched the stored 10-digit one
  expect_true(res[res$phone == "2015550101", "ever_completed"])
})

test_that("campaign_ids filter scopes the underlying rows before rollup", {
  res <- disposition_summary(.disposition_base(), campaign_ids = 2339)
  r1 <- res[res$phone == "2015550101", ]
  expect_equal(r1$campaigns, "2339")
  expect_equal(r1$n_campaigns, 1L)
  expect_equal(r1$latest_disposition, "completed")   # 2354 excluded
})

test_that("statuses filter keeps matching latest_disposition; unknown status errors", {
  res <- disposition_summary(.disposition_base(), statuses = "terminated")
  expect_equal(res$phone, "2015550102")
  expect_error(disposition_summary(.disposition_base(), statuses = "bogus"),
               "unknown status")
})

test_that("date bounds drop rows outside the range (incl. NA close dates)", {
  # date_from keeps only the 2026-04 row (phone 0101 @ 2354).
  res <- disposition_summary(.disposition_base(), date_from = "2026-04-01")
  expect_equal(res$phone, "2015550101")
  expect_equal(res$campaigns, "2354")
  expect_equal(res$latest_disposition, "engaged")
  # date_to keeps only the 2026-03 rows.
  res2 <- disposition_summary(.disposition_base(), date_to = "2026-03-31")
  expect_setequal(res2$campaigns, c("2339", "2339"))
  # a row with an NA close date is dropped by any date bound; when the whole
  # dataset is NA close dates (the beta), that drop-everything is warned.
  p <- write_disposition_parquet(.disposition_row("2015550103", 2400, engaged = 1))  # NA date
  expect_warning(res <- disposition_summary(p, date_from = "2020-01-01"),
                 "returns no rows")
  expect_equal(nrow(res), 0L)
})

test_that("a date bound on empty data does not warn (nothing to drop)", {
  empty <- .disposition_row("x", 1)[0, , drop = FALSE]
  expect_no_warning(res <- disposition_summary(empty, date_from = "2020-01-01"))
  expect_equal(nrow(res), 0L)
})

test_that("each date bound must be a single valid date", {
  d <- .disposition_row("2015550101", 2339, engaged = 1, date_closed_on = "2026-03-01")
  expect_error(disposition_summary(d, date_from = c("2026-01-01", "2026-02-01")),
               "single valid date")
  expect_error(disposition_summary(d, date_to = "not-a-date"), "single valid date")
})

test_that("derived disposition follows funnel precedence", {
  res <- disposition_summary(write_disposition_parquet(rbind(
    .disposition_row("1", 1, engaged = 1, opted_in = 1, completed = 1, web_complete = 1),
    .disposition_row("2", 1, engaged = 1, opted_in = 1, completed = 1),
    .disposition_row("3", 1, engaged = 1, opted_in = 1, terminated = 1),
    .disposition_row("4", 1, engaged = 1, opted_in = 1),
    .disposition_row("5", 1, engaged = 1),
    .disposition_row("6", 1))))
  d <- stats::setNames(res$latest_disposition, res$phone)
  expect_equal(unname(d[c("1", "2", "3", "4", "5", "6")]),
               c("web_complete", "completed", "terminated", "opted_in",
                 "engaged", "non_response"))
})

test_that("t2w_external completed = NA does not become a false completed", {
  res <- disposition_summary(
    write_disposition_parquet(.disposition_row("2015550101", 1, engaged = 1, opted_in = 1,
                      completed = NA_integer_)))
  expect_equal(res$latest_disposition, "opted_in")
  expect_false(res$ever_completed)
})

test_that("pagination slices the phone-ordered result", {
  p <- write_disposition_parquet(rbind(.disposition_row("1", 1), .disposition_row("2", 1), .disposition_row("3", 1)))
  expect_equal(nrow(disposition_summary(p, page = 1, page_size = 2)), 2L)
  expect_equal(disposition_summary(p, page = 2, page_size = 2)$phone, "3")
  expect_equal(nrow(disposition_summary(p, page = 5, page_size = 2)), 0L)
  expect_error(disposition_summary(p, page = 0), "positive integers")
  expect_error(disposition_summary(p, page_size = 1.5), "positive integers")
})

test_that("empty dataset yields an empty result; screened phones come back never-contacted", {
  p0 <- write_disposition_parquet(.disposition_row("x", 1)[0, , drop = FALSE])
  expect_equal(nrow(disposition_summary(p0)), 0L)
  expect_equal(nrow(disposition_summary(p0, page = 1)), 0L)  # page on empty -> no error
  res <- disposition_summary(p0, phones = "2015550101")
  expect_equal(res$phone, "2015550101")
  expect_false(res$ever_contacted)
})

test_that("a blank stored phone is dropped, and all-invalid input yields no rows", {
  p <- write_disposition_parquet(rbind(.disposition_row("2015550101", 1, completed = 1),
                       .disposition_row("", 2)))            # blank phone -> dropped on read
  expect_equal(disposition_summary(p)$phone, "2015550101")
  expect_equal(nrow(disposition_summary(p, phones = "abc")), 0L)
})

test_that("input validation on the x argument", {
  expect_error(disposition_summary(character(0)), "Parquet path.*or.*data frame")
  expect_error(disposition_summary(42), "Parquet path.*or.*data frame")
  expect_error(disposition_summary("/no/such/file.parquet"), "not found")
})

# --- disposition_summary on an in-memory frame -------------------------------

test_that("disposition_summary accepts an in-memory frame and validates input", {
  d <- rbind(
    .disposition_row("2015550101", 2339, engaged = 1, opted_in = 1, completed = 1,
            date_closed_on = "2026-03-01"),
    .disposition_row("2015550101", 2354, engaged = 1, date_closed_on = "2026-04-01"))
  res <- disposition_summary(d, phones = c("2015550101", "2015559999"))
  expect_setequal(res$phone, c("2015550101", "2015559999"))
  expect_true(res[res$phone == "2015550101", "ever_completed"])
  expect_false(res[res$phone == "2015559999", "ever_contacted"])
  # a frame missing the read columns is caught
  expect_error(disposition_summary(d[, c("phone", "campaign_id")]),
               "missing required column")
})

test_that("disposition_summary tolerates a frame without date_closed_on", {
  d <- rbind(
    .disposition_row("2015550101", 2339, engaged = 1, opted_in = 1, completed = 1,
            date_closed_on = "2026-03-01"),
    .disposition_row("2015550101", 2354, engaged = 1, date_closed_on = "2026-04-01"))
  bare <- d[, setdiff(names(d), "date_closed_on"), drop = FALSE]  # un-enriched shape
  res <- disposition_summary(bare, phones = "2015550101")
  expect_true(res$ever_completed)          # summarizes with close dates unknown
  expect_equal(res$n_campaigns, 2L)
  # but a date bound with no date_closed_on column is a clear error
  expect_error(disposition_summary(bare, date_from = "2026-01-01"),
               "date_closed_on")
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
  expect_true(all(c("ever_completed", "latest_disposition", "campaigns") %in%
                    names(out)))
  expect_true(out$ever_completed[1])                       # +1/formatted matched
  expect_equal(out$latest_disposition[2], "terminated")
  expect_false(out$ever_contacted[3])                    # absent -> never_contacted
  expect_equal(out$latest_disposition[3], "never_contacted")
})

test_that("disposition_screen validates sample, phone_col, and column clashes", {
  p <- .disposition_base()
  expect_error(disposition_screen(list(), p), "must be a data frame")
  expect_error(disposition_screen(data.frame(x = 1), p),
               "phone column")
  clash <- data.frame(phone = "2015550101", ever_completed = TRUE,
                      stringsAsFactors = FALSE)
  expect_error(disposition_screen(clash, p), "already has")
})

test_that("disposition_screen appends exactly what disposition_summary computes", {
  # screen is a faithful in-place annotate of the rollup engine: each sample
  # row's appended columns must equal disposition_summary()'s row for that
  # (normalized) phone. Guards the two surfaces against silently diverging.
  p <- .disposition_base()
  sample <- data.frame(
    phone = c("+1 (201) 555-0101", "2015550102", "2015559999"),  # fmt, present, absent
    extra = 1:3, stringsAsFactors = FALSE)
  out <- disposition_screen(sample, p)
  summ <- disposition_summary(p, phones = sample$phone)

  # summ is one row per unique normalized phone; align it to the sample order.
  idx <- match(c("2015550101", "2015550102", "2015559999"), summ$phone)
  for (col in setdiff(names(summ), "phone")) {
    expect_equal(out[[col]], summ[[col]][idx], info = col)
  }
})
