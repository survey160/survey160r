# Coverage for disposition_records() -- the raw per-(phone, campaign) reader.
# Full-schema (SUR-1518) disposition Parquet fixtures, written with nanoparquet.
# records() reads the whole file (col_select = NULL), so a full read -- never a
# `col_select` subset -- runs on these nanoparquet-written fixtures (the subset
# path segfaults on multi-row nanoparquet files; see test-disposition_read.R).

# One full-schema (phone, campaign) row; override any column. Defaults model a
# contacted-but-no-reply t2w record.
.record_row <- function(phone, campaign_id, started = 1L, engaged = 0L,
                        opt_in = 0L, complete = 0L, web_complete = 0L,
                        terminated = 0L, error = NA_character_, loi = NA_real_,
                        topic = NA_character_, mode = "t2w",
                        date_closed_on = as.Date(NA)) {
  data.frame(
    phone = phone, campaign_id = as.integer(campaign_id),
    started = as.integer(started), engaged = as.integer(engaged),
    opt_in = as.integer(opt_in), complete = as.integer(complete),
    web_complete = as.integer(web_complete),
    terminated = as.integer(terminated), error = as.character(error),
    loi = as.numeric(loi), topic = as.character(topic),
    mode = as.character(mode), date_closed_on = as.Date(date_closed_on),
    stringsAsFactors = FALSE
  )
}

# Two phones across three (phone, campaign) rows, written OUT of phone order.
# write_disposition_parquet() (a shared helper in helper-stubs.R) writes the
# rows to a temp Parquet and returns the path.
.record_base <- function() {
  write_disposition_parquet(rbind(
    .record_row("2015550102", 2339, terminated = 1, mode = "sms", loi = 9,
                topic = "Policy", date_closed_on = "2026-03-01"),
    .record_row("2015550101", 2354, engaged = 1, loi = 11, topic = "Brand",
                date_closed_on = "2026-04-01"),
    .record_row("2015550101", 2339, engaged = 1, opt_in = 1, complete = 1,
                web_complete = 1, loi = 12, topic = "Brand",
                date_closed_on = "2026-03-01")
  ))
}

.RECORD_COLS <- c("phone", "campaign_id", "started", "engaged", "opt_in",
                  "complete", "web_complete", "terminated", "error", "loi",
                  "topic", "mode", "date_closed_on")

test_that("returns raw rows, one per (phone, campaign), full schema, ordered", {
  res <- disposition_records(.record_base())
  expect_equal(nrow(res), 3L)
  expect_named(res, .RECORD_COLS)
  # ordered by phone then campaign_id (input was unsorted)
  expect_equal(res$phone, c("2015550101", "2015550101", "2015550102"))
  expect_equal(res$campaign_id, c(2339L, 2354L, 2339L))
  # per-(phone, campaign) values, NOT rolled up
  expect_equal(res$web_complete, c(1L, 0L, 0L))
  expect_equal(res$mode, c("t2w", "t2w", "sms"))
  expect_equal(res$topic, c("Brand", "Brand", "Policy"))
})

test_that("phones filter normalizes and returns only stored rows (no never-contacted)", {
  res <- disposition_records(
    .record_base(),
    phones = c("+1 (201) 555-0101", "2015559999", "()"))  # 11-digit, absent, junk
  expect_equal(unique(res$phone), "2015550101")   # matched; absent/junk -> nothing
  expect_equal(nrow(res), 2L)                      # 0101 is in two campaigns
  expect_false("2015559999" %in% res$phone)        # never-contacted absent, unlike disposition_summary()
})

test_that("campaign_ids filter scopes the rows", {
  res <- disposition_records(.record_base(), campaign_ids = 2339)
  expect_setequal(res$campaign_id, 2339L)
  expect_equal(nrow(res), 2L)                      # 0101@2339 + 0102@2339
})

test_that("date bounds filter on date_closed_on; NA close dates drop", {
  res <- disposition_records(.record_base(), date_from = "2026-04-01")
  expect_equal(res$phone, "2015550101")
  expect_equal(res$campaign_id, 2354L)
  res2 <- disposition_records(.record_base(), date_to = "2026-03-31")
  expect_equal(nrow(res2), 2L)
  expect_true(all(res2$campaign_id == 2339L))
  # a row with an NA close date is dropped by any bound; an all-NA dataset (the
  # beta) warns that the filter drops everything.
  p <- write_disposition_parquet(.record_row("2015550103", 2400, engaged = 1))  # NA date
  expect_warning(res <- disposition_records(p, date_from = "2020-01-01"),
                 "returns no rows")
  expect_equal(nrow(res), 0L)
})

test_that("a date bound with no date_closed_on column errors", {
  bare_cols <- c("phone", "campaign_id", "started", "engaged", "opt_in",
                 "complete", "web_complete", "terminated", "mode")
  p <- write_disposition_parquet(.record_row("2015550101", 2339, engaged = 1)[, bare_cols])
  expect_error(disposition_records(p, date_from = "2026-01-01"), "date_closed_on")
  expect_error(disposition_records(p, date_to = "2026-01-01"), "date_closed_on")
})

test_that("an un-enriched projection returns just the nine computed columns", {
  bare_cols <- c("phone", "campaign_id", "started", "engaged", "opt_in",
                 "complete", "web_complete", "terminated", "mode")
  res <- disposition_records(
    write_disposition_parquet(.record_row("2015550101", 2339, engaged = 1)[, bare_cols]))
  expect_named(res, bare_cols)
  expect_equal(nrow(res), 1L)
})

test_that("output is canonical order; extra (provenance) columns are dropped", {
  row <- .record_row("2015550101", 2339, engaged = 1, loi = 12, topic = "Brand",
                     error = "DELIVERY_FAILED", date_closed_on = "2026-03-01")
  row$source_csv_hash <- "abc123"                        # extra column
  row <- row[, c("mode", "source_csv_hash", "campaign_id", "phone", "loi",
                 "topic", "date_closed_on", "started", "engaged", "opt_in",
                 "complete", "web_complete", "terminated", "error")]  # scrambled
  res <- disposition_records(write_disposition_parquet(row))
  expect_named(res, .RECORD_COLS)                        # canonical order restored
  expect_false("source_csv_hash" %in% names(res))        # extra dropped
  expect_equal(res$error, "DELIVERY_FAILED")             # error can carry a value
})

test_that("pagination slices the ordered rows; invalid page errors", {
  p <- write_disposition_parquet(rbind(.record_row("1", 1), .record_row("2", 1),
                           .record_row("3", 1)))
  expect_equal(nrow(disposition_records(p, page = 1, page_size = 2)), 2L)
  expect_equal(disposition_records(p, page = 2, page_size = 2)$phone, "3")
  expect_equal(nrow(disposition_records(p, page = 5, page_size = 2)), 0L)
  expect_error(disposition_records(p, page = 0), "positive integers")
})

test_that("input validation on the dataset path and date bounds", {
  expect_error(disposition_records(character(0)), "single Parquet path")
  expect_error(disposition_records("/no/such/file.parquet"), "not found")
  p <- .record_base()
  expect_error(disposition_records(p, date_from = c("2026-01-01", "2026-02-01")),
               "single valid date")
  expect_error(disposition_records(p, date_to = "not-a-date"), "single valid date")
})

test_that("empty projection and no-match yield zero rows; blank phone dropped", {
  p0 <- write_disposition_parquet(.record_row("x", 1)[0, , drop = FALSE])
  expect_equal(nrow(disposition_records(p0)), 0L)
  expect_equal(nrow(disposition_records(.record_base(), phones = "abc")), 0L)
  # a blank stored phone is dropped on read
  p <- write_disposition_parquet(rbind(.record_row("2015550101", 1), .record_row("", 2)))
  expect_equal(disposition_records(p)$phone, "2015550101")
})

test_that("phones and campaign_ids filters combine (AND)", {
  res <- disposition_records(.record_base(), phones = "2015550101",
                             campaign_ids = 2339)
  expect_equal(nrow(res), 1L)          # 0101 is in 2339 and 2354; keep only 2339
  expect_equal(res$campaign_id, 2339L)
})

test_that("a projection missing phone or campaign_id errors clearly", {
  full <- .record_row("2015550101", 2339, engaged = 1)
  no_phone <- write_disposition_parquet(full[, setdiff(names(full), "phone")])
  no_campaign <- write_disposition_parquet(full[, setdiff(names(full), "campaign_id")])
  expect_error(disposition_records(no_phone), "missing required column")
  expect_error(disposition_records(no_campaign), "missing required column")
})
