# Coverage for disposition_summary() + its helpers. Fixtures use the shared
# .disposition_row() + write_disposition_parquet() helpers (helper-stubs.R):
# per-(phone, campaign) frames written to a temp Parquet and read back through
# the real nanoparquet path (no arrow, no network). The reader intersects its
# col_select with the file's actual schema (see .disposition_read_parquet), so a
# fixture need only carry the columns a test asserts on.

# A two-phone fixture reused across tests. write_disposition_parquet() (a shared
# helper in helper-stubs.R) writes the rows to a temp Parquet and returns the path.
.disposition_base <- function() {
  write_disposition_parquet(rbind(
    .disposition_row("2015550101", 2339, engaged = 1, opted_in = 1, completed = 1,
            disposition_date = "2026-03-01"),
    .disposition_row("2015550101", 2354, engaged = 1, disposition_date = "2026-04-01"),
    .disposition_row("2015550102", 2339, terminated = 1, disposition_date = "2026-03-01")
  ))
}

test_that("summarizes one row per phone with cross-campaign counts", {
  res <- disposition_summary(.disposition_base())
  expect_equal(nrow(res), 2L)
  expect_named(res, c("phone", "n_campaigns", "campaigns", "n_engaged",
                      "n_opted_in", "n_completed", "n_web_complete",
                      "n_terminated", "n_refused", "n_ineligible", "n_error",
                      "latest_disposition",
                      "latest_campaign_id", "best_disposition", "best_campaign_id",
                      "first_disposition_date", "last_disposition_date"))
  r1 <- res[res$phone == "2015550101", ]
  expect_equal(r1$n_campaigns, 2L)
  expect_equal(r1$n_engaged, 2L)       # cumulative: both 2339 + 2354 engaged
  expect_equal(r1$n_opted_in, 1L)      # only 2339
  expect_equal(r1$n_completed, 1L)     # 2339 (a completed campaign is also engaged)
  expect_equal(r1$n_web_complete, 0L)
  expect_equal(r1$n_terminated, 0L)
  expect_equal(r1$n_error, 0L)         # no error column in the fixture -> 0
  expect_equal(r1$first_disposition_date, as.Date("2026-03-01"))
  expect_equal(r1$last_disposition_date, as.Date("2026-04-01"))
  expect_equal(r1$campaigns, "2339,2354")
  expect_equal(r1$latest_disposition, "engaged")   # 2354 is later + only engaged
  expect_equal(r1$latest_campaign_id, "2354")
  expect_equal(r1$best_disposition, "completed")   # furthest reached, from 2339
  expect_equal(r1$best_campaign_id, "2339")
  r2 <- res[res$phone == "2015550102", ]
  expect_equal(r2$latest_disposition, "terminated")
  expect_equal(r2$best_disposition, "terminated")  # its only campaign
  expect_equal(r2$best_campaign_id, "2339")
  expect_equal(r2$n_terminated, 1L)
  expect_equal(r2$n_completed, 0L)
  expect_equal(r2$first_disposition_date, as.Date("2026-03-01"))
})

test_that("terminated splits into refused / ineligible; the residual stays terminated", {
  # survey160r 0.51.0 split: refused (declined) vs ineligible (screened out),
  # name-derived. A downstream projection keeps `terminated` (an authoritative
  # status) as a SUPERSET, so a row can be terminated with neither flag set (an
  # in-survey `q_7` screener the name match could not classify) -> its category
  # stays `terminated`. p3 is that residual in its real shape: it REACHED q_7 (a
  # continuation, so opted_in = 1) yet was terminated -- `terminated` must win over
  # `opted_in` (a hard stop outranks consent), not fall back to `opted_in`.
  d <- data.frame(
    phone = c("2015550201", "2015550202", "2015550203"),
    campaign_id = 1L,
    engaged = 1L, opted_in = c(0L, 0L, 1L), completed = 0L, web_complete = 0L,
    refused    = c(1L, 0L, 0L),
    ineligible = c(0L, 1L, 0L),
    terminated = c(1L, 1L, 1L),   # all terminal (superset); p3 is the unsplit residual
    disposition_date = as.Date("2026-03-01"), stringsAsFactors = FALSE)
  res <- disposition_summary(d)
  res <- res[order(res$phone), ]
  expect_equal(res$latest_disposition, c("refused", "ineligible", "terminated"))
  expect_equal(res$n_opted_in, c(0L, 0L, 1L))       # p3 reached the continuation
  expect_equal(res$n_refused,    c(1L, 0L, 0L))
  expect_equal(res$n_ineligible, c(0L, 1L, 0L))
  expect_equal(res$n_terminated, c(1L, 1L, 1L))   # the union/superset count
})

test_that("best_disposition ranks refused > ineligible; records + statuses expose the split", {
  # RANK, isolated from recency: refused in the EARLY campaign, ineligible in the
  # LATE one -> latest is `ineligible` (recency) but best is `refused` (higher
  # terminal rank), proving the .DISPOSITION_CATEGORIES ordering terminated <
  # ineligible < refused.
  d <- data.frame(
    phone = "2015550301", campaign_id = c(10L, 20L), sent = 1L,
    engaged = 1L, opted_in = 0L, completed = 0L, web_complete = 0L,
    refused    = c(1L, 0L),
    ineligible = c(0L, 1L),
    terminated = c(1L, 1L),
    disposition_date = as.Date(c("2026-01-01", "2026-05-01")), stringsAsFactors = FALSE)
  res <- disposition_summary(d)
  expect_equal(res$latest_disposition, "ineligible")   # campaign 20, later
  expect_equal(res$best_disposition, "refused")         # refused outranks ineligible
  expect_equal(res$n_terminated, 2L)
  expect_equal(res$n_refused, 1L)
  expect_equal(res$n_ineligible, 1L)
  # the new categories are valid `statuses` filters (keyed on latest_disposition)
  expect_equal(nrow(disposition_summary(d, statuses = "ineligible")), 1L)
  expect_equal(nrow(disposition_summary(d, statuses = "refused")), 0L)  # latest is ineligible
  # disposition_records surfaces the two columns as stored
  rec <- disposition_records(write_disposition_parquet(d))
  expect_contains(names(rec), c("refused", "ineligible"))
  expect_equal(rec$refused, c(1L, 0L))
  expect_equal(rec$ineligible, c(0L, 1L))
})

test_that("a pre-split projection defaults n_refused / n_ineligible to 0", {
  # .disposition_row() carries no refused/ineligible columns (a pre-0.51.0
  # projection): the rollup defaults them to 0, the terminal stays `terminated`,
  # and the two new count columns are present and 0 (backward-compat).
  res <- disposition_summary(write_disposition_parquet(rbind(
    .disposition_row("1", 1, terminated = 1, disposition_date = "2026-03-01"))))
  expect_contains(names(res), c("n_refused", "n_ineligible"))
  expect_equal(res$n_refused, 0L)
  expect_equal(res$n_ineligible, 0L)
  expect_equal(res$latest_disposition, "terminated")
})

test_that("n_completed and n_web_complete are counted separately", {
  # ever_completed folded completed OR web_complete; the counts split them so a
  # manager sees text-complete vs off-channel web-complete distinctly.
  d <- write_disposition_parquet(rbind(
    .disposition_row("1", 1, engaged = 1, completed = 1),
    .disposition_row("1", 2, engaged = 1, web_complete = 1)))
  res <- disposition_summary(d)
  expect_equal(res$n_completed, 1L)
  expect_equal(res$n_web_complete, 1L)
  expect_equal(res$n_engaged, 2L)          # cumulative: both campaigns engaged
})

test_that("n_error counts only campaigns carrying a non-blank error code", {
  # `error` is optional; a campaign "has an error" iff the code is non-NA and
  # non-blank. Built inline since the shared .disposition_row() carries no error
  # column (that path -- error absent -> n_error 0 -- is covered by the tests above).
  d <- data.frame(
    phone = c("2015550101", "2015550101", "2015550102"),
    campaign_id = c(1L, 2L, 1L),
    engaged = 1L, opted_in = 0L, completed = 0L, web_complete = 0L,
    terminated = 0L, error = c("30007", NA, "  "),   # code / none / blank
    disposition_date = as.Date(c("2026-01-01", "2026-01-02", "2026-01-03")),
    stringsAsFactors = FALSE)
  res <- disposition_summary(d)
  expect_equal(res[res$phone == "2015550101", "n_error"], 1L)  # only "30007"
  expect_equal(res[res$phone == "2015550102", "n_error"], 0L)  # blank is not an error
})

test_that("best_disposition is the furthest category reached; latest is recency", {
  # completed in an EARLY campaign, only non_response in the LATEST one.
  d <- write_disposition_parquet(rbind(
    .disposition_row("1", 10, engaged = 1, opted_in = 1, completed = 1,
                     disposition_date = "2026-01-01"),
    .disposition_row("1", 20, disposition_date = "2026-05-01")))   # non_response, later
  res <- disposition_summary(d)
  expect_equal(res$latest_disposition, "non_response")   # recency
  expect_equal(res$latest_campaign_id, "20")
  expect_equal(res$best_disposition, "completed")        # furthest ever reached
  expect_equal(res$best_campaign_id, "10")
})

test_that("n_error is read from a projection PATH carrying an error column", {
  # the inline n_error test above uses an in-memory frame; this exercises the
  # .disposition_read_parquet error read + schema intersect from a real Parquet.
  d <- data.frame(phone = c("1", "1"), campaign_id = 1:2,
    engaged = 1L, opted_in = 0L, completed = 0L, web_complete = 0L,
    terminated = 0L, error = c("30007", NA),
    disposition_date = as.Date("2026-01-01"), stringsAsFactors = FALSE)
  p <- write_disposition_parquet(d)
  expect_contains(nanoparquet::read_parquet_schema(p)$name, "error")
  expect_equal(disposition_summary(p)$n_error, 1L)
})

test_that("an NA integer (completed on t2w_external) decodes to NA, not garbage", {
  # Regression: nanoparquet 0.5.1 mis-decodes NA integers under col_select, so a
  # PROJECTED read of `completed` (NA on t2w_external) returned uninitialized
  # memory (0/1/garbage) and corrupted n_completed. The fixture carries an `error`
  # column so the summary's read set includes it (the trigger); the reader now
  # reads full + subsets, decoding NA correctly.
  d <- data.frame(phone = "9", campaign_id = 1L,
    engaged = 1L, opted_in = 0L, completed = NA_integer_, web_complete = 1L,
    terminated = 0L, error = NA_character_,
    disposition_date = as.Date("2026-01-01"), stringsAsFactors = FALSE)
  res <- disposition_summary(write_disposition_parquet(d))
  expect_equal(res$n_completed, 0L)          # NA completed must NOT count
  expect_equal(res$n_web_complete, 1L)
  expect_equal(res$latest_disposition, "web_complete")
})

test_that("a DuckDB-written projection takes the fast col_select path", {
  # created_by = DuckDB -> col_select (DuckDB's null encoding is NA-safe under
  # nanoparquet col_select, unlike nanoparquet's own writes). The fixtures here
  # are nanoparquet-written, so mock the writer signature and use an
  # NA-integer-free frame (col_select reads it correctly either way); this covers
  # the col_select branch of .disposition_read_parquet.
  # The nanoparquet engine is forced (the duckdb engine never reads via
  # nanoparquet), and the writer signature comes from the one metadata call.
  d <- rbind(.disposition_row("2015550101", 1, engaged = 1, completed = 1),
             .disposition_row("2015550102", 1, terminated = 1))
  p <- write_disposition_parquet(d)
  withr::local_options(survey160r.disposition_engine = "nanoparquet")
  stub_duckdb_created_by()
  res <- disposition_summary(p)
  expect_equal(nrow(res), 2L)
  expect_equal(res[res$phone == "2015550101", "n_completed"], 1L)
  expect_equal(res[res$phone == "2015550102", "n_terminated"], 1L)
})

test_that("with all-NA dates, latest and best fall back to the max campaign id", {
  d <- write_disposition_parquet(rbind(
    .disposition_row("1", 10, engaged = 1),      # NA date
    .disposition_row("1", 20, opted_in = 1)))    # NA date
  res <- disposition_summary(d)
  expect_equal(res$latest_disposition, "opted_in")     # date tie -> max id 20
  expect_equal(res$latest_campaign_id, "20")
  expect_equal(res$best_disposition, "opted_in")       # furthest reached
  expect_equal(res$best_campaign_id, "20")
  expect_true(is.na(res$first_disposition_date))
})

test_that("best_disposition tie on category resolves to the latest campaign", {
  # both campaigns terminal at 'engaged'; best picks the later one (then max id),
  # matching latest_disposition's tie-break.
  d <- write_disposition_parquet(rbind(
    .disposition_row("1", 10, engaged = 1, disposition_date = "2026-01-01"),
    .disposition_row("1", 20, engaged = 1, disposition_date = "2026-02-01")))
  res <- disposition_summary(d)
  expect_equal(res$best_disposition, "engaged")
  expect_equal(res$best_campaign_id, "20")               # later date wins the tie
})

test_that("screens a phone list, normalizing formats and flagging never-contacted", {
  res <- disposition_summary(
    .disposition_base(),
    phones = c("+1 (201) 555-0101", "2015559999", "()"))  # 11-digit, absent, junk
  expect_setequal(res$phone, c("2015550101", "2015559999"))  # junk -> dropped
  nc <- res[res$phone == "2015559999", ]
  expect_equal(nc$latest_disposition, "never_contacted")
  expect_equal(nc$n_campaigns, 0L)
  expect_true(is.na(nc$campaigns))
  # the +1/formatted number matched the stored 10-digit one
  expect_equal(res[res$phone == "2015550101", "n_completed"], 1L)
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
               "disposition_summary: unknown `statuses` value\\(s\\): bogus \\(valid: never_contacted, ")
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
  # dataset is NA close dates, that drop-everything is warned rather than silent.
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
  d <- .disposition_row("2015550101", 2339, engaged = 1, disposition_date = "2026-03-01")
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
  expect_equal(res$n_completed, 0L)          # completed = NA counts as not-set
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
  expect_equal(res$n_campaigns, 0L)          # never-contacted marker
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
            disposition_date = "2026-03-01"),
    .disposition_row("2015550101", 2354, engaged = 1, disposition_date = "2026-04-01"))
  res <- disposition_summary(d, phones = c("2015550101", "2015559999"))
  expect_setequal(res$phone, c("2015550101", "2015559999"))
  expect_equal(res[res$phone == "2015550101", "n_completed"], 1L)
  # a never-contacted phone has zero counts, undated first/last, no campaign ids
  expect_equal(res[res$phone == "2015559999", "n_campaigns"], 0L)
  expect_equal(res[res$phone == "2015559999", "n_engaged"], 0L)
  expect_true(is.na(res[res$phone == "2015559999", "last_disposition_date"]))
  expect_equal(res[res$phone == "2015559999", "best_disposition"], "never_contacted")
  expect_true(is.na(res[res$phone == "2015559999", "best_campaign_id"]))
  # a frame missing the read columns is caught
  expect_error(disposition_summary(d[, c("phone", "campaign_id")]),
               "missing required column")
})

test_that("disposition_summary tolerates a frame without disposition_date", {
  d <- rbind(
    .disposition_row("2015550101", 2339, engaged = 1, opted_in = 1, completed = 1,
            disposition_date = "2026-03-01"),
    .disposition_row("2015550101", 2354, engaged = 1, disposition_date = "2026-04-01"))
  bare <- d[, setdiff(names(d), "disposition_date"), drop = FALSE]  # un-enriched shape
  res <- disposition_summary(bare, phones = "2015550101")
  expect_equal(res$n_completed, 1L)        # summarizes with close dates unknown
  expect_equal(res$n_campaigns, 2L)
  expect_true(is.na(res$first_disposition_date))   # no dates -> NA span
  expect_true(is.na(res$last_disposition_date))
  # but a date bound with no disposition_date column is a clear error
  expect_error(disposition_summary(bare, date_from = "2026-01-01"),
               "disposition_date")
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
  expect_true(all(c("n_completed", "latest_disposition", "campaigns") %in%
                    names(out)))
  expect_equal(out$n_completed[1], 1L)                     # +1/formatted matched
  expect_equal(out$latest_disposition[2], "terminated")
  expect_equal(out$n_campaigns[3], 0L)                   # absent -> never_contacted
  expect_equal(out$latest_disposition[3], "never_contacted")
})

test_that("disposition_screen validates sample, phone_col, and column clashes", {
  p <- .disposition_base()
  expect_error(disposition_screen(list(), p), "must be a data frame")
  expect_error(disposition_screen(data.frame(x = 1), p),
               "`phone_col` \"phone\" is not a column of `sample` \\(`sample` has 1 column\\(s\\): x\\)")
  clash <- data.frame(phone = "2015550101", n_completed = 5L,
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

# --- review regressions + documented behaviors ----------------------------

test_that("summary reads a column-short projection path (no disposition_date)", {
  # Regression (finding #1): .disposition_read_parquet() used to col_select the
  # full read set unconditionally, so a path to an un-enriched projection (no
  # disposition_date) crashed with a raw nanoparquet "Column ... does not exist"
  # before the rollup's optional-date guard could run. It now intersects
  # col_select with the file schema and summarizes cleanly.
  row <- .disposition_row("2015550101", 2339, engaged = 1, opted_in = 1,
                          completed = 1)
  p <- write_disposition_parquet(row[, setdiff(names(row), "disposition_date")])
  res <- disposition_summary(p, phones = "2015550101")
  expect_equal(res$n_completed, 1L)
  expect_equal(res$n_campaigns, 1L)
  # disposition_screen() reads through the same path -- also unbroken now.
  out <- disposition_screen(data.frame(phone = "2015550101"), p)
  expect_equal(out$n_completed, 1L)
})

test_that("a required column missing from a projection path errors cleanly", {
  # The intersect drops only the OPTIONAL disposition_date; a genuinely required
  # funnel column absent from the file still reaches the rollup's clean guard.
  row <- .disposition_row("2015550101", 2339, engaged = 1)
  p <- write_disposition_parquet(row[, setdiff(names(row), "opted_in")])
  expect_error(disposition_summary(p), "missing required column")
})

test_that("page / page_size non-finite (NA/NaN/Inf) errors cleanly", {
  # Regression (finding #3): ok() used `x %% 1 == 0`, and `Inf %% 1` is NaN, so
  # the guard returned NA and `if (!ok(...))` failed with "missing value where
  # TRUE/FALSE needed". is.finite() now rejects NA / NaN / Inf with the intended
  # message.
  p <- write_disposition_parquet(rbind(.disposition_row("1", 1),
                                       .disposition_row("2", 1)))
  for (bad in c(NA_real_, NaN, Inf)) {
    expect_error(disposition_summary(p, page_size = bad), "positive integers")
    expect_error(disposition_summary(p, page = bad), "positive integers")
  }
})

test_that("terminated + completed resolves to completed (funnel order)", {
  # Documented (finding #4): .DISPOSITION_CATEGORIES ranks completed above
  # terminated, so a row carrying both resolves to "completed" (last-assignment
  # -wins). Internally consistent; pinned so a reorder is a conscious choice.
  res <- disposition_summary(write_disposition_parquet(
    .disposition_row("1", 1, engaged = 1, opted_in = 1, completed = 1,
                     terminated = 1)))
  expect_equal(res$latest_disposition, "completed")
  expect_equal(res$n_terminated, 1L)        # the terminated flag still rolls up
  expect_equal(res$n_completed, 1L)
})

test_that("a partially-dated dataset relabels a contacted-but-undated phone", {
  # Documented (finding #5): the "returns no rows" warning fires only when EVERY
  # disposition_date is NA. With a mix, a date bound silently drops the NA-dated
  # (but genuinely contacted) phone, which then screens back as never_contacted
  # -- and no warning fires because the dataset is not all-NA. The mixed case
  # arises when some history predates disposition_date population.
  p <- write_disposition_parquet(rbind(
    .disposition_row("2015550101", 2339, engaged = 1,
                     disposition_date = "2026-03-01"),           # dated
    .disposition_row("2015550102", 2340, engaged = 1)))        # NA date
  expect_no_warning(
    res <- disposition_summary(p, phones = c("2015550101", "2015550102"),
                               date_from = "2026-01-01"))
  undated <- res[res$phone == "2015550102", ]
  expect_equal(undated$n_campaigns, 0L)
  expect_equal(undated$latest_disposition, "never_contacted")
})

# --- disposition_screen(columns = "history") -------------------------------

test_that("the history block says what each number's next send would carry", {
  rows <- rbind(sequence_records(),
                .sequence_row("2015550106", 1L, "2026-01-01", engaged = 1L, opted_in = 1L, completed = 1L),
                .sequence_row("2015550106", 2L, "2026-02-01", engaged = 1L, opted_in = 1L, completed = 1L),
                .sequence_row("2015550106", 3L, "2026-03-01"))
  p <- write_sequence_parquet(rows)
  sample <- data.frame(phone = c("+1 (201) 555-0101", paste0("201555010", 2:6), "2015559999", ""),
                       k = 1:8, stringsAsFactors = FALSE)
  out <- disposition_screen(sample, p, columns = "history", as_of = "2026-04-01")
  expect_equal(out$phone, sample$phone)
  expect_equal(out$k, 1:8)
  expect_false(any(c("n_campaigns", "latest_disposition") %in% names(out)))
  expect_identical(setdiff(names(out), c("phone", "k")), .DISPOSITION_HISTORY_COLS)
  # 0101 N E C; 0102 X N N T (T is a reply); 0103 O W on one date; 0104 N (undated,
  # oldest) then a t2w_external E (a reply, completion unknown); 0105 R I; 0106 C C N
  expect_identical(out$prior_completes, c(1L, 0L, 1L, 0L, 0L, 2L, 0L, NA))
  expect_identical(out$prior_replies, c(2L, 1L, 2L, 1L, 2L, 2L, 0L, NA))
  expect_identical(out$prior_unanswered, c(1L, 3L, 0L, 1L, 0L, 1L, 0L, NA))
  expect_equal(out$history_segment, c("completer_1", "replied", "completer_1", "replied",
                                      "replied", "completer_2_plus", "first_contact", NA))
  expect_identical(out$steps_since_reply, c(0L, 0L, 0L, 0L, 0L, 1L, NA, NA))
  expect_identical(out$steps_since_complete, c(0L, NA, 0L, NA, NA, 1L, NA, NA))
  expect_identical(out$steps_since_undeliverable, c(NA, 3L, NA, NA, NA, NA, NA, NA))
  expect_identical(out$days_since_reply, c(31L, 31L, 59L, 31L, 59L, 59L, NA, NA))
  expect_identical(out$days_since_complete, c(31L, NA, 59L, NA, NA, 59L, NA, NA))
  expect_identical(out$days_since_undeliverable, c(NA, 90L, NA, NA, NA, NA, NA, NA))

  # both blocks, in order; as_of as a Date; the codes decide what is undeliverable
  both <- disposition_screen(sample, p, columns = c("summary", "history"),
                             as_of = as.Date("2026-04-02"), undeliverable_codes = NULL)
  expect_identical(setdiff(names(both), c("phone", "k")),
                   c(setdiff(.DISPOSITION_SUMMARY_COLS, "phone"), .DISPOSITION_HISTORY_COLS))
  expect_identical(both$n_campaigns[1:2], c(3L, 4L))
  expect_identical(both$days_since_reply[1], 32L)
  expect_true(all(is.na(both$steps_since_undeliverable)))
  expect_identical(both$prior_unanswered[2], 3L)   # a plain non-response is still unanswered

  # the scope applies to the history: nothing in range -> first contact
  none <- disposition_screen(sample, p, columns = "history", date_from = "2027-01-01")
  expect_equal(none$history_segment, c(rep("first_contact", 7), NA))
  expect_identical(none$prior_replies, c(rep(0L, 7), NA))
  expect_true(all(is.na(none$days_since_reply)))
  # a sample with no contacted number at all
  fresh <- disposition_screen(data.frame(phone = c("2015559999", "2015559998")), p,
                              columns = "history")
  expect_equal(fresh$history_segment, c("first_contact", "first_contact"))
  expect_identical(fresh$prior_completes, c(0L, 0L))
  expect_identical(fresh$steps_since_reply, c(NA_integer_, NA_integer_))
})

test_that("the history block is the last step's history plus that step", {
  skip_if_no_sequence()
  rows <- rbind(sequence_records(),
                .sequence_row("2015550106", 1L, "2026-01-01", engaged = 1L, opted_in = 1L, completed = 1L),
                .sequence_row("2015550106", 2L, "2026-02-01", engaged = 1L, opted_in = 1L, completed = 1L),
                .sequence_row("2015550106", 3L, "2026-03-01"))
  p <- write_sequence_parquet(rows)
  steps <- collect_sorted(disposition_steps(p, columns = "history"))
  last <- steps[!duplicated(steps$phone, fromLast = TRUE), ]
  out <- disposition_screen(data.frame(phone = last$phone), p, columns = "history",
                            as_of = "2026-04-01")
  is_reply <- !last$label %in% c("non_response", "undeliverable")
  is_complete <- last$label %in% c("completed", "web_complete")
  expect_identical(out$prior_completes, last$prior_completes + as.integer(is_complete))
  expect_identical(out$prior_replies, last$prior_replies + as.integer(is_reply))
  expect_identical(out$prior_unanswered, last$prior_unanswered + as.integer(!is_reply))
  # the recency columns advance by one step, or restart at the last step itself
  advance <- function(since, event) ifelse(event, 0L, since + 1L)
  expect_identical(out$steps_since_reply, advance(last$steps_since_reply, is_reply))
  expect_identical(out$steps_since_complete, advance(last$steps_since_complete, is_complete))
  expect_identical(out$steps_since_undeliverable,
                   advance(last$steps_since_undeliverable, last$label == "undeliverable"))
  gap <- as.integer(as.Date("2026-04-01") - last$disposition_date)
  expect_identical(out$days_since_reply, ifelse(is_reply, gap, last$days_since_reply + gap))
})

test_that("a timestamp-stored disposition_date gives the same history block", {
  rows <- sequence_records()
  dated <- write_sequence_parquet(rows)
  rows$disposition_date <- as.POSIXct(rows$disposition_date, tz = "UTC")
  stamped <- tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(rows, stamped)
  sample <- data.frame(phone = paste0("201555010", 1:5), stringsAsFactors = FALSE)
  expect_no_warning(out <- disposition_screen(sample, stamped, columns = "history", as_of = "2026-04-01"))
  expect_identical(out, disposition_screen(sample, dated, columns = "history", as_of = "2026-04-01"))
  expect_identical(out$days_since_reply, c(31L, 31L, 59L, 31L, 59L))
})

test_that("disposition_screen validates the history arguments", {
  p <- .disposition_base()
  sample <- data.frame(phone = "2015550101", stringsAsFactors = FALSE)
  expect_error(disposition_screen(sample, p, columns = "clocks"),
               "`columns` must be one or both of \"summary\", \"history\"")
  expect_error(disposition_screen(sample, p, columns = character(0)), "`columns`")
  expect_error(disposition_screen(sample, p, columns = "history", as_of = NULL),
               "`as_of` must be a single valid date")
  expect_error(disposition_screen(sample, p, columns = "history", as_of = "yesterday"),
               "`as_of` must be a single valid date")
  expect_error(disposition_screen(sample, p, columns = "history", undeliverable_codes = NA),
               "`undeliverable_codes`")
  clash <- data.frame(phone = "2015550101", history_segment = "x", stringsAsFactors = FALSE)
  expect_error(disposition_screen(clash, p, columns = "history"), "already has")
  expect_s3_class(disposition_screen(clash, p), "data.frame")   # only the requested block clashes
})
