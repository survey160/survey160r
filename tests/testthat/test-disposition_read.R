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
                      "n_terminated", "n_error", "latest_disposition",
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
  expect_true("error" %in% nanoparquet::read_parquet_schema(p)$name)
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
  d <- rbind(.disposition_row("2015550101", 1, engaged = 1, completed = 1),
             .disposition_row("2015550102", 1, terminated = 1))
  p <- write_disposition_parquet(d)
  local_mocked_bindings(
    read_parquet_info = function(...) list(created_by = "DuckDB version v1.5.2"),
    .package = "nanoparquet")
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
               "phone column")
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
