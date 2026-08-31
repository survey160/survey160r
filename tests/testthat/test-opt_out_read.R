# Coverage for opt_out_read.R -- opt_out_screen() + its private reader/lookup.
# Fixtures use write_opt_out_parquet()/.opt_out_row() (helper-stubs.R): a small
# phone + date_added frame written to a temp Parquet and read back through the
# real nanoparquet path (no arrow, no network).

# A fixture with a duplicated phone (first date_added wins) and a blank phone
# (dropped by the lookup) -- reused across tests.
.opt_out_base <- function() {
  write_opt_out_parquet(rbind(
    .opt_out_row("2015550101", "2026-08-01"),
    .opt_out_row("2015550102", "2026-08-15"),
    .opt_out_row("2015550101", "2026-09-01"), # dup phone -> first occurrence wins
    .opt_out_row("", "2026-08-20")            # blank phone -> dropped by the lookup
  ))
}

# --- opt_out_screen ------------------------------------------------------

test_that("opt_out_screen annotates the sample in place, preserving it", {
  sample <- data.frame(
    phone = c("+1 (201) 555-0101", "2015550102", "2015559999"), # fmt, present, absent
    region = c("NE", "NE", "SW"), quota = c("A", "A", "B"),
    stringsAsFactors = FALSE)
  out <- opt_out_screen(sample, .opt_out_base())

  expect_equal(out$phone, sample$phone)         # original formatting kept
  expect_equal(out$region, c("NE", "NE", "SW")) # original columns preserved
  expect_equal(names(out),
               c("phone", "region", "quota", "opted_out", "opt_out_date"))
  expect_equal(out$opted_out, c(TRUE, TRUE, FALSE)) # +1/formatted matched
  expect_equal(out$opt_out_date, c("2026-08-01", "2026-08-15", NA))
})

test_that("opt_out_screen marks a blank/unparseable phone opted_out = NA", {
  sample <- data.frame(phone = c("2015550101", "", "not a phone"),
                       stringsAsFactors = FALSE)
  out <- opt_out_screen(sample, .opt_out_base())
  expect_equal(out$opted_out, c(TRUE, NA, NA)) # unknown, not FALSE
  expect_equal(out$opt_out_date, c("2026-08-01", NA, NA))
})

test_that("opt_out_screen keeps the first date_added for a duplicated phone", {
  out <- opt_out_screen(
    data.frame(phone = "2015550101", stringsAsFactors = FALSE), .opt_out_base())
  expect_true(out$opted_out)
  expect_equal(out$opt_out_date, "2026-08-01") # first occurrence wins
})

test_that("opt_out_screen tolerates a list with no date_added column", {
  p <- write_opt_out_parquet(
    data.frame(phone = c("2015550101", "2015550102"), stringsAsFactors = FALSE))
  out <- opt_out_screen(
    data.frame(phone = c("2015550101", "2015559999"), stringsAsFactors = FALSE), p)
  expect_equal(out$opted_out, c(TRUE, FALSE))
  expect_true(all(is.na(out$opt_out_date))) # no date column -> NA throughout
})

test_that("opt_out_screen preserves a POSIXct date_added (the snapshot's type)", {
  # The real opt-out snapshot stores date_added as a timestamp, not a string;
  # opt_out_date must carry the POSIXct through untouched (matched rows) and be
  # NA for the rest -- lock the type-agnostic contract the fixtures otherwise
  # only exercise with character dates.
  ts <- as.POSIXct(c("2026-08-01 12:00:00", "2026-08-15 09:30:00"), tz = "UTC")
  p <- write_opt_out_parquet(data.frame(
    phone = c("2015550101", "2015550102"), date_added = ts,
    stringsAsFactors = FALSE))
  out <- opt_out_screen(
    data.frame(phone = c("2015550101", "2015559999"), stringsAsFactors = FALSE), p)
  expect_s3_class(out$opt_out_date, "POSIXct")
  expect_equal(as.numeric(out$opt_out_date[1]), as.numeric(ts[1])) # instant kept
  expect_true(is.na(out$opt_out_date[2]))                          # absent -> NA
})

test_that("opt_out_screen returns a 0-row sample with both columns appended", {
  out <- opt_out_screen(
    data.frame(phone = character(0), region = character(0),
               stringsAsFactors = FALSE), .opt_out_base())
  expect_equal(nrow(out), 0L)
  expect_equal(names(out), c("phone", "region", "opted_out", "opt_out_date"))
  expect_type(out$opted_out, "logical")
})

test_that("opt_out_screen honors a custom phone_col", {
  sample <- data.frame(cell = c("2015550101", "2015559999"),
                       stringsAsFactors = FALSE)
  out <- opt_out_screen(sample, .opt_out_base(), phone_col = "cell")
  expect_equal(out$opted_out, c(TRUE, FALSE))
})

test_that("opt_out_screen validates sample, phone_col, and column clashes", {
  p <- .opt_out_base()
  expect_error(opt_out_screen(list(), p), "must be a data frame")
  expect_error(opt_out_screen(data.frame(x = 1), p), "phone column")
  clash <- data.frame(phone = "2015550101", opted_out = TRUE,
                      stringsAsFactors = FALSE)
  expect_error(opt_out_screen(clash, p), "already has")
})

# --- private reader / lookup ---------------------------------------------

test_that(".opt_out_read_parquet rejects a non-path and a missing file", {
  df <- data.frame(phone = "2015550101", stringsAsFactors = FALSE)
  expect_error(opt_out_screen(df, 123), "single Parquet path")
  expect_error(opt_out_screen(df, tempfile(fileext = ".parquet")), "not found")
})

test_that(".opt_out_lookup requires a phone column", {
  expect_error(.opt_out_lookup(data.frame(x = 1), fn = "opt_out_screen"),
               "missing required column")
})
