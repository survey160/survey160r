# Coverage for the disposition read engines (.disposition_engine and the
# duckdb / nanoparquet readers behind .disposition_read_parquet and
# .disposition_read_scoped). The duckdb engine pushes the phone match into the
# Parquet scan; the nanoparquet engine is the zero-dependency fallback. Both must
# return exactly the same frames, so most tests here run a reader under each
# engine and compare. Fixtures use write_disposition_parquet() (helper-stubs.R).

# Tests that exercise the duckdb engine skip when duckdb is not installed; the
# engine-agnostic ones run either way.

# A fixture exercising every rollup edge: phone formats (11-digit, punctuated,
# blank, NA), an NA campaign id, a duplicate row, NA flags and dates, blank and
# real error codes, and the full record schema.
.engine_fixture <- function() {
  d <- data.frame(
    phone = c("2015550101", "12015550101", "(201) 555-0102", "2015550102", "",
              NA, "2015550103", "2015550103", "2015550104", "2015550104"),
    campaign_id = c(10L, 11L, 10L, NA, 10L, 10L, 12L, 12L, 10L, 11L),
    sent = 1L,
    engaged = c(1L, 1L, 0L, 1L, 1L, 1L, NA, NA, 1L, 1L),
    opted_in = c(1L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L),
    completed = c(1L, NA, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    web_complete = 0L,
    refused = c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 1L),
    ineligible = c(0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L),
    terminated = c(0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L),
    error = c(NA, "30007", " ", "", NA, NA, "4720", "4720", NA, NA),
    carrier = c("verizon", NA, "att", "att", NA, NA, "tmobile", "tmobile", NA, NA),
    survey_mode = "t2w",
    tracker_loi = c(5.5, NA, 5.5, 5.5, NA, NA, 7, 7, 5.5, 5.5),
    tracker_n_questions = 12L,
    disposition_date = as.Date("2026-03-01") + c(0L, 5L, 0L, NA, 0L, 0L, 2L, 2L, 1L, 1L),
    stringsAsFactors = FALSE
  )
  write_disposition_parquet(d)
}

test_that("every reader returns the same frame under both engines", {
  skip_if_not_installed("duckdb")
  p <- .engine_fixture()
  phones <- c("2015550101", "1-201-555-0102", "2015550103", "2015550104",
              "9995550000", "", NA)
  sample <- data.frame(phone = phones, k = seq_along(phones), stringsAsFactors = FALSE)
  calls <- list(
    function() disposition_summary(p),
    function() disposition_summary(p, phones = phones),
    function() {
      disposition_summary(p, phones = phones, campaign_ids = 10:11,
                          date_from = "2026-03-01", date_to = "2026-03-02")
    },
    function() disposition_records(p),
    function() disposition_records(p, phones = phones, page = 1, page_size = 3),
    function() disposition_screen(sample, p),
    function() disposition_screen(sample, p, campaign_ids = 12L),
    function() disposition_screen(sample, p, columns = c("summary", "history"), as_of = "2026-04-01"),
    function() disposition_funnel(p, by = "carrier")
  )
  for (call in calls) {
    expect_engine_parity(call())
  }
})

test_that("the scoped read matches the rollup's own phone normalization", {
  skip_if_not_installed("duckdb")
  p <- .engine_fixture()
  out <- with_engine("duckdb", disposition_screen(
    data.frame(phone = c("+1 (201) 555-0101", "2015550102"), stringsAsFactors = FALSE), p))
  # "+1 (201) 555-0101" and the stored "2015550101" / "12015550101" are one phone.
  expect_equal(out$n_campaigns, c(2L, 2L))   # 0102: campaign 10 + the NA id
  expect_equal(out$campaigns, c("10,11", "10"))
  expect_equal(out$latest_campaign_id[1], "11")
  expect_equal(out$best_disposition[1], "completed")
  expect_equal(out$n_error[1], 1L)
})

test_that("collapse counts an NA campaign id once and leaves it out of the id list", {
  res <- disposition_summary(.engine_fixture(), phones = "2015550102")
  expect_equal(res$n_campaigns, 2L)
  expect_equal(res$campaigns, "10")
  expect_equal(res$latest_campaign_id, "10")   # the dated row outranks the NA date
  expect_equal(res$best_disposition, "ineligible")
  expect_equal(res$n_error, 0L)                # " " and "" are not error codes
})

test_that("collapse counts a fully duplicated row in the flag counts but not in n_campaigns", {
  res <- disposition_summary(.engine_fixture(), phones = "2015550103")
  expect_equal(res$n_campaigns, 1L)
  expect_equal(res$n_error, 2L)
  expect_equal(res$latest_disposition, "non_response")   # NA engaged -> not set
})

test_that("character campaign ids sort and join like sort() does", {
  d <- data.frame(phone = "1", campaign_id = c("b", "a", "b", NA),
                  engaged = 1L, opted_in = 0L, completed = 0L, web_complete = 0L,
                  terminated = 0L, stringsAsFactors = FALSE)
  res <- suppressWarnings(disposition_summary(d))
  expect_equal(res$n_campaigns, 3L)
  expect_equal(res$campaigns, "a,b")
})

test_that("the date span ignores NA dates and is a plain Date (no stray dim)", {
  res <- disposition_summary(.engine_fixture(), phones = "2015550102")
  expect_equal(res$first_disposition_date, as.Date("2026-03-01"))
  expect_equal(res$last_disposition_date, as.Date("2026-03-01"))
  expect_null(attr(res$first_disposition_date, "dim"))
  expect_null(attr(res$last_disposition_date, "dim"))
})

test_that("a date bound on an all-NA date column warns under either engine", {
  skip_if_not_installed("duckdb")
  d <- rbind(.disposition_row("2015550101", 1, engaged = 1),
             .disposition_row("2015550102", 1))
  p <- write_disposition_parquet(d)
  for (engine in c("duckdb", "nanoparquet")) {
    expect_warning(
      res <- with_engine(engine, disposition_screen(
        data.frame(phone = "2015550101"), p, date_from = "2026-01-01")),
      "NA for every row")
    expect_equal(res$latest_disposition, "never_contacted")
  }
})

test_that("a scoped read warns on all-NA dates only when the WHOLE file is undated", {
  skip_if_not_installed("duckdb")
  # The matched phone is undated, but another row is dated: no warning, since
  # .disposition_filter judges the whole dataset, not the matched subset.
  d <- rbind(.disposition_row("2015550101", 1, engaged = 1),
             .disposition_row("2015550102", 1, disposition_date = "2026-02-01"))
  p <- write_disposition_parquet(d)
  for (engine in c("duckdb", "nanoparquet")) {
    expect_no_warning(with_engine(engine, disposition_screen(
      data.frame(phone = "2015550101"), p, date_from = "2026-01-01")))
  }
})

test_that("a numeric stored phone falls back to the in-R match", {
  skip_if_not_installed("duckdb")
  d <- data.frame(phone = c(2015550101, 12015550101), campaign_id = 1:2,
                  engaged = 1L, opted_in = 0L, completed = 0L, web_complete = 0L,
                  terminated = 0L)
  p <- write_disposition_parquet(d)
  res <- with_engine("duckdb", disposition_summary(p, phones = "2015550101"))
  expect_equal(res$n_campaigns, 2L)
  expect_engine_parity(disposition_summary(p, phones = "2015550101"))
})

test_that("a projection without a phone column errors under either engine", {
  skip_if_not_installed("duckdb")
  p <- write_disposition_parquet(data.frame(campaign_id = 1L, engaged = 1L))
  for (engine in c("duckdb", "nanoparquet")) {
    expect_error(with_engine(engine, disposition_screen(data.frame(phone = "1"), p)),
                 "missing required column")
    expect_error(with_engine(engine, disposition_records(p, phones = "1")),
                 "missing required column")
  }
})

test_that("a DuckDB-written file with no phone column errors on the nanoparquet engine", {
  p <- write_disposition_parquet(data.frame(campaign_id = 1L, engaged = 1L))
  stub_duckdb_created_by()
  expect_error(with_engine("nanoparquet", disposition_screen(data.frame(phone = "1"), p)),
               "missing required column")
})

test_that("the engine option is validated, and auto picks duckdb when installed", {
  skip_if_not_installed("duckdb")
  with_engine("auto", expect_equal(.disposition_engine(), "duckdb"))
  with_engine("nanoparquet", expect_equal(.disposition_engine(), "nanoparquet"))
  with_engine("duckdb", expect_equal(.disposition_engine(), "duckdb"))
  with_engine("arrow", expect_error(
    .disposition_engine(),
    "option `survey160r.disposition_engine` must be one of auto, duckdb, nanoparquet, not \"arrow\".",
    fixed = TRUE))
  with_engine(NA, expect_error(.disposition_engine(), "must be one of .*, not NA"))
})

test_that("without duckdb, auto falls back to nanoparquet and a forced duckdb errors", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  with_engine("auto", expect_equal(.disposition_engine(), "nanoparquet"))
  with_engine("duckdb", expect_error(
    .disposition_engine(),
    paste0("option `survey160r.disposition_engine` is \"duckdb\" but duckdb and DBI are ",
           "not installed: install.packages(c(\"duckdb\", \"DBI\")), or set the option to ",
           "\"auto\" to fall back to nanoparquet."),
    fixed = TRUE))
})

# A recorder for rlang::check_installed(), so the offer branch never tries a real
# install: it just captures the packages asked for and the reason.
stub_check_installed <- function(cap, env = parent.frame()) {
  testthat::local_mocked_bindings(
    check_installed = function(pkg, reason = NULL, ...) {
      cap$pkg <- pkg
      cap$reason <- reason
      invisible()
    }, .package = "rlang", .env = env)
}

test_that("auto + large file + no duckdb offers to install it, naming the escape hatch", {
  p <- .engine_fixture()
  cap <- new_capture()
  stub_check_installed(cap)
  local_mocked_bindings(file.size = function(...) 2e8,
                        requireNamespace = function(...) FALSE, .package = "base")
  with_engine("auto", disposition_screen(data.frame(phone = "2015550101"), p))
  expect_equal(cap$pkg, c("duckdb", "DBI"))
  expect_match(cap$reason, "disposition_engine")
})

test_that("a pinned nanoparquet engine takes the whole-file read with no duckdb offer", {
  p <- .engine_fixture()
  cap <- new_capture()
  stub_check_installed(cap)
  local_mocked_bindings(file.size = function(...) 2e8,
                        requireNamespace = function(...) FALSE, .package = "base")
  with_engine("nanoparquet", disposition_screen(data.frame(phone = "2015550101"), p))
  expect_null(cap$pkg)
})

test_that("the duckdb offer is skipped for a small file and for a glob path", {
  p <- .engine_fixture()
  cap <- new_capture()
  stub_check_installed(cap)
  # small file (real fixture size, well under 100 MB): a fixture or extract
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  with_engine("auto", .disposition_offer_duckdb(p))
  expect_null(cap$pkg)
  # large, but a glob path -- the duckdb read is declined anyway
  local_mocked_bindings(file.size = function(...) 2e8, .package = "base")
  with_engine("auto", .disposition_offer_duckdb("a*.parquet"))
  expect_null(cap$pkg)
})

test_that("no duckdb offer when duckdb is already installed", {
  skip_if_not_installed("duckdb")
  p <- .engine_fixture()
  cap <- new_capture()
  stub_check_installed(cap)
  local_mocked_bindings(file.size = function(...) 2e8, .package = "base")
  with_engine("auto", .disposition_offer_duckdb(p))
  expect_null(cap$pkg)
})

test_that(".onAttach hints at duckdb only when it is not installed", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_message(.onAttach("lib", "survey160r"), "duckdb")
  local_mocked_bindings(requireNamespace = function(...) TRUE, .package = "base")
  expect_no_message(.onAttach("lib", "survey160r"))
})

test_that("a path with a glob metacharacter is read literally, not expanded", {
  skip_if_not_installed("duckdb")
  skip_on_os("windows")   # "*" is not a legal Windows file name character
  # DuckDB's read_parquet() would expand "b*.parquet" to every match; the
  # nanoparquet read takes the path literally.
  dir <- withr::local_tempdir()
  row <- .disposition_row("2015550101", 1, engaged = 1)
  plain <- nanoparquet::parquet_options(write_arrow_metadata = FALSE)
  nanoparquet::write_parquet(row, file.path(dir, "b*.parquet"), options = plain)
  nanoparquet::write_parquet(transform(row, phone = "2015550199"),
                             file.path(dir, "bX.parquet"), options = plain)
  out <- with_engine("duckdb", disposition_screen(
    data.frame(phone = c("2015550101", "2015550199")), file.path(dir, "b*.parquet")))
  expect_equal(out$n_campaigns, c(1L, 0L))
})

test_that("a projection with none of the wanted columns errors cleanly under either engine", {
  skip_if_not_installed("duckdb")
  p <- write_disposition_parquet(data.frame(x = 1:2))
  for (engine in c("duckdb", "nanoparquet")) {
    expect_error(with_engine(engine, disposition_summary(p)), "missing required column")
    expect_error(with_engine(engine, disposition_screen(data.frame(phone = "1"), p)),
                 "missing required column")
    expect_error(with_engine(engine, disposition_records(p)), "missing required column")
  }
})

test_that("a stored column named like the SQL helper does not shadow the phone match", {
  skip_if_not_installed("duckdb")
  d <- .disposition_row("2015550101", 1, engaged = 1)
  d$s160_digits <- "x"
  d$s160_phone <- "y"
  p <- write_disposition_parquet(d)
  out <- with_engine("duckdb", disposition_screen(data.frame(phone = "2015550101"), p))
  expect_equal(out$latest_disposition, "engaged")
})

test_that("a scoped duckdb read keeps file order across row groups (tie-break parity)", {
  skip_if_not_installed("duckdb")
  # Many row groups and full (phone, date, campaign) ties whose flags differ:
  # latest/best pick the first such row in FILE order, so a scoped read must hand
  # rows back in file order even though DuckDB matches them in parallel.
  withr::local_seed(1)
  # Force a parallel scan even on a 1-core runner, so a lost ORDER BY shows up.
  real_duckdb <- duckdb::duckdb
  local_mocked_bindings(duckdb = function(...) {
    args <- list(...)
    args$config$threads <- "4"
    do.call(real_duckdb, args)
  }, .package = "duckdb")
  n <- 40000L
  phones <- sprintf("201555%04d", sample.int(2000L, n, replace = TRUE))
  d <- data.frame(phone = phones, campaign_id = sample(1:3, n, TRUE),
                  engaged = sample(0:1, n, TRUE), opted_in = sample(0:1, n, TRUE),
                  completed = 0L, web_complete = 0L, terminated = sample(0:1, n, TRUE),
                  disposition_date = as.Date("2026-01-01"), stringsAsFactors = FALSE)
  p <- tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(d, p, options = nanoparquet::parquet_options(
    num_rows_per_row_group = 500L, write_arrow_metadata = FALSE))
  req <- sprintf("201555%04d", 1:1000)
  for (call in list(function() disposition_summary(p, phones = req),
                    function() disposition_records(p, phones = req))) {
    expect_engine_parity(call())
  }
})

test_that("arrow-annotated factors and timestamps are read with nanoparquet's types", {
  skip_if_not_installed("duckdb")
  d <- .disposition_row(c("2015550101", "2015550102"), 1:2, engaged = 1)
  d$carrier <- factor(c("verizon", "att"), levels = c("verizon", "att"))
  p <- tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(d, p)   # default: with ARROW:schema metadata
  rec <- with_engine("duckdb", disposition_records(p))
  expect_s3_class(rec$carrier, "factor")
  expect_identical(rec, with_engine("nanoparquet", disposition_records(p)))
  expect_engine_parity(disposition_records(p, phones = "2015550101"))
  d2 <- .disposition_row("2015550101", 1, engaged = 1)
  d2$disposition_date <- as.POSIXct("2026-01-01 23:00", tz = "UTC")
  p2 <- tempfile(fileext = ".parquet")
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  duckdb::duckdb_register(con, "d2", d2)
  DBI::dbExecute(con, sprintf("COPY d2 TO %s (FORMAT parquet)", DBI::dbQuoteString(con, p2)))
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_engine_parity(disposition_records(p2, phones = "2015550101"))
})

test_that("a stored file_row_number column falls back instead of failing the scoped read", {
  skip_if_not_installed("duckdb")
  d <- .disposition_row("2015550101", 1, engaged = 1)
  d$File_Row_Number <- 99L
  p <- write_disposition_parquet(d)
  out <- with_engine("duckdb", disposition_summary(p, phones = "2015550101"))
  expect_equal(out$latest_disposition, "engaged")
})

test_that("the duckdb read gives DuckDB a spill directory it can create", {
  skip_if_not_installed("duckdb")
  captured <- new_capture()
  real_connect <- DBI::dbConnect
  local_mocked_bindings(dbConnect = function(drv, ...) {
    con <- real_connect(drv, ...)
    captured$temp <- DBI::dbGetQuery(con, "SELECT current_setting('temp_directory') AS t")$t
    con
  }, .package = "DBI")
  with_engine("duckdb", disposition_screen(data.frame(phone = "2015550101"), .engine_fixture()))
  expect_true(dir.exists(dirname(captured$temp)))
  expect_false(dir.exists(captured$temp))   # cleaned up on exit
})

test_that("best breaks a category + date tie by the larger campaign id", {
  d <- rbind(.disposition_row("1", 7, engaged = 1, disposition_date = "2026-01-01"),
             .disposition_row("1", 3, engaged = 1, disposition_date = "2026-01-01"))
  res <- disposition_summary(d)
  expect_equal(res$best_campaign_id, "7")
  expect_equal(disposition_summary(d[2:1, ])$best_campaign_id, "7")
})

test_that("an undated row never outranks a dated one for latest, even before 1970", {
  d <- rbind(.disposition_row("1", 9, engaged = 1),
             .disposition_row("1", 2, opted_in = 1, disposition_date = "1969-12-31"))
  expect_equal(disposition_summary(d)$latest_campaign_id, "2")
})

test_that("several NA campaign ids count once; an all-NA id list is an empty string", {
  d <- rbind(.disposition_row("1", NA), .disposition_row("1", NA), .disposition_row("1", 5),
             .disposition_row("2", NA))
  res <- disposition_summary(d)
  expect_equal(res$n_campaigns, c(2L, 1L))
  expect_equal(res$campaigns, c("5", ""))
})

test_that(".normalize_phone keeps its exact edge cases", {
  expect_identical(.normalize_phone("2015550101\n"), "2015550101")
  expect_identical(.normalize_phone("22015550101"), "22015550101")
  expect_identical(.normalize_phone(c("12015550101", "", NA)), c("2015550101", NA, NA))
})

test_that("an 11-digit number without a leading 1 does not match under either engine", {
  p <- write_disposition_parquet(.disposition_row("22015550101", 1, engaged = 1))
  engines <- c(if (requireNamespace("duckdb", quietly = TRUE)) "duckdb", "nanoparquet")
  for (engine in engines) {
    out <- with_engine(engine, disposition_screen(data.frame(phone = "2015550101"), p))
    expect_equal(out$n_campaigns, 0L)
  }
})

test_that("scoped records/summary judge the all-NA-date warning on the whole file", {
  skip_if_not_installed("duckdb")
  d <- rbind(.disposition_row("2015550101", 1, engaged = 1),
             .disposition_row("2015550102", 1, disposition_date = "2026-02-01"))
  p <- write_disposition_parquet(d)
  empty <- write_disposition_parquet(d[0L, ])
  for (engine in c("duckdb", "nanoparquet")) {
    expect_no_warning(with_engine(engine, disposition_records(
      p, phones = "2015550101", date_from = "2026-01-01")))
    expect_no_warning(with_engine(engine, disposition_summary(
      p, phones = "2015550101", date_from = "2026-01-01")))
    expect_no_warning(with_engine(engine, disposition_summary(
      empty, phones = "2015550101", date_from = "2026-01-01")))
  }
})

test_that("an ordinary projection stays on the duckdb path (no nanoparquet read)", {
  skip_if_not_installed("duckdb")
  p <- .engine_fixture()
  local_mocked_bindings(.disposition_read_nanoparquet = function(...) stop("fallback"))
  expect_no_error(with_engine("duckdb", disposition_summary(p)))
  expect_no_error(with_engine("duckdb", disposition_summary(p, phones = "2015550101")))
  expect_no_error(with_engine("duckdb", disposition_records(p, phones = "2015550101")))
})

test_that("the nanoparquet engine column-projects a DuckDB-written file", {
  p <- .engine_fixture()
  captured <- new_capture()
  stub_duckdb_created_by()
  real_read <- nanoparquet::read_parquet
  local_mocked_bindings(
    read_parquet = function(file, col_select = NULL, ...) {
      captured$col_select <- col_select
      real_read(file, col_select = col_select, ...)
    },
    .package = "nanoparquet")
  with_engine("nanoparquet", disposition_summary(p))
  expect_type(captured$col_select, "character")  # projected by name
})

test_that("disposition_funnel's in-scan aggregation matches the in-R grouping", {
  skip_if_not_installed("duckdb")
  p <- .engine_fixture()
  # The fixture has an NA campaign id (counted as one more distinct value), an
  # all-NA `completed` group, NA carriers, a double group column, and NA dates.
  expect_false(is.null(with_engine("duckdb", .disposition_funnel_duckdb(p, "carrier"))))
  calls <- list(
    function() disposition_funnel(p),
    function() disposition_funnel(p, by = c("carrier", "completed"), grain = "all"),
    function() disposition_funnel(p, by = "tracker_loi", percent = TRUE),
    function() disposition_funnel(p, by = c("campaign_id", "survey_mode"), rates = FALSE),
    function() disposition_funnel(p, by = "opted_in", grain = "all")
  )
  for (call in calls) {
    expect_engine_parity(call())
  }
  # ...and the frame form of the same rows groups identically.
  records <- as.data.frame(nanoparquet::read_parquet(p))
  expect_identical(with_engine("duckdb", disposition_funnel(p, by = "carrier")),
                   disposition_funnel(records, by = "carrier"))
})

test_that("disposition_funnel falls back to the in-R grouping when the scan cannot", {
  skip_if_not_installed("duckdb")
  base <- data.frame(phone = c("1", "2", "3"), campaign_id = c(1L, 1L, 2L), sent = 1L,
                     engaged = c(1L, 0L, 1L), opted_in = 0L, completed = c(NA, 0L, 1L),
                     carrier = c("a", "a", NA), n_sent = 1L,
                     disposition_date = as.Date("2026-03-01"), stringsAsFactors = FALSE)
  # No refused / ineligible columns: the scan defaults them to 0 like the R path.
  p <- write_disposition_parquet(base)
  expect_false(is.null(with_engine("duckdb", .disposition_funnel_duckdb(p, "carrier"))))
  expect_engine_parity(disposition_funnel(p, grain = "all"))
  # A group column named like a count -> declined, grouped in R.
  expect_null(with_engine("duckdb", .disposition_funnel_duckdb(p, "n_sent")))
  # A missing column -> declined, so the R path raises its usual error.
  expect_null(with_engine("duckdb", .disposition_funnel_duckdb(p, "survey_mode")))
  expect_error(with_engine("duckdb", disposition_funnel(p, by = "survey_mode")),
               "missing required column")
  # A non-numeric flag column -> declined.
  pb <- write_disposition_parquet(transform(base, engaged = engaged == 1L))
  expect_null(with_engine("duckdb", .disposition_funnel_duckdb(pb, "carrier")))
  expect_engine_parity(disposition_funnel(pb, grain = "all"))
  # A DOUBLE flag (a NaN R skips, a fractional sum) -> declined, grouped in R.
  pd <- write_disposition_parquet(transform(base, engaged = c(NaN, 0.1, 0.9)))
  expect_null(with_engine("duckdb", .disposition_funnel_duckdb(pd, "carrier")))
  expect_engine_parity(disposition_funnel(pd, grain = "all"))
  # A zero-row file -> declined, so the typed empty frame comes from the R path.
  pe <- write_disposition_parquet(base[0L, ])
  expect_null(with_engine("duckdb", .disposition_funnel_duckdb(pe, "carrier")))
  expect_engine_parity(disposition_funnel(pe))
  # A non-plain column type (TIMESTAMP) -> declined.
  pt <- write_disposition_parquet(transform(base, at = as.POSIXct("2026-03-01", tz = "UTC")))
  expect_null(with_engine("duckdb", .disposition_funnel_duckdb(pt, "at")))
  # The nanoparquet engine never takes the scan.
  expect_null(with_engine("nanoparquet", .disposition_funnel_duckdb(p, "carrier")))
})

test_that("campaign / date filters pre-filtered in the scan match the in-R filter", {
  skip_if_not_installed("duckdb")
  p <- .engine_fixture()
  phones <- c("2015550101", "2015550102", "2015550103", "2015550104")
  sample <- data.frame(phone = phones, stringsAsFactors = FALSE)
  filters <- list(
    list(campaign_ids = 10L),
    list(campaign_ids = c(10, 12)),            # numeric ids, matched as text
    list(campaign_ids = c("11", "99")),
    list(campaign_ids = c("011", "12")),       # not an integer's spelling: matched as text
    list(campaign_ids = "10.0"),
    list(campaign_ids = c(10L, NA)),           # NA id: R matches the NA row -> not pushed
    list(campaign_ids = integer(0)),
    list(date_from = "2026-03-02"),
    list(date_to = as.Date("2026-03-01") + 0.5),  # fractional Date: widened, R decides
    list(date_from = as.POSIXct("2026-03-01 12:00", tz = "UTC"), date_to = "2026-03-06"),
    list(campaign_ids = 10:11, date_from = "2026-03-01", date_to = "2026-03-01"),
    list(date_from = "2030-01-01"),            # nothing in range
    list(date_from = as.Date(-1e6))            # no SQL literal for the bound -> R only
  )
  for (f in filters) {
    calls <- list(
      function() do.call(disposition_records, c(list(p), f)),
      function() do.call(disposition_records, c(list(p, phones = phones), f)),
      function() do.call(disposition_summary, c(list(p), f)),
      function() do.call(disposition_summary, c(list(p, phones = phones), f)),
      function() do.call(disposition_screen, c(list(sample, p), f))
    )
    for (call in calls) {
      expect_engine_parity(call())
    }
  }
})

test_that("the scan pre-filter keeps the whole-file all-NA-date warning and skips odd files", {
  skip_if_not_installed("duckdb")
  base <- data.frame(phone = c("2015550101", "2015550102"), campaign_id = c(1L, 2L),
                     sent = 1L, engaged = 0L, opted_in = 0L, completed = 0L,
                     web_complete = 0L, terminated = 0L,
                     disposition_date = as.Date(c(NA, NA)), stringsAsFactors = FALSE)
  p <- write_disposition_parquet(base)
  for (engine in c("duckdb", "nanoparquet")) {
    expect_warning(with_engine(engine, disposition_records(p, campaign_ids = 1L,
                                                           date_from = "2026-01-01")),
                   "NA for every row")
  }
  # A stored file_row_number column, a BIGINT-free double id, and a non-DATE date
  # column are read unfiltered (no predicate), still with the same answer.
  odd <- list(transform(base, file_row_number = 1:2),
              transform(base, campaign_id = c(1, 2)),
              transform(base, disposition_date = c("2026-01-01", "2026-01-02")))
  for (d in odd) {
    po <- write_disposition_parquet(d)
    expect_engine_parity(disposition_records(po, campaign_ids = 1))
  }
  con <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  schema <- data.frame(column_name = c("campaign_id", "disposition_date"),
                       column_type = c("DOUBLE", "VARCHAR"))
  expect_identical(.disposition_scan_where(con, schema, list(campaign_ids = 1,
                                                            date_from = "2026-01-01")),
                   character(0))
  expect_identical(.disposition_scan_where(con, schema, NULL), character(0))
})

test_that("the scoped read matches short and dirty phones on both scan forms", {
  skip_if_not_installed("duckdb")
  d <- data.frame(phone = c("555-0101", "5550101", "2015550101", "12015550101",
                            "201.555.0101", "20155501010"),
                  campaign_id = 1:6, sent = 1L, engaged = 0L, opted_in = 0L,
                  completed = 0L, web_complete = 0L, terminated = 0L,
                  stringsAsFactors = FALSE)
  p <- write_disposition_parquet(d)
  # All-ten-digit requests take the stored-value semi-join + normalized rest;
  # a short request takes the all-normalized scan.
  for (phones in list("2015550101", c("2015550101", "5550101"), "20155501010")) {
    expect_engine_parity(disposition_records(p, phones = phones))
  }
  expect_identical(with_engine("duckdb", disposition_records(p, phones = "2015550101"))$campaign_id,
                   3:5)
})
