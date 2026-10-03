# Coverage for the disposition read engines (.disposition_engine and the
# duckdb / nanoparquet readers behind .disposition_read_parquet and
# .disposition_read_scoped). The duckdb engine pushes the phone match into the
# Parquet scan; the nanoparquet engine is the zero-dependency fallback. Both must
# return exactly the same frames, so most tests here run a reader under each
# engine and compare. Fixtures use write_disposition_parquet() (helper-stubs.R).

# Tests that exercise the duckdb engine skip when duckdb is not installed; the
# engine-agnostic ones run either way.

# Run `expr` under one engine.
with_engine <- function(engine, expr) {
  withr::with_options(list(survey160r.disposition_engine = engine), expr)
}

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
    function() disposition_funnel(p, by = "carrier")
  )
  for (call in calls) {
    expect_identical(with_engine("duckdb", call()), with_engine("nanoparquet", call()))
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
  expect_identical(res, with_engine("nanoparquet", disposition_summary(p, phones = "2015550101")))
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
  real_metadata <- nanoparquet::read_parquet_metadata
  local_mocked_bindings(
    read_parquet_metadata = function(file) {
      m <- real_metadata(file)
      m$file_meta_data$created_by <- "DuckDB version v1.5.2"
      m
    },
    .package = "nanoparquet")
  expect_error(with_engine("nanoparquet", disposition_screen(data.frame(phone = "1"), p)),
               "missing required column")
})

test_that("the engine option is validated, and auto picks duckdb when installed", {
  skip_if_not_installed("duckdb")
  with_engine("auto", expect_equal(.disposition_engine(), "duckdb"))
  with_engine("nanoparquet", expect_equal(.disposition_engine(), "nanoparquet"))
  with_engine("duckdb", expect_equal(.disposition_engine(), "duckdb"))
  with_engine("arrow", expect_error(.disposition_engine(), "must be one of"))
  with_engine(NA, expect_error(.disposition_engine(), "must be one of"))
})

test_that("without duckdb, auto falls back to nanoparquet and a forced duckdb errors", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  with_engine("auto", expect_equal(.disposition_engine(), "nanoparquet"))
  with_engine("duckdb", expect_error(.disposition_engine(), "not installed"))
})

test_that("a large file on the nanoparquet engine points the caller at duckdb", {
  withr::local_options(rlib_message_verbosity = "verbose")
  p <- .engine_fixture()
  local_mocked_bindings(file.size = function(...) 2e8,
                        requireNamespace = function(...) FALSE, .package = "base")
  expect_message(with_engine("nanoparquet", disposition_screen(data.frame(phone = "1"), p)),
                 "install.packages")
})

test_that("a path with a glob metacharacter is read literally, not expanded", {
  skip_if_not_installed("duckdb")
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
    expect_identical(with_engine("duckdb", call()), with_engine("nanoparquet", call()))
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
  expect_identical(with_engine("duckdb", disposition_records(p, phones = "2015550101")),
                   with_engine("nanoparquet", disposition_records(p, phones = "2015550101")))
  d2 <- .disposition_row("2015550101", 1, engaged = 1)
  d2$disposition_date <- as.POSIXct("2026-01-01 23:00", tz = "UTC")
  p2 <- tempfile(fileext = ".parquet")
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  duckdb::duckdb_register(con, "d2", d2)
  DBI::dbExecute(con, sprintf("COPY d2 TO %s (FORMAT parquet)", DBI::dbQuoteString(con, p2)))
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_identical(with_engine("duckdb", disposition_records(p2, phones = "2015550101")),
                   with_engine("nanoparquet", disposition_records(p2, phones = "2015550101")))
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
  real_metadata <- nanoparquet::read_parquet_metadata
  real_read <- nanoparquet::read_parquet
  local_mocked_bindings(
    read_parquet_metadata = function(file) {
      m <- real_metadata(file)
      m$file_meta_data$created_by <- "DuckDB version v1.5.2"
      m
    },
    read_parquet = function(file, col_select = NULL, ...) {
      captured$col_select <- col_select
      real_read(file, col_select = col_select, ...)
    },
    .package = "nanoparquet")
  with_engine("nanoparquet", disposition_summary(p))
  expect_false(is.null(captured$col_select))
})
