# Coverage for the work the duckdb engine pushes into the Parquet scan: the
# disposition_funnel() GROUP BY (.disposition_funnel_duckdb) and the campaign /
# date pre-filter (.disposition_scan_where). Each must give exactly what the
# in-R path gives, so these tests compare the two engines over many filter
# shapes. Split from test-disposition_engine.R, which covers the engine choice
# and the phone match: these are the long-running comparisons, and a file of
# their own lets a parallel run give them their own worker.

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
