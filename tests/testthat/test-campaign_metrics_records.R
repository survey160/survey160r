# A raw projection-shaped frame: rows 1 and 2 are the SAME cell (campaign 1,
# 2026-01-01, hour 9) replicated across the latency `threshold`, with identical
# funnel counts; row 3 is a second cell.
raw_cells <- function() {
  data.frame(
    campaign_id = c(1, 1, 2),
    date        = as.Date(c("2026-01-01", "2026-01-01", "2026-01-02")),
    hour_local  = c(9L, 9L, 10L),
    tracker_registration_id   = c("R1", "R1", "R2"),
    tracker_brand             = c("B", "B", "B"),
    tracker_client            = c("C", "C", "C"),
    tracker_fielding_location = c("F", "F", "F"),
    tracker_mode              = c("T2W", "T2W", "T2W"),
    n_sent      = c(100, 100, 50),
    n_engaged   = c(10, 10, 5),
    n_opted_in  = c(2, 2, 1),
    n_completed = c(1, 1, 0),
    threshold   = c(1, 2, 1),
    stringsAsFactors = FALSE)
}

test_that("records renames tracker_* to short names and drops non-cell columns", {
  rec <- campaign_metrics_records(raw_cells())
  expect_setequal(
    names(rec),
    c("campaign_id", "date", "hour_local", "registration_id", "brand", "client",
      "fielding_location", "mode", "n_sent", "n_engaged", "n_opted_in", "n_completed"))
  expect_false("threshold" %in% names(rec))
})

test_that("records dedups the threshold replication to one row per cell", {
  rec <- campaign_metrics_records(raw_cells())
  expect_equal(nrow(rec), 2)          # the two replicated rows collapse to one
  expect_equal(sum(rec$n_sent), 150)  # 100 + 50, never 250
})

test_that("records dedup = FALSE keeps the replicated rows", {
  rec <- campaign_metrics_records(raw_cells(), dedup = FALSE)
  expect_equal(nrow(rec), 3)
})

test_that("records is idempotent on a frame already using short names", {
  once  <- campaign_metrics_records(raw_cells())
  twice <- campaign_metrics_records(once)
  expect_equal(twice, once)
})

test_that("records errors when funnel columns are missing", {
  expect_error(campaign_metrics_records(data.frame(campaign_id = 1)),
               "missing funnel columns")
})

test_that("records errors on a non-existent path", {
  expect_error(campaign_metrics_records("/no/such/file.parquet"), "not found")
})

test_that("records rejects an input that is neither a path nor a data frame", {
  expect_error(campaign_metrics_records(123), "single Parquet path or a data frame")
})

test_that("records reads and dedups from a Parquet path", {
  tmp <- withr::local_tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(raw_cells(), tmp)
  rec <- campaign_metrics_records(tmp)
  expect_equal(nrow(rec), 2)
  expect_equal(sum(rec$n_sent), 150)
  expect_false("threshold" %in% names(rec))
})
