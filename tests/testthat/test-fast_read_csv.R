# Coverage for R/s160_gcs.R::fast_read_csv -- the shared reader behind
# s160_read_csv / s160_gcs_campaign_results_read. Both the data.table::fread
# path and the utils::read.csv fallback (data.table unavailable) are exercised.
# stub_no_data_table() lives in helper-stubs.R.

test_that("fread path: munges bracket headers to dot form (parity with read.csv)", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id[q1]scriptDate,campaignid", "2026-01-01,5"), tmp)

  data <- fast_read_csv(tmp)

  expect_s3_class(data, "data.frame")
  expect_false(inherits(data, "data.table"))
  expect_equal(names(data), c("id.q1.scriptDate", "campaignid"))
})

test_that("fallback path produces the same munged names as fread", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id[q1]scriptDate,campaignid", "2026-01-01,5"), tmp)

  stub_no_data_table()
  data <- fast_read_csv(tmp)

  expect_equal(names(data), c("id.q1.scriptDate", "campaignid"))
})

test_that("large integers come back as character, not bit64::integer64", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("userid,n", "99999999999,3"), tmp)

  data <- fast_read_csv(tmp)

  # The integer64 = "character" pin keeps big IDs as plain strings so they
  # don't surprise interactive callers (View / joins / ==).
  expect_type(data$userid, "character")
  expect_equal(data$userid, "99999999999")
  expect_type(data$n, "integer")
})

test_that("columns projection keeps only requested (dot-form) names -- fread", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id[q1]scriptDate,campaignid,extra", "2026-01-01,5,junk"), tmp)

  data <- fast_read_csv(tmp, columns = c("id.q1.scriptDate", "campaignid"))

  expect_equal(names(data), c("id.q1.scriptDate", "campaignid"))
})

test_that("columns projection keeps only requested names -- fallback", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("id[q1]scriptDate,campaignid,extra", "2026-01-01,5,junk"), tmp)

  stub_no_data_table()
  data <- fast_read_csv(tmp, columns = c("id.q1.scriptDate", "campaignid"))

  expect_equal(names(data), c("id.q1.scriptDate", "campaignid"))
})

test_that("columns with no header match falls back to a full read", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), tmp)

  # No requested column matches -> select is skipped, every column returned.
  data <- fast_read_csv(tmp, columns = c("does.not.exist"))

  expect_equal(names(data), c("a", "b"))
})

test_that("`...` overrides the pinned defaults (e.g. sep)", {
  tmp <- withr::local_tempfile(fileext = ".tsv")
  writeLines(c("a\tb", "1\t2"), tmp)

  expect_equal(names(fast_read_csv(tmp, sep = "\t")), c("a", "b"))
})
