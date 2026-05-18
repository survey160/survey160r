# Coverage for R/s160_gcs.R::s160_read_csv. Local-source sibling of
# s160_gcs_pull_csv -- reads + stamps the same provenance attrs.

test_that("s160_read_csv stamps source_csv_hash and source_csv_path", {
  tmp <- tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2", "3,4"), tmp)
  on.exit(unlink(tmp), add = TRUE)

  data <- s160_read_csv(tmp)

  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 2L)
  expect_true(grepl("^sha256:", attr(data, "source_csv_hash")))
  expect_equal(attr(data, "source_csv_path"), tmp)
  # stringsAsFactors defaults to FALSE.
  expect_type(data$a, "integer")
})

test_that("s160_read_csv errors when the file is missing", {
  expect_error(
    s160_read_csv("/path/does/not/exist.csv"),
    "file not found"
  )
})

test_that("s160_read_csv forwards `...` to read.csv (e.g., sep)", {
  tmp <- tempfile(fileext = ".tsv")
  writeLines(c("a\tb", "1\t2"), tmp)
  on.exit(unlink(tmp), add = TRUE)

  data <- s160_read_csv(tmp, sep = "\t")

  expect_equal(names(data), c("a", "b"))
  expect_equal(data$a, 1L)
  expect_equal(data$b, 2L)
})

test_that("s160_read_csv result is consumable by latency_run downstream", {
  # Use the same synthetic fixture the algorithm tests rely on. The
  # reader's job is to produce a stamped data frame; the algorithm
  # surfaces source_csv_hash / source_csv_path on result$meta.
  fx_path <- test_path("fixtures/synthetic.csv")
  data <- s160_read_csv(fx_path)
  result <- latency_run(campaign_id = 1, data = data)
  expect_equal(result$meta$source_csv_path, fx_path)
  expect_true(grepl("^sha256:", result$meta$source_csv_hash))
})
