# Coverage for R/s160_gcs.R download_with_verify(). Tests fall into two
# groups: single-attempt behavior (uses stub_gcs_download_ok) and retry
# behavior (needs per-attempt logic, kept inline).

test_that("download succeeds when file size matches GCS metadata", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  stub_gcs_download_ok(name_override = "100/data.csv")

  download_with_verify("100/data.csv", tmp)
  expect_true(file.exists(tmp))
})

test_that("download skips verification when metadata unavailable", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  # Empty listing -> verification skipped.
  local_mocked_bindings(
    gcs_list_objects = function(prefix = NULL, ...) {
      data.frame(name = character(0), size = numeric(0),
                 stringsAsFactors = FALSE)
    },
    gcs_get_object = function(object_name, saveToDisk, ...) { # nolint object_name_linter
      writeLines(c("a,b", "1,2"), saveToDisk)
      TRUE
    }
  )

  download_with_verify("100/data.csv", tmp)
  expect_true(file.exists(tmp))
})

test_that("download skips verification when gcs_list_objects returns a formatted-string size", {
  # Real googleCloudStorageR returns `size` as a human-readable string like
  # "483.3 Kb"; as.numeric() yields NA. Treat as "unknown size" and skip the
  # comparison rather than crashing the if(NA) compare.
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  stub_gcs_download_ok(
    name_override = "100/data.csv",
    size_override = "483.3 Kb"
  )

  download_with_verify("100/data.csv", tmp)
  expect_true(file.exists(tmp))
})

test_that("download skips verification when gcs_list_objects errors", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  stub_gcs_download_ok(fail_list = "403 Forbidden")

  download_with_verify("100/data.csv", tmp)
  expect_true(file.exists(tmp))
})

test_that("download errors when file not written to disk", {
  tmp <- tempfile(fileext = ".csv")
  stub_gcs_download_ok(
    name_override = "100/data.csv",
    size_override = 100,
    skip_write = TRUE
  )

  expect_error(
    download_with_verify("100/data.csv", tmp),
    "Download produced no file"
  )
})

# --- retry behavior -------------------------------------------------------
# Per-attempt logic doesn't fit a generic helper; kept inline. Sys.sleep is
# mocked via local_mocked_bindings so tests don't actually sleep.

test_that("download retries on size mismatch then fails", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  attempts <- 0L

  local_mocked_bindings(Sys.sleep = function(...) NULL, .package = "base")
  local_mocked_bindings(
    gcs_list_objects = function(prefix, ...) {
      data.frame(name = "100/data.csv", size = 999999L,
                 stringsAsFactors = FALSE)
    },
    gcs_get_object = function(object_name, saveToDisk, ...) { # nolint object_name_linter
      attempts <<- attempts + 1L
      writeLines("a,b", saveToDisk)
      TRUE
    }
  )

  expect_error(
    suppressMessages(download_with_verify("100/data.csv", tmp,
                                          max_retries = 1L)),
    "Download incomplete"
  )
  expect_equal(attempts, 2L)  # initial + 1 retry
})

test_that("download retries then succeeds on second attempt", {
  csv_content <- c("a,b", "1,2")
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  size_probe <- tempfile()
  writeLines(csv_content, size_probe)
  expected_size <- file.info(size_probe)$size
  unlink(size_probe)
  attempts <- 0L

  local_mocked_bindings(Sys.sleep = function(...) NULL, .package = "base")
  local_mocked_bindings(
    gcs_list_objects = function(prefix, ...) {
      data.frame(name = "100/data.csv", size = expected_size,
                 stringsAsFactors = FALSE)
    },
    gcs_get_object = function(object_name, saveToDisk, ...) { # nolint object_name_linter
      attempts <<- attempts + 1L
      if (attempts == 1L) {
        writeLines("x", saveToDisk)  # truncated
      } else {
        writeLines(csv_content, saveToDisk)
      }
      TRUE
    }
  )

  expect_message(
    download_with_verify("100/data.csv", tmp, max_retries = 2L),
    "size mismatch"
  )
  expect_equal(attempts, 2L)
  expect_true(file.exists(tmp))
})
