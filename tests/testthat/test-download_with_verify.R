test_that("download succeeds when file size matches GCS metadata", {
  csv_content <- c("a,b", "1,2")
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  local_mocked_bindings(
    gcs_list_objects = function(prefix, ...) {
      data.frame(name = "100/data.csv", size = 8, stringsAsFactors = FALSE)
    },
    gcs_get_object = function(object_name, saveToDisk, ...) { # nolint object_name_linter
      writeLines(csv_content, saveToDisk)
      TRUE
    }
  )

  result <- download_with_verify("100/data.csv", tmp)
  expect_true(file.exists(tmp))
})

test_that("download retries on size mismatch then fails", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  attempt_count <- 0L

  local_mocked_bindings(
    gcs_list_objects = function(prefix, ...) {
      data.frame(name = "100/data.csv", size = 999999, stringsAsFactors = FALSE)
    },
    gcs_get_object = function(object_name, saveToDisk, ...) { # nolint object_name_linter
      attempt_count <<- attempt_count + 1L
      writeLines("a,b", saveToDisk)  # small file, won't match expected size
      TRUE
    }
  )

  expect_error(
    suppressMessages(download_with_verify("100/data.csv", tmp, max_retries = 1L)),
    "Download incomplete"
  )
  expect_equal(attempt_count, 2L)  # initial + 1 retry
})

test_that("download skips verification when metadata unavailable", {
  csv_content <- c("a,b", "1,2")
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  local_mocked_bindings(
    gcs_list_objects = function(prefix, ...) {
      data.frame(name = character(0), size = numeric(0), stringsAsFactors = FALSE)
    },
    gcs_get_object = function(object_name, saveToDisk, ...) { # nolint object_name_linter
      writeLines(csv_content, saveToDisk)
      TRUE
    }
  )

  result <- download_with_verify("100/data.csv", tmp)
  expect_true(file.exists(tmp))
})

test_that("download retries then succeeds on second attempt", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  attempt_count <- 0L
  expected_size <- 8L

  local_mocked_bindings(
    gcs_list_objects = function(prefix, ...) {
      data.frame(name = "100/data.csv", size = expected_size, stringsAsFactors = FALSE)
    },
    gcs_get_object = function(object_name, saveToDisk, ...) { # nolint object_name_linter
      attempt_count <<- attempt_count + 1L
      if (attempt_count == 1L) {
        writeLines("x", saveToDisk)  # truncated
      } else {
        writeLines(c("a,b", "1,2"), saveToDisk)  # correct size
      }
      TRUE
    }
  )

  expect_message(
    download_with_verify("100/data.csv", tmp, max_retries = 2L),
    "size mismatch"
  )
  expect_equal(attempt_count, 2L)
  expect_true(file.exists(tmp))
})
