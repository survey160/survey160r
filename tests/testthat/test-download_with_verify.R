# Coverage for R/s160_gcs.R download_with_verify(). The expected size comes
# from gcs_get_object(meta = TRUE) (the raw object metadata's byte count), NOT
# from gcs_list_objects() whose `size` is a human-readable string. Tests fall
# into two groups: single-attempt behavior (uses stub_gcs_download_ok) and
# retry behavior (needs per-attempt logic, kept inline).

test_that("download succeeds when file size matches object metadata", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  stub_gcs_download_ok()

  download_with_verify("100/data.csv", tmp)
  expect_true(file.exists(tmp))
})

test_that("download skips verification when object metadata is unavailable", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  # Metadata fetch fails (e.g. permissions / transient error) -> skip, with a
  # message rather than silently.
  stub_gcs_download_ok(fail_meta = "403 Forbidden")

  expect_message(
    download_with_verify("100/data.csv", tmp),
    "Skipping size verification"
  )
  expect_true(file.exists(tmp))
})

test_that("download skips verification when metadata size is non-numeric", {
  # Defensive: if any code path ever hands back a formatted size string like
  # "483.3 Kb", as.numeric() yields NA -- treat as unknown size and skip the
  # comparison rather than crashing the compare.
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  stub_gcs_download_ok(size_override = "483.3 Kb")

  expect_message(
    download_with_verify("100/data.csv", tmp),
    "Skipping size verification"
  )
  expect_true(file.exists(tmp))
})

test_that("download skips size verification for a Content-Encoded (gzip) object", {
  # campaign_results CSVs are stored Content-Encoding: gzip. GCS decompresses on
  # download, so the saved file (decompressed) is larger than meta$size (the
  # compressed byte count). A byte-for-byte check would then always fail --
  # encoded objects must skip it and trust the (HTTP-verified) download.
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  local_mocked_bindings(
    gcs_get_object = function(object_name, saveToDisk = NULL, meta = FALSE, ...) { # nolint object_name_linter
      if (isTRUE(meta)) {
        return(structure(
          list(name = object_name, size = 12L, contentEncoding = "gzip"),
          class = "gcs_objectmeta"))
      }
      # Decompressed payload is far larger than the compressed meta size (12).
      writeLines(c("a,b,c", "1,2,3", "4,5,6"), saveToDisk)
      TRUE
    }
  )

  expect_message(
    download_with_verify("100/data.csv", tmp),
    "Content-Encoding"
  )
  expect_true(file.exists(tmp))
  expect_gt(file.info(tmp)$size, 12L)   # decompressed size exceeds compressed meta
})

test_that("download errors when file not written to disk", {
  tmp <- tempfile(fileext = ".csv")
  stub_gcs_download_ok(skip_write = TRUE)

  expect_error(
    download_with_verify("100/data.csv", tmp),
    "Download produced no file"
  )
})

# --- retry behavior -------------------------------------------------------
# Per-attempt logic doesn't fit a generic helper; kept inline. The metadata
# call (meta = TRUE) reports the expected size once, up front; the download
# call increments `attempts`. Sys.sleep is mocked so tests don't actually sleep.

meta_or_download <- function(expected_size, on_download) {
  function(object_name, saveToDisk = NULL, meta = FALSE, ...) { # nolint object_name_linter
    if (isTRUE(meta)) {
      return(structure(list(name = object_name, size = expected_size),
                       class = "gcs_objectmeta"))
    }
    on_download(saveToDisk)
    TRUE
  }
}

test_that("download with max_retries = 0 fails on the first mismatch without retrying", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  captured <- new_capture()
  captured$attempts <- 0L

  local_mocked_bindings(Sys.sleep = function(...) NULL, .package = "base")
  local_mocked_bindings(
    gcs_get_object = meta_or_download(999999L, function(saveToDisk) {
      captured$attempts <- captured$attempts + 1L
      writeLines("x", saveToDisk)  # truncated
    })
  )

  expect_error(
    suppressMessages(download_with_verify("100/data.csv", tmp,
                                          max_retries = 0L)),
    "Download incomplete after 1 attempts"
  )
  expect_equal(captured$attempts, 1L)
})

test_that("download retries on size mismatch then fails", {
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  captured <- new_capture()
  captured$attempts <- 0L

  local_mocked_bindings(Sys.sleep = function(...) NULL, .package = "base")
  local_mocked_bindings(
    gcs_get_object = meta_or_download(999999L, function(saveToDisk) {
      captured$attempts <- captured$attempts + 1L
      writeLines("a,b", saveToDisk)  # always short of expected
    })
  )

  expect_error(
    suppressMessages(download_with_verify("100/data.csv", tmp,
                                          max_retries = 1L)),
    "Download incomplete"
  )
  expect_equal(captured$attempts, 2L)  # initial + 1 retry
})

test_that("download retries then succeeds on second attempt", {
  csv_content <- c("a,b", "1,2")
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  size_probe <- tempfile()
  writeLines(csv_content, size_probe)
  expected_size <- file.info(size_probe)$size
  unlink(size_probe)
  captured <- new_capture()
  captured$attempts <- 0L

  local_mocked_bindings(Sys.sleep = function(...) NULL, .package = "base")
  local_mocked_bindings(
    gcs_get_object = meta_or_download(expected_size, function(saveToDisk) {
      captured$attempts <- captured$attempts + 1L
      if (captured$attempts == 1L) {
        writeLines("x", saveToDisk)  # truncated
      } else {
        writeLines(csv_content, saveToDisk)
      }
    })
  )

  expect_message(
    download_with_verify("100/data.csv", tmp, max_retries = 2L),
    "size mismatch"
  )
  expect_equal(captured$attempts, 2L)
  expect_true(file.exists(tmp))
})
