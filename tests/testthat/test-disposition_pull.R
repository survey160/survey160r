# disposition_pull(): GCS fetch of the derived projection. download_with_verify
# is mocked so no real GCS/network is touched. The download path also checks
# GCS readiness, so download-path tests call stub_gcs_base() (which no-ops
# check_gcs_ready); the dedicated readiness test below leaves it real.

# A stand-in for download_with_verify(): records its args and writes a stub file
# (or fails with `fail`, to exercise the error branches).
mock_download <- function(capture = NULL, fail = NULL, not_found = FALSE,
                          write_first = FALSE) {
  function(object_name, local_path, bucket) {
    if (!is.null(capture)) {
      capture$object_name <- object_name
      capture$bucket <- bucket
      capture$local_path <- local_path
    }
    # write_first mimics download_with_verify() writing a partial file to the
    # temp path before its size check fails, so disposition_pull()'s on.exit
    # cleanup has a real partial to remove.
    if (isTRUE(write_first)) writeLines("partial", local_path)
    # not_found mirrors the real download_with_verify(): a GCS 404 surfaces as a
    # classed s160_not_found so disposition_pull dispatches on the class.
    if (isTRUE(not_found)) {
      stop(survey160r:::s160_condition(
        sprintf("object not found: %s", object_name), "s160_not_found"
      ))
    }
    if (!is.null(fail)) stop(fail)
    writeLines("x", local_path)
    invisible(local_path)
  }
}

test_that("default pulls prod into the user cache and returns the path", {
  stub_gcs_base()
  tmp <- withr::local_tempdir()
  cap <- new.env(parent = emptyenv())
  mockery::stub(disposition_pull, "tools::R_user_dir", function(...) tmp)
  mockery::stub(disposition_pull, "download_with_verify", mock_download(cap))

  p <- suppressMessages(disposition_pull())

  expect_equal(p, file.path(tmp, "s160_disposition_prod.parquet"))
  expect_equal(cap$object_name, "disposition_by_phone/disposition_all.parquet")
  expect_equal(cap$bucket, "s160_disposition_prod")
  expect_true(file.exists(p))
})

test_that("env = 'dev' selects the dev bucket", {
  stub_gcs_base()
  cap <- new.env(parent = emptyenv())
  mockery::stub(disposition_pull, "download_with_verify", mock_download(cap))
  suppressMessages(disposition_pull(env = "dev", dest = withr::local_tempdir()))
  expect_equal(cap$bucket, "s160_disposition_dev")
})

test_that("explicit bucket overrides the env default", {
  stub_gcs_base()
  cap <- new.env(parent = emptyenv())
  mockery::stub(disposition_pull, "download_with_verify", mock_download(cap))
  suppressMessages(disposition_pull(bucket = "custom_bucket",
                                    dest = withr::local_tempdir()))
  expect_equal(cap$bucket, "custom_bucket")
})

test_that("dest directory saves the env-named file inside it", {
  stub_gcs_base()
  d <- withr::local_tempdir()
  mockery::stub(disposition_pull, "download_with_verify", mock_download())
  p <- suppressMessages(disposition_pull(dest = d))
  expect_equal(p, file.path(d, "s160_disposition_prod.parquet"))
  expect_true(file.exists(p))
})

test_that("dest file path is used verbatim and its parent is created", {
  stub_gcs_base()
  target <- file.path(withr::local_tempdir(), "sub", "my.parquet")
  mockery::stub(disposition_pull, "download_with_verify", mock_download())
  p <- suppressMessages(disposition_pull(dest = target))
  expect_equal(p, target)
  expect_true(file.exists(target))
})

test_that("invalid dest is rejected", {
  expect_error(disposition_pull(dest = 123), "single non-empty")
  expect_error(disposition_pull(dest = ""), "single non-empty")
  expect_error(disposition_pull(dest = c("a", "b")), "single non-empty")
})

test_that("an existing local copy is reused without downloading", {
  d <- withr::local_tempdir()
  cached <- file.path(d, "s160_disposition_prod.parquet")
  writeLines("old", cached)
  mockery::stub(disposition_pull, "download_with_verify",
                function(...) stop("should not download on a cache hit"))
  expect_message(p <- disposition_pull(dest = d), "disposition.pull.cache_hit")
  expect_equal(p, cached)
})

test_that("refresh = TRUE re-downloads over an existing file", {
  stub_gcs_base()
  d <- withr::local_tempdir()
  cached <- file.path(d, "s160_disposition_prod.parquet")
  writeLines("old", cached)
  mockery::stub(disposition_pull, "download_with_verify", mock_download())
  suppressMessages(disposition_pull(dest = d, refresh = TRUE))
  expect_equal(readLines(cached), "x")   # overwritten by the (mock) download
})

test_that("a 404 gives a clear not-found error", {
  stub_gcs_base()
  mockery::stub(disposition_pull, "download_with_verify",
                mock_download(not_found = TRUE))
  expect_error(
    suppressMessages(disposition_pull(dest = withr::local_tempdir())),
    "not found.*s160_disposition_prod")
})

test_that("a non-404 error gives a download-failed error", {
  stub_gcs_base()
  mockery::stub(disposition_pull, "download_with_verify",
                mock_download(fail = "connection reset"))
  expect_error(
    suppressMessages(disposition_pull(dest = withr::local_tempdir())),
    "Failed to download.*connection reset")
})

test_that("a download without an initialized GCS session errors clearly", {
  # No stub_gcs_base(): the real check_gcs_ready() runs. Force "not ready" by
  # reporting an empty global bucket, and prove the readiness error fires
  # before any download is attempted.
  testthat::local_mocked_bindings(gcs_get_global_bucket = function() "")
  testthat::local_mocked_bindings(
    download_with_verify = function(...) stop("readiness check should have stopped us"))
  expect_error(
    suppressMessages(disposition_pull(dest = withr::local_tempdir())),
    "GCS not initialized")
})

test_that("invalid refresh is rejected", {
  expect_error(disposition_pull(refresh = NULL), "single TRUE or FALSE")
  expect_error(disposition_pull(refresh = NA), "single TRUE or FALSE")
  expect_error(disposition_pull(refresh = c(TRUE, FALSE)), "single TRUE or FALSE")
  expect_error(disposition_pull(refresh = 1), "single TRUE or FALSE")
})

test_that("different bucket overrides use separate default caches", {
  stub_gcs_base()
  tmp <- withr::local_tempdir()
  mockery::stub(disposition_pull, "tools::R_user_dir", function(...) tmp)
  mockery::stub(disposition_pull, "download_with_verify", mock_download())
  p1 <- suppressMessages(disposition_pull(bucket = "bucket_a"))
  p2 <- suppressMessages(disposition_pull(bucket = "bucket_b"))
  expect_false(p1 == p2)                       # not one shared cache file
  expect_true(file.exists(p1) && file.exists(p2))
})

test_that("a failed download preserves the cache and leaves no partial file", {
  stub_gcs_base()
  d <- withr::local_tempdir()
  cached <- file.path(d, "s160_disposition_prod.parquet")
  writeLines("good", cached)
  mockery::stub(disposition_pull, "download_with_verify",
                mock_download(fail = "connection reset", write_first = TRUE))
  expect_error(suppressMessages(disposition_pull(dest = d, refresh = TRUE)),
               "Failed to download")
  expect_equal(readLines(cached), "good")                  # existing cache untouched
  expect_length(list.files(d, pattern = "\\.part$"), 0L)   # written partial cleaned up
})

test_that("rename fallback copies, and a total move failure errors", {
  stub_gcs_base()
  d <- withr::local_tempdir()
  mockery::stub(disposition_pull, "download_with_verify", mock_download())
  mockery::stub(disposition_pull, "file.rename", function(...) FALSE)
  p <- suppressMessages(disposition_pull(dest = d))         # rename fails -> copy
  expect_true(file.exists(p))
  mockery::stub(disposition_pull, "file.copy", function(...) FALSE)
  expect_error(suppressMessages(disposition_pull(dest = d, refresh = TRUE)),
               "move the downloaded file into place")       # both fail -> error
})
