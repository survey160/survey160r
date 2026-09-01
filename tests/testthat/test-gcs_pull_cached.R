# .gcs_pull_cached(): the shared cached-GCS-pull behind disposition_pull() and
# opt_out_pull(). Its logic is family-agnostic, so this suite exercises every
# branch once, through one representative (disposition) parameter set, calling
# the helper directly so mockery::stub() reaches its dependencies. The wrappers'
# forwarding of family-specific params lives in test-disposition_pull.R /
# test-opt_out_pull.R. download_with_verify is mocked so no real GCS is touched.

# A stand-in for download_with_verify(): records its args and writes a stub file
# (or fails with `fail`, to exercise the error branches).
mock_download <- function(capture = NULL, fail = NULL, not_found = FALSE,
                          write_first = FALSE) {
  function(object_name, local_path, bucket, progress = FALSE) {
    if (!is.null(capture)) {
      capture$object_name <- object_name
      capture$bucket <- bucket
      capture$local_path <- local_path
      capture$progress <- progress
    }
    # write_first mimics download_with_verify() writing a partial file before its
    # size check fails, so the on.exit cleanup has a real partial to remove.
    if (isTRUE(write_first)) writeLines("partial", local_path)
    # not_found mirrors the real download_with_verify(): a GCS 404 surfaces as a
    # classed s160_not_found so .gcs_pull_cached dispatches on the class.
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

# The fixed disposition-family params; overrides come through `...`. A plain list
# builder (no call to .gcs_pull_cached), so tests do
# `do.call(.gcs_pull_cached, gpc_args(...))` -- `.gcs_pull_cached` then resolves
# in the test frame, picking up whatever mockery::stub() rebound it to.
gpc_args <- function(dest = NULL, bucket = NULL, refresh = FALSE,
                     progress = FALSE, env = "prod") {
  list(fn = "disposition_pull", env = env, dest = dest, bucket = bucket,
       refresh = refresh, progress = progress,
       object_name = "disposition_by_phone/disposition_all.parquet",
       cache_suffix = ".parquet", noun = "disposition projection")
}

test_that("default pulls into the user cache and returns the path", {
  stub_gcs_base()
  tmp <- withr::local_tempdir()
  cap <- new.env(parent = emptyenv())
  mockery::stub(.gcs_pull_cached, "tools::R_user_dir", function(...) tmp)
  mockery::stub(.gcs_pull_cached, "download_with_verify", mock_download(cap))

  p <- suppressMessages(do.call(.gcs_pull_cached, gpc_args()))

  expect_equal(p, file.path(tmp, "s160_disposition_prod.parquet"))
  expect_equal(cap$object_name, "disposition_by_phone/disposition_all.parquet")
  expect_equal(cap$bucket, "s160_disposition_prod")
  expect_true(file.exists(p))
})

test_that("env = 'dev' selects the dev bucket", {
  stub_gcs_base()
  cap <- new.env(parent = emptyenv())
  mockery::stub(.gcs_pull_cached, "download_with_verify", mock_download(cap))
  suppressMessages(do.call(.gcs_pull_cached,
                           gpc_args(env = "dev", dest = withr::local_tempdir())))
  expect_equal(cap$bucket, "s160_disposition_dev")
})

test_that("explicit bucket overrides the env default", {
  stub_gcs_base()
  cap <- new.env(parent = emptyenv())
  mockery::stub(.gcs_pull_cached, "download_with_verify", mock_download(cap))
  suppressMessages(do.call(.gcs_pull_cached,
                           gpc_args(bucket = "custom_bucket",
                                    dest = withr::local_tempdir())))
  expect_equal(cap$bucket, "custom_bucket")
})

test_that("dest directory saves the default-named file inside it", {
  stub_gcs_base()
  d <- withr::local_tempdir()
  mockery::stub(.gcs_pull_cached, "download_with_verify", mock_download())
  p <- suppressMessages(do.call(.gcs_pull_cached, gpc_args(dest = d)))
  expect_equal(p, file.path(d, "s160_disposition_prod.parquet"))
  expect_true(file.exists(p))
})

test_that("dest file path is used verbatim and its parent is created", {
  stub_gcs_base()
  target <- file.path(withr::local_tempdir(), "sub", "my.parquet")
  mockery::stub(.gcs_pull_cached, "download_with_verify", mock_download())
  p <- suppressMessages(do.call(.gcs_pull_cached, gpc_args(dest = target)))
  expect_equal(p, target)
  expect_true(file.exists(target))
})

test_that("invalid dest is rejected", {
  expect_error(do.call(.gcs_pull_cached, gpc_args(dest = 123)), "single non-empty")
  expect_error(do.call(.gcs_pull_cached, gpc_args(dest = "")), "single non-empty")
  expect_error(do.call(.gcs_pull_cached, gpc_args(dest = c("a", "b"))),
               "single non-empty")
})

test_that("an existing local copy is reused without downloading", {
  d <- withr::local_tempdir()
  cached <- file.path(d, "s160_disposition_prod.parquet")
  writeLines("old", cached)
  mockery::stub(.gcs_pull_cached, "download_with_verify",
                function(...) stop("should not download on a cache hit"))
  expect_message(p <- do.call(.gcs_pull_cached, gpc_args(dest = d)),
                 "Using cached.*old")
  expect_equal(p, cached)
})

test_that(".format_file_age buckets by minutes, hours, days", {
  f <- withr::local_tempfile()
  writeLines("x", f)
  now <- as.POSIXct(Sys.time(), tz = "UTC")
  Sys.setFileTime(f, now - 5 * 60)
  expect_match(.format_file_age(f), "^[0-9]+ min old$")
  Sys.setFileTime(f, now - 3 * 3600)
  expect_match(.format_file_age(f), "^[0-9]+ hr old$")
  Sys.setFileTime(f, now - 3 * 86400)
  expect_match(.format_file_age(f), "^[0-9]+ days old$")
})

test_that("refresh = TRUE re-downloads over an existing file", {
  stub_gcs_base()
  d <- withr::local_tempdir()
  cached <- file.path(d, "s160_disposition_prod.parquet")
  writeLines("old", cached)
  mockery::stub(.gcs_pull_cached, "download_with_verify", mock_download())
  suppressMessages(do.call(.gcs_pull_cached, gpc_args(dest = d, refresh = TRUE)))
  expect_equal(readLines(cached), "x")   # overwritten by the (mock) download
})

test_that("a 404 gives a clear not-found error", {
  stub_gcs_base()
  mockery::stub(.gcs_pull_cached, "download_with_verify",
                mock_download(not_found = TRUE))
  expect_error(
    suppressMessages(do.call(.gcs_pull_cached,
                             gpc_args(dest = withr::local_tempdir()))),
    "not found.*s160_disposition_prod")
})

test_that("a non-404 error gives a download-failed error", {
  stub_gcs_base()
  mockery::stub(.gcs_pull_cached, "download_with_verify",
                mock_download(fail = "connection reset"))
  expect_error(
    suppressMessages(do.call(.gcs_pull_cached,
                             gpc_args(dest = withr::local_tempdir()))),
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
    suppressMessages(do.call(.gcs_pull_cached,
                             gpc_args(dest = withr::local_tempdir()))),
    "GCS not initialized")
})

test_that("invalid refresh is rejected", {
  expect_error(do.call(.gcs_pull_cached, gpc_args(refresh = NULL)),
               "single TRUE or FALSE")
  expect_error(do.call(.gcs_pull_cached, gpc_args(refresh = NA)),
               "single TRUE or FALSE")
  expect_error(do.call(.gcs_pull_cached, gpc_args(refresh = c(TRUE, FALSE))),
               "single TRUE or FALSE")
  expect_error(do.call(.gcs_pull_cached, gpc_args(refresh = 1)),
               "single TRUE or FALSE")
})

test_that("invalid progress is rejected", {
  expect_error(do.call(.gcs_pull_cached, gpc_args(progress = NULL)),
               "single TRUE or FALSE")
  expect_error(do.call(.gcs_pull_cached, gpc_args(progress = NA)),
               "single TRUE or FALSE")
  expect_error(do.call(.gcs_pull_cached, gpc_args(progress = c(TRUE, FALSE))),
               "single TRUE or FALSE")
  expect_error(do.call(.gcs_pull_cached, gpc_args(progress = 1)),
               "single TRUE or FALSE")
})

test_that("progress flag threads through to download_with_verify", {
  stub_gcs_base()
  tmp <- withr::local_tempdir()
  cap <- new.env(parent = emptyenv())
  mockery::stub(.gcs_pull_cached, "tools::R_user_dir", function(...) tmp)
  mockery::stub(.gcs_pull_cached, "download_with_verify", mock_download(cap))

  suppressMessages(do.call(.gcs_pull_cached, gpc_args(progress = TRUE)))
  expect_true(cap$progress)

  # refresh past the cache the first pull just wrote, so download runs again.
  suppressMessages(do.call(.gcs_pull_cached,
                           gpc_args(progress = FALSE, refresh = TRUE)))
  expect_false(cap$progress)
})

test_that("different bucket overrides use separate default caches", {
  stub_gcs_base()
  tmp <- withr::local_tempdir()
  mockery::stub(.gcs_pull_cached, "tools::R_user_dir", function(...) tmp)
  mockery::stub(.gcs_pull_cached, "download_with_verify", mock_download())
  p1 <- suppressMessages(do.call(.gcs_pull_cached, gpc_args(bucket = "bucket_a")))
  p2 <- suppressMessages(do.call(.gcs_pull_cached, gpc_args(bucket = "bucket_b")))
  expect_false(p1 == p2)                       # not one shared cache file
  expect_true(file.exists(p1) && file.exists(p2))
})

test_that("a failed download preserves the cache and leaves no partial file", {
  stub_gcs_base()
  d <- withr::local_tempdir()
  cached <- file.path(d, "s160_disposition_prod.parquet")
  writeLines("good", cached)
  mockery::stub(.gcs_pull_cached, "download_with_verify",
                mock_download(fail = "connection reset", write_first = TRUE))
  expect_error(suppressMessages(do.call(.gcs_pull_cached,
                                        gpc_args(dest = d, refresh = TRUE))),
               "Failed to download")
  expect_equal(readLines(cached), "good")                  # existing cache untouched
  expect_length(list.files(d, pattern = "\\.part$"), 0L)   # written partial cleaned up
})

test_that("rename fallback copies, and a total move failure errors", {
  stub_gcs_base()
  d <- withr::local_tempdir()
  mockery::stub(.gcs_pull_cached, "download_with_verify", mock_download())
  mockery::stub(.gcs_pull_cached, "file.rename", function(...) FALSE)
  p <- suppressMessages(do.call(.gcs_pull_cached, gpc_args(dest = d)))  # rename -> copy
  expect_true(file.exists(p))
  mockery::stub(.gcs_pull_cached, "file.copy", function(...) FALSE)
  expect_error(suppressMessages(do.call(.gcs_pull_cached,
                                        gpc_args(dest = d, refresh = TRUE))),
               "move the downloaded file into place")       # both fail -> error
})
