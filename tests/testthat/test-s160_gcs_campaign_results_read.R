test_that("default call uses campaign_id filename, cleans up temp, no working dir leak", {
  stub_gcs_base()
  captured <- new_capture()
  stub_gcs_download_ok(capture_env = captured)

  wd_before <- list.files(getwd(), pattern = "\\.csv$")
  expect_message(s160_gcs_campaign_results_read(1980), "1980/1980_raw_data_download.csv")
  wd_after <- list.files(getwd(), pattern = "\\.csv$")

  expect_equal(captured$args$object_name, "1980/1980_raw_data_download.csv")
  expect_false(file.exists(captured$args$saveToDisk))
  expect_equal(wd_after, wd_before)
})

test_that("custom filename overrides default", {
  stub_gcs_base()
  captured <- new_capture()
  stub_gcs_download_ok(capture_env = captured)

  expect_message(s160_gcs_campaign_results_read(1980, filename = "custom.csv"), "1980/custom.csv")
  expect_equal(captured$args$object_name, "1980/custom.csv")
})

test_that("a missing object (gcs 'File not found') gives a clear not-found message", {
  # googleCloudStorageR 0.7.0's gcs_get_object() raises exactly this on a 404 --
  # note it carries NO "404" in the text, so the boundary must match the message.
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(...) data.frame(name = character(0), size = numeric(0), stringsAsFactors = FALSE),
    gcs_get_object = function(...) {
      stop("File not found. Check object_name and read permissions. Looked for 9999/x.csv")
    }
  )

  expect_error(
    suppressMessages(s160_gcs_campaign_results_read(9999)),
    "file not found.*campaign_results"
  )
})

test_that("an http_404 error also maps to a clear not-found message", {
  # The httr/googleAuthR-level form of a 404 (distinct from gcs_get_object's own
  # "File not found" branch) must be recognised too.
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(...) data.frame(name = character(0), size = numeric(0), stringsAsFactors = FALSE),
    gcs_get_object = function(...) stop("http_404 Unspecified error")
  )

  expect_error(
    suppressMessages(s160_gcs_campaign_results_read(9999)),
    "file not found.*campaign_results"
  )
})

test_that("non-404 error gives download failed message", {
  stub_gcs_base()
  local_mocked_bindings(
    gcs_list_objects = function(...) data.frame(name = character(0), size = numeric(0), stringsAsFactors = FALSE),
    gcs_get_object = function(...) stop("connection timeout")
  )

  expect_error(
    suppressMessages(s160_gcs_campaign_results_read(1980)),
    "Failed to download.*connection timeout"
  )
})

test_that("filename with path separator is rejected", {
  stub_gcs_base()

  expect_error(
    s160_gcs_campaign_results_read(1980, filename = "../evil.csv"),
    "path separators"
  )
  expect_error(
    s160_gcs_campaign_results_read(1980, filename = "subdir/file.csv"),
    "path separators"
  )
})

test_that("non-string destdir is rejected", {
  stub_gcs_base()

  expect_error(s160_gcs_campaign_results_read(1980, destdir = 123), "single character string")
  expect_error(s160_gcs_campaign_results_read(1980, destdir = TRUE), "single character string")
  expect_error(s160_gcs_campaign_results_read(1980, destdir = c("a", "b")), "single character string")
})

test_that("nonexistent destdir is rejected", {
  stub_gcs_base()

  expect_error(
    suppressMessages(s160_gcs_campaign_results_read(1980, destdir = "/nonexistent/path")),
    "does not exist"
  )
})

test_that("destdir saves file, prints path, and works with '.'", {
  stub_gcs_base()
  stub_gcs_download_ok()
  tmp_dir <- tempdir()
  dest_file <- file.path(tmp_dir, "1980_raw_data_download.csv")

  expect_message(s160_gcs_campaign_results_read(1980, destdir = tmp_dir), "Saved to:")
  expect_true(file.exists(dest_file))
  unlink(dest_file)

  withr::with_dir(tmp_dir, {
    expect_message(s160_gcs_campaign_results_read(1980, destdir = "."), "Saved to:")
    expect_true(file.exists("1980_raw_data_download.csv"))
  })
  unlink(dest_file)
})

test_that("hash = TRUE stamps sha256 + canonical gs:// provenance", {
  stub_gcs_base()
  stub_gcs_download_ok()
  res <- suppressMessages(s160_gcs_campaign_results_read(1980, hash = TRUE))
  expect_true(grepl("^sha256:", attr(res, "source_csv_hash")))
  expect_equal(attr(res, "source_csv_path"),
               "gs://campaign_results/1980/1980_raw_data_download.csv")
})

test_that("hash = FALSE (default) returns a plain frame, no provenance attrs", {
  stub_gcs_base()
  stub_gcs_download_ok()
  res <- suppressMessages(s160_gcs_campaign_results_read(1980))
  expect_null(attr(res, "source_csv_hash"))
  expect_null(attr(res, "source_csv_path"))
})

test_that("a non-logical hash is rejected", {
  stub_gcs_base()
  expect_error(s160_gcs_campaign_results_read(1980, hash = "yes"),
               "single TRUE or FALSE")
  expect_error(s160_gcs_campaign_results_read(1980, hash = NA),
               "single TRUE or FALSE")
})

test_that("columns_fn projects columns using the downloaded file's header", {
  stub_gcs_base()
  stub_gcs_download_ok(content = c("a,b,c", "1,2,3"))
  seen <- new_capture()

  res <- suppressMessages(s160_gcs_campaign_results_read(
    1980,
    columns_fn = function(header) {
      seen$header <- header
      c("a", "c")
    }))

  # The resolver receives the file's full header and its result is the projection.
  expect_equal(seen$header, c("a", "b", "c"))
  expect_equal(names(res), c("a", "c"))
})

test_that("an explicit columns= wins over columns_fn", {
  stub_gcs_base()
  stub_gcs_download_ok(content = c("a,b,c", "1,2,3"))
  called <- new_capture()

  res <- suppressMessages(s160_gcs_campaign_results_read(
    1980,
    columns = "a",
    columns_fn = function(header) {
      called$was <- TRUE
      c("b", "c")
    }))

  expect_equal(names(res), "a")
  # columns_fn is not consulted when columns is supplied.
  expect_null(called$was)
})

test_that("a columns_fn error warns and falls back to a full read", {
  stub_gcs_base()
  stub_gcs_download_ok(content = c("a,b", "1,2"))

  expect_warning(
    res <- suppressMessages(s160_gcs_campaign_results_read(
      1980,
      columns_fn = function(header) stop("cannot derive columns"))),
    "column projection via `columns_fn` failed.*cannot derive columns"
  )
  expect_equal(names(res), c("a", "b"))
})

test_that("a non-function columns_fn is rejected up front", {
  stub_gcs_base()
  expect_error(
    s160_gcs_campaign_results_read(1980, columns_fn = c("a", "b")),
    "`columns_fn` must be a function"
  )
})

test_that("columns_fn receives the munged (dot-form) header, not raw bracket names", {
  stub_gcs_base()
  # Raw export headers are bracket-form (id[q1]scriptDate); s160_csv_header()
  # munges them to the dot-form the latency column helpers key off, so the
  # resolver must see the munged names to project correctly.
  stub_gcs_download_ok(content = c("id[q1]scriptDate,phone",
                                   "2024-01-01 00:00:00,5551234"))
  seen <- new_capture()

  res <- suppressMessages(s160_gcs_campaign_results_read(
    1980,
    columns_fn = function(header) {
      seen$header <- header
      "id.q1.scriptDate"
    }))

  expect_equal(seen$header, c("id.q1.scriptDate", "phone"))
  expect_equal(names(res), "id.q1.scriptDate")
})
