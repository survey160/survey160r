# opt_out_pull(): a thin wrapper. Mirrors test-disposition_pull.R plus the
# cross-family cache-collision guard (opt-out shares the disposition bucket).

test_that("opt_out_pull resolves env to the opt-out bucket + object", {
  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(.gcs_pull_cached = function(...) {
    cap$args <- list(...)
    "PATH"
  })
  expect_equal(opt_out_pull(env = "dev"), "PATH")
  a <- cap$args
  expect_equal(a$fn, "opt_out_pull")
  expect_equal(a$bucket, "s160_disposition_dev")
  expect_equal(a$object_name, "global_opt_out/global_opt_out.parquet")
  expect_equal(a$cache_suffix, ".global_opt_out.parquet")
  expect_equal(a$noun, "opt-out list")
})

test_that("opt_out_pull defaults env to prod and threads its args", {
  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(.gcs_pull_cached = function(...) {
    cap$args <- list(...)
    "P"
  })
  opt_out_pull(dest = "/tmp/x", refresh = TRUE, progress = TRUE)
  a <- cap$args
  expect_equal(a$bucket, "s160_disposition_prod")
  expect_equal(a$dest, "/tmp/x")
  expect_true(a$refresh)
  expect_true(a$progress)
})

test_that("opt_out_pull's deprecated `bucket=` warns and is honored", {
  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(.gcs_pull_cached = function(...) {
    cap$args <- list(...)
    "P"
  })
  expect_warning(opt_out_pull(bucket = "b"), "deprecated")
  expect_equal(cap$args$bucket, "b")
})

test_that("opt_out_pull errors on an env tier that does not exist", {
  expect_error(opt_out_pull(env = "staging"), "no staging tier")
})

test_that("opt-out and disposition caches do not collide in a shared bucket", {
  stub_gcs_base()
  d <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    download_with_verify = function(object_name, local_path, bucket,
                                    progress = FALSE) {
      writeLines("x", local_path)
      invisible(local_path)
    })
  p_disp <- suppressMessages(disposition_pull(dest = d))
  p_opt  <- suppressMessages(opt_out_pull(dest = d))
  expect_equal(basename(p_disp), "s160_disposition_prod.parquet")
  expect_equal(basename(p_opt), "s160_disposition_prod.global_opt_out.parquet")
  expect_true(file.exists(p_disp) && file.exists(p_opt))
})
