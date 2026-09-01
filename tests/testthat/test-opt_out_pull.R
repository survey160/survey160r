# opt_out_pull(): a thin wrapper over .gcs_pull_cached(). The pull machinery is
# exercised once in test-gcs_pull_cached.R; here we assert this wrapper resolves
# `env` and forwards the opt-out object, cache suffix, and noun -- and that the
# opt-out and disposition caches do not collide in their shared bucket.

test_that("opt_out_pull forwards the list params and resolves env", {
  cap <- new.env(parent = emptyenv())
  mockery::stub(opt_out_pull, ".gcs_pull_cached", function(...) {
    cap$args <- list(...)
    "PATH"
  })

  expect_equal(opt_out_pull(env = "dev"), "PATH")
  a <- cap$args
  expect_equal(a$fn, "opt_out_pull")
  expect_equal(a$env, "dev")                                   # match.arg resolved
  expect_equal(a$object_name, "global_opt_out/global_opt_out.parquet")
  expect_equal(a$cache_suffix, ".global_opt_out.parquet")
  expect_equal(a$noun, "opt-out list")
})

test_that("opt_out_pull defaults env to prod and threads its args", {
  cap <- new.env(parent = emptyenv())
  mockery::stub(opt_out_pull, ".gcs_pull_cached", function(...) {
    cap$args <- list(...)
    "P"
  })
  opt_out_pull(dest = "/tmp/x", bucket = "b", refresh = TRUE, progress = TRUE)
  a <- cap$args
  expect_equal(a$env, "prod")
  expect_equal(a$dest, "/tmp/x")
  expect_equal(a$bucket, "b")
  expect_true(a$refresh)
  expect_true(a$progress)
})

test_that("opt-out and disposition caches do not collide in a shared bucket", {
  # Both share the s160_disposition_<env> bucket; the cache_suffix keeps their
  # default cache files distinct. End-to-end (real .gcs_pull_cached, mocked
  # download) so the two wrappers' filenames are checked against each other.
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
