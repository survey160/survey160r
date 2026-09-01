# disposition_pull(): a thin wrapper over .gcs_pull_cached(). The pull machinery
# (cache, download, atomic move, error paths) is exercised once in
# test-gcs_pull_cached.R; here we only assert this wrapper resolves `env` and
# forwards the disposition-specific object, cache suffix, noun, and fn.

test_that("disposition_pull forwards the projection params and resolves env", {
  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(.gcs_pull_cached = function(...) {
    cap$args <- list(...)
    "PATH"
  })

  expect_equal(disposition_pull(env = "dev"), "PATH")
  a <- cap$args
  expect_equal(a$fn, "disposition_pull")
  expect_equal(a$env, "dev")                                   # match.arg resolved
  expect_equal(a$object_name, "disposition_by_phone/disposition_all.parquet")
  expect_equal(a$cache_suffix, ".parquet")
  expect_equal(a$noun, "disposition projection")
})

test_that("disposition_pull defaults env to prod and threads its args", {
  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(.gcs_pull_cached = function(...) {
    cap$args <- list(...)
    "P"
  })
  disposition_pull(dest = "/tmp/x", bucket = "b", refresh = TRUE, progress = TRUE)
  a <- cap$args
  expect_equal(a$env, "prod")
  expect_equal(a$dest, "/tmp/x")
  expect_equal(a$bucket, "b")
  expect_true(a$refresh)
  expect_true(a$progress)
})
