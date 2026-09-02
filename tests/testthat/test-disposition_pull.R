# disposition_pull(): a thin wrapper. The pull machinery is exercised once in
# test-gcs_pull_cached.R; here we assert it resolves (dataset, env) to the right
# bucket + object and threads its other args, plus the deprecated `bucket=`.

test_that("disposition_pull resolves env to the disposition bucket + object", {
  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(.gcs_pull_cached = function(...) {
    cap$args <- list(...)
    "PATH"
  })
  expect_equal(disposition_pull(env = "dev"), "PATH")
  a <- cap$args
  expect_equal(a$fn, "disposition_pull")
  expect_equal(a$bucket, "s160_disposition_dev")
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
  disposition_pull(dest = "/tmp/x", refresh = TRUE, progress = TRUE)
  a <- cap$args
  expect_equal(a$bucket, "s160_disposition_prod")   # prod default
  expect_equal(a$dest, "/tmp/x")
  expect_true(a$refresh)
  expect_true(a$progress)
})

test_that("disposition_pull's deprecated `bucket=` warns and is honored", {
  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(.gcs_pull_cached = function(...) {
    cap$args <- list(...)
    "P"
  })
  expect_warning(disposition_pull(bucket = "b"), "deprecated")
  expect_equal(cap$args$bucket, "b")
})

test_that("disposition_pull errors on an env tier that does not exist", {
  expect_error(disposition_pull(env = "staging"), "no staging tier")
})
