# campaign_metrics_pull(): a thin wrapper over .gcs_pull_cached, mirroring
# test-opt_out_pull.R. It fetches the consolidated campaign-metrics projection
# (campaign_all.parquet) from the analytics bucket -- not the disposition bucket.

test_that("campaign_metrics_pull resolves env to the analytics bucket + object", {
  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(.gcs_pull_cached = function(...) {
    cap$args <- list(...)
    "PATH"
  })
  expect_equal(campaign_metrics_pull(env = "dev"), "PATH")
  a <- cap$args
  expect_equal(a$fn, "campaign_metrics_pull")
  expect_equal(a$bucket, "s160_analytics_dev")
  expect_equal(a$object_name, "campaign_all.parquet")
  expect_equal(a$cache_suffix, ".campaign_all.parquet")
  expect_equal(a$noun, "campaign metrics projection")
})

test_that("campaign_metrics_pull defaults env to prod and threads its args", {
  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(.gcs_pull_cached = function(...) {
    cap$args <- list(...)
    "P"
  })
  campaign_metrics_pull(dest = "/tmp/x", refresh = TRUE, progress = TRUE)
  a <- cap$args
  expect_equal(a$bucket, "s160_analytics_prod")
  expect_equal(a$dest, "/tmp/x")
  expect_true(a$refresh)
  expect_true(a$progress)
})

test_that("campaign_metrics_pull's deprecated `bucket=` warns and is honored", {
  cap <- new.env(parent = emptyenv())
  testthat::local_mocked_bindings(.gcs_pull_cached = function(...) {
    cap$args <- list(...)
    "P"
  })
  expect_warning(campaign_metrics_pull(bucket = "b"), "deprecated")
  expect_equal(cap$args$bucket, "b")
})

test_that("campaign_metrics_pull errors on an env tier that does not exist", {
  expect_error(campaign_metrics_pull(env = "staging"), "no staging tier")
})
