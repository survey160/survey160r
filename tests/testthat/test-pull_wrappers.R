# disposition_pull() / opt_out_pull() / campaign_metrics_pull(): thin wrappers
# over .gcs_pull_cached(), whose machinery is exercised once in
# test-gcs_pull_cached.R. Here each wrapper must resolve (dataset, env) to its
# bucket + object, thread its other args, honor the deprecated `bucket=`, and
# reject an env tier the dataset lacks.

pull_wrappers <- list(
  disposition_pull = list(bucket = "s160_disposition", object = "disposition_all.parquet",
                          suffix = ".parquet", noun = "disposition projection"),
  opt_out_pull = list(bucket = "s160_disposition", object = "global_opt_out.parquet",
                      suffix = ".global_opt_out.parquet", noun = "opt-out list"),
  campaign_metrics_pull = list(bucket = "s160_analytics", object = "campaign_all.parquet",
                               suffix = ".campaign_all.parquet",
                               noun = "campaign metrics projection")
)

# Replace .gcs_pull_cached with a recorder: its arguments land in `cap$args`.
stub_pull_cached <- function(cap, env = parent.frame()) {
  testthat::local_mocked_bindings(.gcs_pull_cached = function(...) {
    cap$args <- list(...)
    "PATH"
  }, .env = env)
}

for (fn in names(pull_wrappers)) {
  w <- pull_wrappers[[fn]]
  pull <- getExportedValue("survey160r", fn)

  test_that(sprintf("%s resolves env to its bucket + object", fn), {
    cap <- new_capture()
    stub_pull_cached(cap)
    expect_identical(pull(env = "dev"), "PATH")
    a <- cap$args
    expect_identical(a$fn, fn)
    expect_identical(a$bucket, paste0(w$bucket, "_dev"))
    expect_identical(a$object_name, w$object)
    expect_identical(a$cache_suffix, w$suffix)
    expect_identical(a$noun, w$noun)
  })

  test_that(sprintf("%s defaults env to prod and threads its args", fn), {
    cap <- new_capture()
    stub_pull_cached(cap)
    pull(dest = "/tmp/x", refresh = TRUE, progress = TRUE)
    a <- cap$args
    expect_identical(a$bucket, paste0(w$bucket, "_prod"))
    expect_identical(a$dest, "/tmp/x")
    expect_true(a$refresh)
    expect_true(a$progress)
  })

  test_that(sprintf("%s's deprecated `bucket=` warns and is honored", fn), {
    cap <- new_capture()
    stub_pull_cached(cap)
    expect_warning(pull(bucket = "b"), "deprecated")
    expect_identical(cap$args$bucket, "b")
  })

  test_that(sprintf("%s errors on an env tier that does not exist", fn), {
    expect_error(pull(env = "staging"), "no staging tier")
  })
}

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
  expect_identical(basename(p_disp), "s160_disposition_prod.parquet")
  expect_identical(basename(p_opt), "s160_disposition_prod.global_opt_out.parquet")
  expect_true(file.exists(p_disp))
  expect_true(file.exists(p_opt))
})
