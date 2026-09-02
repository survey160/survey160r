# resolve_dataset() / .locate(): the dataset registry, the one place physical
# GCS bucket names live. resolve_dataset maps a logical (dataset, env) to its
# physical bucket + object; .locate adds the deprecated `bucket=` escape hatch.

test_that("resolve_dataset returns the physical bucket + object per tier", {
  d_prod <- survey160r:::resolve_dataset("disposition", "prod")
  expect_equal(d_prod$bucket, "s160_disposition_prod")
  expect_equal(d_prod$object, "disposition_by_phone/disposition_all.parquet")
  expect_equal(survey160r:::resolve_dataset("disposition", "dev")$bucket,
               "s160_disposition_dev")

  o_prod <- survey160r:::resolve_dataset("opt_out", "prod")
  expect_equal(o_prod$bucket, "s160_disposition_prod")
  expect_equal(o_prod$object, "global_opt_out/global_opt_out.parquet")

  c_prod <- survey160r:::resolve_dataset("campaign_results", "prod")
  expect_equal(c_prod$bucket, "campaign_results")
  expect_null(c_prod$object)
  expect_equal(survey160r:::resolve_dataset("campaign_results", "staging")$bucket,
               "campaign_results_staging")
})

test_that("resolve_dataset errors clearly on a missing env tier", {
  expect_error(survey160r:::resolve_dataset("disposition", "staging"),
               "disposition.*no staging tier.*prod, dev")
  expect_error(survey160r:::resolve_dataset("campaign_results", "dev"),
               "campaign_results.*no dev tier.*prod, staging")
})

test_that("resolve_dataset errors on an unknown dataset", {
  expect_error(survey160r:::resolve_dataset("nope", "prod"),
               "unknown dataset: nope")
})

test_that(".locate resolves from (dataset, env) when no bucket is given", {
  loc <- survey160r:::.locate("campaign_results", "staging", NULL,
                              "s160_gcs_campaign_results_read")
  expect_equal(loc$bucket, "campaign_results_staging")
  expect_null(loc$object)
})

test_that(".locate honors a deprecated explicit bucket with a warning", {
  w <- capture_warnings(
    loc <- survey160r:::.locate("disposition", "prod", "my_bucket",
                                "disposition_pull")
  )
  expect_match(w, "deprecated")
  # Guidance points at the prod default + `env =`, not at another bucket.
  expect_match(w, "prod")
  expect_match(w, "env =")
  expect_equal(loc$bucket, "my_bucket")
  expect_equal(loc$object, "disposition_by_phone/disposition_all.parquet")
})

test_that(".locate rejects an invalid explicit bucket", {
  expect_error(
    survey160r:::.locate("disposition", "prod", "", "disposition_pull"),
    "non-empty string"
  )
})

# --- s160_datasets (public view of the registry) ------------------------------

test_that("s160_datasets lists every registry tier as (dataset, env)", {
  d <- s160_datasets()

  expect_s3_class(d, "data.frame")
  expect_named(d, c("dataset", "env"))
  expect_type(d$dataset, "character")
  expect_type(d$env, "character")
  expect_false("bucket" %in% names(d))  # physical location stays hidden

  expect_equal(
    paste(d$dataset, d$env),
    c("campaign_results prod", "campaign_results staging",
      "disposition prod", "disposition dev",
      "opt_out prod", "opt_out dev")
  )
  expect_equal(rownames(d), as.character(seq_len(nrow(d))))
})

test_that("every tier s160_datasets advertises actually resolves", {
  d <- s160_datasets()
  for (i in seq_len(nrow(d))) {
    loc <- survey160r:::resolve_dataset(d$dataset[[i]], d$env[[i]])
    expect_true(nzchar(loc$bucket))
  }
})
