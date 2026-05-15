# Coverage for run_latency_all(): bucket-wide orchestrator. The per-campaign
# call to run_latency is mocked; we exercise the discovery, error handling,
# and bucket-threading logic.

test_that("run_latency_all iterates discovered campaigns and returns a status frame", {
  captured_calls <- new.env(parent = emptyenv())
  captured_calls$ids <- character(0)
  local_mocked_bindings(
    s160_gcs_campaign_results_list = function(bucket = NULL) {
      c("1001", "1002", "1003")
    },
    run_latency = function(campaign_id, bucket, source_bucket = NULL, ...) {
      captured_calls$ids <- c(captured_calls$ids, as.character(campaign_id))
      sprintf("gs://%s/latency/%s_latency.parquet", bucket, campaign_id)
    }
  )

  out <- suppressMessages(run_latency_all(
    source_bucket = "campaign_results",
    bucket = "s160_analytics_dev"
  ))

  expect_equal(captured_calls$ids, c("1001", "1002", "1003"))
  expect_equal(nrow(out), 3L)
  expect_equal(out$status, rep("ok", 3L))
  expect_equal(out$parquet_uri,
               sprintf("gs://s160_analytics_dev/latency/%s_latency.parquet",
                       c("1001", "1002", "1003")))
  expect_true(all(is.na(out$error_message)))
  expect_true(all(out$elapsed_s >= 0))
})

test_that("run_latency_all forwards overrides to run_latency", {
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    s160_gcs_campaign_results_list = function(bucket = NULL) c("42"),
    run_latency = function(campaign_id, bucket, source_bucket = NULL,
                           field_timezone, date_filter,
                           respondent_id_column, run_by, uploader, ...) {
      captured$args <- list(
        campaign_id = campaign_id, bucket = bucket,
        source_bucket = source_bucket,
        field_timezone = field_timezone,
        date_filter = date_filter,
        respondent_id_column = respondent_id_column,
        run_by = run_by
      )
      "gs://dst/latency/42_latency.parquet"
    }
  )

  suppressMessages(run_latency_all(
    source_bucket = "campaign_results",
    bucket = "dst",
    field_timezone = "America/New_York",
    date_filter = "2026-01-26",
    respondent_id_column = "userid",
    run_by = "custom_label"
  ))

  expect_equal(captured$args$bucket, "dst")
  expect_equal(captured$args$source_bucket, "campaign_results")
  expect_equal(captured$args$field_timezone, "America/New_York")
  expect_equal(captured$args$date_filter, "2026-01-26")
  expect_equal(captured$args$respondent_id_column, "userid")
  expect_equal(captured$args$run_by, "custom_label")
})

test_that("run_latency_all continues past per-campaign failures by default", {
  local_mocked_bindings(
    s160_gcs_campaign_results_list = function(bucket = NULL) {
      c("good", "bad", "also_good")
    },
    run_latency = function(campaign_id, bucket, ...) {
      if (campaign_id == "bad") stop("malformed CSV")
      sprintf("gs://%s/latency/%s_latency.parquet", bucket, campaign_id)
    }
  )

  out <- suppressMessages(run_latency_all(
    source_bucket = "campaign_results",
    bucket = "dst"
  ))
  expect_equal(out$status, c("ok", "failed", "ok"))
  expect_equal(out$error_message[2], "malformed CSV")
  expect_true(is.na(out$parquet_uri[2]))
})

test_that("run_latency_all aborts when continue_on_error = FALSE", {
  local_mocked_bindings(
    s160_gcs_campaign_results_list = function(bucket = NULL) c("good", "bad"),
    run_latency = function(campaign_id, bucket, ...) {
      if (campaign_id == "bad") stop("malformed CSV")
      "gs://dst/latency/good_latency.parquet"
    }
  )
  expect_error(
    suppressMessages(run_latency_all(
      source_bucket = "campaign_results",
      bucket = "dst",
      continue_on_error = FALSE
    )),
    "malformed CSV"
  )
})

test_that("run_latency_all honors an explicit campaign_ids list", {
  captured <- new.env(parent = emptyenv())
  captured$ids <- character(0)
  local_mocked_bindings(
    s160_gcs_campaign_results_list = function(bucket = NULL) {
      stop("should not be called")
    },
    run_latency = function(campaign_id, bucket, ...) {
      captured$ids <- c(captured$ids, as.character(campaign_id))
      "gs://dst/x.parquet"
    }
  )
  out <- suppressMessages(run_latency_all(
    source_bucket = "campaign_results",
    bucket = "dst",
    campaign_ids = c(2133L, 2134L)
  ))
  expect_equal(captured$ids, c("2133", "2134"))
  expect_equal(nrow(out), 2L)
})

test_that("run_latency_all forwards source_bucket to the list call", {
  captured_bucket <- NULL
  local_mocked_bindings(
    s160_gcs_campaign_results_list = function(bucket = NULL) {
      captured_bucket <<- bucket
      character(0)
    },
    run_latency = function(...) "gs://dst/x.parquet"
  )
  suppressMessages(run_latency_all(
    source_bucket = "campaign_results",
    bucket = "dst"
  ))
  expect_equal(captured_bucket, "campaign_results")
})

test_that("run_latency_all stamps a single fleet-wide run_at on every campaign", {
  seen_run_ats <- list()
  local_mocked_bindings(
    s160_gcs_campaign_results_list = function(bucket = NULL) {
      c("a", "b", "c")
    },
    run_latency = function(campaign_id, bucket, run_at = NULL, ...) {
      seen_run_ats[[length(seen_run_ats) + 1L]] <<- run_at
      "gs://dst/x.parquet"
    }
  )
  suppressMessages(run_latency_all(
    source_bucket = "campaign_results",
    bucket = "dst"
  ))
  expect_length(seen_run_ats, 3L)
  expect_false(any(vapply(seen_run_ats, is.null, logical(1))))
  # All three campaigns got the exact same POSIXct instant.
  unique_stamps <- unique(do.call(c, seen_run_ats))
  expect_length(unique_stamps, 1L)
  expect_equal(attr(unique_stamps, "tzone"), "UTC")
})

test_that("run_latency_all validates source_bucket and bucket", {
  expect_error(run_latency_all(source_bucket = "", bucket = "dst"),
               "source_bucket")
  expect_error(run_latency_all(source_bucket = "src", bucket = ""),
               "bucket")
  expect_error(run_latency_all(source_bucket = c("a", "b"), bucket = "dst"),
               "source_bucket")
})

test_that("run_latency_all validates workers", {
  expect_error(run_latency_all(source_bucket = "src", bucket = "dst",
                               workers = 0),
               "workers")
  expect_error(run_latency_all(source_bucket = "src", bucket = "dst",
                               workers = c(1, 2)),
               "workers")
  expect_error(run_latency_all(source_bucket = "src", bucket = "dst",
                               workers = NA_integer_),
               "workers")
})

test_that("run_latency_all errors when workers > 1 and future.apply missing", {
  local_mocked_bindings(
    requireNamespace = function(package, ...) {
      if (identical(package, "future.apply")) FALSE else TRUE
    },
    .package = "base"
  )
  expect_error(
    run_latency_all(source_bucket = "src", bucket = "dst",
                    campaign_ids = c("1"), workers = 2),
    "future.apply"
  )
})

test_that("run_latency_all skips campaigns whose output is already current", {
  ran <- character(0)
  local_mocked_bindings(
    s160_gcs_campaign_results_status = function(campaign_id, bucket = NULL) {
      list(name = sprintf("%s_raw_data_download.csv", campaign_id),
           updated = as.POSIXct("2026-01-01 00:00:00", tz = "UTC"),
           size = 100)
    },
    s160_gcs_latency_output_status = function(campaign_id, bucket) {
      if (campaign_id == "fresh") {
        list(name = "latency/fresh_latency.parquet",
             updated = as.POSIXct("2026-01-02 00:00:00", tz = "UTC"),
             size = 50)
      } else {
        list(name = sprintf("latency/%s_latency.parquet", campaign_id),
             updated = as.POSIXct("2025-12-25 00:00:00", tz = "UTC"),
             size = 50)
      }
    },
    run_latency = function(campaign_id, bucket, ...) {
      ran <<- c(ran, as.character(campaign_id))
      sprintf("gs://%s/latency/%s_latency.parquet", bucket, campaign_id)
    }
  )
  out <- suppressMessages(run_latency_all(
    source_bucket = "src", bucket = "dst",
    campaign_ids = c("stale", "fresh"),
    skip_unchanged = TRUE
  ))
  expect_equal(out$status, c("ok", "skipped"))
  expect_equal(ran, "stale")
  expect_equal(out$parquet_uri[2], "gs://dst/latency/fresh_latency.parquet")
  expect_equal(out$elapsed_s[2], 0)
})

test_that("run_latency_all skip_unchanged runs campaigns missing source or dest", {
  ran <- character(0)
  local_mocked_bindings(
    s160_gcs_campaign_results_status = function(campaign_id, bucket = NULL) {
      if (campaign_id == "no_source") return(NULL)
      list(name = "x.csv",
           updated = as.POSIXct("2026-01-01", tz = "UTC"),
           size = 1)
    },
    s160_gcs_latency_output_status = function(campaign_id, bucket) {
      if (campaign_id == "no_dest") return(NULL)
      list(name = "x.parquet",
           updated = as.POSIXct("2026-01-01", tz = "UTC"),
           size = 1)
    },
    run_latency = function(campaign_id, bucket, ...) {
      ran <<- c(ran, as.character(campaign_id))
      sprintf("gs://%s/latency/%s_latency.parquet", bucket, campaign_id)
    }
  )
  out <- suppressMessages(run_latency_all(
    source_bucket = "src", bucket = "dst",
    campaign_ids = c("no_source", "no_dest"),
    skip_unchanged = TRUE
  ))
  expect_equal(out$status, c("ok", "ok"))
  expect_equal(sort(ran), c("no_dest", "no_source"))
})

test_that(".skip_unchanged_uri handles errors and unparseable timestamps", {
  # Source listing errors -> NULL (process the campaign).
  local_mocked_bindings(
    s160_gcs_campaign_results_status = function(...) stop("transient 503"),
    s160_gcs_latency_output_status = function(...) NULL
  )
  expect_null(.skip_unchanged_uri("1", "src", "dst"))

  # Destination listing errors -> NULL.
  local_mocked_bindings(
    s160_gcs_campaign_results_status = function(...) {
      list(name = "x",
           updated = as.POSIXct("2026-01-01", tz = "UTC"), size = 1)
    },
    s160_gcs_latency_output_status = function(...) stop("403 forbidden")
  )
  expect_null(.skip_unchanged_uri("1", "src", "dst"))

  # Both sides present but `updated` is unparseable.
  local_mocked_bindings(
    s160_gcs_campaign_results_status = function(...) {
      list(name = "x", updated = "not-a-date", size = 1)
    },
    s160_gcs_latency_output_status = function(...) {
      list(name = "y", updated = "also-bad", size = 1)
    }
  )
  expect_null(.skip_unchanged_uri("1", "src", "dst"))

  # Source has no `updated` field -> NULL.
  local_mocked_bindings(
    s160_gcs_campaign_results_status = function(...) {
      list(name = "x", updated = NULL, size = 1)
    },
    s160_gcs_latency_output_status = function(...) {
      list(name = "y",
           updated = as.POSIXct("2026-01-01", tz = "UTC"), size = 1)
    }
  )
  expect_null(.skip_unchanged_uri("1", "src", "dst"))
})

test_that("run_latency_all dispatches via future.apply when workers > 1", {
  skip_if_not_installed("future.apply")
  skip_if_not_installed("future")
  old_plan <- future::plan(future::sequential)
  on.exit(future::plan(old_plan), add = TRUE)

  local_mocked_bindings(
    s160_gcs_campaign_results_list = function(bucket = NULL) c("p1", "p2"),
    .ensure_worker_gcs_auth = function(parent_oauth) NULL,
    run_latency = function(campaign_id, bucket, ...) {
      sprintf("gs://%s/latency/%s_latency.parquet", bucket, campaign_id)
    }
  )
  out <- suppressMessages(run_latency_all(
    source_bucket = "src", bucket = "dst", workers = 2
  ))
  expect_equal(out$status, c("ok", "ok"))
  expect_equal(out$parquet_uri,
               c("gs://dst/latency/p1_latency.parquet",
                 "gs://dst/latency/p2_latency.parquet"))
})
