# Coverage for run_latency_all(): bucket-wide orchestrator. The per-campaign
# call to run_latency is mocked; we exercise the discovery, error handling,
# and bucket-threading logic.

run_all <- function(...) {
  suppressMessages(run_latency_all(...))
}

test_that("run_latency_all iterates discovered campaigns and returns a status frame", {
  captured <- new_capture()
  stub_campaign_list(c("1001", "1002", "1003"))
  stub_run_latency(capture = captured)

  out <- run_all(source_bucket = "campaign_results",
                 bucket = "s160_analytics_dev")

  expect_equal(captured$ids, c("1001", "1002", "1003"))
  expect_equal(nrow(out), 3L)
  expect_equal(out$status, rep("ok", 3L))
  expect_equal(out$parquet_uri,
               sprintf("gs://s160_analytics_dev/latency/%s_latency.parquet",
                       c("1001", "1002", "1003")))
  expect_true(all(is.na(out$error_message)))
  expect_true(all(out$elapsed_s >= 0))
})

test_that("run_latency_all forwards overrides to run_latency", {
  captured <- new_capture()
  stub_campaign_list(c("42"))
  stub_run_latency(capture = captured)

  run_all(
    source_bucket = "campaign_results",
    bucket = "dst",
    field_timezone = "America/New_York",
    date_filter = "2026-01-26",
    respondent_id_column = "userid",
    run_by = "custom_label"
  )

  args <- captured$last_args
  expect_equal(args$bucket, "dst")
  expect_equal(args$source_bucket, "campaign_results")
  expect_equal(args$field_timezone, "America/New_York")
  expect_equal(args$date_filter, "2026-01-26")
  expect_equal(args$respondent_id_column, "userid")
  expect_equal(args$run_by, "custom_label")
})

test_that("run_latency_all continues past per-campaign failures by default", {
  stub_campaign_list(c("good", "bad", "also_good"))
  stub_run_latency(fail_on = "bad")

  out <- run_all(source_bucket = "campaign_results", bucket = "dst")

  expect_equal(out$status, c("ok", "failed", "ok"))
  expect_equal(out$error_message[2], "malformed CSV")
  expect_true(is.na(out$parquet_uri[2]))
})

test_that("run_latency_all aborts when continue_on_error = FALSE", {
  stub_campaign_list(c("good", "bad"))
  stub_run_latency(fail_on = "bad")

  expect_error(
    run_all(source_bucket = "campaign_results", bucket = "dst",
            continue_on_error = FALSE),
    "malformed CSV"
  )
})

test_that("run_latency_all honors an explicit campaign_ids list", {
  captured <- new_capture()
  local_mocked_bindings(
    s160_gcs_campaign_results_list = function(bucket = NULL) {
      stop("should not be called")
    }
  )
  stub_run_latency(capture = captured)

  out <- run_all(source_bucket = "campaign_results", bucket = "dst",
                 campaign_ids = c(2133L, 2134L))

  expect_equal(captured$ids, c("2133", "2134"))
  expect_equal(nrow(out), 2L)
})

test_that("run_latency_all forwards source_bucket to the list call", {
  captured_bucket <- NULL
  local_mocked_bindings(
    s160_gcs_campaign_results_list = function(bucket = NULL) {
      captured_bucket <<- bucket
      character(0)
    }
  )
  stub_run_latency()

  run_all(source_bucket = "campaign_results", bucket = "dst")

  expect_equal(captured_bucket, "campaign_results")
})

test_that("run_latency_all stamps a single fleet-wide run_at on every campaign", {
  captured <- new_capture()
  stub_campaign_list(c("a", "b", "c"))
  stub_run_latency(capture = captured)

  run_all(source_bucket = "campaign_results", bucket = "dst")

  expect_length(captured$run_ats, 3L)
  expect_false(any(vapply(captured$run_ats, is.null, logical(1))))
  # All three campaigns got the exact same POSIXct instant.
  unique_stamps <- unique(do.call(c, captured$run_ats))
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
  for (bad_workers in list(0, c(1, 2), NA_integer_)) {
    expect_error(
      run_latency_all(source_bucket = "src", bucket = "dst",
                      workers = bad_workers),
      "workers"
    )
  }
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

# --- skip_unchanged paths -------------------------------------------------

test_that("run_latency_all skips campaigns whose output is already current", {
  cap <- new_capture()
  local_mocked_bindings(
    s160_gcs_campaign_results_status = function(campaign_id, bucket = NULL) {
      gcs_status(name = sprintf("%s_raw_data_download.csv", campaign_id),
                 updated = "2026-01-01 00:00:00")
    },
    s160_gcs_latency_output_status = function(campaign_id, bucket) {
      updated <- if (campaign_id == "fresh") {
        "2026-01-02 00:00:00"
      } else {
        "2025-12-25 00:00:00"
      }
      gcs_status(name = sprintf("latency/%s_latency.parquet", campaign_id),
                 updated = updated)
    }
  )
  stub_run_latency(capture = cap)

  out <- run_all(source_bucket = "src", bucket = "dst",
                 campaign_ids = c("stale", "fresh"),
                 skip_unchanged = TRUE)

  expect_equal(out$status, c("ok", "skipped"))
  expect_equal(cap$ids, "stale")
  expect_equal(out$parquet_uri[2], "gs://dst/latency/fresh_latency.parquet")
  expect_equal(out$elapsed_s[2], 0)
})

test_that("run_latency_all skip_unchanged runs campaigns missing source or dest", {
  cap <- new_capture()
  local_mocked_bindings(
    s160_gcs_campaign_results_status = function(campaign_id, bucket = NULL) {
      if (campaign_id == "no_source") return(NULL)
      gcs_status(name = "x.csv", updated = "2026-01-01")
    },
    s160_gcs_latency_output_status = function(campaign_id, bucket) {
      if (campaign_id == "no_dest") return(NULL)
      gcs_status(name = "x.parquet", updated = "2026-01-01")
    }
  )
  stub_run_latency(capture = cap)

  out <- run_all(source_bucket = "src", bucket = "dst",
                 campaign_ids = c("no_source", "no_dest"),
                 skip_unchanged = TRUE)

  expect_equal(out$status, c("ok", "ok"))
  expect_equal(sort(cap$ids), c("no_dest", "no_source"))
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
      gcs_status(updated = "2026-01-01")
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
      gcs_status(updated = "2026-01-01")
    }
  )
  expect_null(.skip_unchanged_uri("1", "src", "dst"))
})

test_that("run_latency_all dispatches via future.apply when workers > 1", {
  skip_if_not_installed("future.apply")
  skip_if_not_installed("future")
  old_plan <- future::plan(future::sequential)
  on.exit(future::plan(old_plan), add = TRUE)

  cap <- new_capture()
  stub_campaign_list(c("p1", "p2"))
  local_mocked_bindings(.ensure_worker_gcs_auth = function(parent_oauth) NULL)
  stub_run_latency(capture = cap)

  out <- run_all(source_bucket = "src", bucket = "dst", workers = 2)

  expect_equal(out$status, c("ok", "ok"))
  expect_equal(out$parquet_uri,
               c("gs://dst/latency/p1_latency.parquet",
                 "gs://dst/latency/p2_latency.parquet"))
})
