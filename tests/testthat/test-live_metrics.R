# live_metrics_pull() / live_metrics_records() / live_metrics_error_rates().
# The pull is tested against a recording .gcs_pull_cached (its own machinery is
# covered in test-gcs_pull_cached.R); the readers against frames and real
# Parquet files written to a temp dir.

live_cells <- function() {
  data.frame(
    et_date = as.Date(c("2026-11-01", "2026-11-01", "2026-11-01",
                        "2026-11-02", "2026-11-02", "2026-11-02")),
    et_hour = c(10L, 10L, 11L, 10L, 10L, 12L),
    carrier = c("TMobile", "TMobile", NA, "TMobile", "TMobile", "Verizon"),
    brand = c("B1", "B1", "B2", "B1", NA, "B2"),
    campaign_id = c(1L, 1L, 2L, 1L, 1L, 2L),
    error_code = c("(none)", "4780", "(none)", "(none)", "4700", NA),
    n_sent = c(90L, 10L, 5L, 80L, 20L, 50L),
    n_engaged = c(30L, 0L, 1L, 20L, 0L, 10L),
    stringsAsFactors = FALSE)
}

write_live_days <- function(dir, d = live_cells()) {
  vapply(split(d, d$et_date), function(day) {
    p <- file.path(dir, sprintf("dt=%s.parquet", day$et_date[[1L]]))
    nanoparquet::write_parquet(day, p)
    p
  }, character(1))
}

# --- live_metrics_records ---------------------------------------------------

test_that("records fill unknown carrier/brand and a missing code, and filter campaigns", {
  r <- live_metrics_records(live_cells())
  expect_named(r, survey160r:::.LIVE_METRICS_COLS)
  expect_equal(r$carrier[[3]], "(unknown)")
  expect_equal(r$brand[[5]], "(unknown)")
  expect_equal(r$error_code[[6]], "(none)")
  expect_equal(nrow(live_metrics_records(live_cells(), campaign_ids = 2L)), 2L)
})

test_that("records read and stack the day files", {
  dir <- withr::local_tempdir()
  paths <- write_live_days(dir)
  r <- live_metrics_records(paths)
  expect_equal(nrow(r), 6L)
  expect_s3_class(r$et_date, "Date")
  expect_equal(sum(r$n_sent), 255L)
})

test_that("records of no files is an empty frame with the columns", {
  r <- live_metrics_records(character(0))
  expect_equal(nrow(r), 0L)
  expect_named(r, survey160r:::.LIVE_METRICS_COLS)
})

test_that("records reject a missing file, a non-path, and a short frame", {
  expect_error(live_metrics_records(file.path(tempdir(), "nope.parquet")),
               class = "s160_not_found")
  expect_error(live_metrics_records(42), "paths from live_metrics_pull")
  expect_error(live_metrics_records(live_cells()[, -1]), "missing column\\(s\\): et_date")
})

# --- live_metrics_error_rates -------------------------------------------------

test_that("error rates by carrier: daily, cumulative and per code", {
  g <- live_metrics_error_rates(live_cells())
  expect_equal(names(g), c("et_date", "carrier", "n_sent", "n_error", "error_rate",
                           "cum_sent", "cum_error", "cum_error_rate",
                           "n_4780", "rate_4780", "cum_rate_4780"))
  t <- g[g$carrier == "TMobile", ]
  expect_equal(t$n_sent, c(100L, 100L))
  expect_equal(t$n_error, c(10L, 20L))
  expect_equal(t$error_rate, c(0.1, 0.2))
  expect_equal(t$cum_sent, c(100L, 200L))
  expect_equal(t$cum_error_rate, c(0.1, 0.15))
  expect_equal(t$n_4780, c(10L, 0L))
  expect_equal(t$rate_4780, c(0.1, 0))
  expect_equal(t$cum_rate_4780, c(0.1, 0.05))
  expect_equal(g$error_rate[g$carrier == "(unknown)"], 0)
  expect_equal(g$error_rate[g$carrier == "Verizon"], 0)  # NA code = no error
})

test_that("error rates fleet-wide (by = NULL), in percent, with no codes", {
  g <- live_metrics_error_rates(live_cells(), by = NULL, codes = character(0),
                                percent = TRUE)
  expect_equal(names(g), c("et_date", "n_sent", "n_error", "error_rate",
                           "cum_sent", "cum_error", "cum_error_rate"))
  expect_equal(g$error_rate, c(100 * 10 / 105, 100 * 20 / 150))
  expect_equal(g$cum_error_rate[[2]], 100 * 30 / 255)
})

test_that("error rates by several dimensions keep each group's running total", {
  g <- live_metrics_error_rates(live_cells(), by = c("carrier", "brand"))
  t <- g[g$carrier == "TMobile" & g$brand == "B1", ]
  expect_equal(t$cum_sent, c(100L, 180L))
})

test_that("a missing `by` value is its own group with a running total", {
  d <- live_cells()
  d$campaign_id <- NA_integer_
  g <- live_metrics_error_rates(d, by = "campaign_id")
  expect_equal(g$cum_sent, c(105L, 255L))
})

test_that("a group with no sends has NA rates, and an empty input is empty", {
  d <- live_cells()
  d$n_sent[d$carrier %in% "Verizon"] <- 0L
  g <- live_metrics_error_rates(d)
  expect_true(is.na(g$error_rate[g$carrier == "Verizon"]))
  e <- live_metrics_error_rates(live_cells()[0, ])
  expect_equal(nrow(e), 0L)
  expect_true(all(c("cum_sent", "cum_error_rate", "cum_rate_4780") %in% names(e)))
})

test_that("records tolerate a frame without n_engaged and counts read back as text", {
  d <- live_cells()
  d$n_engaged <- NULL
  d$n_sent <- factor(d$n_sent)
  r <- live_metrics_records(d)
  expect_true(all(is.na(r$n_engaged)))
  expect_equal(sum(r$n_sent), 255)
  expect_equal(live_metrics_error_rates(d, by = NULL)$n_sent, c(105, 150))
})

test_that("duplicated `by` and `codes` collapse; an NA or empty code is rejected", {
  g <- live_metrics_error_rates(live_cells(), by = c("carrier", "carrier"),
                                codes = c("4780", "4780"))
  expect_equal(names(g), c("et_date", "carrier", "n_sent", "n_error", "error_rate",
                           "cum_sent", "cum_error", "cum_error_rate",
                           "n_4780", "rate_4780", "cum_rate_4780"))
  expect_error(live_metrics_error_rates(live_cells(), codes = NA), "non-empty error codes")
  expect_error(live_metrics_error_rates(live_cells(), codes = ""), "non-empty error codes")
})

test_that("error rates reject an unknown `by` and a bad `percent`", {
  expect_error(live_metrics_error_rates(live_cells(), by = "zip"), "`by` must be among")
  expect_error(live_metrics_error_rates(live_cells(), percent = NA), "single TRUE or FALSE")
})

# --- live_metrics_pull --------------------------------------------------------

# A recording .gcs_pull_cached: writes a stub file at `dest` unless the object's
# day is in `absent` (then a classed not-found, like a GCS 404).
stub_live_pull <- function(cap, absent = character(0), env = parent.frame()) {
  cap$calls <- list()
  testthat::local_mocked_bindings(
    .live_metrics_today = function() as.Date("2026-11-04"),
    .gcs_pull_cached = function(fn, dest, bucket, refresh, progress, object_name,
                                cache_suffix, noun) {
      cap$calls[[length(cap$calls) + 1L]] <- list(
        dest = dest, bucket = bucket, refresh = refresh, object = object_name)
      if (any(vapply(absent, grepl, logical(1), x = object_name, fixed = TRUE))) {
        stop(survey160r:::s160_condition("object not found", "s160_not_found"))
      }
      if (refresh || !file.exists(dest)) writeLines("x", dest)
      dest
    },
    .env = env)
}

test_that("pull fetches each day, re-fetching only the last two and missing ones", {
  dir <- withr::local_tempdir()
  cache <- file.path(dir, "s160_analytics_prod.live_metrics")
  dir.create(cache)
  writeLines("old", file.path(cache, "dt=2026-11-01.parquet"))
  cap <- new_capture()
  stub_live_pull(cap, absent = "dt=2026-11-02")
  expect_message(
    p <- live_metrics_pull("2026-11-01", "2026-11-04", dest = dir),
    "3 day\\(s\\) \\(2 downloaded, 1 cached\\); no file for 2026-11-02")
  expect_equal(names(p), c("2026-11-01", "2026-11-03", "2026-11-04"))
  objects <- vapply(cap$calls, `[[`, "", "object")
  expect_equal(objects[[1]], "live/metrics/dt=2026-11-01.parquet")
  expect_equal(vapply(cap$calls, `[[`, NA, "refresh"), c(FALSE, TRUE, TRUE, TRUE))
  expect_true(all(vapply(cap$calls, `[[`, "", "bucket") == "s160_analytics_prod"))
})

test_that("a day cached while it was still open is fetched again, not kept as final", {
  dir <- withr::local_tempdir()
  cache <- file.path(dir, "s160_analytics_prod.live_metrics")
  dir.create(cache)
  early <- file.path(cache, "dt=2026-10-20.parquet")
  late <- file.path(cache, "dt=2026-10-21.parquet")
  writeLines("partial", early)
  writeLines("final", late)
  Sys.setFileTime(early, as.POSIXct("2026-10-21 23:00", tz = "America/New_York"))
  Sys.setFileTime(late, as.POSIXct("2026-10-23 02:00", tz = "America/New_York"))
  cap <- new_capture()
  stub_live_pull(cap)
  expect_message(live_metrics_pull("2026-10-20", "2026-10-21", dest = dir),
                 "\\(1 downloaded, 1 cached\\)")
  expect_equal(vapply(cap$calls, `[[`, NA, "refresh"), c(TRUE, FALSE))
})

test_that("pull defaults to the last 30 days in the user cache, and refresh re-fetches all", {
  withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir())
  cap <- new_capture()
  stub_live_pull(cap)
  expect_message(p <- live_metrics_pull(refresh = TRUE, env = "dev"),
                 "31 day\\(s\\) \\(31 downloaded, 0 cached\\)\n")
  expect_equal(names(p)[c(1, 31)], c("2026-10-05", "2026-11-04"))
  expect_true(all(vapply(cap$calls, `[[`, NA, "refresh")))
  expect_equal(cap$calls[[1]]$bucket, "s160_analytics_dev")
  expect_match(cap$calls[[1]]$dest, "survey160r")
})

test_that("a missing day is skipped through the real download helper", {
  # Only the network edge is mocked, so .gcs_pull_cached's own handlers run.
  dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    check_gcs_ready = function() NULL,
    download_with_verify = function(object_name, local_path, ...) {
      if (grepl("2026-10-02", object_name, fixed = TRUE)) {
        stop_not_found("object", object_name)
      }
      writeLines("x", local_path)
    })
  expect_message(p <- live_metrics_pull("2026-10-01", "2026-10-03", dest = dir),
                 "2 day\\(s\\).*no file for 2026-10-02")
  expect_equal(names(p), c("2026-10-01", "2026-10-03"))
})

test_that("a date-time bound is read as its ET day", {
  cap <- new_capture()
  stub_live_pull(cap)
  late <- as.POSIXct("2026-11-01 22:30", tz = "America/New_York")  # Nov 2 in UTC
  suppressMessages(p <- live_metrics_pull(late, late, dest = withr::local_tempdir()))
  expect_equal(names(p), "2026-11-01")
})

test_that("pull rejects reversed, mis-parsed and ancient dates, and bad args", {
  expect_error(live_metrics_pull("2026-11-04", "2026-11-01"), "is after")
  expect_error(live_metrics_pull("not a date"), "single date \\(YYYY-MM-DD\\)")
  expect_error(live_metrics_pull("10/01/2026"), "single date \\(YYYY-MM-DD\\)")
  expect_error(live_metrics_pull(as.Date("0010-01-20")), "on or after 2020-01-01")
  expect_error(live_metrics_pull(as.Date(c("2026-10-01", "2026-10-02"))), "single date")
  expect_error(live_metrics_pull(refresh = NA), "single TRUE or FALSE")
  expect_error(live_metrics_pull(dest = ""), "non-empty string")
  expect_error(live_metrics_pull(env = "staging"), "no staging tier")
})

test_that("pull rejects a dest that is a file, and a cache dir it cannot create", {
  f <- withr::local_tempfile()
  writeLines("x", f)
  expect_error(live_metrics_pull(dest = f), "must be a directory")
  testthat::local_mocked_bindings(dir.create = function(...) FALSE, .package = "base")
  expect_error(live_metrics_pull(dest = withr::local_tempdir()),
               "create the cache directory")
})

test_that("the ET today helper returns a Date", {
  expect_s3_class(survey160r:::.live_metrics_today(), "Date")
})
