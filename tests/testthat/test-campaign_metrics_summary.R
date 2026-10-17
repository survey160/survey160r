# Two registrations; campaign 1's cell is replicated across `threshold` (rows 1-2).
# After dedup the cells are: (c1) 100/10/2/1, (c2) 50/5/1/0 on R1; (c3) 200/40/10/4 on R2.
raw_metrics <- function() {
  data.frame(
    campaign_id = c(1, 1, 2, 3),
    date        = as.Date(c("2026-01-01", "2026-01-01", "2026-01-01", "2026-02-01")),
    hour_local  = c(9L, 9L, 10L, 11L),
    tracker_registration_id   = c("R1", "R1", "R1", "R2"),
    tracker_brand             = c("B1", "B1", "B1", "B2"),
    tracker_client            = c("C", "C", "C", "C"),
    tracker_fielding_location = c("F", "F", "F", "F"),
    tracker_mode              = c("T2W", "T2W", "T2W", "Live"),
    n_sent      = c(100, 100, 50, 200),
    n_engaged   = c(10, 10, 5, 40),
    n_opted_in  = c(2, 2, 1, 10),
    n_completed = c(1, 1, 0, 4),
    threshold   = c(1, 2, 1, 1),
    stringsAsFactors = FALSE)
}

test_that("summary rolls up by registration and does not double count replication", {
  s  <- campaign_metrics_summary(raw_metrics(), by = "registration_id")
  r1 <- s[s$registration_id == "R1", ]
  expect_equal(r1$campaigns, 2)          # campaigns 1 and 2
  expect_equal(r1$n_sent, 150)           # 100 + 50, not 250
  expect_equal(r1$n_engaged, 15)
  expect_equal(r1$engagement_rate, 0.1)  # 15 / 150
  r2 <- s[s$registration_id == "R2", ]
  expect_equal(r2$n_sent, 200)
  expect_equal(r2$engagement_rate, 0.2)  # 40 / 200
})

test_that("summary percent scales the rates", {
  s <- campaign_metrics_summary(raw_metrics(), by = "registration_id", percent = TRUE)
  expect_equal(s$engagement_rate[s$registration_id == "R1"], 10)
})

test_that("summary groups by multiple dimensions", {
  s <- campaign_metrics_summary(raw_metrics(), by = c("brand", "mode"))
  expect_equal(nrow(s), 2)               # (B1, T2W) and (B2, Live)
  expect_setequal(s$brand, c("B1", "B2"))
})

test_that("summary rates = FALSE omits the rate columns", {
  s <- campaign_metrics_summary(raw_metrics(), by = "registration_id", rates = FALSE)
  expect_false(any(c("engagement_rate", "opted_in_rate", "opted_in_engaged_rate", "completion_rate") %in% names(s)))
})

test_that("summary errors on an unknown `by` column", {
  expect_error(campaign_metrics_summary(raw_metrics(), by = "nope"),
               "campaign_metrics_summary: `by` column\\(s\\) not in `x`: nope \\(available: ")
})

test_that("summary errors when `campaign_id` is absent (needed for `campaigns`)", {
  df <- data.frame(registration_id = "R1", n_sent = 100, n_engaged = 10,
                   n_opted_in = 2, n_completed = 1, stringsAsFactors = FALSE)
  expect_error(campaign_metrics_summary(df, by = "registration_id"),
               "campaign_metrics_summary: `x` has no `campaign_id` column\\.")
})

test_that("summary reads from a Parquet path and dedups before summing", {
  tmp <- withr::local_tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(raw_metrics(), tmp)
  s <- campaign_metrics_summary(tmp, by = "registration_id")
  expect_equal(s$n_sent[s$registration_id == "R1"], 150)
})

test_that("summary counts each send once across the hour and day-total grains", {
  # Campaign 1, 2026-01-01: two hour cells plus their day total (hour_local NA),
  # as the projection stores them. Campaign 2 has hour cells only.
  d <- data.frame(
    campaign_id = c(1, 1, 1, 2, 2),
    date        = as.Date("2026-01-01"),
    hour_local  = c(9L, 10L, NA, 9L, 10L),
    tracker_registration_id = "R1",
    n_sent      = c(60, 40, 100, 30, 20),
    n_engaged   = c(6, 4, 10, 3, 2),
    n_opted_in  = c(3, 1, 4, 1, 1),
    n_completed = c(1, 1, 2, 0, 1),
    stringsAsFactors = FALSE)
  s <- campaign_metrics_summary(d, by = "registration_id", rates = FALSE)
  expect_identical(s$n_sent, 150)        # 100 (day total) + 30 + 20, not 250
  expect_identical(s$n_completed, 3)
  expect_identical(s$campaigns, 2L)
})

test_that("summary on a real latency report matches latency_funnel()", {
  fx <- synthetic_fixture()
  cons <- latency_report(fx$data, fx$config)$consolidated   # hour rows + day rollup
  s <- campaign_metrics_summary(cons, rates = FALSE)
  f <- latency_funnel(cons)
  expect_identical(s$n_sent, sum(f$n_sent))
  expect_identical(s$n_completed, sum(f$n_completed))
})
