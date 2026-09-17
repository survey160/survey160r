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
  expect_false(any(c("engagement_rate", "optin_engaged_rate", "completion_rate") %in% names(s)))
})

test_that("summary errors on an unknown `by` column", {
  expect_error(campaign_metrics_summary(raw_metrics(), by = "nope"), "not available")
})

test_that("summary reads from a Parquet path and dedups before summing", {
  tmp <- withr::local_tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(raw_metrics(), tmp)
  s <- campaign_metrics_summary(tmp, by = "registration_id")
  expect_equal(s$n_sent[s$registration_id == "R1"], 150)
})
