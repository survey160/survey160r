# Coverage for disposition_funnel() -- the funnel-count rollup over the
# disposition dataset (one row per (phone, campaign_id), contacted-only). Rolls
# the per-recipient 0/1 flags up to counts per `by` dimension (+ disposition_date
# on the day grain). In-memory fixtures for the compute paths; a nanoparquet
# fixture (write_disposition_parquet / .record_row, helper-stubs.R) for the path
# input branch.

# Eight recipients across two campaigns and three carriers (one NA = unknown).
.funnel_records <- function() {
  data.frame(
    phone = as.character(1:8),
    campaign_id = c(101L, 101L, 101L, 101L, 102L, 102L, 102L, 102L),
    sent = rep(1L, 8),
    engaged   = c(1L, 1L, 0L, 1L, 1L, 0L, 0L, 0L),
    opted_in  = c(1L, 0L, 0L, 1L, 1L, 0L, 0L, 0L),
    completed = c(1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L),
    ineligible = c(0L, 0L, 1L, 0L, 0L, 0L, 0L, 0L),
    refused   = c(0L, 1L, 0L, 0L, 0L, 0L, 0L, 1L),
    carrier   = c("AT&T", "AT&T", "Verizon", NA, "AT&T", "Verizon", "Verizon", NA),
    survey_mode = rep("sms", 8),
    disposition_date = as.Date(c("2026-01-10", "2026-01-10", "2026-01-11",
                                 "2026-01-11", "2026-02-01", "2026-02-01",
                                 "2026-02-01", "2026-02-02")),
    stringsAsFactors = FALSE
  )
}

test_that("grain='all' rolls up by carrier with the NA bucket last, counts + rates", {
  res <- disposition_funnel(.funnel_records(), grain = "all")
  expect_named(res, c("carrier", "n_sent", "n_engaged", "n_opted_in",
                      "n_completed", "n_ineligible", "n_refused",
                      "pct_engaged", "pct_opted_in", "pct_completed"))
  # order: AT&T, Verizon, then NA (unknown carrier) last
  expect_equal(res$carrier, c("AT&T", "Verizon", NA))
  expect_equal(res$n_sent, c(3L, 3L, 2L))
  expect_equal(res$n_engaged, c(3L, 0L, 1L))
  expect_equal(res$n_opted_in, c(2L, 0L, 1L))
  expect_equal(res$n_completed, c(2L, 0L, 0L))
  expect_equal(res$n_ineligible, c(0L, 1L, 0L))
  expect_equal(res$n_refused, c(1L, 0L, 1L))
  # send-anchored rates
  expect_equal(res$pct_opted_in, c(200 / 3, 0, 50))
  expect_equal(res$pct_completed, c(200 / 3, 0, 0))
  expect_equal(res$pct_engaged, c(100, 0, 50))
})

test_that("grain='day' adds disposition_date to the grouping", {
  res <- disposition_funnel(.funnel_records(), grain = "day")
  expect_named(res, c("carrier", "disposition_date", "n_sent", "n_engaged",
                      "n_opted_in", "n_completed", "n_ineligible", "n_refused",
                      "pct_engaged", "pct_opted_in", "pct_completed"))
  expect_equal(res$carrier,
               c("AT&T", "AT&T", "Verizon", "Verizon", NA, NA))
  expect_equal(res$disposition_date,
               as.Date(c("2026-01-10", "2026-02-01", "2026-01-11",
                         "2026-02-01", "2026-01-11", "2026-02-02")))
  # AT&T on 2026-01-10 = recipients 1 + 2
  expect_equal(res$n_sent[1], 2L)
  expect_equal(res$n_engaged[1], 2L)
  expect_equal(res$n_opted_in[1], 1L)
  expect_equal(res$n_completed[1], 1L)
  expect_equal(res$n_refused[1], 1L)
})

test_that("by accepts multiple dimensions", {
  res <- disposition_funnel(.funnel_records(), by = c("campaign_id", "carrier"),
                            grain = "all")
  expect_equal(names(res)[1:2], c("campaign_id", "carrier"))
  # campaign 101 has AT&T, Verizon, NA; campaign 102 has AT&T, Verizon, NA
  expect_equal(res$campaign_id, c(101L, 101L, 101L, 102L, 102L, 102L))
  expect_equal(res$carrier, c("AT&T", "Verizon", NA, "AT&T", "Verizon", NA))
  # campaign 101 AT&T = recipients 1 + 2
  expect_equal(res$n_sent[1], 2L)
})

test_that("an all-NA completed group returns NA n_completed (off-channel), not 0", {
  ext <- data.frame(
    phone = as.character(1:2), campaign_id = c(200L, 200L), sent = c(1L, 1L),
    engaged = c(1L, 1L), opted_in = c(1L, 0L),
    completed = c(NA_integer_, NA_integer_),
    ineligible = c(0L, 0L), refused = c(0L, 0L),
    carrier = c("AT&T", "AT&T"),
    disposition_date = as.Date(c("2026-03-01", "2026-03-01")),
    stringsAsFactors = FALSE
  )
  res <- disposition_funnel(ext, grain = "all")
  expect_true(is.na(res$n_completed))
  expect_true(is.na(res$pct_completed))
  expect_equal(res$n_engaged, 2L)      # in-channel flags still count
  expect_equal(res$n_opted_in, 1L)
})

test_that("refused/ineligible default to 0 when absent (pre-0.51.0 projection)", {
  recs <- .funnel_records()
  recs <- recs[, setdiff(names(recs), c("refused", "ineligible"))]
  res <- disposition_funnel(recs, grain = "all")
  expect_equal(res$n_refused, c(0L, 0L, 0L))
  expect_equal(res$n_ineligible, c(0L, 0L, 0L))
})

test_that("reads a Parquet path and rolls it up", {
  path <- write_disposition_parquet(rbind(
    .record_row("2015550101", 101, engaged = 1, opted_in = 1, completed = 1,
                carrier = "AT&T", disposition_date = "2026-01-10"),
    .record_row("2015550102", 101, engaged = 1, carrier = "AT&T",
                disposition_date = "2026-01-10"),
    .record_row("2015550103", 101, carrier = "Verizon",
                disposition_date = "2026-01-11")
  ))
  res <- disposition_funnel(path, grain = "all")
  expect_equal(res$carrier, c("AT&T", "Verizon"))
  expect_equal(res$n_sent, c(2L, 1L))
  expect_equal(res$n_engaged, c(2L, 0L))
  expect_equal(res$n_completed, c(1L, 0L))
  # .record_row carries no refused/ineligible -> defaulted to 0
  expect_equal(res$n_refused, c(0L, 0L))
})

test_that("empty input returns a typed zero-row frame with the schema", {
  res <- disposition_funnel(.funnel_records()[0, , drop = FALSE], grain = "day")
  expect_equal(nrow(res), 0L)
  expect_named(res, c("carrier", "disposition_date", "n_sent", "n_engaged",
                      "n_opted_in", "n_completed", "n_ineligible", "n_refused",
                      "pct_engaged", "pct_opted_in", "pct_completed"))
  expect_type(res$n_sent, "integer")
  expect_type(res$pct_completed, "double")
})

test_that("missing a required column errors", {
  recs <- .funnel_records()
  recs$opted_in <- NULL
  expect_error(disposition_funnel(recs, grain = "all"),
               "missing required column")
})

test_that("grain='day' errors when disposition_date is absent", {
  recs <- .funnel_records()
  recs$disposition_date <- NULL
  expect_error(disposition_funnel(recs, grain = "day"),
               "disposition_date")
})

test_that("bad `by` errors", {
  expect_error(disposition_funnel(.funnel_records(), by = character(0)),
               "non-empty character")
  expect_error(disposition_funnel(.funnel_records(), by = NA_character_),
               "non-empty character")
  expect_error(disposition_funnel(.funnel_records(), by = 1L),
               "non-empty character")
})

test_that("bad `x` errors", {
  expect_error(disposition_funnel(42), "Parquet path")
  expect_error(disposition_funnel(c("a", "b")), "Parquet path")
})
