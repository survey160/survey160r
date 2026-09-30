# Coverage for disposition_funnel() -- the funnel-count + rates rollup over the
# disposition dataset (one row per (phone, campaign_id), contacted-only). The
# disposition analogue of campaign_metrics_summary(): by / rates / percent, a
# campaigns count, rates via funnel_rates(), plus the terminal split
# (n_ineligible / n_refused). In-memory fixtures for the compute paths; a
# nanoparquet fixture (write_disposition_parquet / .record_row, helper-stubs.R)
# for the path input branch.

.FUNNEL_COUNT_COLS <- c("campaigns", "n_sent", "n_engaged", "n_opted_in",
                        "n_completed", "n_ineligible", "n_refused")
.FUNNEL_RATE_COLS <- c("engagement_rate", "opted_in_rate",
                       "opted_in_engaged_rate", "completion_rate")

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

test_that("grain='all' rolls up by carrier: counts, campaigns, and both opt-in rates", {
  res <- disposition_funnel(.funnel_records(), grain = "all")
  expect_named(res, c("carrier", .FUNNEL_COUNT_COLS, .FUNNEL_RATE_COLS))
  # order: AT&T, Verizon, then NA (unknown carrier) last
  expect_equal(res$carrier, c("AT&T", "Verizon", NA))
  expect_equal(res$campaigns, c(2L, 2L, 2L))      # each carrier spans both campaigns
  expect_equal(res$n_sent, c(3L, 3L, 2L))
  expect_equal(res$n_engaged, c(3L, 0L, 1L))
  expect_equal(res$n_opted_in, c(2L, 0L, 1L))
  expect_equal(res$n_completed, c(2L, 0L, 0L))
  expect_equal(res$n_ineligible, c(0L, 1L, 0L))
  expect_equal(res$n_refused, c(1L, 0L, 1L))
  # proportions by default. The two opt-in rates diverge for the NA-carrier group:
  # opted_in/sent = 1/2 = 0.5 vs opted_in/engaged = 1/1 = 1.
  expect_equal(res$opted_in_rate, c(2 / 3, 0, 0.5))
  expect_equal(res$opted_in_engaged_rate[c(1, 3)], c(2 / 3, 1))
  expect_true(is.na(res$opted_in_engaged_rate[2]))   # 0 engaged -> NA (not 0)
  expect_equal(res$engagement_rate, c(1, 0, 0.5))
  expect_equal(res$completion_rate, c(2 / 3, 0, 0))
})

test_that("percent = TRUE scales the rates to 0-100", {
  res <- disposition_funnel(.funnel_records(), grain = "all", percent = TRUE)
  expect_equal(res$opted_in_rate, c(200 / 3, 0, 50))
})

test_that("rates = FALSE returns counts only", {
  res <- disposition_funnel(.funnel_records(), grain = "all", rates = FALSE)
  expect_named(res, c("carrier", .FUNNEL_COUNT_COLS))
  expect_false(any(.FUNNEL_RATE_COLS %in% names(res)))
})

test_that("grain='day' adds disposition_date to the grouping", {
  res <- disposition_funnel(.funnel_records(), grain = "day")
  expect_named(res, c("carrier", "disposition_date", .FUNNEL_COUNT_COLS,
                      .FUNNEL_RATE_COLS))
  expect_equal(res$carrier, c("AT&T", "AT&T", "Verizon", "Verizon", NA, NA))
  expect_equal(res$disposition_date,
               as.Date(c("2026-01-10", "2026-02-01", "2026-01-11",
                         "2026-02-01", "2026-01-11", "2026-02-02")))
  expect_equal(res$n_sent[1], 2L)          # AT&T on 2026-01-10 = recipients 1 + 2
  expect_equal(res$campaigns[1], 1L)       # both on campaign 101
})

test_that("by accepts multiple dimensions", {
  res <- disposition_funnel(.funnel_records(), by = c("campaign_id", "carrier"),
                            grain = "all", rates = FALSE)
  expect_equal(names(res)[1:2], c("campaign_id", "carrier"))
  expect_equal(res$campaign_id, c(101L, 101L, 101L, 102L, 102L, 102L))
  expect_equal(res$carrier, c("AT&T", "Verizon", NA, "AT&T", "Verizon", NA))
  expect_true(all(res$campaigns == 1L))    # each (campaign, carrier) is one campaign
})

test_that("an all-NA completed group returns NA completes + NA completion rate", {
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
  expect_true(is.na(res$completion_rate))
  expect_equal(res$n_engaged, 2L)          # in-channel flags still count
  expect_equal(res$opted_in_rate, 0.5)     # 1 / 2
})

test_that("refused/ineligible default to 0 when absent (pre-0.51.0 projection)", {
  recs <- .funnel_records()
  recs <- recs[, setdiff(names(recs), c("refused", "ineligible"))]
  res <- disposition_funnel(recs, grain = "all", rates = FALSE)
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
  res <- disposition_funnel(path, grain = "all", rates = FALSE)
  expect_equal(res$carrier, c("AT&T", "Verizon"))
  expect_equal(res$campaigns, c(1L, 1L))
  expect_equal(res$n_sent, c(2L, 1L))
  expect_equal(res$n_engaged, c(2L, 0L))
  expect_equal(res$n_completed, c(1L, 0L))
  expect_equal(res$n_refused, c(0L, 0L))   # .record_row carries none -> defaulted
})

test_that("empty input returns a typed zero-row frame with the full schema", {
  res <- disposition_funnel(.funnel_records()[0, , drop = FALSE], grain = "day")
  expect_equal(nrow(res), 0L)
  expect_named(res, c("carrier", "disposition_date", .FUNNEL_COUNT_COLS,
                      .FUNNEL_RATE_COLS))
  expect_type(res$n_sent, "integer")
  expect_type(res$completion_rate, "double")
})

test_that("grouping by a funnel flag still counts the full group (not the key)", {
  # data.table exposes a `by` column as the length-1 group key inside `j`; reading
  # the counts from `.SD` keeps them full-group. Regression for the miscount when
  # the aggregated column is also the grouping column.
  res <- disposition_funnel(.funnel_records(), by = "opted_in", grain = "all",
                            rates = FALSE)
  expect_equal(res$opted_in, c(0L, 1L))
  expect_equal(res$n_sent, c(5L, 3L))
  expect_equal(res$n_opted_in, c(0L, 3L))   # the opted_in==1 group has 3 rows, not 1
  expect_equal(res$n_completed, c(0L, 2L))
  expect_equal(res$campaigns, c(2L, 2L))
})

test_that("grain='day' equals putting disposition_date in `by` (idiom equivalence)", {
  recs <- .funnel_records()
  a <- disposition_funnel(recs, by = "carrier", grain = "day")
  b <- disposition_funnel(recs, by = c("carrier", "disposition_date"), grain = "all")
  expect_equal(a, b)
})

test_that(".disposition_funnel_count keeps NA only when every value is NA", {
  expect_identical(.disposition_funnel_count(c(1L, 0L, NA)), 1L)   # some non-NA -> real sum
  expect_identical(.disposition_funnel_count(c(0L, 0L)), 0L)       # genuine zero
  expect_identical(.disposition_funnel_count(c(NA_integer_, NA_integer_)),
                   NA_integer_)                                     # all NA -> NA, not 0
})

test_that("accepts a data.table input, empty and non-empty (base-frame semantics)", {
  dt <- data.table::as.data.table(.funnel_records())
  res <- disposition_funnel(dt, grain = "all", rates = FALSE)
  expect_false(data.table::is.data.table(res))   # normalised to a base data frame
  expect_equal(res$carrier, c("AT&T", "Verizon", NA))
  expect_equal(res$n_sent, c(3L, 3L, 2L))
  # empty data.table must still return the typed zero-row frame (regression:
  # data[integer(0), group_cols] is a `j` expression on a data.table, not a select)
  res0 <- disposition_funnel(dt[0], grain = "all")
  expect_s3_class(res0, "data.frame")
  expect_equal(nrow(res0), 0L)
  expect_named(res0, c("carrier", .FUNNEL_COUNT_COLS, .FUNNEL_RATE_COLS))
})

test_that("missing a required funnel column errors", {
  recs <- .funnel_records()
  recs$opted_in <- NULL
  expect_error(disposition_funnel(recs, grain = "all"),
               "missing required column")
})

test_that("missing campaign_id errors (needed for the campaigns count)", {
  recs <- .funnel_records()
  recs$campaign_id <- NULL
  expect_error(disposition_funnel(recs, grain = "all"),
               "campaign_id")
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
