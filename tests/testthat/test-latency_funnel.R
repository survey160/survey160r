# Coverage for R/latency_funnel.R: the latency_funnel() consolidated reducer.
# Uses the synthetic fixture (3 segments: intro->q1->q2->close, 4 respondents
# across hours 15/16/17 ET on 2026-01-26). Its true funnel is
# sent=4, engaged=3, opted_in=3, completed=3.

.funnel_con <- function() {
  latency_report(load_synthetic_data(), synthetic_config())$consolidated
}

test_that("day grain returns one row per (campaign, date) with true anchors", {
  f <- latency_funnel(.funnel_con())

  expect_identical(
    names(f),
    c("campaign_id", "date", "n_sent", "n_engaged", "n_opted_in",
      "n_completed")
  )
  expect_equal(nrow(f), 1L)
  expect_equal(f$campaign_id, 1L)
  expect_equal(f$date, as.Date("2026-01-26"))
  expect_equal(f$n_sent, 4L)
  expect_equal(f$n_engaged, 3L)
  expect_equal(f$n_opted_in, 3L)
  expect_equal(f$n_completed, 3L)
  # A plain data frame, even though the reduction is column-selection heavy.
  expect_s3_class(f, "data.frame")
  expect_false(data.table::is.data.table(f))
})

test_that("hour grain returns one row per hour, ordered, with hour_local", {
  fh <- latency_funnel(.funnel_con(), grain = "hour")

  expect_true("hour_local" %in% names(fh))
  expect_equal(nrow(fh), 3L)
  expect_equal(fh$hour_local, c(15L, 16L, 17L))
  expect_equal(fh$n_sent, c(1L, 1L, 2L))
})

test_that("hour anchors sum to the day anchors (no grain double-count)", {
  con <- .funnel_con()
  fd <- latency_funnel(con, grain = "day")
  fh <- latency_funnel(con, grain = "hour")

  for (a in c("n_sent", "n_engaged", "n_opted_in", "n_completed")) {
    expect_equal(sum(fh[[a]]), fd[[a]], info = a)
  }
})

test_that("hour == day holds even when a send timestamp is unparseable", {
  # r2 replied and opted in, but its send timestamp is blanked. Anchors are
  # send-anchored, so r2 is excluded from every count and nothing lands in an
  # unknown-hour bucket -- the hour grain must still sum to the day grain
  # (guards the documented invariant against the NA-hour edge a reviewer flagged).
  con <- latency_report(
    load_synthetic_data(mutate = function(d) {
      d$id.intro.scriptDate[d$userid == "r2"] <- ""
      d
    }),
    synthetic_config()
  )$consolidated
  fd <- latency_funnel(con, grain = "day")
  fh <- latency_funnel(con, grain = "hour")

  # r2 dropped from every anchor (baseline was 4 / 3 / 3 / 3).
  expect_equal(fd$n_sent, 3L)
  expect_equal(fd$n_opted_in, 2L)
  for (a in c("n_sent", "n_engaged", "n_opted_in", "n_completed")) {
    expect_equal(sum(fh[[a]]), fd[[a]], info = a)
  }
})

test_that("it collapses the denormalised fan-out instead of summing it", {
  con <- .funnel_con()
  # The raw frame repeats each anchor across (3 segments x 4 thresholds x 2
  # grains); a naive column sum is the trap this accessor exists to avoid.
  expect_equal(sum(con$n_sent), 96L)
  expect_equal(latency_funnel(con)$n_sent, 4L)
})

test_that("a multi-campaign consolidated reduces per campaign", {
  con <- .funnel_con()
  con2 <- con
  con2$campaign_id <- 2L
  con2$n_sent <- con2$n_sent * 10L
  both <- rbind(con, con2)

  f <- latency_funnel(both)
  expect_equal(f$campaign_id, c(1L, 2L))
  expect_equal(f$n_sent, c(4L, 40L))
})

test_that("a data.table input is accepted and coerced", {
  dt <- data.table::as.data.table(.funnel_con())
  f <- latency_funnel(dt)

  expect_false(data.table::is.data.table(f))
  expect_equal(nrow(f), 1L)
  expect_equal(f$n_sent, 4L)
})

test_that("a non-data-frame input is rejected", {
  expect_error(
    latency_funnel(list(consolidated = .funnel_con())),
    "must be a data frame"
  )
})

test_that("a consolidated missing an anchor column is rejected", {
  con <- .funnel_con()
  expect_error(
    latency_funnel(con[, setdiff(names(con), "n_sent"), drop = FALSE]),
    "missing column"
  )
})

test_that("an unknown grain is rejected", {
  expect_error(latency_funnel(.funnel_con(), grain = "week"),
               "should be one of")
})

test_that("an empty consolidated yields an empty, correctly typed funnel", {
  con <- .funnel_con()

  ed <- latency_funnel(con[0, , drop = FALSE])
  expect_equal(nrow(ed), 0L)
  expect_false("hour_local" %in% names(ed))
  expect_s3_class(ed, "data.frame")

  eh <- latency_funnel(con[0, , drop = FALSE], grain = "hour")
  expect_equal(nrow(eh), 0L)
  expect_true("hour_local" %in% names(eh))
})

test_that("a grain with no rows in the frame yields an empty funnel", {
  con <- .funnel_con()
  # Only hour rows present -> a day reduction has nothing to read.
  hour_only <- con[!is.na(con$hour_local), , drop = FALSE]
  expect_equal(nrow(latency_funnel(hour_only, grain = "day")), 0L)

  # Only day-rollup rows present -> an hour reduction has nothing to read.
  day_only <- con[is.na(con$hour_local), , drop = FALSE]
  expect_equal(nrow(latency_funnel(day_only, grain = "hour")), 0L)
})
