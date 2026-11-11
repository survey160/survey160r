test_that("every date bound is read the same way", {
  expect_null(.day_bound(NULL, "date_from"))
  expect_equal(.day_bound("2026-03-01", "date_from"), as.Date("2026-03-01"))
  expect_equal(.day_bound(as.Date("2026-03-01"), "date_from"), as.Date("2026-03-01"))
  # a date-time is its day in Eastern time
  expect_equal(.day_bound(as.POSIXct("2026-03-01 02:00", tz = "UTC"), "date_from"),
               as.Date("2026-02-28"))
  expect_equal(.day_bound(as.POSIXlt("2026-03-01 12:00", tz = "UTC"), "date_from"),
               as.Date("2026-03-01"))
  for (bad in list("2026/03/01", "03/01/2026", "2026-03-01junk", "2026-02-30", 20260301,
                   c("2026-01-01", "2026-02-01"), NA_character_, TRUE)) {
    expect_error(.day_bound(bad, "date_from", "disposition_summary"),
                 "`date_from` must be a single date", class = "s160_error")
  }
})

test_that("a reversed range is an error, an open or equal one is not", {
  d <- as.Date("2026-03-01")
  expect_error(.check_day_range(d, d - 1, "disposition_steps"),
               "`date_from` \\(2026-03-01\\) is after `date_to` \\(2026-02-28\\)", class = "s160_error")
  expect_null(.check_day_range(d, d))
  expect_null(.check_day_range(NULL, d))
  expect_null(.check_day_range(d, NULL))
})
