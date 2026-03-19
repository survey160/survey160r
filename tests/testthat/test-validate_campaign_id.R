test_that("numeric input returns character", {
  expect_equal(survey160r:::validate_campaign_id(1980), "1980")
})

test_that("character input passes through", {
  expect_equal(survey160r:::validate_campaign_id("1980"), "1980")
})

test_that("factor is coerced to character", {
  expect_equal(survey160r:::validate_campaign_id(factor("1980")), "1980")
})

test_that("NA errors", {
  expect_error(survey160r:::validate_campaign_id(NA), "non-empty scalar")
})

test_that("empty string errors", {
  expect_error(survey160r:::validate_campaign_id(""), "non-empty scalar")
})

test_that("whitespace-only errors", {
  expect_error(survey160r:::validate_campaign_id("  "), "non-empty scalar")
})

test_that("vector input errors", {
  expect_error(survey160r:::validate_campaign_id(c(1001, 1002)), "single value, not a vector")
})
