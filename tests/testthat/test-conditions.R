# Standardized errors (R/conditions.R): classes, prefixes, and the value
# description every "must be X, not <value>" message ends with.

test_that("stop_s160 raises a classed s160_error with the fn prefix", {
  err <- tryCatch(survey160r:::stop_s160("boom.", fn = "f"), error = identity)
  expect_s3_class(err, "s160_error")
  expect_equal(conditionMessage(err), "f: boom.")
  expect_null(conditionCall(err))

  err <- tryCatch(survey160r:::stop_s160("boom.", subclass = "s160_custom"),
                  error = identity)
  expect_s3_class(err, c("s160_custom", "s160_error"))
  expect_equal(conditionMessage(err), "boom.")
})

test_that("stop_not_found and stop_http_error carry their subclass", {
  err <- tryCatch(survey160r:::stop_not_found("file", "x.csv", fn = "f"),
                  error = identity)
  expect_s3_class(err, c("s160_not_found", "s160_error"))
  expect_equal(conditionMessage(err), "f: file not found: x.csv")

  err <- tryCatch(survey160r:::stop_http_error(503, "down"), error = identity)
  expect_s3_class(err, c("s160_http_error", "s160_error"))
  expect_identical(err$status, 503L)
})

test_that(".describe_value renders scalars literally and the rest by shape", {
  d <- survey160r:::.describe_value
  expect_equal(d(NULL), "NULL")
  expect_equal(d(NA), "NA")
  expect_equal(d(NA_character_), "NA")
  expect_equal(d("abc"), "\"abc\"")
  expect_equal(d(""), "\"\"")
  expect_equal(d(strrep("x", 50)), paste0("\"", strrep("x", 37), "...\""))
  expect_equal(d(3.5), "3.5")
  expect_equal(d(TRUE), "TRUE")
  expect_equal(d(c("a", "b")), "a character vector of length 2")
  expect_equal(d(1:3), "a integer vector of length 3")
  expect_equal(d(character(0)), "an empty character vector")
  expect_equal(d(list()), "an empty list")
  expect_equal(d(list(1, 2)), "a list of length 2")
  expect_equal(d(data.frame(a = 1:2, b = 3:4)), "a data frame (2 x 2)")
  expect_equal(d(mean), "a function")
  expect_equal(d(new.env()), "an object of class <environment>")
  expect_equal(survey160r:::.not_got(2L), ", not 2")
})

test_that("the check_* validators say what they received", {
  expect_error(survey160r:::check_nonempty_string(c("a", "b"), "x", fn = "f"),
               "f: `x` must be a non-empty string, not a character vector of length 2.",
               fixed = TRUE)
  expect_error(survey160r:::check_nonempty_string("  ", "x"),
               "`x` must be a non-empty string, not \"  \".", fixed = TRUE)
  expect_error(survey160r:::check_positive_number(-1, "timeout", fn = "f"),
               "f: `timeout` must be a positive number, not -1.", fixed = TRUE)
  expect_error(survey160r:::check_positive_number(Inf, "timeout"),
               "`timeout` must be a positive number, not Inf.", fixed = TRUE)
  expect_error(survey160r:::check_data_frame(list(), "data", fn = "f"),
               "f: `data` must be a data frame, not an empty list.", fixed = TRUE)
  expect_error(survey160r:::.require_single_logical(NA, "refresh", "f"),
               "f: `refresh` must be a single TRUE or FALSE, not NA.", fixed = TRUE)
  expect_error(survey160r:::.require_single_logical("yes", "refresh", "f"),
               "f: `refresh` must be a single TRUE or FALSE, not \"yes\".", fixed = TRUE)
})

test_that(".preview_values shows the first few values and counts the rest", {
  p <- survey160r:::.preview_values
  expect_equal(p(character(0)), "none")
  expect_equal(p(c("a", "b")), "a, b")
  expect_equal(p(letters[1:7]), "a, b, c, d, e, ... (+2 more)")
  expect_equal(p(1:3, n = 2L), "1, 2, ... (+1 more)")
})

test_that("stop_not_found appends a hint and .path_hint fires only for relative paths", {
  expect_error(survey160r:::stop_not_found("file", "x.csv", hint = "Try y."),
               "file not found: x.csv. Try y.", fixed = TRUE)
  expect_null(survey160r:::.path_hint("/abs/x.csv"))
  expect_null(survey160r:::.path_hint("~/x.csv"))
  expect_null(survey160r:::.path_hint("C:/x.csv"))
  expect_equal(survey160r:::.path_hint("data/x.csv"),
               sprintf("The path is relative to the working directory, %s.", getwd()))
  expect_error(s160_read_csv("nope.csv"),
               "s160_read_csv: file not found: nope.csv. The path is relative to the working directory, ",
               fixed = TRUE)
})

test_that("stop_not_initialized is classed and carries the hint", {
  err <- tryCatch(survey160r:::stop_not_initialized("GCS", "s160_gcs_init", hint = "Then retry."),
                  error = identity)
  expect_s3_class(err, c("s160_not_initialized", "s160_error"))
  expect_equal(conditionMessage(err),
               "GCS not initialized. Run s160_gcs_init() first. Then retry.")
})
