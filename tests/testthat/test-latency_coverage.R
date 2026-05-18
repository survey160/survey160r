# Coverage for defensive branches that the main test suites don't naturally
# reach: short-circuit returns, helper guards, and error paths. These tests
# call package internals directly with `:::` to keep the public-surface test
# files focused on user-observable behavior.

# --- apply_population_filter -----------------------------------------------

test_that("apply_population_filter returns data unchanged when expr is NULL or blank", {
  d <- data.frame(x = 1:3)
  expect_equal(survey160r:::apply_population_filter(d, NULL), d)
  expect_equal(survey160r:::apply_population_filter(d, ""), d)
})

test_that("apply_population_filter errors when expression evaluation fails", {
  d <- data.frame(x = 1:3)
  # `nope` is not a column on `d`; eval() raises an object-not-found error.
  expect_error(
    survey160r:::apply_population_filter(d, "nope > 0"),
    "filters.population evaluation failed"
  )
})

test_that("apply_population_filter errors when expr returns non-logical", {
  d <- data.frame(x = 1:3)
  expect_error(
    survey160r:::apply_population_filter(d, "1 + 1"),
    "logical vector"
  )
})

# --- dedupe_keep_rows / date_filter_keep_rows ------------------------------

test_that("dedupe_keep_rows errors when respondent_id_column is missing", {
  d <- data.frame(x = 1:2)
  expect_error(
    survey160r:::dedupe_keep_rows(d, "userid"),
    "respondent_id_column not found"
  )
})

test_that("dedupe_keep_rows returns all rows when id.intro.scriptDate absent", {
  d <- data.frame(userid = c("a", "b", "a"))
  expect_equal(survey160r:::dedupe_keep_rows(d, "userid"), 1:3)
})

test_that("date_filter_keep_rows returns all rows when id.intro.scriptDate absent", {
  d <- data.frame(x = 1:3)
  expect_equal(
    survey160r:::date_filter_keep_rows(d, "2026-01-26", "UTC"),
    1:3
  )
})

# --- segment_parse_fail_mask -----------------------------------------------

test_that("segment_parse_fail_mask returns all-FALSE when mask list is NULL", {
  out <- survey160r:::segment_parse_fail_mask(NULL, "a", "b", 4)
  expect_equal(out, rep(FALSE, 4))
})

test_that("segment_parse_fail_mask substitutes FALSE for missing columns", {
  mask <- list(a = c(TRUE, FALSE, FALSE))
  # Column "b" is absent from the mask -- helper should treat it as all-FALSE
  # rather than erroring.
  out <- survey160r:::segment_parse_fail_mask(mask, "a", "b", 3)
  expect_equal(out, c(TRUE, FALSE, FALSE))
  out2 <- survey160r:::segment_parse_fail_mask(mask, "b", "a", 3)
  expect_equal(out2, c(TRUE, FALSE, FALSE))
})

# --- validate_flow_order ---------------------------------------------------

test_that("validate_flow_order short-circuits for single-question flows", {
  expect_invisible(
    survey160r:::validate_flow_order(list(flow = list(questions = "intro")),
                                     data.frame())
  )
})

test_that("validate_flow_order skips segments with no comparable rows", {
  # Segment intro->q1 has comparable rows (clean); segment q1->q2 has all-NA
  # so it should `next` and not contribute to the ratio.
  cfg <- list(flow = list(questions = c("intro", "q1", "q2")))
  d <- data.frame(
    id.intro.batchDate = "2026-01-26 21:00:00.000000Z",
    id.q1.scriptDate = "2026-01-26 21:00:30.000000Z",
    id.q1.batchDate = NA_character_,
    id.q2.scriptDate = NA_character_,
    stringsAsFactors = FALSE
  )
  expect_invisible(survey160r:::validate_flow_order(cfg, d))
})

test_that("validate_flow_order short-circuits when no rows have parseable pairs", {
  cfg <- list(flow = list(questions = c("intro", "q1")))
  d <- data.frame(
    id.intro.batchDate = NA_character_,
    id.q1.scriptDate = NA_character_,
    stringsAsFactors = FALSE
  )
  expect_invisible(survey160r:::validate_flow_order(cfg, d))
})

# --- s160_gcs_pull_csv filename override -----------------------------------

test_that("s160_gcs_pull_csv honors a caller-supplied filename", {
  stub_gcs_base()
  # Stub the download so it writes a file with the OVERRIDE filename instead
  # of the auto-derived one. The hash attribute should still be populated.
  local_mocked_bindings(
    gcs_get_object = function(object_name, saveToDisk, ...) { # nolint
      writeLines(c("a,b", "1,2"), saveToDisk)
      TRUE
    },
    gcs_list_objects = function(prefix = NULL, ...) {
      tmp <- tempfile()
      writeLines(c("a,b", "1,2"), tmp)
      sz <- file.info(tmp)$size
      unlink(tmp)
      data.frame(name = "1980/custom_export.csv",
                 size = sz, stringsAsFactors = FALSE)
    }
  )
  data <- suppressMessages(
    s160_gcs_pull_csv(1980, filename = "custom_export.csv")
  )
  expect_true(grepl("^sha256:", attr(data, "source_csv_hash")))
})

test_that("s160_gcs_pull_csv derives the default filename from campaign_id", {
  stub_gcs_base()
  # Without an explicit filename, the helper falls back to
  # `<campaign_id>_raw_data_download.csv` for both the hash lookup and the
  # canonical source_csv_path attribute.
  local_mocked_bindings(
    gcs_get_object = function(object_name, saveToDisk, ...) { # nolint
      writeLines(c("a,b", "1,2"), saveToDisk)
      TRUE
    },
    gcs_list_objects = function(prefix = NULL, ...) {
      tmp <- tempfile()
      writeLines(c("a,b", "1,2"), tmp)
      sz <- file.info(tmp)$size
      unlink(tmp)
      data.frame(name = "1980/1980_raw_data_download.csv",
                 size = sz, stringsAsFactors = FALSE)
    }
  )
  data <- suppressMessages(s160_gcs_pull_csv(1980))
  expect_true(grepl("^sha256:", attr(data, "source_csv_hash")))
  expect_equal(
    attr(data, "source_csv_path"),
    "gs://test_bucket/1980/1980_raw_data_download.csv"
  )
})
