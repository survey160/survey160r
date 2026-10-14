# End-to-end checks on a RAW export in the production byte shape (see
# helper-raw-export.R for the corpus profile it reproduces): the readers, the
# header munging, latency_run() and disposition_run() on quoted fields,
# duplicated headers, CRLF, multilingual text, embedded line breaks, numeric
# carrier codes in `status`, and "None" / "NA" tokens.

raw_fixture <- function() test_path("fixtures", "raw_export.csv")

# The read frame without the provenance attributes (hash and path differ per file).
strip_provenance <- function(d) {
  attr(d, "source_csv_hash") <- NULL
  attr(d, "source_csv_path") <- NULL
  d
}

# A timestamp cell that carries a send (fread types an all-valid column POSIXct).
stamped <- function(x) !is.na(x) & nzchar(as.character(x))

# A latency table without the per-file provenance columns.
without_source <- function(x) x[, !startsWith(names(x), "source_csv_"), drop = FALSE]

run_at <- as.POSIXct("2026-10-01", tz = "UTC")
latency <- function(d) {
  latency_run(1980L, d, field_timezone = "America/New_York", run_at = run_at)
}

test_that("the committed raw fixture is the generator's output", {
  # Regenerate with: write_raw_export(synthetic_raw_export(n = 30L, seed = 1L), <path>)
  expect_identical(readLines(raw_fixture(), encoding = "UTF-8"),
                   unlist(strsplit(synthetic_raw_export(n = 30L, seed = 1L), "\n",
                                   fixed = TRUE)))
})

test_that("s160_read_csv reads the raw shape: munged names, repeated headers, typed codes", {
  d <- s160_read_csv(raw_fixture())
  expect_identical(nrow(d), 30L)
  # Bracket headers become dot form; the repeated phone / userid get a suffix.
  expect_true(all(c("id.intro.scriptDate", "id.close.scriptText", "phone.1",
                    "userid.1") %in% names(d)))
  expect_false(anyDuplicated(names(d)) > 0L)
  expect_identical(d$phone, d$phone.1)
  expect_identical(s160_csv_header(raw_fixture()), names(d))
  # Phones stay ten-digit strings; a numeric-only column comes back numeric.
  expect_type(d$phone, "character")
  expect_true(all(grepl("^[0-9]{10}$", d$phone)))
  expect_type(d$campaignid, "integer")
  # `status` mixes words and carrier codes, so it stays character.
  expect_type(d$status, "character")
  expect_true(any(grepl("^[0-9]+$", d$status)) && any(d$status == "terminated"))
  # A quoted "NA" in a character column is text to fread; all-"NA" is NA.
  expect_contains(d$error_code, "NA")
  expect_true(all(is.na(d$deactivationDate)))
})

test_that("multilingual text and an embedded line break survive the read", {
  d <- s160_read_csv(raw_fixture())
  multiline <- which(grepl("\n", d$id.intro.batchText, fixed = TRUE))
  expect_length(multiline, 1L)
  expect_identical(d$id.intro.batchText[[multiline]], "Yes\nplease")
  # It is a real reply: stamped, with the intro's final answer.
  expect_true(stamped(d$id.intro.batchDate[[multiline]]))
  expect_identical(d$id.intro.finalText[[multiline]], "Yes")
  close_text <- d$id.close.scriptText[nzchar(d$id.close.scriptText)][[1L]]
  expect_identical(Encoding(close_text), "UTF-8")
  expect_match(close_text, "回覆", fixed = TRUE)
  expect_match(d$id.q1.scriptText[nzchar(d$id.q1.scriptText)][[1L]], "\"good\"",
               fixed = TRUE)
})

test_that("CRLF and BOM files read identically to the LF original", {
  lines <- synthetic_raw_export(n = 30L, seed = 1L)
  lf <- strip_provenance(s160_read_csv(write_raw_export(lines)))
  crlf <- strip_provenance(s160_read_csv(write_raw_export(lines, eol = "\r\n")))
  expect_identical(crlf, lf)
  bom_path <- withr::local_tempfile(fileext = ".csv")
  writeBin(c(as.raw(c(0xef, 0xbb, 0xbf)), readBin(raw_fixture(), "raw", 1e6)), bom_path)
  expect_identical(strip_provenance(s160_read_csv(bom_path)),
                   strip_provenance(s160_read_csv(raw_fixture())))
  expect_identical(s160_csv_header(bom_path), names(lf))
})

test_that("a header-only export reads as zero rows with the full header", {
  path <- write_raw_export(synthetic_raw_export(n = 30L)[1L])
  d <- s160_read_csv(path)
  expect_identical(nrow(d), 0L)
  expect_identical(names(d), s160_csv_header(raw_fixture()))
})

test_that("latency_run on the raw shape: no parse failures, same result for any read", {
  d <- s160_read_csv(raw_fixture())
  r <- latency(d)
  expect_true(all(unlist(r$diagnostics$parse_failures_per_column) == 0L))
  expect_gt(nrow(r$consolidated), 0L)
  # Read with POSIXct timestamps, or from a CRLF copy: the same latency result.
  p <- latency(s160_read_csv(raw_fixture(), timestamps = "POSIXct"))
  expect_identical(p$consolidated, r$consolidated)
  expect_identical(p$latency_frame, r$latency_frame)
  expect_identical(disposition_run(1980L, s160_read_csv(raw_fixture(), timestamps = "POSIXct")),
                   disposition_run(1980L, d))
  crlf <- write_raw_export(synthetic_raw_export(n = 30L, seed = 1L), eol = "\r\n")
  expect_identical(without_source(latency(s160_read_csv(crlf))$consolidated),
                   without_source(r$consolidated))
})

test_that("latency_run ignores where the non-flow columns sit", {
  # The flow order is read from the id.<q>.* column order (so those keep their
  # relative order); every other column may sit anywhere.
  d <- s160_read_csv(raw_fixture())
  flow <- grep("^id\\.", names(d))
  set.seed(9)
  other <- sample(setdiff(seq_along(d), flow))
  shuffled <- d[, c(other[1:5], flow, other[-(1:5)]), drop = FALSE]
  attributes(shuffled)[c("source_csv_hash", "source_csv_path")] <-
    attributes(d)[c("source_csv_hash", "source_csv_path")]
  a <- latency(d)
  b <- latency(shuffled)
  expect_identical(b$consolidated, a$consolidated)
  expect_identical(b$latency_frame, a$latency_frame)
})

test_that("trimmed fractional seconds (1-6 digits) parse to the exact instant", {
  x <- c("2026-05-13 19:00:32.2Z", "2026-05-13 19:00:32.21Z",
         "2026-05-13 19:00:32.216Z", "2026-05-13 19:00:32.21616Z",
         "2026-05-13 19:00:32.216160Z")
  got <- parse_campaign_timestamps(x)
  expect_null(attr(got, "parse_failed"))
  expect_equal(as.numeric(got) - as.numeric(as.POSIXct("2026-05-13 19:00:32", tz = "UTC")),
               c(0.2, 0.21, 0.216, 0.21616, 0.21616), tolerance = 1e-6)
})

test_that("disposition_run on the raw shape: codes, tokens and terminals", {
  d <- s160_read_csv(raw_fixture())
  out <- disposition_run(1980L, d)$consolidated
  expect_identical(nrow(out), 30L)                     # every row was sent
  expect_setequal(out$phone, d$phone)
  # "None", "" and a literal "NA" are no error; real codes come through as text.
  coded <- grepl("^[0-9]+$", d$error_code)
  expect_identical(out$error[match(d$phone, out$phone)],
                   ifelse(coded, as.character(d$error_code), NA_character_))
  # refused / ineligible follow the terminal questions' send stamps.
  idx <- match(d$phone, out$phone)
  expect_identical(out$refused[idx], as.integer(stamped(d$id.refusal.scriptDate)))
  expect_identical(out$ineligible[idx], as.integer(stamped(d$id.ineligible.scriptDate)))
  expect_identical(out$terminated, as.integer(out$refused | out$ineligible))
  # A survey answer in `carrier` is carried as-is; it is not a carrier lookup.
  expect_setequal(unique(out$carrier), unique(d$carrier))
})

test_that("an export whose phones all fit an integer reads integer and still works", {
  lines <- synthetic_raw_export(n = 12L, seed = 4L)
  # Rewrite every phone (both phone columns) into the 201-214 area-code range,
  # all <= 2147483647, which fread types as integer.
  small <- sprintf("20%08d", 1:12)
  for (i in seq_along(small)) {
    old <- regmatches(lines[[i + 1L]], regexpr("^\"1980\",\"[0-9]{10}\"", lines[[i + 1L]]))
    phone <- sub(".*,\"([0-9]{10})\"$", "\\1", old)
    lines[[i + 1L]] <- gsub(phone, small[[i]], lines[[i + 1L]], fixed = TRUE)
  }
  d <- s160_read_csv(write_raw_export(lines))
  expect_type(d$phone, "integer")
  out <- disposition_run(1980L, d)$consolidated
  expect_type(out$phone, "character")
  expect_setequal(out$phone, small)
  as_text <- d
  as_text$phone <- as.character(as_text$phone)
  expect_identical(disposition_run(1980L, as_text)$consolidated, out)
})

test_that("raw exports the pipeline rejects fail with the documented errors", {
  # Only the opener and terminal steps -> fewer than two flow questions.
  one <- s160_read_csv(write_raw_export(synthetic_raw_export(n = 10L, n_body = 0L)))
  one <- one[, !grepl("^id\\.(close|refusal|ineligible)\\.", names(one)), drop = FALSE]
  expect_error(latency(one), "at least two questions")
})
