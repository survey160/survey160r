# Coverage for R/encoding.R: fix_double_utf8() and has_double_utf8().

# Byte helpers keep the intent legible and the source pure ASCII: build a value
# from the exact bytes involved, and assert on the exact bytes back.
utf8 <- function(bytes) {
  s <- rawToChar(as.raw(bytes))
  Encoding(s) <- "UTF-8"
  s
}
bytes_of <- function(s) as.integer(charToRaw(enc2utf8(s)))

# Corruption signatures observed in real exports (Latin-1 intermediate):
MOJI_ENDASH <- utf8(c(0xC3, 0xA2, 0xC2, 0x80, 0xC2, 0x93)) # -> en dash  (E2 80 93)
MOJI_TM     <- utf8(c(0xC3, 0xA2, 0xC2, 0x84, 0xC2, 0xA2)) # -> trademark (E2 84 A2)
MOJI_EACUTE <- utf8(c(0xC3, 0x83, 0xC2, 0xA9))             # -> e-acute  (C3 A9)
CLEAN_ENDASH <- utf8(c(0xE2, 0x80, 0x93))                  # a genuine en dash
CLEAN_EACUTE <- utf8(c(0xC3, 0xA9))                        # single-encoded e-acute
CLEAN_EMOJI  <- utf8(c(0xF0, 0x9F, 0x91, 0x8D))            # thumbs-up

test_that("reverses a doubly-encoded en dash to clean UTF-8 bytes", {
  out <- suppressMessages(fix_double_utf8(MOJI_ENDASH))
  expect_equal(bytes_of(out), c(0xE2, 0x80, 0x93))
  expect_equal(out, CLEAN_ENDASH)
})

test_that("reverses other real signatures (trademark, accented letter)", {
  expect_equal(suppressMessages(fix_double_utf8(MOJI_TM)), utf8(c(0xE2, 0x84, 0xA2)))
  expect_equal(suppressMessages(fix_double_utf8(MOJI_EACUTE)), CLEAN_EACUTE)
})

test_that("leaves already-clean text unchanged (en dash, emoji, ASCII)", {
  expect_equal(suppressMessages(fix_double_utf8(CLEAN_ENDASH)), CLEAN_ENDASH)
  expect_equal(suppressMessages(fix_double_utf8(CLEAN_EMOJI)), CLEAN_EMOJI)
  expect_equal(suppressMessages(fix_double_utf8("Control")), "Control")
})

test_that("a fully clean vector is a strict, byte-identical no-op", {
  clean <- c("Democrat", "Republican", "No opinion", CLEAN_EACUTE, CLEAN_ENDASH, CLEAN_EMOJI)
  out <- suppressMessages(fix_double_utf8(clean))
  expect_identical(out, clean)
})

test_that("does not damage legitimately single-encoded Latin-1 text", {
  cafe <- utf8(c(0x63, 0x61, 0x66, 0xC3, 0xA9)) # "cafe-acute", single-encoded
  expect_equal(suppressMessages(fix_double_utf8(cafe)), cafe)
})

test_that("repairs mojibake mixed with a clean multi-byte character in one value", {
  mixed <- paste0(MOJI_EACUTE, " ", CLEAN_ENDASH, " x")
  expect_equal(suppressMessages(fix_double_utf8(mixed)),
               paste0(CLEAN_EACUTE, " ", CLEAN_ENDASH, " x"))
})

test_that("a mojibake run abutting a legit high byte is left unchanged, not damaged", {
  # "cafe-acute" immediately followed by a doubled e-acute forms one run whose
  # bytes do not re-read as valid UTF-8, so the run is kept verbatim.
  adj <- utf8(c(0x63, 0x61, 0x66, 0xC3, 0xA9, 0xC3, 0x83, 0xC2, 0xA9))
  expect_identical(suppressMessages(fix_double_utf8(adj)), adj)
})

test_that("is idempotent -- a second pass changes nothing", {
  once <- suppressMessages(fix_double_utf8(MOJI_ENDASH))
  expect_equal(suppressMessages(fix_double_utf8(once)), once)
})

test_that("is vectorized and preserves NA, empty strings, and names", {
  vec <- c(a = MOJI_ENDASH, b = NA_character_, c = "", d = CLEAN_EACUTE)
  out <- suppressMessages(fix_double_utf8(vec))
  expect_equal(length(out), 4L)
  expect_equal(names(out), c("a", "b", "c", "d"))
  expect_equal(out[["a"]], CLEAN_ENDASH)
  expect_true(is.na(out[["b"]]))
  expect_equal(out[["c"]], "")
  expect_equal(out[["d"]], CLEAN_EACUTE)
})

test_that("repairs only the character columns of a data frame", {
  df <- data.frame(TREATMENT = MOJI_ENDASH, complete = 1L, stringsAsFactors = FALSE)
  out <- suppressMessages(fix_double_utf8(df))
  expect_equal(out$TREATMENT, CLEAN_ENDASH)
  expect_equal(out$complete, 1L)   # numeric column untouched
  expect_s3_class(out, "data.frame")
})

test_that("handles a data.table (what the survey160r readers return)", {
  dt <- data.table::data.table(TREATMENT = c(MOJI_ENDASH, MOJI_ENDASH), complete = c(1L, 0L))
  out <- suppressMessages(fix_double_utf8(dt))
  expect_s3_class(out, "data.table")
  expect_equal(out$TREATMENT, c(CLEAN_ENDASH, CLEAN_ENDASH))
  expect_equal(out$complete, c(1L, 0L))
})

test_that("leaves non-character (factor) columns unchanged, as documented", {
  df <- data.frame(TREATMENT = MOJI_ENDASH, stringsAsFactors = TRUE)
  out <- suppressMessages(fix_double_utf8(df))
  expect_s3_class(out$TREATMENT, "factor")
  expect_equal(as.character(out$TREATMENT), MOJI_ENDASH) # factor level untouched
})

test_that("rejects input that is neither a character vector nor a data frame", {
  expect_error(fix_double_utf8(1:5), "must be a character vector or a data frame")
})

# --- logging -----------------------------------------------------------------

test_that("logs a one-line repair summary for a vector", {
  expect_message(fix_double_utf8(MOJI_ENDASH), "repaired 1 value")
})

test_that("logs that a clean input had nothing to repair", {
  expect_message(fix_double_utf8("Democrat"), "no double-encoded values found")
})

test_that("logs affected column names for a data frame", {
  df <- data.frame(TREATMENT = MOJI_ENDASH, note = "ascii", stringsAsFactors = FALSE)
  expect_message(fix_double_utf8(df), "column\\(s\\): TREATMENT")
})

test_that("quiet = TRUE silences the summary", {
  expect_silent(fix_double_utf8(MOJI_ENDASH, quiet = TRUE))
})

# --- has_double_utf8 ---------------------------------------------------------

test_that("has_double_utf8 detects corruption without modifying data", {
  expect_true(has_double_utf8(MOJI_ENDASH))
  expect_false(has_double_utf8("Democrat"))
  expect_false(has_double_utf8(CLEAN_ENDASH))
})

test_that("has_double_utf8 scans the character columns of a data frame", {
  dirty <- data.frame(TREATMENT = MOJI_ENDASH, complete = 1L, stringsAsFactors = FALSE)
  clean <- data.frame(TREATMENT = CLEAN_ENDASH, complete = 1L, stringsAsFactors = FALSE)
  expect_true(has_double_utf8(dirty))
  expect_false(has_double_utf8(clean))
})

test_that("has_double_utf8 rejects an unsupported type", {
  expect_error(has_double_utf8(1:5), "must be a character vector or a data frame")
})
