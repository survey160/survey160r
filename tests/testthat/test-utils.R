# Coverage for R/utils.R: utils_fix_double_utf8() (dry-run by default, apply to fix).

# Byte helpers keep the intent legible and the source pure ASCII: build a value
# from the exact bytes involved, and assert on the exact bytes back.
utf8 <- function(bytes) {
  s <- rawToChar(as.raw(bytes))
  Encoding(s) <- "UTF-8"
  s
}
bytes_of <- function(s) as.integer(charToRaw(enc2utf8(s)))
fix <- function(x, ...) suppressMessages(utils_fix_double_utf8(x, apply = TRUE, ...))

# Corruption signatures observed in real exports (Latin-1 intermediate):
MOJI_ENDASH <- utf8(c(0xC3, 0xA2, 0xC2, 0x80, 0xC2, 0x93)) # -> en dash  (E2 80 93)
MOJI_TM     <- utf8(c(0xC3, 0xA2, 0xC2, 0x84, 0xC2, 0xA2)) # -> trademark (E2 84 A2)
MOJI_EACUTE <- utf8(c(0xC3, 0x83, 0xC2, 0xA9))             # -> e-acute  (C3 A9)
CLEAN_ENDASH <- utf8(c(0xE2, 0x80, 0x93))                  # a genuine en dash
CLEAN_EACUTE <- utf8(c(0xC3, 0xA9))                        # single-encoded e-acute
CLEAN_EMOJI  <- utf8(c(0xF0, 0x9F, 0x91, 0x8D))            # thumbs-up

# --- dry run is the default --------------------------------------------------

test_that("dry run is the default: the input is returned unchanged", {
  expect_identical(suppressMessages(utils_fix_double_utf8(MOJI_ENDASH)), MOJI_ENDASH)
  df <- data.frame(TREATMENT = MOJI_ENDASH, complete = 1L, stringsAsFactors = FALSE)
  expect_identical(suppressMessages(utils_fix_double_utf8(df)), df)
})

# --- repair (apply = TRUE) ---------------------------------------------------

test_that("reverses a doubly-encoded en dash to clean UTF-8 bytes", {
  out <- fix(MOJI_ENDASH)
  expect_equal(bytes_of(out), c(0xE2, 0x80, 0x93))
  expect_equal(out, CLEAN_ENDASH)
})

test_that("reverses other real signatures (trademark, accented letter)", {
  expect_equal(fix(MOJI_TM), utf8(c(0xE2, 0x84, 0xA2)))
  expect_equal(fix(MOJI_EACUTE), CLEAN_EACUTE)
})

test_that("leaves already-clean text unchanged (en dash, emoji, ASCII)", {
  expect_equal(fix(CLEAN_ENDASH), CLEAN_ENDASH)
  expect_equal(fix(CLEAN_EMOJI), CLEAN_EMOJI)
  expect_equal(fix("Control"), "Control")
})

test_that("a fully clean vector is a strict, byte-identical no-op", {
  clean <- c("Democrat", "Republican", "No opinion", CLEAN_EACUTE, CLEAN_ENDASH, CLEAN_EMOJI)
  expect_identical(fix(clean), clean)
})

test_that("does not damage legitimately single-encoded Latin-1 text", {
  cafe <- utf8(c(0x63, 0x61, 0x66, 0xC3, 0xA9)) # "cafe-acute", single-encoded
  expect_equal(fix(cafe), cafe)
})

test_that("repairs mojibake mixed with a clean multi-byte character in one value", {
  mixed <- paste0(MOJI_EACUTE, " ", CLEAN_ENDASH, " x")
  expect_equal(fix(mixed), paste0(CLEAN_EACUTE, " ", CLEAN_ENDASH, " x"))
})

test_that("repairs a doubled character directly adjacent to a genuine accented one", {
  # "caf" + single-encoded e-acute + doubled e-acute, no separator: one run that
  # is not valid UTF-8 whole, so sub-runs are repaired independently.
  adj      <- utf8(c(0x63, 0x61, 0x66, 0xC3, 0xA9, 0xC3, 0x83, 0xC2, 0xA9))
  expected <- utf8(c(0x63, 0x61, 0x66, 0xC3, 0xA9, 0xC3, 0xA9)) # "cafe-acute" + repaired e-acute
  expect_equal(fix(adj), expected)
})

test_that("keeps a trailing lone high byte after a doubled run (sub-run tail)", {
  # doubled e-acute followed by a single-encoded e-acute: the tail byte starts no
  # valid sequence and is kept.
  run      <- utf8(c(0xC3, 0x83, 0xC2, 0xA9, 0xC3, 0xA9))
  expected <- utf8(c(0xC3, 0xA9, 0xC3, 0xA9)) # repaired e-acute + kept e-acute
  expect_equal(fix(run), expected)
})

test_that("is idempotent -- a second pass changes nothing", {
  once <- fix(MOJI_ENDASH)
  expect_equal(fix(once), once)
})

test_that("is vectorized and preserves NA, empty strings, and names", {
  vec <- c(a = MOJI_ENDASH, b = NA_character_, c = "", d = CLEAN_EACUTE)
  out <- fix(vec)
  expect_equal(length(out), 4L)
  expect_equal(names(out), c("a", "b", "c", "d"))
  expect_equal(out[["a"]], CLEAN_ENDASH)
  expect_true(is.na(out[["b"]]))
  expect_equal(out[["c"]], "")
  expect_equal(out[["d"]], CLEAN_EACUTE)
})

test_that("repairs only the character columns of a data frame", {
  df <- data.frame(TREATMENT = MOJI_ENDASH, complete = 1L, stringsAsFactors = FALSE)
  out <- fix(df)
  expect_equal(out$TREATMENT, CLEAN_ENDASH)
  expect_equal(out$complete, 1L)
  expect_s3_class(out, "data.frame")
})

test_that("handles a data.table (what the survey160r readers return)", {
  # Three rows so a column count (2) can never be mistaken for a row index --
  # guards the explicit-column-access path against `x[is_chr]` row semantics.
  dt <- data.table::data.table(TREATMENT = rep(MOJI_ENDASH, 3L), complete = c(1L, 0L, 1L))
  out <- fix(dt)
  expect_s3_class(out, "data.table")
  expect_equal(out$TREATMENT, rep(CLEAN_ENDASH, 3L))
  expect_equal(out$complete, c(1L, 0L, 1L))
})

test_that("repairs every character column even under duplicate column names", {
  df <- data.frame(DUP = MOJI_ENDASH, DUP = MOJI_ENDASH, n = 1L,
                   check.names = FALSE, stringsAsFactors = FALSE)
  out <- fix(df)
  expect_equal(out[[1]], CLEAN_ENDASH)
  expect_equal(out[[2]], CLEAN_ENDASH) # positional access, not by name
  expect_equal(out[[3]], 1L)
})

test_that("leaves non-character (factor) columns unchanged, as documented", {
  df <- data.frame(TREATMENT = MOJI_ENDASH, stringsAsFactors = TRUE)
  out <- fix(df)
  expect_s3_class(out$TREATMENT, "factor")
  expect_equal(as.character(out$TREATMENT), MOJI_ENDASH)
})

test_that("rejects input that is neither a character vector nor a data frame", {
  expect_error(utils_fix_double_utf8(1:5), "must be a character vector or a data frame")
})

# --- logging -----------------------------------------------------------------

test_that("dry run reports what it would repair and how to apply it", {
  expect_message(utils_fix_double_utf8(MOJI_ENDASH),
                 "found 1 double-encoded value.*apply = TRUE")
})

test_that("apply logs a one-line repair summary for a vector", {
  expect_message(utils_fix_double_utf8(MOJI_ENDASH, apply = TRUE), "repaired 1 value")
})

test_that("logs affected column names for a data frame (dry run)", {
  df <- data.frame(TREATMENT = MOJI_ENDASH, note = "ascii", stringsAsFactors = FALSE)
  expect_message(utils_fix_double_utf8(df), "column\\(s\\): TREATMENT")
})

test_that("apply logs affected column names for a data frame", {
  df <- data.frame(TREATMENT = MOJI_ENDASH, note = "ascii", stringsAsFactors = FALSE)
  expect_message(utils_fix_double_utf8(df, apply = TRUE), "repaired 1 value.*TREATMENT")
})

test_that("logs that a clean input had nothing to repair", {
  expect_message(utils_fix_double_utf8("Democrat"), "no double-encoded values found")
})

test_that("quiet = TRUE silences the summary", {
  expect_silent(utils_fix_double_utf8(MOJI_ENDASH, apply = TRUE, quiet = TRUE))
})
