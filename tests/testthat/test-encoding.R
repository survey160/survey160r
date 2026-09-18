# Coverage for R/encoding.R: fix_double_utf8().

# Byte helpers keep the intent legible: build a mojibake value from the exact
# bytes a Latin-1 mis-decode produces, and assert on the exact repaired bytes.
utf8 <- function(bytes) {
  s <- rawToChar(as.raw(bytes))
  Encoding(s) <- "UTF-8"
  s
}
bytes_of <- function(s) as.integer(charToRaw(enc2utf8(s)))

# Corruption signatures observed in real exports (Latin-1 intermediate):
MOJI_ENDASH <- utf8(c(0xC3, 0xA2, 0xC2, 0x80, 0xC2, 0x93)) # -> en dash  (E2 80 93)
MOJI_TM     <- utf8(c(0xC3, 0xA2, 0xC2, 0x84, 0xC2, 0xA2)) # -> (TM)     (E2 84 A2)
MOJI_EACUTE <- utf8(c(0xC3, 0x83, 0xC2, 0xA9))             # -> e-acute  (C3 A9)
CLEAN_ENDASH <- "–"
CLEAN_EMOJI  <- "\U0001f44d"

test_that("reverses a doubly-encoded en dash to clean UTF-8 bytes", {
  out <- fix_double_utf8(MOJI_ENDASH)
  expect_equal(bytes_of(out), c(0xE2, 0x80, 0x93))
  expect_equal(out, CLEAN_ENDASH)
})

test_that("reverses other real signatures (trademark, accented letter)", {
  expect_equal(fix_double_utf8(MOJI_TM), "™")
  expect_equal(fix_double_utf8(MOJI_EACUTE), "é")
})

test_that("leaves already-clean text unchanged (en dash, emoji, ASCII)", {
  expect_equal(fix_double_utf8(CLEAN_ENDASH), CLEAN_ENDASH)
  expect_equal(fix_double_utf8(CLEAN_EMOJI), CLEAN_EMOJI)
  expect_equal(fix_double_utf8("Control"), "Control")
})

test_that("does not damage legitimately single-encoded Latin-1 text", {
  # "cafe-acute" is a valid single-encoded U+00E9; its lone high byte is not
  # valid UTF-8 when re-read, so the guard keeps it as-is.
  cafe <- "café"
  expect_equal(fix_double_utf8(cafe), cafe)
})

test_that("repairs mojibake mixed with a clean multi-byte character in one value", {
  mixed <- paste0(MOJI_EACUTE, " ", CLEAN_ENDASH, " x") # "A(c) - x" doubled + clean en dash
  expect_equal(fix_double_utf8(mixed), paste0("é ", CLEAN_ENDASH, " x"))
})

test_that("is idempotent -- a second pass changes nothing", {
  once <- fix_double_utf8(MOJI_ENDASH)
  expect_equal(fix_double_utf8(once), once)
})

test_that("is vectorized and preserves NA and empty strings", {
  vec <- c(MOJI_ENDASH, NA_character_, "", "café")
  out <- fix_double_utf8(vec)
  expect_equal(length(out), 4L)
  expect_equal(out[[1]], CLEAN_ENDASH)
  expect_true(is.na(out[[2]]))
  expect_equal(out[[3]], "")
  expect_equal(out[[4]], "café")
})

test_that("repairs only the character columns of a data frame", {
  df <- data.frame(
    TREATMENT = MOJI_ENDASH,
    complete = 1L,
    stringsAsFactors = FALSE
  )
  out <- fix_double_utf8(df)
  expect_equal(out$TREATMENT, CLEAN_ENDASH)
  expect_equal(out$complete, 1L)     # numeric column untouched
  expect_s3_class(out, "data.frame")
})

test_that("rejects input that is neither a character vector nor a data frame", {
  expect_error(fix_double_utf8(1:5),
               "must be a character vector or a data frame")
})
