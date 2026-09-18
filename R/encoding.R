# Repair double-UTF-8 ("Latin-1 intermediate") mojibake in campaign export text.
#
# When a sample upload reads a UTF-8 file as Latin-1 and re-encodes it to UTF-8,
# every non-ASCII character is doubly encoded: an en dash (UTF-8 bytes
# E2 80 93) is read as three Latin-1 code points (U+00E2 U+0080 U+0093) and
# re-emitted as UTF-8 bytes C3 A2 C2 80 C2 93. The doubled bytes are carried
# verbatim through the export CSV, so a raw campaign export can arrive with
# appended sample columns (treatment labels, employer/title text) mojibaked
# while live SMS answers stay clean. This module reverses that one layer.

# Reverse the doubling within each maximal run of Latin-1-supplement code points
# (U+0080..U+00FF). Those code points are the only thing a Latin-1 mis-decode of
# UTF-8 bytes can produce, so confining the reversal to such runs leaves every
# other character (a genuine multi-byte character that was never doubled, or
# ASCII) byte-for-byte unchanged. A run is reversed only when re-reading its
# bytes as UTF-8 yields valid UTF-8; otherwise it is a legitimately
# single-encoded value (e.g. an accented name) and is kept as-is. This makes the
# repair idempotent -- a reversed value leaves the U+0080..U+00FF band, so a
# second pass matches nothing. All runs across all elements are collapsed and
# re-decoded in a single iconv call, then spliced back, so a whole export column
# is repaired in one vectorized pass rather than element by element.
.fix_double_utf8_chr <- function(x) {
  na <- is.na(x)
  x <- enc2utf8(x)
  matches <- gregexpr("[\u0080-\u00ff]+", x, perl = TRUE)
  runs <- regmatches(x, matches)
  lens <- lengths(runs)
  flat <- unlist(runs, use.names = FALSE)
  if (length(flat) == 0L) {
    return(x)
  }
  decoded <- iconv(flat, from = "UTF-8", to = "latin1") # each run's code points -> its bytes
  Encoding(decoded) <- "UTF-8"                           # reinterpret those bytes as UTF-8
  reversible <- !is.na(decoded) & validUTF8(decoded)
  flat[reversible] <- decoded[reversible]
  runs[lens > 0L] <- unname(split(flat, rep.int(seq_along(runs), lens)))
  regmatches(x, matches) <- runs
  x[na] <- NA_character_ # regmatches<- reconstructs an NA element as "NA"; restore it
  x
}

# Count values that actually changed (content, not just encoding tag). enc2utf8
# only re-tags identical content, and == compares by code point, so a re-tag is
# not counted -- only real repairs are.
.count_repaired <- function(before, after) {
  changed <- before != after
  changed[is.na(changed)] <- FALSE
  sum(changed)
}

# One-line summary of a repair, on the message stream (stderr). `cols` is NULL
# for a vector and the character-column names for a data frame.
.log_repair <- function(counts, cols) {
  total <- sum(counts)
  if (total == 0L) {
    message("fix_double_utf8: no double-encoded values found.")
  } else if (is.null(cols)) {
    message(sprintf("fix_double_utf8: repaired %d value(s).", total))
  } else {
    affected <- cols[counts > 0L]
    message(sprintf(
      "fix_double_utf8: repaired %d value(s) across %d column(s): %s.",
      total, length(affected), paste(affected, collapse = ", ")
    ))
  }
  invisible(NULL)
}

#' Repair double-encoded UTF-8 (mojibake) in export text
#'
#' Reverses a double UTF-8 encoding in which a value's original UTF-8 bytes were
#' mis-read as Latin-1 (ISO-8859-1) and re-encoded to UTF-8 -- so an en dash
#' arrives as three stray Latin-1-supplement characters, a trademark sign as
#' three, an accented letter as two, and so on. This is the corruption a raw
#' Survey160 campaign export can carry in its appended sample columns (treatment
#' labels, vendor-appended employer/title text). Use it on a campaign export
#' frame before analysis or re-export.
#'
#' The reversal is applied per maximal run of Latin-1-supplement code points
#' (U+0080..U+00FF) and only where re-reading the run as UTF-8 is valid, so text
#' that was never doubled -- a genuine en dash, an emoji, an accented name that
#' is correctly single-encoded -- is left byte-for-byte unchanged. A clean input
#' is therefore returned unchanged. The operation is idempotent: running it twice
#' is the same as running it once.
#'
#' Only character columns of a data frame are repaired; factor and other columns
#' are returned unchanged (read the export with \code{stringsAsFactors = FALSE},
#' which is the R 4.x default, so sample columns are character). Unless
#' \code{quiet = TRUE}, a one-line summary of how many values were repaired (and,
#' for a data frame, which columns) is emitted with \code{message()}, so a repair
#' is never silent. Use \code{\link{has_double_utf8}} to check for corruption
#' without modifying the data (for example, to confirm a repair cleared it).
#'
#' Mojibake repair is heuristic. The guard above makes a false repair unlikely,
#' but a value that legitimately contains a Latin-1 run whose bytes happen to be
#' valid UTF-8 would be "repaired" to the character(s) it decodes to. The durable
#' fix is to stop the double encoding at its source (the upload that produced the
#' export); this function repairs exports produced before that fix.
#'
#' @param x A character vector, or a data frame (its character columns are
#'   repaired; other columns are returned unchanged).
#' @param quiet If \code{FALSE} (the default), emit a one-line \code{message()}
#'   summarizing how many values were repaired. Set \code{TRUE} to silence it
#'   (e.g. when calling in a loop).
#' @return \code{x} with double-encoded runs reversed: a character vector of the
#'   same length, or the same data frame with its character columns repaired.
#'   \code{NA} and non-character columns pass through untouched.
#' @seealso \code{\link{has_double_utf8}} to detect corruption without
#'   modifying the data; \code{\link{s160_read_csv}} and
#'   \code{\link{s160_gcs_campaign_results_read}}, which read the raw export
#'   this repairs.
#' @examples
#' # The escaped bytes below are the mojibake form of a single en dash:
#' fix_double_utf8("Treatment Group \u00e2\u0080\u0093 Control")
#'
#' # A whole export frame -- character columns are repaired, others untouched:
#' df <- data.frame(
#'   EMPLOYER = "Acme Corp\u00e2\u0084\u00a2",
#'   complete = 1L,
#'   stringsAsFactors = FALSE
#' )
#' fix_double_utf8(df, quiet = TRUE)
#' @export
fix_double_utf8 <- function(x, quiet = FALSE) {
  if (is.data.frame(x)) {
    is_chr <- vapply(x, is.character, logical(1))
    chr_cols <- names(x)[is_chr]
    repaired <- lapply(x[is_chr], .fix_double_utf8_chr)
    counts <- vapply(
      seq_along(repaired),
      function(i) .count_repaired(x[[chr_cols[i]]], repaired[[i]]),
      integer(1)
    )
    x[is_chr] <- repaired
    if (!quiet) .log_repair(counts, chr_cols)
    return(x)
  }
  if (!is.character(x)) {
    stop_s160("`x` must be a character vector or a data frame.", fn = "fix_double_utf8")
  }
  out <- .fix_double_utf8_chr(x)
  if (!quiet) .log_repair(.count_repaired(x, out), NULL)
  out
}

#' Detect double-encoded UTF-8 (mojibake) in export text
#'
#' Reports whether any value would be changed by \code{\link{fix_double_utf8}},
#' without modifying the data. Use it to verify a raw export before repairing, or
#' to confirm a repaired export is clean (it should then return \code{FALSE}).
#'
#' Like \code{\link{fix_double_utf8}}, it inspects only character columns of a
#' data frame.
#'
#' @param x A character vector or a data frame.
#' @return A single \code{TRUE}/\code{FALSE}: \code{TRUE} if any value is
#'   double-encoded (would be repaired), else \code{FALSE}.
#' @seealso \code{\link{fix_double_utf8}} to perform the repair.
#' @examples
#' has_double_utf8("Treatment Group \u00e2\u0080\u0093 Control") # TRUE
#' has_double_utf8("Democrat")                                            # FALSE
#' @export
has_double_utf8 <- function(x) {
  if (is.data.frame(x)) {
    is_chr <- vapply(x, is.character, logical(1))
    return(any(vapply(x[is_chr], .has_double_utf8_chr, logical(1))))
  }
  if (!is.character(x)) {
    stop_s160("`x` must be a character vector or a data frame.", fn = "has_double_utf8")
  }
  .has_double_utf8_chr(x)
}

.has_double_utf8_chr <- function(x) {
  .count_repaired(x, .fix_double_utf8_chr(x)) > 0L
}
