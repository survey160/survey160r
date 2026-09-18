# Utilities -- standalone helpers that operate on an in-memory campaign export,
# independent of any Survey160 data source (hence the `utils_` domain rather than
# a data-domain prefix). Pure compute; no I/O.
#
# utils_fix_double_utf8: repair double-UTF-8 ("Latin-1 intermediate") mojibake.
# When a sample upload reads a UTF-8 file as Latin-1 and re-encodes it to UTF-8,
# every non-ASCII character is doubly encoded: an en dash (UTF-8 bytes E2 80 93)
# is read as three Latin-1 code points (U+00E2 U+0080 U+0093) and re-emitted as
# UTF-8 bytes C3 A2 C2 80 C2 93. The doubled bytes are carried verbatim through
# the export CSV, so a raw campaign export can arrive with appended sample
# columns (treatment labels, employer/title text) mojibaked while live SMS
# answers stay clean. This reverses that one layer.

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

# One-line summary on the message stream (stderr). `cols` is NULL for a vector
# and the character-column names for a data frame; `apply` picks the dry-run vs
# applied wording.
.log_repair <- function(counts, cols, apply) {
  total <- sum(counts)
  if (total == 0L) {
    message("utils_fix_double_utf8: no double-encoded values found.")
    return(invisible(NULL))
  }
  where <- if (is.null(cols)) {
    "."
  } else {
    affected <- cols[counts > 0L]
    sprintf(" across %d column(s): %s.", length(affected), paste(affected, collapse = ", "))
  }
  if (apply) {
    message(sprintf("utils_fix_double_utf8: repaired %d value(s)%s", total, where))
  } else {
    message(sprintf(
      "utils_fix_double_utf8: found %d double-encoded value(s)%s Re-run with `apply = TRUE` to repair.",
      total, where
    ))
  }
  invisible(NULL)
}

#' Detect or repair double-encoded UTF-8 (mojibake) in export text
#'
#' Reverses a double UTF-8 encoding in which a value's original UTF-8 bytes were
#' mis-read as Latin-1 (ISO-8859-1) and re-encoded to UTF-8 -- so an en dash
#' arrives as three stray Latin-1-supplement characters, a trademark sign as
#' three, an accented letter as two, and so on. This is the corruption a raw
#' Survey160 campaign export can carry in its appended sample columns (treatment
#' labels, vendor-appended employer/title text).
#'
#' The function verifies by default and repairs on request. With
#' \code{apply = FALSE} (the default) it is a \strong{dry run}: it reports how
#' many values it would repair (and, for a data frame, which columns) and returns
#' \code{x} unchanged. With \code{apply = TRUE} it returns the repaired data. Run
#' it once to see what it would do, then again with \code{apply = TRUE}; or, since
#' the repair is a no-op on clean input, call it with \code{apply = TRUE}
#' directly.
#'
#' The reversal is applied per maximal run of Latin-1-supplement code points
#' (U+0080..U+00FF) and only where re-reading the run as UTF-8 is valid, so text
#' that was never doubled -- a genuine en dash, an emoji, an accented name that is
#' correctly single-encoded -- is left byte-for-byte unchanged. A clean input is
#' therefore returned unchanged, and the operation is idempotent. Only character
#' columns of a data frame are inspected; factor and other columns are left
#' unchanged (read the export with \code{stringsAsFactors = FALSE}, the R 4.x
#' default, so sample columns are character). Unless \code{quiet = TRUE}, a
#' one-line summary is emitted with \code{message()}, so neither a dry run nor a
#' repair is silent.
#'
#' Mojibake repair is heuristic. The guard above makes a false repair unlikely,
#' but a value that legitimately contains a Latin-1 run whose bytes happen to be
#' valid UTF-8 would be "repaired" to the character(s) it decodes to. The durable
#' fix is to stop the double encoding at its source (the upload that produced the
#' export); this function repairs exports produced before that fix.
#'
#' @param x A character vector, or a data frame (its character columns are
#'   inspected; other columns are returned unchanged).
#' @param apply If \code{FALSE} (the default), do a dry run: report what would be
#'   repaired and return \code{x} unchanged. Set \code{TRUE} to return the
#'   repaired data.
#' @param quiet If \code{FALSE} (the default), emit a one-line \code{message()}
#'   summary. Set \code{TRUE} to silence it (e.g. when calling in a loop).
#' @return With \code{apply = TRUE}, \code{x} with double-encoded runs reversed
#'   (a character vector of the same length, or the same data frame with its
#'   character columns repaired). With \code{apply = FALSE}, \code{x} unchanged.
#'   \code{NA} and non-character columns always pass through untouched.
#' @seealso \code{\link{s160_read_csv}} and
#'   \code{\link{s160_gcs_campaign_results_read}}, which read the raw export
#'   this repairs.
#' @examples
#' # The escaped bytes below are the mojibake form of a single en dash.
#' # Dry run (default): reports what it would repair, returns the input unchanged.
#' utils_fix_double_utf8("Treatment Group \u00e2\u0080\u0093 Control")
#'
#' # Apply the repair:
#' utils_fix_double_utf8("Treatment Group \u00e2\u0080\u0093 Control", apply = TRUE)
#'
#' # A whole export frame -- character columns are repaired, others untouched:
#' df <- data.frame(
#'   EMPLOYER = "Acme Corp\u00e2\u0084\u00a2",
#'   complete = 1L,
#'   stringsAsFactors = FALSE
#' )
#' utils_fix_double_utf8(df, apply = TRUE, quiet = TRUE)
#' @export
utils_fix_double_utf8 <- function(x, apply = FALSE, quiet = FALSE) {
  if (is.data.frame(x)) {
    is_chr <- vapply(x, is.character, logical(1))
    chr_cols <- names(x)[is_chr]
    repaired <- lapply(x[is_chr], .fix_double_utf8_chr)
    counts <- vapply(
      seq_along(repaired),
      function(i) .count_repaired(x[[chr_cols[i]]], repaired[[i]]),
      integer(1)
    )
    if (!quiet) .log_repair(counts, chr_cols, apply)
    if (apply) {
      x[is_chr] <- repaired
    }
    return(x)
  }
  if (!is.character(x)) {
    stop_s160("`x` must be a character vector or a data frame.", fn = "utils_fix_double_utf8")
  }
  out <- .fix_double_utf8_chr(x)
  if (!quiet) .log_repair(.count_repaired(x, out), NULL, apply)
  if (apply) out else x
}
