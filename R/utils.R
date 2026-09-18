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

# Reverse a single maximal run of Latin-1-supplement code points (U+0080..U+00FF)
# byte by byte: greedily consume the longest valid UTF-8 sequence from the run's
# Latin-1 bytes (that is a doubled character, so emit its decoded form), and when
# the next byte starts no valid sequence (a legitimately single-encoded Latin-1
# character), keep that one character unchanged. Only reached for runs the fast
# vectorized path could not reverse whole -- e.g. a doubled character directly
# adjacent to a genuine accented character.
.repair_run_partial <- function(run) {
  bytes <- charToRaw(iconv(run, from = "UTF-8", to = "latin1"))
  n <- length(bytes)
  out <- character(n)
  filled <- 0L
  i <- 1L
  while (i <= n) {
    took <- 1L
    maxk <- min(4L, n - i + 1L)
    if (maxk >= 2L) {
      for (k in maxk:2L) {
        cand <- rawToChar(bytes[i:(i + k - 1L)])
        Encoding(cand) <- "UTF-8"
        if (validUTF8(cand)) {
          took <- k
          break
        }
      }
    }
    piece <- rawToChar(bytes[i:(i + took - 1L)])
    Encoding(piece) <- if (took == 1L) "latin1" else "UTF-8"
    filled <- filled + 1L
    out[filled] <- enc2utf8(piece)
    i <- i + took
  }
  paste0(out[seq_len(filled)], collapse = "")
}

# Reverse the doubling within each maximal run of Latin-1-supplement code points
# (U+0080..U+00FF): those are the only code points a Latin-1 mis-decode of UTF-8
# bytes can produce, so every other character (a genuine multi-byte character
# that was never doubled, or ASCII) is left byte-for-byte unchanged. The common
# case -- a run that is entirely a doubled sequence -- is decoded for the whole
# column in one vectorized iconv call; the rare run that mixes doubled and clean
# Latin-1 characters falls back to the byte-by-byte .repair_run_partial. A run
# that decodes to no valid UTF-8 (a lone single-encoded accent) is kept as-is,
# which makes the repair idempotent: a reversed value leaves the U+0080..U+00FF
# band, so a second pass matches nothing.
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
  if (!all(reversible)) {
    flat[!reversible] <- vapply(flat[!reversible], .repair_run_partial, character(1), USE.NAMES = FALSE)
  }
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
#' The reversal is applied only to Latin-1-supplement code points
#' (U+0080..U+00FF) -- the only thing a Latin-1 mis-decode can produce -- and only
#' where the bytes re-read as valid UTF-8, so text that was never doubled (a
#' genuine en dash, an emoji, an accented name that is correctly single-encoded)
#' is left byte-for-byte unchanged, even where such a character sits directly
#' beside a doubled one. A clean input is therefore returned unchanged, and the
#' operation is idempotent. Only character columns of a data frame are inspected;
#' factor and other columns are left unchanged (read the export with
#' \code{stringsAsFactors = FALSE}, the R 4.x default, so sample columns are
#' character). Unless \code{quiet = TRUE}, a one-line summary is emitted with
#' \code{message()}, so neither a dry run nor a repair is silent.
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
    chr_cols <- names(x)[vapply(x, is.character, logical(1))]
    counts <- integer(length(chr_cols))
    for (i in seq_along(chr_cols)) {
      col <- chr_cols[[i]]
      repaired <- .fix_double_utf8_chr(x[[col]]) # explicit column access -> data.frame/data.table safe
      counts[[i]] <- .count_repaired(x[[col]], repaired)
      if (apply) {
        x[[col]] <- repaired
      }
    }
    if (!quiet) .log_repair(counts, chr_cols, apply)
    return(x)
  }
  if (!is.character(x)) {
    stop_s160("`x` must be a character vector or a data frame.", fn = "utils_fix_double_utf8")
  }
  out <- .fix_double_utf8_chr(x)
  if (!quiet) .log_repair(.count_repaired(x, out), NULL, apply)
  if (apply) out else x
}
