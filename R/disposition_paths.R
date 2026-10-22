# disposition_paths(): one row per phone, its campaigns as a letter path, plus an
# optional sequence-pattern match. Shares the connection, scoping and labels with
# disposition_steps() (R/disposition_steps.R).
#
# The path is built with a WINDOW string_agg over the phone's ordered campaigns,
# keeping the last row, not with GROUP BY phone + an ordered string_agg: on the
# production projection (~85M phones) the grouped form exhausts DuckDB's memory
# even at 8 GB (its ordered aggregate does not spill), while the window form runs
# in ~8 s at a 4 GB limit with identical output (verified on a 1% sample).

# Pattern syntax -> RE2 regex over the letter path.
#
#   step      a label name or its letter, or `*` (any label); alternatives with
#             `|` ("completed|web_complete", "C|W"); a quantifier `+`, `{m}`,
#             `{m,}` or `{m,n}`
#   a > b     b comes later than a (other campaigns may sit between)
#   a >> b    b is the very next campaign after a
#   ^ / $     at the start / end of the pattern: the phone's first / most recent
#             campaign
#
# RE2 (DuckDB's regex engine) has no backreferences, which is why the syntax has
# no "same label again" form; every step compiles to a character class.
.disposition_pattern_compile <- function(pattern, fn) {
  if (!is.character(pattern) || length(pattern) != 1L || is.na(pattern) ||
        !nzchar(trimws(pattern))) {
    stop_s160(sprintf("`pattern` must be a single non-empty string%s.", .not_got(pattern)),
              fn = fn)
  }
  body <- trimws(pattern)
  start <- startsWith(body, "^")
  end <- endsWith(body, "$")
  body <- trimws(sub("\\$$", "", sub("^\\^", "", body)))
  bad <- function(why) {
    stop_s160(sprintf("`pattern` %s: %s. %s", why, deparse(pattern), .disposition_pattern_help()),
              fn = fn)
  }
  if (grepl("[$^]", body)) bad("may use `^` / `$` only at its start / end")
  seps <- regmatches(body, gregexpr(">>|>", body))[[1]]
  steps <- trimws(strsplit(body, ">>|>")[[1]])
  if (length(steps) != length(seps) + 1L || any(!nzchar(steps))) {
    bad("has an empty step (a `>` or `>>` with nothing on one side)")
  }
  regex <- vapply(steps, .disposition_pattern_step, character(1), bad = bad)
  joins <- c(ifelse(seps == ">", ".*", ""), "")
  paste0(if (start) "^", paste0(regex, joins, collapse = ""), if (end) "$")
}

# A function, not a constant: the label codes live in R/disposition_steps.R,
# which collates after this file.
.disposition_pattern_help <- function() {
  paste0(
    "Steps are label names or letters (",
    paste0(names(.DISPOSITION_PATH_CODES), " = ", .DISPOSITION_PATH_CODES, collapse = ", "),
    ") or `*`, joined by `>` (later) or `>>` (next), with optional `|`, `+`, `{m,n}`, ",
    "and `^` / `$` anchors.")
}

# One step ("non_response|undeliverable{3,}") -> its RE2 piece ("[NX]{3,}").
.disposition_pattern_step <- function(step, bad) {
  m <- regmatches(step, regexec("^([A-Za-z_*| ]+?)\\s*(\\+|\\{[0-9]+(?:,[0-9]*)?\\})?$",
                                step, perl = TRUE))[[1]]
  if (length(m) == 0L) bad(sprintf("has a step it cannot read (`%s`)", step))
  tokens <- trimws(strsplit(m[2], "|", fixed = TRUE)[[1]])
  letters <- vapply(tokens, function(tok) {
    if (tok == "*") return(".")
    if (tolower(tok) %in% names(.DISPOSITION_PATH_CODES)) {
      return(.DISPOSITION_PATH_CODES[[tolower(tok)]])
    }
    if (toupper(tok) %in% .DISPOSITION_PATH_CODES) return(toupper(tok))
    bad(sprintf("has an unknown label `%s`", tok))
  }, character(1))
  class <- if ("." %in% letters) {
    "."
  } else {
    paste0("[", paste(intersect(.DISPOSITION_PATH_CODES, letters), collapse = ""), "]")
  }
  quant <- m[3]
  if (nzchar(quant) && quant != "+") {
    bounds <- as.integer(strsplit(gsub("[{}]", "", quant), ",", fixed = TRUE)[[1]])
    if (any(bounds > 1000L, na.rm = TRUE) || (length(bounds) == 2L && bounds[1] > bounds[2])) {
      bad(sprintf("has a repeat `%s` outside 0-1000 or with min > max", quant))
    }
  }
  paste0(class, quant)
}

# Collapse runs in a letter path to one letter plus `+` ("NNENN" -> "N+E+N+"):
# the run-length shape, which brings ~26k distinct prod paths down to ~6.6k.
.disposition_shape_sql <- function(expr) {
  for (code in .DISPOSITION_PATH_CODES) {
    expr <- sprintf("regexp_replace(%s, '%s+', '%s+', 'g')", expr, code, code)
  }
  expr
}

#' Read the disposition dataset as one path per phone, and match sequence patterns
#'
#' Returns one row per phone with its campaigns, in time order, as a letter path
#' (\code{"NNEC"}: two non-responses, engaged, completed), the run-length
#' \code{shape} (\code{"N+E+C+"}), and -- with \code{pattern} -- whether and where
#' the path matches a sequence such as
#' \code{"non_response > engaged|opted_in > completed|web_complete"} (no reply,
#' then later engaged or opted in, then later completed). Count shapes, filter
#' matches, or join the result back to a sample to flag numbers by their history.
#' For per-campaign history (what happened \emph{after} a given step) see
#' \code{\link{disposition_steps}}, which shares the ordering, labels, letters and
#' resources documented there.
#'
#' \strong{Pattern syntax.} Steps are label names or their letters --
#' \code{non_response} N, \code{undeliverable} X, \code{engaged} E,
#' \code{opted_in} O, \code{terminated} T, \code{ineligible} I, \code{refused} R,
#' \code{completed} C, \code{web_complete} W -- or \code{*} for any label.
#' \itemize{
#'   \item \code{a > b}: \code{b} comes later than \code{a}; other campaigns may
#'     sit between.
#'   \item \code{a >> b}: \code{b} is the very next campaign.
#'   \item \code{a|b}: either label; \code{a+}, \code{a\{3\}}, \code{a\{3,\}},
#'     \code{a\{2,4\}}: repeats in a row.
#'   \item \code{^} / \code{$} at the start / end: the phone's first / most recent
#'     campaign.
#' }
#' \code{"non_response > completed"} and \code{"N > C"} are the same pattern.
#' \code{match_start} / \code{match_end} locate the first (leftmost, shortest)
#' match.
#'
#' \strong{Date window.} Unlike \code{\link{disposition_steps}}, \code{date_from} /
#' \code{date_to} choose which campaigns form the path (the phone's 2025+ path,
#' say), so they apply before the path is built.
#'
#' @inheritParams disposition_steps
#' @param pattern Optional sequence pattern (see Pattern syntax). Adds
#'   \code{matched}, \code{match_start}, \code{match_end} (step positions in the
#'   path, \code{NA} without a match), \code{match_start_date} and
#'   \code{match_end_date}. Every phone is returned; \code{filter(matched)} keeps
#'   the matches.
#' @param date_from,date_to Optional \code{Date}/date-string bounds on
#'   \code{disposition_date} (inclusive): only campaigns in range form the path.
#' @return A lazy \pkg{dbplyr} table, one row per phone: \code{phone},
#'   \code{n_campaigns}, \code{path}, \code{shape}, \code{first_date},
#'   \code{last_date}, \code{n_unknown_completion} (campaigns whose completion is
#'   never reported -- text-to-web external -- so a \code{completed} step there
#'   cannot match), and with \code{pattern} the match columns.
#' @seealso \code{\link{disposition_steps}} (per-campaign history),
#'   \code{\link{disposition_screen}} (screen a sample on counts)
#' @examples
#' \dontrun{
#' library(dplyr)
#' dataset <- disposition_pull()
#'
#' # The commonest path shapes
#' disposition_paths(dataset) |> count(shape, sort = TRUE) |> head(20) |> collect()
#'
#' # No reply, then engaged or opted in, then completed
#' disposition_paths(dataset, "N > E|O > C|W") |> filter(matched) |> count() |> collect()
#'
#' # Flag a sample's numbers whose last three contacts got no reply
#' flags <- disposition_paths(dataset, "non_response|undeliverable{3,}$",
#'                            phones = my_sample$phone) |>
#'   select(phone, path, matched) |>
#'   collect()
#' }
#' @export
disposition_paths <- function(dataset, pattern = NULL, phones = NULL, sample_frac = NULL,
                              date_from = NULL, date_to = NULL,
                              terminal = c("raw", "engaged"),
                              undeliverable_codes = .DISPOSITION_UNDELIVERABLE_CODES) {
  fn <- "disposition_paths"
  terminal <- match.arg(terminal)
  .disposition_lazy_check(fn)
  pattern_re <- if (!is.null(pattern)) .disposition_pattern_compile(pattern, fn)
  codes <- .disposition_check_terminal_codes(undeliverable_codes, fn)
  sample_frac <- .disposition_check_sample_frac(sample_frac, fn)
  phones <- .disposition_request_phones(phones)

  con <- .disposition_lazy_con()
  source <- .disposition_lazy_source(con, dataset, fn)
  window <- .disposition_after_filters(con, NULL, date_from, date_to, fn)
  base <- .disposition_base_sql(con, source, phones, sample_frac,
                                .disposition_label_sql(con, source$cols, terminal, codes),
                                pre = window)
  # Every column is a running aggregate over the ONE ordered window, read at the
  # phone's last row (where nothing follows): a second, unordered
  # PARTITION BY phone window (count / min / max over the whole phone) costs a
  # second sort -- ~2.5x the time and memory on prod.
  windowed <- paste0(
    "SELECT phone, ",
    "lead(phone) OVER w IS NULL AS s160_last, row_number() OVER w AS n_campaigns, ",
    "string_agg(s160_code, '') OVER w AS path, ",
    "min(disposition_date) OVER w AS first_date, max(disposition_date) OVER w AS last_date, ",
    "sum(CAST(NOT completion_known AS INTEGER)) OVER w AS n_unknown_completion",
    if (!is.null(pattern_re)) ", list(disposition_date) OVER w AS s160_dates",
    " FROM (", base, ") ", .DISPOSITION_SEQUENCE_WINDOW)
  last <- paste0("SELECT * FROM (", windowed, ") WHERE s160_last")
  cols <- c("phone", "n_campaigns", "path", paste(.disposition_shape_sql("path"), "AS shape"),
            "first_date", "last_date", "CAST(n_unknown_completion AS INTEGER) AS n_unknown_completion")
  if (is.null(pattern_re)) {
    sql <- paste0("SELECT ", paste(cols, collapse = ", "), " FROM (", last, ")")
    return(dplyr::tbl(con, dbplyr::sql(sql)))
  }
  re <- function(prefix, suffix = "") {
    DBI::dbQuoteString(con, paste0(prefix, pattern_re, suffix))
  }
  matched <- paste0(
    "SELECT *, ",
    "CASE WHEN matched THEN CAST(length(regexp_extract(path, ", re("^(.*?)"), ", 1)) + 1 AS INTEGER) ",
    "END AS match_start, ",
    "CASE WHEN matched THEN CAST(length(regexp_extract(path, ", re("^(.*?", ")"), ", 1)) AS INTEGER) ",
    "END AS match_end ",
    "FROM (SELECT *, regexp_matches(path, ", DBI::dbQuoteString(con, pattern_re), ") AS matched ",
    "FROM (", last, "))")
  sql <- paste0("SELECT ", paste(c(cols, "matched", "match_start", "match_end",
                                   "s160_dates[match_start] AS match_start_date",
                                   "s160_dates[match_end] AS match_end_date"), collapse = ", "),
                " FROM (", matched, ")")
  dplyr::tbl(con, dbplyr::sql(sql))
}
