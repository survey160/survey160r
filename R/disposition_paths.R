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
.disposition_pattern_compile <- function(pattern, fn, breaks = FALSE) {
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
  regex <- vapply(steps, .disposition_pattern_step, character(1), bad = bad, breaks = breaks)
  # Every step must consume at least one campaign: a pattern that can match an
  # empty path ("N{0}", "*{0,2}") matches every phone at no position (match_end
  # 0). Our RE2 subset reads the same under PCRE, so R can test it up front.
  if (grepl(paste0("^(?:", paste(regex, collapse = ".*?"), ")$"), "", perl = TRUE)) {
    bad("can match zero campaigns; give every step at least one repeat")
  }
  # `>` is a LAZY gap: it does not change what matches, but it ends
  # match_end at the earliest next step (the first completion after the
  # non-response, not the last one).
  # With `breaks` the path carries a `-` where a gap exceeds max_gap_days
  # (disposition_paths()), and no part of a match may cross one.
  joins <- c(ifelse(seps == ">", if (breaks) "[^-]*?" else ".*?", ""), "")
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
.disposition_pattern_step <- function(step, bad, breaks = FALSE) {
  m <- regmatches(step, regexec("^([A-Za-z_*| ]+?)\\s*(\\+|\\{[0-9]+(?:,[0-9]*)?\\})?$",
                                step, perl = TRUE))[[1]]
  if (length(m) == 0L) bad(sprintf("has a step it cannot read (`%s`)", step))
  if (grepl("^\\s*[|]|[|]\\s*$|[|]\\s*[|]", m[2])) {
    bad(sprintf("has an empty `|` alternative in `%s`", step))
  }
  tokens <- trimws(strsplit(m[2], "|", fixed = TRUE)[[1]])
  codes <- vapply(tokens, function(tok) {
    if (tok == "*") return(".")
    if (tolower(tok) %in% names(.DISPOSITION_PATH_CODES)) {
      return(.DISPOSITION_PATH_CODES[[tolower(tok)]])
    }
    if (toupper(tok) %in% .DISPOSITION_PATH_CODES) return(toupper(tok))
    bad(sprintf("has an unknown label `%s`", tok))
  }, character(1))
  class <- if ("." %in% codes) {
    if (breaks) "[^-]" else "."
  } else {
    paste0("[", paste(intersect(.DISPOSITION_PATH_CODES, codes), collapse = ""), "]")
  }
  quant <- m[3]
  if (nzchar(quant) && quant != "+") {
    bounds <- as.numeric(strsplit(gsub("[{}]", "", quant), ",", fixed = TRUE)[[1]])
    if (any(bounds > 1000) || (length(bounds) == 2L && bounds[1] > bounds[2])) {
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
#' \code{match_start} / \code{match_end} locate the leftmost match: a repeat
#' takes its whole run (\code{"N+"} on \code{"XNNT"} is steps 2-3) and a
#' \code{>} gap stops at the earliest next step (\code{"N > C"} on
#' \code{"NCNC"} is steps 1-2).
#'
#' \strong{Gap limit.} With \code{max_gap_days}, a gap of more than that many
#' days between two consecutive campaigns breaks the sequence: no part of a
#' match -- a \code{>}, a \code{>>} or a repeat -- may span it, so
#' \code{"N > C"} with \code{max_gap_days = 365} needs a complete reached with
#' no year-long silence after the non-response. A missing date never breaks.
#' The rule looks at consecutive campaigns, not at the distance from the first
#' matched step: on prod it finds every phone a strict "within 365 days of the
#' non-response" rule finds, plus 1-2\% more. \code{^} / \code{$} still mean
#' the phone's first / most recent campaign; \code{path} and \code{shape} are
#' unchanged.
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
#' @param max_gap_days Optional single positive number, with \code{pattern}: a
#'   match may not span a gap of more than this many days between two
#'   consecutive campaigns (see Gap limit). Costs ~8 s more on the full
#'   projection.
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
#' # The same, with no gap of more than a year along the way
#' disposition_paths(dataset, "N > E|O > C|W", max_gap_days = 365) |>
#'   filter(matched) |> count() |> collect()
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
                              undeliverable_codes = .DISPOSITION_UNDELIVERABLE_CODES,
                              max_gap_days = NULL) {
  fn <- "disposition_paths"
  terminal <- match.arg(terminal)
  .disposition_lazy_check(fn)
  max_gap_days <- .disposition_check_max_gap(max_gap_days, pattern, fn)
  breaks <- !is.null(max_gap_days)
  pattern_re <- if (!is.null(pattern)) .disposition_pattern_compile(pattern, fn, breaks = breaks)
  codes <- .disposition_check_terminal_codes(undeliverable_codes, fn)
  sample_frac <- .disposition_check_sample_frac(sample_frac, fn)
  phones <- .disposition_request_phones(phones)

  con <- .disposition_lazy_con()
  source <- .disposition_lazy_source(con, dataset, fn)
  window <- .disposition_after_filters(con, source, NULL, date_from, date_to, fn)
  base <- .disposition_base_sql(con, source, phones, sample_frac,
                                .disposition_label_sql(con, source, terminal, codes),
                                pre = window)
  # Every column is a running aggregate over the ONE ordered window, read at the
  # phone's last row (where nothing follows): a second, unordered
  # PARTITION BY phone window (count / min / max over the whole phone) costs a
  # second sort -- ~2.5x the time and memory on prod.
  windowed <- paste0(
    "SELECT phone, ",
    "lead(phone) OVER w IS NULL AS s160_last, CAST(row_number() OVER w AS INTEGER) AS n_campaigns, ",
    "string_agg(s160_code, '') OVER w AS path, ",
    "min(disposition_date) OVER w AS first_date, max(disposition_date) OVER w AS last_date, ",
    "sum(CAST(NOT completion_known AS INTEGER)) OVER w AS n_unknown_completion",
    if (!is.null(pattern_re)) ", list(disposition_date) OVER w AS s160_dates",
    " FROM (", base, ") ", .disposition_sequence_window(source))
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
  # With a gap limit the pattern runs on a marked path: a `-` before each step
  # more than max_gap_days after the previous one. It is built at the phone's
  # last row from the ordered date list the match dates need anyway -- a lag()
  # feeding the path's string_agg would nest windows and cost a second sort
  # (~2x on prod). Positions count letters only, so the markers are stripped
  # from the prefix before its length is taken.
  target <- "path"
  if (breaks) {
    target <- "s160_bpath"
    last <- paste0(
      "SELECT *, array_to_string(list_transform(range(1, len(s160_dates) + 1), ",
      .disposition_lambda("i", paste0(
        "CASE WHEN i > 1 AND s160_dates[i] - s160_dates[i - 1] > ", sprintf("%.0f", max_gap_days),
        " THEN '-' ELSE '' END || path[i]")),
      "), '') AS s160_bpath FROM (", last, ")")
  }
  position <- function(regex) {
    sprintf("CAST(length(replace(regexp_extract(%s, %s, 1), '-', '')) AS INTEGER)", target, regex)
  }
  matched <- paste0(
    "SELECT *, ",
    "CASE WHEN matched THEN ", position(re("^(.*?)")), " + 1 END AS match_start, ",
    "CASE WHEN matched THEN ", position(re("^(.*?", ")")), " END AS match_end ",
    "FROM (SELECT *, regexp_matches(", target, ", ", DBI::dbQuoteString(con, pattern_re), ") AS matched ",
    "FROM (", last, "))")
  sql <- paste0("SELECT ", paste(c(cols, "matched", "match_start", "match_end",
                                   "s160_dates[match_start] AS match_start_date",
                                   "s160_dates[match_end] AS match_end_date"), collapse = ", "),
                " FROM (", matched, ")")
  dplyr::tbl(con, dbplyr::sql(sql))
}

# max_gap_days: NULL, or one positive number -- and only with a pattern, since
# it limits what a match may span.
.disposition_check_max_gap <- function(max_gap_days, pattern, fn) {
  if (is.null(max_gap_days)) return(NULL)
  if (!is.numeric(max_gap_days) || length(max_gap_days) != 1L || !is.finite(max_gap_days) ||
        max_gap_days <= 0) {
    stop_s160(sprintf("`max_gap_days` must be a single positive number%s.", .not_got(max_gap_days)),
              fn = fn)
  }
  if (is.null(pattern)) {
    stop_s160(paste0("`max_gap_days` limits what a pattern match may span, so it needs a ",
                     "`pattern`, e.g. pattern = \"N > C\", max_gap_days = 365."), fn = fn)
  }
  # Date gaps are whole days, so "more than 365.5" is "more than 365": floor it
  # here and the SQL gets a plain integer literal (no locale or digits option).
  floor(max_gap_days)
}

# A one-argument SQL lambda in the installed DuckDB's syntax: `lambda x: body`
# from 1.3 (2.0 rejects the arrow form), `x -> body` before it.
.disposition_lambda <- function(arg, body, version = .disposition_duckdb_version()) {
  if (version >= "1.3.0") sprintf("lambda %s: %s", arg, body) else sprintf("%s -> %s", arg, body)
}

.disposition_duckdb_version <- function() utils::packageVersion("duckdb")
