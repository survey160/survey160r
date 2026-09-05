# Per-question accessors over a Survey160 campaign export. A "question" is the
# stem of an export column family named id.<question>.<field> (scriptDate = the
# outbound send of that question, batchDate = the inbound reply). These are pure
# compute over an in-memory export frame -- no I/O, no raw-source access -- so
# they take bare names.

#' Read a question's timestamps from a campaign export
#'
#' Resolves the \code{id.<question>.<field>} column of a Survey160 campaign
#' export and parses it to \code{POSIXct} (UTC) via
#' \code{\link{parse_campaign_timestamps}}. A robust replacement for hand-built
#' \code{df[[paste0("id.", q, ".", field)]]} access in report/analysis code.
#'
#' @param df A campaign export data frame (e.g. from
#'   \code{\link{s160_gcs_campaign_results_read}} or
#'   \code{\link{s160_read_csv}}).
#' @param question Question stem (character scalar), e.g. \code{"intro"},
#'   \code{"close"}, \code{"adrecall"}.
#' @param field Timestamp field (character scalar): \code{"scriptDate"} (the
#'   outbound send, the default) or \code{"batchDate"} (the inbound reply).
#' @return A \code{POSIXct} vector in UTC, one element per row of \code{df}.
#' @seealso \code{\link{question_funnel}} for the per-question reached-count
#'   funnel built on this.
#' @examples
#' df <- data.frame(
#'   id.close.scriptDate = c("2026-01-26 21:02:00Z", NA),
#'   check.names = FALSE
#' )
#' question_timestamps(df, "close")
#' @export
question_timestamps <- function(df, question, field = "scriptDate") {
  if (!is.data.frame(df)) {
    stop_s160("`df` must be a data frame (a campaign export).",
              fn = "question_timestamps")
  }
  check_nonempty_string(question, "question", fn = "question_timestamps")
  check_nonempty_string(field, "field", fn = "question_timestamps")
  col <- sprintf("id.%s.%s", question, field)
  if (!col %in% names(df)) {
    stop_s160(
      sprintf(
        "column `%s` not found in `df` (question = \"%s\", field = \"%s\").",
        col, question, field
      ),
      fn = "question_timestamps"
    )
  }
  parse_campaign_timestamps(df[[col]])
}

#' Per-question reached-count funnel for a campaign export
#'
#' For each question in \code{questions} (in flow order), counts the records
#' that reached it -- those with a parseable \code{field} timestamp for the
#' question (i.e. were shown it). Returns one row per question with the reached
#' count and its share of the funnel head. This is the per-question companion
#' to \code{\link{latency_funnel}} (which returns the send / opt-in / complete
#' anchors): use it for the "records reaching each stage" view that
#' \code{latency_funnel} does not provide.
#'
#' Reached is a monotone non-increasing count down the flow. The first entry of
#' \code{questions} is the funnel head, and \code{pct_reached} is each stage's
#' percentage of that head (so the head is 100).
#'
#' @param df A campaign export data frame.
#' @param questions Ordered character vector of question stems forming the
#'   funnel, e.g. \code{c("adrecall", "favorability", "transparent",
#'   "problemsolution", "close")}. The first entry is the funnel head.
#' @param field Timestamp field marking "reached" (default \code{"scriptDate"},
#'   the send of that question).
#' @return A data frame with one row per question, in the order given:
#'   \code{question}, \code{question_index} (1-based flow order),
#'   \code{n_reached}, and \code{pct_reached} (share of the funnel head,
#'   0-100; \code{NA} when the head count is zero).
#' @seealso \code{\link{latency_funnel}} for the send/opt-in/complete anchors.
#' @examples
#' df <- data.frame(
#'   id.adrecall.scriptDate = c("2026-01-26 21:01:00Z", "2026-01-26 21:05:00Z"),
#'   id.close.scriptDate    = c("2026-01-26 21:02:00Z", NA),
#'   check.names = FALSE
#' )
#' question_funnel(df, c("adrecall", "close"))
#' @export
question_funnel <- function(df, questions, field = "scriptDate") {
  if (!is.data.frame(df)) {
    stop_s160("`df` must be a data frame (a campaign export).",
              fn = "question_funnel")
  }
  if (!is.character(questions) || length(questions) == 0L ||
        anyNA(questions) || !all(nzchar(questions))) {
    stop_s160(
      "`questions` must be a non-empty character vector of question stems.",
      fn = "question_funnel"
    )
  }
  n_reached <- vapply(
    questions,
    function(q) sum(!is.na(question_timestamps(df, q, field))),
    integer(1)
  )
  head_n <- n_reached[[1]]
  out <- data.frame(
    question = questions,
    question_index = seq_along(questions),
    n_reached = as.integer(n_reached),
    pct_reached = if (head_n > 0L) 100 * n_reached / head_n else NA_real_,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}
