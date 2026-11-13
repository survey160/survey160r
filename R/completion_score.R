# completion_score(): the per-phone step of completion_estimate(), exported.
# One row per phone with its history before the planned start and the context
# table's P(complete) (refined by prior completes) / P(reply); the estimate
# averages the context rate of this table.

# The per-phone table: every requested phone (normalized, unique) in input
# order, with its last row before `start_date` from the history SQL. A phone
# with no row is a first contact (`___`). `p_context` is the context cell's
# rate (the estimate averages it); `p_complete` refines it by the number's
# prior completes (.completion_refine). `survey_mode` is the table block to
# read (NA: pooled). `campaign_id` (the last campaign) is kept for the
# estimate's provenance term and dropped by completion_score(), as is
# `p_context`.
.completion_score_table <- function(con, source, phones, model, year, survey_mode, start_date) {
  sql <- paste0(
    "SELECT phone, campaign_id, disposition_date, letter, context, n_prior, prior_completes, ",
    "prior_replies FROM (",
    .completion_history_sql(con, source, phones, before = start_date), ") WHERE is_last")
  h <- DBI::dbGetQuery(con, sql)
  idx <- match(phones, h$phone)
  seen <- !is.na(idx)
  letter <- h$letter[idx]
  context <- rep("___", length(phones))
  context[seen] <- paste0(letter[seen], substr(h$context[idx[seen]], 1L, 2L))
  days <- rep(NA_real_, length(phones))
  days[seen] <- as.numeric(start_date - as.Date(h$disposition_date[idx[seen]]))
  # n_context is the count of the cell p_complete came from, through the same
  # fallbacks (a cell can have a count but no completion rate, when every send
  # in it had unknown completion)
  row <- .completion_context_row(model$contexts, year, context, survey_mode, "p_complete")
  prior_completes <- ifelse(seen, h$prior_completes[idx] + (letter == "C"), 0L)
  p_context <- model$contexts$p_complete[row]
  data.frame(
    phone = phones, context = context,
    n_prior = ifelse(seen, h$n_prior[idx] + 1L, 0L),
    prior_completes = prior_completes,
    prior_replies = ifelse(seen, h$prior_replies[idx] + letter %in% c("E", "O", "R", "C"), 0L),
    days_since_contact = days,
    n_context = model$contexts$n[row],
    p_complete = .completion_refine(model$completes, model$contexts, row, prior_completes, p_context),
    p_reply = .completion_context_lookup(model$contexts, year, context, survey_mode, "p_reply"),
    campaign_id = h$campaign_id[idx], p_context = p_context,
    stringsAsFactors = FALSE)
}

# The warning for phones whose context has no row on the year's table.
.completion_score_no_row <- function(p, year, mode) {
  if (!anyNA(p)) return(character(0))
  sprintf(paste0("%d of %d numbers have a context with no row in the %d table for mode %s; ",
                 "they are left out of sample_score"), sum(is.na(p)), length(p), year, mode)
}

#' Score each number in a sample by its contact history
#'
#' The per-phone step behind \code{\link{completion_estimate}}: one row per
#' number with its history before the planned start and the fitted model's
#' P(complete) and P(reply) for that history, so a list can be ranked or
#' trimmed before it is quoted. The estimate's \code{sample_score} is the
#' logit of the mean context rate over this table (\code{p_complete} before
#' the prior-completes step below), so the estimate does not change with it.
#'
#' A number's score starts from the completion rate of every past send to a
#' number with the same \code{context} (its previous three outcomes, most
#' recent first) in the same survey mode, on the context table of the start's
#' year, and is then refined by its \code{prior_completes} (0, 1, 2, 3 or
#' more): the rate of that cell's sends with the same count, pulled toward the
#' cell's rate with the weight of 200 sends, so a thin count stays near its
#' cell. Older completes matter: in 2026, among numbers whose last three
#' contacts went unanswered, one that completed earlier completed about ten
#' times as often as one that never did. A model fitted before 0.89.0 has no
#' such split and scores on the context alone.
#'
#' It is a group rate, not a personal probability: it ranks a list well (on
#' the production data, a random complete outranks a random non-complete about
#' two times in three inside one list; on 2026 lists the prior-completes step
#' raised that from 0.77 to 0.78 for live and kept it near 0.65 for
#' text-to-web),
#' and a difference between two numbers' scores is meaningful only when their
#' cells differ. Its level is the
#' history's, not this year's: on live lists the table has run about 1.3
#' times the completion actually seen in 2025-2026 (the fleet completes less
#' than it used to), while text-to-web is within 5-25\%. Read the mean of a
#' list from \code{completion_estimate()}, whose model carries the current
#' level; read the scores as a ranking.
#'
#' \strong{Trimming a list.} Sort by \code{p_complete}, then
#' \code{prior_replies}, then \code{days_since_contact} (older first), and
#' keep the top. Every cut falls inside one cell (most numbers share a
#' handful of them), and inside a cell only the older history separates
#' numbers: a reply anywhere before the last three contacts, and a longer
#' silence since the last contact, both raise completion. The contact count
#' does not. Dropping the lowest fifth of a list raises its completion rate
#' by about a seventh and keeps nine completes in ten.
#'
#' \strong{Mode.} Without \code{mode} the scores come from the pooled block
#' of the context table: they rank numbers exactly as the mode's block does,
#' but the level is averaged over modes (text-to-web runs lower than live).
#' Pass the mode for scores whose mean matches \code{completion_estimate()}.
#'
#' Conditions that the estimate reports in its \code{warnings} column are
#' R warnings here: a start year outside the stored tables, a context with no
#' row on the year's table (its \code{p_complete} is NA), and the text-to-web
#' external mode, where completion is never recorded so \code{p_complete} is
#' NA and only \code{p_reply} is scored.
#'
#' @param model A \code{completion_model} from \code{\link{completion_model_fit}}.
#' @param phones Character vector of the sample's phone numbers. Matched
#'   digit-normalized (punctuation and a leading US \code{1} are ignored);
#'   duplicates collapse to one row, in the order first seen.
#' @param dataset Path to the disposition Parquet file, e.g. from
#'   \code{\link{disposition_pull}()}, the history is read from.
#' @param mode \code{NULL} (default) for the pooled context table, or one of
#'   \code{"Live"}, \code{"T2W"}, \code{"T2W external"} for that mode's block.
#' @param start_date Planned start (Date or date string; default today): only
#'   history before it is read, and it picks the context table of its year.
#' @return A data frame, one row per unique number, in input order:
#'   \code{phone} (the 10 bare digits), \code{context} (the previous three
#'   outcomes, most recent first, in the coarse letters of
#'   \code{\link{disposition_steps}}, \code{_}-padded; \code{"___"} never
#'   contacted), \code{n_prior} (contacts before the start),
#'   \code{prior_completes} and \code{prior_replies} (among them),
#'   \code{days_since_contact} (from the last contact to the start; NA when
#'   never contacted), \code{n_context} (the total training sends
#'   in the cell \code{p_complete} came from, through the same fallbacks,
#'   including sends whose completion is unknown, so not the denominator of
#'   \code{p_complete}), \code{p_complete} (the context rate refined by
#'   \code{prior_completes}) and \code{p_reply} (the context rate).
#' @seealso \code{\link{completion_estimate}} (the list's rate),
#'   \code{\link{disposition_paths}} (flag a sample by a sequence pattern
#'   instead), \code{vignette("disposition-paths")}
#' @examples
#' \dontrun{
#' dataset <- disposition_pull()
#' options(survey160r.duckdb_memory = "16GB")   # the default 4 GB runs out
#' m <- completion_model_fit(dataset)
#' sample <- read.csv("client_sample.csv")
#' scores <- completion_score(m, sample$phone, dataset = dataset, mode = "T2W",
#'                            start_date = "2026-12-01")
#' # keep the best four fifths
#' ranked <- scores[order(-scores$p_complete, -scores$prior_replies,
#'                        -scores$days_since_contact), ]
#' keep <- head(ranked, ceiling(0.8 * nrow(ranked)))
#' }
#' @export
completion_score <- function(model, phones, dataset, mode = NULL, start_date = Sys.Date()) {
  fn <- "completion_score"
  if (!inherits(model, "completion_model")) {
    stop_s160(sprintf("`model` must be a completion_model from completion_model_fit()%s.",
                      .not_got(model)), fn = fn)
  }
  .disposition_lazy_check(fn)
  req <- if (is.character(phones)) .disposition_request_phones(phones) else character(0)
  if (length(req) == 0L) {
    stop_s160(sprintf("`phones` must be a character vector with at least one phone number%s.",
                      .not_got(phones)), fn = fn)
  }
  if (!is.null(mode) && (!is.character(mode) || length(mode) != 1L || !(mode %in% .COMPLETION_MODES))) {
    stop_s160(sprintf("`mode` must be NULL or one of %s%s.",
                      paste(sprintf('"%s"', .COMPLETION_MODES), collapse = ", "), .not_got(mode)),
              fn = fn)
  }
  start_date <- .completion_check_date(start_date, "start_date", fn)
  year <- .completion_estimate_year(start_date, model$contexts)
  survey_mode <- if (is.null(mode)) NA_character_ else .COMPLETION_SURVEY_MODES[[mode]]
  con <- .disposition_lazy_con()
  source <- .disposition_lazy_source(con, dataset, fn)
  out <- .completion_score_table(con, source, req, model, year$year, survey_mode, start_date)
  out$campaign_id <- NULL
  out$p_context <- NULL
  warnings <- c(year$warning,
                .completion_score_no_row(out$p_complete, year$year, mode %||% "pooled"))
  if (identical(mode, "T2W external")) {
    out$p_complete <- NA_real_
    warnings <- c(warnings, "completion is never recorded for a text-to-web external survey: p_complete is NA")
  }
  for (w in warnings) warning(fn, ": ", w, call. = FALSE)
  out
}
