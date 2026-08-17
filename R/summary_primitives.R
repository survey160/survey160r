# Pure helpers for the summary metrics view (spec §4.1).
# Counts per (campaign_id, date, hour_local) of:
#   - n_sent     -- intro dispatched (id.intro.scriptDate non-NA, the send)
#   - n_engaged    -- subset that replied to the intro (id.intro.batchDate)
#   - n_opted_in  -- subset that passes config$filters$population
#   - n_complete  -- subset that has id.close.scriptDate non-NA (sms) /
#                     web_complete callback (t2w)
# plus per (campaign_id, date, hour_local, segment_index):
#   - n_ineligible -- screened out at q_k, anchored by intro.batchDate
#
# The funnel counts (sent/engaged/opted_in/complete) are cohort-by-send-
# time: respondents are bucketed by the hour the platform dispatched the intro
# to them (id.intro.scriptDate), not by when the downstream event occurred.
# That is the only anchor available to every counted recipient -- a
# texted-but-never-replied recipient has no batchDate -- and it gives a
# one-line cohort definition operators can reason about ("of recipients we
# texted at hour H, how many later did X?"). n_ineligible stays anchored on
# intro.batchDate: it is a segment-level screen-out count that joins onto the
# reply-anchored latency cells, so it keeps the latency view's anchor.
#
# Pure functions, no I/O. Input is the raw, pre-filter data frame.

# Evaluate the population filter expression against `data` and return a
# length-nrow(data) logical vector (without subsetting). Used by
# build_summary_frame() to count consenters within the pre-filter
# population, in contrast to apply_population_filter() which subsets.
# Returns all-TRUE when `expr` is NULL/empty so the consent count
# matches the texted count -- the same default as filter behaviour.
#
# NA-coercion: rows where the expression evaluates to NA (e.g. a
# respondent whose `id.intro.finalText` is NA against
# `id.intro.finalText == "Yes"`) are treated as FALSE. The contract is
# "consented means the row matched", and NA → unknown is closer to "did
# not match" than to "matched". Operators who want to surface NA as a
# distinct bucket (n_unknown) should compute it from the raw CSV
# separately; this function does not split it out.
population_filter_mask <- function(data, expr) {
  if (is.null(expr) || !nzchar(expr)) return(rep(TRUE, nrow(data)))
  parsed <- tryCatch(parse(text = expr),
                     error = function(e) {
                       stop(sprintf("`filters.population` is not valid R: %s", expr),
                            call. = FALSE)
                     })
  env <- list2env(as.list(data), parent = baseenv())
  keep <- tryCatch(eval(parsed, envir = env),
                   error = function(e) {
                     stop(sprintf("`filters.population` evaluation failed: %s",
                                  conditionMessage(e)),
                          call. = FALSE)
                   })
  if (!is.logical(keep) || length(keep) != nrow(data)) {
    stop("`filters.population` must evaluate to a logical vector matching nrow(data).",
         call. = FALSE)
  }
  keep[is.na(keep)] <- FALSE
  keep
}

# Resolve the per-respondent "last reached" question index (1-based into
# config$flow$questions). NA where no scriptDate is set on any question
# in the flow. Used by build_ineligible_frame() to map a screened
# respondent to the segment_index that ended at the screening question.
last_reached_question_index <- function(data, questions) {
  n <- nrow(data)
  last_idx <- rep(NA_integer_, n)
  for (k in seq_along(questions)) {
    col <- sprintf("id.%s.scriptDate", questions[k])
    if (!col %in% names(data)) next
    has_ts <- !is.na(data[[col]])
    last_idx[has_ts] <- k
  }
  last_idx
}
