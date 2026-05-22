# Row-level filters applied by campaign_report() before frame construction.
# Each is a pure function: data in, data (or indices) out.

# Evaluate a population-filter expression against the data. The expression is
# whatever string the analyst placed in `filters.population` (e.g.
# 'id.intro.finalText == "Yes"'). Evaluated in a `baseenv()`-parented env so
# only base R functions and columns of `data` are accessible.
#
# Trust model: configs are authored by analysts with commit access and
# go through the standard PR review process; the strings we eval here are
# treated as trusted input, the same way an R script committed to the
# legacy latency-scripts repo would be. Do NOT extend this to accept
# user-uploaded configs without first restricting the expression grammar
# (e.g., via rlang::parse_expr + a small allowlist of operators).
apply_population_filter <- function(data, expr) {
  if (is.null(expr) || !nzchar(expr)) return(data)
  parsed <- tryCatch(parse(text = expr),
                     error = function(e) {
                       stop(sprintf("filters.population is not valid R: %s", expr),
                            call. = FALSE)
                     })
  env <- list2env(as.list(data), parent = baseenv())
  keep <- tryCatch(eval(parsed, envir = env),
                   error = function(e) {
                     stop(sprintf("filters.population evaluation failed: %s",
                                  conditionMessage(e)), call. = FALSE)
                   })
  if (!is.logical(keep) || length(keep) != nrow(data)) {
    stop("filters.population must evaluate to a logical vector matching nrow(data).",
         call. = FALSE)
  }
  keep[is.na(keep)] <- FALSE
  data[keep, , drop = FALSE]
}

# Return the row indices to keep when deduping by respondent_id, choosing the
# row with the earliest id.intro.scriptDate per id. Rows where the id is NA
# pass through (they are unidentifiable and cannot be deduped). Indices are
# in original row order so callers can apply them to parallel per-row masks.
dedupe_keep_rows <- function(data, resp_id_col) {
  if (!resp_id_col %in% names(data)) {
    stop(sprintf("respondent_id_column not found: %s", resp_id_col), call. = FALSE)
  }
  n <- nrow(data)
  intro <- data[["id.intro.scriptDate"]]
  if (is.null(intro)) return(seq_len(n))
  rid <- data[[resp_id_col]]
  ord <- order(rid, intro, na.last = TRUE)
  rid_sorted <- rid[ord]
  has_id_sorted <- !is.na(rid_sorted) & nzchar(as.character(rid_sorted))
  is_dup_sorted <- has_id_sorted & duplicated(rid_sorted)
  sort(ord[!is_dup_sorted])
}

# Return row indices whose intro.scriptDate (in field_tz) falls in date_filter.
date_filter_keep_rows <- function(data, date_filter, field_tz) {
  intro <- data[["id.intro.scriptDate"]]
  if (is.null(intro)) return(seq_len(nrow(data)))
  local_dates <- as.Date(format(intro, tz = field_tz))
  target <- as.Date(date_filter)
  which(!is.na(local_dates) & local_dates %in% target)
}
