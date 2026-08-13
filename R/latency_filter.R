# Row-level filters applied by latency_report() before frame construction.
# Each is a pure function: data in, data (or indices) out.

# Evaluate a population-filter expression against the data. The expression is
# whatever string the config author placed in `filters.population` (e.g.
# 'id.intro.finalText == "Yes"'). Evaluated in a `baseenv()`-parented env so
# only base R functions and columns of `data` are accessible.
#
# Trust model: configs come from authors with commit access and
# go through the standard PR review process; the strings we eval here are
# treated as trusted input, the same way an R script committed to the
# legacy latency-scripts repo would be. Do NOT extend this to accept
# user-uploaded configs without first restricting the expression grammar
# (e.g., via rlang::parse_expr + a small allowlist of operators).
apply_population_filter <- function(data, expr) {
  data[population_filter_mask(data, expr), , drop = FALSE]
}

# Return the row indices to keep when deduping by respondent_id, choosing the
# row with the earliest id.intro.scriptDate per id. Rows where the id is NA
# pass through (they are unidentifiable and cannot be deduped). Indices are
# in original row order so callers can apply them to parallel per-row masks.
dedupe_keep_rows <- function(data, resp_id_col) {
  if (!resp_id_col %in% names(data)) {
    stop_not_found("respondent-id column", resp_id_col)
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
