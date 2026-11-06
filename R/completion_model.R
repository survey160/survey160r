# Completion forecast: fit two campaign-level linear models (opt-ins per sent,
# completes per sent) from the disposition projection. The per-row history
# (letter, previous three letters, prior counts) is one DuckDB window pass over
# the lazy-reader plumbing in disposition_steps.R; a second pass groups it to one
# row per campaign; the fits are ordinary least squares on the logit of each
# rate. completion_forecast() (completion_forecast.R) scores a planned survey on
# the same context tables.

# The per-row history: one row per stored (phone, campaign) with the collapsed
# letter (T -> E, I -> R, W -> C: the model reads terminated as engaged,
# ineligible as refused and a web complete as a complete), whether the row is an
# opt-in (letter O or C), the running counts of prior contacts / completes /
# replies, the previous three letters as `context` (`_` when there is none) and
# whether the row is the phone's latest. The optional tracker columns and
# survey_mode come through as NULL when the file lacks them. `phones` scopes the
# read to a sample (the forecast path); NULL reads every phone (the fit path).
.completion_history_sql <- function(con, source, phones = NULL) {
  q <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  base <- .disposition_base_sql(
    con, source, phones, NULL,
    .disposition_label_sql(con, source, "raw", .DISPOSITION_UNDELIVERABLE_CODES))
  optional <- function(col) {
    if (col %in% source$cols) paste(q(col), "AS", col) else paste("NULL AS", col)
  }
  letter <- "replace(replace(replace(s160_code, 'T', 'E'), 'I', 'R'), 'W', 'C')"
  lettered <- paste0(
    "SELECT phone, campaign_id, disposition_date, completion_known, ",
    paste(vapply(c("survey_mode", "tracker_mode", "tracker_topic", "tracker_loi"), optional,
                 character(1)), collapse = ", "),
    ", ", letter, " AS letter, ",
    "CAST(", letter, " = 'C' AS INTEGER) AS s160_complete, ",
    "CAST(", letter, " IN ('E', 'O', 'R', 'C') AS INTEGER) AS s160_reply, s160_row ",
    "FROM (", base, ")")
  lag_letter <- function(k) sprintf("coalesce(lag(letter, %d) OVER w, '_')", k)
  paste0(
    "SELECT phone, campaign_id, disposition_date, survey_mode, tracker_mode, tracker_topic, ",
    "tracker_loi, letter, letter IN ('O', 'C') AS opted, completion_known, ",
    "CAST(row_number() OVER w - 1 AS INTEGER) AS n_prior, ",
    "CAST(sum(s160_complete) OVER w - s160_complete AS INTEGER) AS prior_completes, ",
    "CAST(sum(s160_reply) OVER w - s160_reply AS INTEGER) AS prior_replies, ",
    lag_letter(1L), " || ", lag_letter(2L), " || ", lag_letter(3L), " AS context, ",
    "lead(phone) OVER w IS NULL AS is_last ",
    "FROM (", lettered, ") ", .disposition_sequence_window(source))
}
