# Fixtures for the sequence readers (disposition_steps / disposition_paths). They
# return lazy dbplyr tables on DuckDB, so every test needs duckdb + DBI + dbplyr;
# skip_if_no_sequence() skips otherwise.

# Spill under the session tempdir, never the user's package cache, during tests.
options(survey160r.duckdb_temp_dir = file.path(tempdir(), "survey160r-spill"))

skip_if_no_sequence <- function() {
  testthat::skip_if_not_installed("duckdb")
  testthat::skip_if_not_installed("DBI")
  testthat::skip_if_not_installed("dbplyr")
}

# One stored (phone, campaign) row with every funnel flag; override any column.
.sequence_row <- function(phone, campaign_id, date, engaged = 0L, opted_in = 0L,
                          completed = 0L, web_complete = 0L, refused = 0L,
                          ineligible = 0L, terminated = 0L, error = NA_character_,
                          survey_mode = "sms") {
  data.frame(phone = phone, campaign_id = as.integer(campaign_id), sent = 1L,
             engaged = as.integer(engaged), opted_in = as.integer(opted_in),
             completed = as.integer(completed), web_complete = as.integer(web_complete),
             refused = as.integer(refused), ineligible = as.integer(ineligible),
             terminated = as.integer(terminated), error = as.character(error),
             survey_mode = survey_mode, disposition_date = as.Date(date),
             stringsAsFactors = FALSE)
}

# Five phones covering the ordering and labelling edge cases:
#   0101  N -> E -> C                       the canonical funnel path
#   0102  X (4720) -> N (soft 4740) -> N -> T (unsplit terminal, engaged)
#   0103  stored with a leading 1; two campaigns on one date -> campaign_id order: O, W
#   0104  t2w_external engaged (completed NA, completion unknown), then a missing date (last)
#   0105  R -> I, stored out of date order
sequence_records <- function() {
  rbind(
    .sequence_row("2015550101", 1L, "2026-01-01"),
    .sequence_row("2015550101", 2L, "2026-02-01", engaged = 1L),
    .sequence_row("2015550101", 3L, "2026-03-01", engaged = 1L, opted_in = 1L, completed = 1L),
    .sequence_row("2015550102", 1L, "2026-01-01", error = "4720"),
    .sequence_row("2015550102", 2L, "2026-01-15", error = "4740"),
    .sequence_row("2015550102", 3L, "2026-02-01"),
    .sequence_row("2015550102", 4L, "2026-03-01", engaged = 1L, terminated = 1L),
    .sequence_row("12015550103", 2L, "2026-02-01", engaged = 1L, opted_in = 1L,
                  completed = 1L, web_complete = 1L, survey_mode = "t2w"),
    .sequence_row("12015550103", 1L, "2026-02-01", engaged = 1L, opted_in = 1L),
    .sequence_row("2015550104", 3L, "2026-03-01", engaged = 1L, completed = NA_integer_,
                  survey_mode = "t2w_external"),
    .sequence_row("2015550104", 1L, NA),
    .sequence_row("2015550105", 2L, "2026-02-01", engaged = 1L, ineligible = 1L, terminated = 1L),
    .sequence_row("2015550105", 1L, "2026-01-01", engaged = 1L, refused = 1L, terminated = 1L)
  )
}

write_sequence_parquet <- function(rows = sequence_records()) write_disposition_parquet(rows)

# Collect a lazy sequence table into a base data frame sorted by phone (then step).
collect_sorted <- function(tbl) {
  d <- as.data.frame(dplyr::collect(tbl))
  keys <- intersect(c("phone", "step"), names(d))
  d <- d[do.call(order, unname(as.list(d[keys]))), , drop = FALSE]
  rownames(d) <- NULL
  d
}
