# A guided tour of disposition_paths() and disposition_steps()
#
# Open this file in RStudio and run it one step at a time (Ctrl+Enter on a
# line, or Ctrl+Alt+T for a whole section). Each step says what it shows; read
# the output before moving on. Open your own copy with:
#
#   file.copy(system.file("scripts", "disposition-paths-tour.R",
#                         package = "survey160r"), "disposition-paths-tour.R")
#   file.edit("disposition-paths-tour.R")
#
# The whole tour runs on a 10% sample of phones, so each step takes seconds.
# Drop `sample_frac = 0.1` for full-data numbers (each call then takes ~10-20 s).
# Full reference: ?disposition_paths and ?disposition_steps.


# ---- Step 0. Setup ----------------------------------------------------------

# The sequence readers need duckdb, DBI and dbplyr (no in-R fallback):
# install.packages(c("duckdb", "DBI", "dbplyr"))

library(survey160r)
library(dplyr)

s160_gcs_init()                  # one browser sign-in, cached
dataset <- disposition_pull()    # ~1 GB the first time, then the local copy
# disposition_pull(refresh = TRUE) picks up a newer projection.

frac <- 0.1                      # share of phones the tour reads
completes <- c("completed", "web_complete")   # a complete on the phone or the web

# How many phones match a pattern (used from Step 3 on)
n_matched <- function(paths) {
  paths |>
    filter(matched) |>
    count() |>
    pull(n)
}


# ---- Step 1. One row per phone: the path ----------------------------------

# Each phone's campaigns, oldest first, spelled as one letter per campaign:
#
#   N non_response   X undeliverable   E engaged   O opted_in   T terminated
#   I ineligible     R refused         C completed W web_complete
#
# `path` is the full sequence ("NNEC"), `shape` the same with repeats folded
# ("N+E+C+"). The result is a lazy table: nothing is computed until you
# collect() or print it.

paths <- disposition_paths(dataset, sample_frac = frac)
paths                            # prints the first rows

paths |>
  filter(n_campaigns >= 4) |>
  select(phone, n_campaigns, path, shape, first_date, last_date) |>
  head(10) |>
  collect()


# ---- Step 2. The commonest shapes -----------------------------------------

# Aggregate in DuckDB, collect only the small result.

paths |>
  count(shape, sort = TRUE) |>
  head(20) |>
  collect()

# How many campaigns phones have had
paths |>
  count(n_campaigns) |>
  arrange(n_campaigns) |>
  collect()


# ---- Step 3. Match a pattern ----------------------------------------------

# Give a pattern and every row gains `matched`, `match_start` / `match_end`
# (step positions) and `match_start_date` / `match_end_date`.
#
#   a > b    b comes later than a (other campaigns may sit between)
#   a >> b   b is the very next campaign
#   a|b      either label
#   *        any label
#
# Names and letters mean the same: "non_response > completed" == "N > C".

no_reply_then_complete <- disposition_paths(dataset, "N > C", sample_frac = frac)

no_reply_then_complete |>
  count(matched) |>
  collect()

no_reply_then_complete |>
  filter(matched) |>
  select(phone, path, match_start, match_end, match_start_date, match_end_date) |>
  head(10) |>
  collect()

# A three-step story: no reply, later engaged or opted in, later completed
# (on the phone or on the web)
disposition_paths(dataset, "N > E|O > C|W", sample_frac = frac) |>
  n_matched()


# ---- Step 4. Next campaign vs. any later campaign -------------------------

# `>` allows campaigns in between, `>>` does not. Compare the two counts.

later <- disposition_paths(dataset, "N > C", sample_frac = frac) |>
  n_matched()
next_one <- disposition_paths(dataset, "N >> C", sample_frac = frac) |>
  n_matched()
c(any_later = later, very_next = next_one)


# ---- Step 5. Repeats ------------------------------------------------------

# `+` one or more in a row, `{3}` exactly three, `{3,}` three or more,
# `{2,4}` two to four. A repeat matches consecutive campaigns.

# Three or more unanswered in a row, then a complete right after
disposition_paths(dataset, "non_response|undeliverable{3,} >> C|W",
                  sample_frac = frac) |>
  filter(matched) |>
  select(phone, path, match_start, match_end) |>
  head(10) |>
  collect()


# ---- Step 6. Anchors: first and most recent campaign ----------------------

# `^` at the start ties the pattern to the phone's first campaign, `$` at the
# end to its most recent one.

# Completed on its first contact
disposition_paths(dataset, "^C|W", sample_frac = frac) |>
  n_matched()

# The last three contacts got no reply (a candidate to suppress)
disposition_paths(dataset, "N|X{3}$", sample_frac = frac) |>
  n_matched()

# Anything at all, then undeliverable as the most recent outcome
disposition_paths(dataset, "* >> X$", sample_frac = frac) |>
  n_matched()


# ---- Step 7. Gap limit: ignore stories broken by long silences ------------

# With `max_gap_days`, no part of a match may span a gap of more than that
# many days between two consecutive campaigns. "N > C" with a 365-day limit
# needs the complete reached with no year-long silence along the way.

no_limit <- disposition_paths(dataset, "N > C", sample_frac = frac) |>
  n_matched()
one_year <- disposition_paths(dataset, "N > C", sample_frac = frac,
                              max_gap_days = 365) |>
  n_matched()
c(no_limit = no_limit, within_a_year = one_year)


# ---- Step 8. Scope: which campaigns form the path -------------------------

# `date_from` / `date_to` and `campaign_ids` decide which campaigns exist at
# all, so the path starts at `date_from` (step 1 is the phone's first campaign
# in range). Phones with no campaign in scope have no row.

paths_2026 <- disposition_paths(dataset, sample_frac = frac,
                                date_from = "2026-01-01")
paths_2026 |>
  count(shape, sort = TRUE) |>
  head(10) |>
  collect()

# One client's or one study's campaigns: pass their ids
# disposition_paths(dataset, "N > C", campaign_ids = c(1234, 1235, 1236))


# ---- Step 9. Flag the numbers of a sample ---------------------------------

# Pass `phones` to read only those numbers: fast, even for millions of
# phones. Here the "sample" is 500 numbers taken from the dataset itself;
# in practice it is the sample file you are about to load.

my_sample <- disposition_paths(dataset, sample_frac = 0.01) |>
  filter(n_campaigns >= 3) |>
  head(500) |>
  select(phone) |>
  collect()

flags <- disposition_paths(dataset, "N|X{3,}$", phones = my_sample$phone) |>
  select(phone, path, matched) |>
  collect()

my_sample <- my_sample |>
  left_join(flags, by = "phone") |>
  mutate(three_plus_unanswered = matched %in% TRUE)  # NA-safe: no row = FALSE

count(my_sample, three_plus_unanswered)


# ---- Step 10. One row per campaign: what happens next ---------------------

# disposition_steps() has one row per (phone, campaign) with the history up
# to that campaign: `step` of `n_campaigns`, `label`, `prev_label` /
# `next_label`, running counts, `run_length` (e.g. the 3rd no-reply in a row)
# and `path_so_far`. Use it for "after X, how often does Y happen next?".

steps <- disposition_steps(dataset, sample_frac = frac)

steps |>
  select(phone, step, n_campaigns, label, prev_label, next_label,
         run_length, path_so_far) |>
  head(10) |>
  collect()

# After k no-replies in a row, what does the next campaign get?
steps |>
  filter(label == "non_response", !is.na(next_label)) |>
  mutate(next_reply = !next_label %in% c("non_response", "undeliverable")) |>
  group_by(run_length = pmin(run_length, 6L, na.rm = TRUE)) |>  # 6 = six or more
  summarise(sends = n(), next_reply = mean(as.integer(next_reply), na.rm = TRUE)) |>
  arrange(run_length) |>
  collect()


# ---- Step 11. History segments and recency --------------------------------

# columns = "history" adds prior_* counts, a `history_segment`
# (first_contact, completer_2_plus, completer_1, replied, unanswered_3_plus,
# unanswered_1_2) and recency columns (steps_since_* / days_since_*).
# Filter the result, not the read, to keep recent sends with their full
# history.

history <- disposition_steps(dataset, sample_frac = frac, columns = "history")

history |>
  filter(disposition_date >= as.Date("2026-01-01"), completion_known) |>
  group_by(history_segment) |>
  summarise(sends = n(),
            completion = mean(as.integer(label %in% completes), na.rm = TRUE)) |>
  arrange(desc(completion)) |>
  collect()

# Completion by how many days since the phone last replied
history |>
  filter(disposition_date >= as.Date("2026-01-01"), completion_known,
         !is.na(days_since_reply)) |>
  mutate(since_reply = case_when(days_since_reply <= 30 ~ "0-30 days",
                                 days_since_reply <= 180 ~ "31-180 days",
                                 days_since_reply <= 365 ~ "181-365 days",
                                 TRUE ~ "over a year")) |>
  group_by(since_reply) |>
  summarise(sends = n(),
            completion = mean(as.integer(label %in% completes), na.rm = TRUE)) |>
  collect() |>
  arrange(factor(since_reply, levels = c("0-30 days", "31-180 days",
                                         "181-365 days", "over a year")))


# ---- Step 12. Patterns on the history so far ------------------------------

# disposition_steps(pattern = ) adds `history_matched`: does the history up to
# and including this step match? With `$` that reads "history ends with".

disposition_steps(dataset, sample_frac = frac, columns = NULL,
                  pattern = "C|W > *$") |>                # completed before
  filter(step > 1, completion_known) |>
  group_by(completed_before = history_matched) |>
  summarise(sends = n(),
            completion = mean(as.integer(label %in% completes), na.rm = TRUE)) |>
  collect()


# ---- Step 13. Labeling choices ---------------------------------------------

# terminal = "engaged" relabels an unsplit `terminated` row by the step it
# reached (opted_in or engaged). undeliverable_codes = NULL turns every X back
# into N. Compare the shape counts with and without.

disposition_paths(dataset, sample_frac = frac, terminal = "engaged",
                  undeliverable_codes = NULL) |>
  count(shape, sort = TRUE) |>
  head(10) |>
  collect()


# ---- Where next ------------------------------------------------------------

# ?disposition_paths   pattern syntax, gap limit, scope, result columns
# ?disposition_steps   every history column group, neighbor_columns, resources
# ?disposition_screen  screen a sample on counts instead of sequences
