# Sequence readers over the disposition dataset: disposition_steps() (one row per
# (phone, campaign), with the phone's history up to that campaign) and
# disposition_paths() (one row per phone, its campaigns as a letter path; see
# R/disposition_paths.R). Both answer "what happened to a number over time" --
# no reply first, then engaged, then completed -- which the per-phone rollup
# (disposition_summary) cannot, because it collapses the order away.
#
# Both return a LAZY dbplyr table on one session-wide DuckDB connection instead of
# a data frame. The history columns are window functions over every campaign a
# phone has, so on the full production projection they only
# fit when DuckDB computes them -- spilling to disk past `memory_limit` -- and R
# receives just what the caller collect()s. Measured on prod (4 GB limit): a
# full-scan steps aggregate in ~10 s, paths in ~8 s, <= 5.5 GB RSS and up to
# ~20 GB of transient spill; a phone-scoped table (up to millions of phones) in
# 1-5 s. A whole-file read into R (nanoparquet) peaks past 20 GB, so there is no
# fallback engine: these two need duckdb + DBI + dbplyr (all Suggests).
#
# Ordering and labels are shared by the two readers:
#   * A phone's campaigns are ordered by disposition_date (its last-send date in
#     that campaign), then campaign_id; a missing date sorts first (the oldest).
#   * Each (phone, campaign) gets ONE label, the furthest step reached, by the
#     same precedence as the summary's latest/best disposition
#     (.disposition_derive_category), plus `undeliverable`: no reply and a
#     delivery-error code in `undeliverable_codes`. A hard carrier rejection is
#     not a non-response: a reply rarely follows one of those codes, while
#     softer codes (4740, 4405, 4780) are followed by replies as often as a plain
#     non-response, so the default list holds only the hard ones.
#   * The path alphabet is one letter per label (.DISPOSITION_PATH_CODES).

# One letter per label, in funnel order. `undeliverable` sits beside
# `non_response`: both are "no reply", split by whether the send was rejected.
.DISPOSITION_PATH_CODES <- c(non_response = "N", undeliverable = "X", engaged = "E",
                             opted_in = "O", terminated = "T", ineligible = "I",
                             refused = "R", completed = "C", web_complete = "W")

# Carrier delivery-error codes that mean the number could not be reached (a
# reply rarely follows them).
.DISPOSITION_UNDELIVERABLE_CODES <- c("4700", "4701", "4702", "4720", "4750", "4753")

# Stored columns the sequence readers cannot do without; the rest of the funnel
# flags (web_complete, terminated, refused, ineligible) and `error` are optional,
# as on a projection published before they existed.
.DISPOSITION_SEQUENCE_REQUIRED <- c("phone", "campaign_id", "disposition_date",
                                    "engaged", "opted_in", "completed")

# disposition_steps() column groups (`columns =`), each a few window columns.
.DISPOSITION_STEP_GROUPS <- c("neighbors", "so_far", "run_length", "path_so_far", "history",
                              "context")

# The coarse letters the completion model reads a history in: terminated reads
# as engaged, ineligible as refused and a web complete as a complete, so a
# context cell is one of six letters and the per-year context tables stay
# dense. A SQL expression over a letter (or a path) of the full alphabet.
.disposition_coarse_sql <- function(expr) {
  sprintf("replace(replace(replace(%s, 'T', 'E'), 'I', 'R'), 'W', 'C')", expr)
}

# The context of a step: its previous three outcomes, newest first, in the
# coarse letters, padded with `_` (`___` on a first contact) -- the cell the
# completion model's context tables are keyed by. Read off the history path
# before this step, so it shares the one window sort.
.disposition_context_sql <- function() {
  before <- "substr(path_so_far, 1, length(path_so_far) - 1)"
  .disposition_coarse_sql(sprintf("rpad(reverse(right(%s, 3)), 3, '_')", before))
}

# The window columns for each requested group, over the base rows' letters. The
# history path is also built when only run_length or a pattern needs it.
# `prior_counts` is an internal group (the completion model's): the history
# group's running counts without its three last-date windows.
.disposition_step_windows <- function(groups, need_path) {
  in_codes <- function(codes) {
    sprintf("CAST(sum(CAST(s160_code IN (%s) AS INTEGER)) OVER w AS INTEGER)",
            paste0("'", codes, "'", collapse = ", "))
  }
  # the date of the last step BEFORE this one with one of these letters (NULL: none yet)
  last_date <- function(codes, name) {
    sprintf("max(CASE WHEN s160_code IN (%s) THEN disposition_date END) OVER w_prior AS %s",
            paste0("'", codes, "'", collapse = ", "), name)
  }
  c("CAST(row_number() OVER w AS INTEGER) AS step",
    "CAST(count(*) OVER (PARTITION BY phone) AS INTEGER) AS n_campaigns",
    if ("neighbors" %in% groups) {
      c("lag(s160_code) OVER w AS s160_prev", "lead(s160_code) OVER w AS s160_next",
        "lag(disposition_date) OVER w AS s160_prev_date",
        "lead(disposition_date) OVER w AS next_date")
    },
    if ("so_far" %in% groups) {
      c(paste(in_codes(c("C", "W")), "AS completes_so_far"),
        paste(in_codes(c("N", "X")), "AS non_responses_so_far"),
        paste(in_codes(c("E", "O", "T", "I", "R", "C", "W")), "AS replies_so_far"))
    },
    if (any(c("history", "prior_counts") %in% groups)) {
      # running counts INCLUDING this step; the select subtracts this step's own letter
      c(paste(in_codes(c("C", "W")), "AS s160_comp_sf"),
        paste(in_codes(c("E", "O", "T", "I", "R", "C", "W")), "AS s160_reply_sf"))
    },
    if ("history" %in% groups) {
      c(last_date(c("E", "O", "T", "I", "R", "C", "W"), "s160_last_reply_date"),
        last_date(c("C", "W"), "s160_last_complete_date"),
        last_date("X", "s160_last_undeliverable_date"))
    },
    if (need_path) "string_agg(s160_code, '') OVER w AS path_so_far")
}

# The history group: what came BEFORE this step (prior_* exclude it), the
# segment a list-screening rule reads off it, and the recency columns: how
# long since the last reply / complete / undeliverable. Segments, first match wins: first_contact
# (step 1), completer_2_plus / completer_1 (prior completes), replied (a prior
# reply that never completed), unanswered_3_plus / unanswered_1_2 (only
# non-responses or undeliverables so far).
# A steps_since_* column is the trailing run of OTHER letters on the history
# before this step (path_so_far minus its last letter): rtrim() strips them, and
# an empty remainder means the event never happened (NULL). The days_since_*
# columns subtract the last such step's date (a w_prior window max).
.disposition_step_history_sql <- function() {
  comp  <- "(s160_comp_sf - CAST(s160_code IN ('C', 'W') AS INTEGER))"
  reply <- "(s160_reply_sf - CAST(s160_code NOT IN ('N', 'X') AS INTEGER))"
  unans <- sprintf("(step - 1 - %s)", reply)
  before <- "substr(path_so_far, 1, length(path_so_far) - 1)"
  steps_since <- function(other, name) {
    sprintf(paste0("CASE WHEN rtrim(%1$s, '%2$s') = '' THEN NULL ",
                   "ELSE CAST(length(%1$s) - length(rtrim(%1$s, '%2$s')) AS INTEGER) END AS %3$s"),
            before, other, name)
  }
  days_since <- function(col, name) {
    sprintf("CAST(disposition_date - %s AS INTEGER) AS %s", col, name)
  }
  c(sprintf("CAST(%s AS INTEGER) AS prior_completes", comp),
    sprintf("CAST(%s AS INTEGER) AS prior_replies", reply),
    sprintf("CAST(%s AS INTEGER) AS prior_unanswered", unans),
    sprintf(paste0("CASE WHEN step = 1 THEN 'first_contact' ",
                   "WHEN %1$s >= 2 THEN 'completer_2_plus' WHEN %1$s = 1 THEN 'completer_1' ",
                   "WHEN %2$s > 0 THEN 'replied' ",
                   "WHEN %3$s >= 3 THEN 'unanswered_3_plus' ELSE 'unanswered_1_2' END AS history_segment"),
            comp, reply, unans),
    steps_since("NX", "steps_since_reply"),
    steps_since("NXEOTIR", "steps_since_complete"),
    steps_since("NEOTIRCW", "steps_since_undeliverable"),
    days_since("s160_last_reply_date", "days_since_reply"),
    days_since("s160_last_complete_date", "days_since_complete"),
    days_since("s160_last_undeliverable_date", "days_since_undeliverable"))
}

# neighbor_columns: stored columns whose previous / next campaign value is
# wanted (prev_<col>, next_<col>). The identity columns already have their own
# neighbors (step, gap_*_days, next_date), and a generated name must not shadow
# a stored column. Returned unchanged, or NULL for none.
.disposition_check_neighbors <- function(neighbor_columns, cols, groups, fn) {
  if (is.null(neighbor_columns)) return(NULL)
  if (!is.character(neighbor_columns) || anyNA(neighbor_columns) || anyDuplicated(neighbor_columns) > 0L) {
    stop_s160(sprintf("`neighbor_columns` must be NULL or a character vector of distinct column names%s.",
                      .not_got(neighbor_columns)), fn = fn)
  }
  identity <- neighbor_columns[tolower(neighbor_columns) %in% c("phone", "campaign_id", "disposition_date")]
  if (length(identity) > 0L) {
    stop_s160(sprintf(paste0("`neighbor_columns` cannot include %s: the history already places ",
                             "each campaign (step, next_date, gap_prev_days, gap_next_days)."),
                      paste(identity, collapse = ", ")), fn = fn)
  }
  unknown <- setdiff(neighbor_columns, cols)
  if (length(unknown) > 0L) {
    stop_s160(sprintf("`neighbor_columns` names column(s) the dataset does not have: %s. It has: %s.",
                      paste(unknown, collapse = ", "), .preview_values(cols)), fn = fn)
  }
  # A generated name must not repeat a stored column, or a prev_/next_ column
  # the neighbors group adds when requested (neighbor "date" beside next_date).
  added <- .disposition_neighbor_names(neighbor_columns)
  taken <- c(cols, if ("neighbors" %in% groups) c("prev_label", "next_label", "next_date"))
  clash <- added[tolower(added) %in% tolower(taken)]
  if (length(clash) > 0L) {
    stop_s160(sprintf(paste0("`neighbor_columns` would add column(s) the result already has: %s. ",
                             "Rename the stored column(s) in a copy of the file."),
                      paste(clash, collapse = ", ")), fn = fn)
  }
  neighbor_columns
}

.disposition_neighbor_names <- function(neighbor_columns) {
  c(rbind(paste0("prev_", neighbor_columns), paste0("next_", neighbor_columns)))
}

# lag / lead of each neighbor column over the shared window (no extra sort).
.disposition_neighbor_windows <- function(con, neighbor_columns) {
  if (length(neighbor_columns) == 0L) return(NULL)
  q <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  c(rbind(sprintf("lag(%s) OVER w AS %s", q(neighbor_columns), q(paste0("prev_", neighbor_columns))),
          sprintf("lead(%s) OVER w AS %s", q(neighbor_columns), q(paste0("next_", neighbor_columns)))))
}

# The output projection: identity, label, history, then the stored columns. A
# run length is read off the history path -- its trailing run of the current
# letter -- so it shares the one window sort (a separate (phone, label)
# partition, the classic gaps-and-islands form, cost ~4x more on prod).
.disposition_step_select <- function(con, cols, groups, pattern_re, neighbor_columns = NULL) {
  q <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  stored <- setdiff(cols, c("phone", "campaign_id", "disposition_date"))
  c("phone", "campaign_id", "disposition_date", "step", "n_campaigns",
    paste(.disposition_label_name_sql("s160_code"), "AS label"),
    "completion_known",
    if ("neighbors" %in% groups) {
      c(paste(.disposition_label_name_sql("s160_prev"), "AS prev_label"),
        paste(.disposition_label_name_sql("s160_next"), "AS next_label"),
        "next_date",
        "CAST(disposition_date - s160_prev_date AS INTEGER) AS gap_prev_days",
        "CAST(next_date - disposition_date AS INTEGER) AS gap_next_days")
    },
    if (length(neighbor_columns) > 0L) q(.disposition_neighbor_names(neighbor_columns)),
    if ("so_far" %in% groups) c("completes_so_far", "non_responses_so_far", "replies_so_far"),
    if ("run_length" %in% groups) {
      "CAST(length(path_so_far) - length(rtrim(path_so_far, s160_code)) AS INTEGER) AS run_length"
    },
    if ("path_so_far" %in% groups) "path_so_far",
    if ("history" %in% groups) .disposition_step_history_sql(),
    if ("context" %in% groups) paste(.disposition_context_sql(), "AS context"),
    if (!is.null(pattern_re)) {
      paste0("regexp_matches(path_so_far, ", DBI::dbQuoteString(con, pattern_re),
             ") AS history_matched")
    },
    q(stored))
}

#' Read the disposition dataset as a sequence: one row per campaign, with history
#'
#' Orders each phone's campaigns in time and returns one row per
#' \code{(phone, campaign_id)} carrying the phone's \emph{history up to that
#' campaign}: its position (\code{step} of \code{n_campaigns}), a single
#' disposition \code{label}, the previous and next labels and the gaps between
#' them, running counts, the length of the current run (e.g. the third
#' non-response in a row), and the history itself as a letter path. Filter, group
#' and summarise it with \pkg{dplyr} verbs to answer questions that depend on
#' what came before -- "after three non-responses in a row, how often does the
#' next campaign get a reply?", "once a number has completed twice, how often
#' does it complete again?". For one row per phone and sequence patterns
#' (non-response, then engaged, then completed) see
#' \code{\link{disposition_paths}}.
#'
#' \strong{A lazy table.} The result is a \pkg{dbplyr} table that DuckDB
#' computes when you \code{collect()} (or print) it, not a data frame: the history
#' columns are window functions over every campaign a phone has, which on the
#' production projection only fit inside DuckDB. Aggregate before
#' you \code{collect()} -- the full table is about 16 GB in R -- or scope it with
#' \code{phones} / \code{sample_frac}. Rows come back unordered; \code{arrange()}
#' when order matters. All tables share one session-wide DuckDB connection (see
#' Resources).
#'
#' \strong{Order and labels.} A phone's campaigns are ordered by
#' \code{disposition_date} (its last send in that campaign), then
#' \code{campaign_id} compared as a number; a missing date or id counts as the
#' oldest, as in \code{\link{disposition_summary}}, so a phone's last step is its
#' \code{latest_disposition}. Campaigns that overlapped in time
#' can come out in either order (0.5\% of consecutive pairs on prod, all under 30
#' days apart). Each row gets the furthest step it reached, by the precedence of
#' \code{\link{disposition_summary}}'s \code{latest_disposition}:
#' \code{web_complete}, \code{completed}, \code{refused}, \code{ineligible},
#' \code{terminated}, \code{opted_in}, \code{engaged}, then \code{undeliverable}
#' (no reply and a delivery-error code in \code{undeliverable_codes}) and
#' \code{non_response}. Path letters: N non_response, X undeliverable, E
#' engaged, O opted_in, T terminated, I ineligible, R refused, C completed, W
#' web_complete.
#'
#' \strong{History segments.} With \code{columns = "history"} each row also
#' carries what the phone did \emph{before} this campaign -- \code{prior_completes},
#' \code{prior_replies} (any label but non_response / undeliverable) and
#' \code{prior_unanswered} (the rest, so \code{step - 1 - prior_replies}) -- and
#' \code{history_segment}, the first of: \code{first_contact} (step 1),
#' \code{completer_2_plus}, \code{completer_1}, \code{replied} (replied before,
#' never completed), \code{unanswered_3_plus}, \code{unanswered_1_2}. A common
#' rule suppresses \code{unanswered_3_plus} and a recent \code{undeliverable}
#' and sends the completers first; group by the segment to measure its rates.
#'
#' \strong{Recency.} The same group carries how long ago the phone last did
#' something, counted both in campaigns and in days: \code{steps_since_reply},
#' \code{steps_since_complete}, \code{steps_since_undeliverable} (how many
#' campaigns since the last reply / complete / undeliverable before this one;
#' \code{0} means the previous campaign was one) and \code{days_since_reply},
#' \code{days_since_complete}, \code{days_since_undeliverable} (this step's
#' \code{disposition_date} minus that campaign's). All \code{NA} until the
#' event has happened.
#'
#' \strong{Context.} With \code{columns = "context"} each row carries
#' \code{context}: the phone's previous three outcomes, newest first, as three
#' coarse letters padded with \code{_} (\code{___} on a first contact,
#' \code{N__} after one no-reply, \code{CEN} after a complete, an engaged and a
#' no-reply before that). The coarse letters fold the path alphabet to six:
#' \code{T} (terminated) reads as \code{E}, \code{I} (ineligible) as \code{R}
#' and \code{W} (web complete) as \code{C}. This is the cell the completion
#' model's context tables are keyed by (\code{\link{completion_model_fit}}): group
#' by it to reproduce P(complete | context) on a newer projection, or to see
#' which cell a number in a sample is scored on.
#'
#' \strong{Neighbor columns.} \code{neighbor_columns} carries any stored
#' column across to the previous and next campaign, for transitions that depend
#' on what changed between them: a switch of \code{survey_mode}, the same
#' \code{tracker_client} or \code{tracker_topic} again.
#' \code{tracker_client} / \code{tracker_topic} are empty before 2024 (and
#' \code{tracker_client} on some recent rows), \code{tracker_topic} has four
#' broad values, and \code{carrier} comes from each list's lookup, which
#' changed twice in 2026: compare it across campaigns only within lists from
#' May 2026 on.
#'
#' \strong{Scope.} Every filter applies \emph{before} the history is computed,
#' as in every other disposition reader: \code{phones} and \code{sample_frac}
#' keep or drop whole phones, and \code{campaign_ids}, \code{date_from} and
#' \code{date_to} decide which campaigns exist at all -- with
#' \code{date_from = "2025-01-01"} a phone's first 2025 campaign is its step 1
#' and its history starts there. To keep some rows but give them their full
#' history (2026 sends, with every earlier campaign counted), filter the result
#' instead: \code{filter(disposition_date >= as.Date("2026-01-01"))}.
#'
#' \strong{Resources.} The connection uses a 4 GB DuckDB \code{memory_limit} and
#' spills to disk past it (a full-projection query can spill ~20 GB, freed when it
#' finishes); set \code{options(survey160r.duckdb_memory = "8GB")} or
#' \code{options(survey160r.duckdb_temp_dir = "/path")} to change either (a new
#' spill directory applies only before the session's first spill; each session
#' spills into its own subdirectory, removed when the session ends). Measured on
#' the production projection: a full-scan aggregate in ~16 s at
#' ~5.5 GB of process memory; a phone-scoped table (up to millions of phones) in
#' 1-5 s. Each distinct \code{phones} vector stays registered on the connection
#' for the session, so later operations on the table keep working.
#'
#' @param dataset Path to the disposition Parquet file, e.g. from
#'   \code{\link{disposition_pull}}.
#' @param phones Optional character vector of phone numbers; keeps only these
#'   phones (all of their campaigns), and a number with no campaign has no row
#'   (\code{\link{disposition_summary}} returns every valid, distinct input
#'   number instead, before status filtering and pagination).
#'   Matched digit-normalized, as the other readers do. \code{character(0)}
#'   returns no rows.
#' @param sample_frac Optional number in (0, 1]: keep a deterministic sample of
#'   that fraction of phones (by an md5 of the phone, so the same phones on every
#'   machine and DuckDB version). \code{0.1} makes full-projection exploration
#'   ~10x cheaper; the 10\% sample matches the full data to within 0.02 points
#'   on prod.
#' @param campaign_ids Optional vector of campaign ids: only these campaigns
#'   are read, so the history covers only them (see Scope).
#' @param date_from,date_to Optional inclusive bounds on \code{disposition_date},
#'   each a \code{Date} or a \code{"YYYY-MM-DD"} string (a date-time is
#'   converted to its America/New_York day, so \code{2026-02-01 02:00 UTC} is
#'   January 31): only campaigns in range are read, so the history starts at
#'   \code{date_from} (see Scope).
#' @param columns Which history column groups to compute: any of
#'   \code{"neighbors"} (\code{prev_label}, \code{next_label}, \code{next_date},
#'   \code{gap_prev_days}, \code{gap_next_days}), \code{"so_far"}
#'   (\code{completes_so_far}, \code{non_responses_so_far}, \code{replies_so_far},
#'   counting this step), \code{"run_length"} (how many steps in a row, ending
#'   here, have this label), \code{"path_so_far"} (the history as letters,
#'   ending with this step) and \code{"history"} (\code{prior_*} counts, the
#'   \code{history_segment}, and the \code{steps_since_*} / \code{days_since_*}
#'   recency columns; see below) and \code{"context"} (the previous three
#'   outcomes, newest first, in the coarse letters; see below). The default is
#'   the first four; \code{"history"} and \code{"context"} are opt-in. Each costs
#'   a few seconds on the full projection.
#' @param neighbor_columns Optional character vector of stored columns other
#'   than \code{phone}, \code{campaign_id} and \code{disposition_date}, e.g.
#'   \code{c("survey_mode", "tracker_topic")}: adds \code{prev_<col>} and
#'   \code{next_<col>}, the value on the phone's previous / next campaign
#'   (\code{NA} at the ends). Compare them with \pkg{dplyr} --
#'   \code{tracker_topic == next_tracker_topic} is \code{NA} when either side
#'   is unknown. Each column costs ~3 s on the full projection.
#' @param pattern Optional sequence pattern (see \code{\link{disposition_paths}}
#'   for the syntax); adds \code{history_matched}: whether the history up to and
#'   including this step matches. Use a \code{$} anchor for "history ends with",
#'   e.g. \code{"non_response|undeliverable{3,}$"}.
#' @param terminal How to label an unsplit \code{terminated} row (terminated but
#'   neither refused nor ineligible): \code{"raw"} (default) keeps
#'   \code{terminated}, matching \code{\link{disposition_summary}};
#'   \code{"engaged"} labels it by the step it reached (\code{opted_in} or
#'   \code{engaged}). On prod most of these rows come from a producer change in
#'   August 2026 rather than respondent behaviour.
#' @param undeliverable_codes Delivery-error codes that make a no-reply row
#'   \code{undeliverable} rather than \code{non_response}. Defaults to the hard
#'   carrier rejections (4700, 4701, 4702, 4720, 4750, 4753); \code{NULL} labels
#'   every no-reply row \code{non_response}, as the summary does.
#' @return A lazy \pkg{dbplyr} table, one row per \code{(phone, campaign_id)}:
#'   \code{phone}, \code{campaign_id}, \code{disposition_date}, \code{step},
#'   \code{n_campaigns}, \code{label}, \code{completion_known} (\code{FALSE} where
#'   \code{completed} is \code{NA}, i.e. text-to-web external, where a complete is
#'   never reported), the requested history columns (with
#'   \code{prev_<col>} / \code{next_<col>} after the neighbors group), \code{history_matched} (with
#'   \code{pattern}), then every other stored column (funnel flags,
#'   \code{survey_mode}, \code{carrier}, \code{error}, \code{tracker_*}, ...).
#' @seealso \code{\link{disposition_paths}} (one row per phone),
#'   \code{\link{disposition_summary}} (per-phone counts),
#'   \code{\link{disposition_records}} (the stored rows)
#' @examples
#' \dontrun{
#' library(dplyr)
#' dataset <- disposition_pull()
#' steps <- disposition_steps(dataset, sample_frac = 0.1)
#'
#' # Completion of sends in 2026 by what the number did before: read every
#' # campaign (the history spans 2021+), then keep the 2026 rows
#' disposition_steps(dataset, columns = "history") |>
#'   filter(disposition_date >= as.Date("2026-01-01"), completion_known) |>
#'   group_by(history_segment) |>
#'   summarise(sends = n(), completion = mean(as.integer(
#'     label %in% c("completed", "web_complete")), na.rm = TRUE)) |>
#'   arrange(desc(completion)) |>
#'   collect()
#'
#' # Completion by how many campaigns ago the number last completed
#' disposition_steps(dataset, columns = "history") |>
#'   filter(disposition_date >= as.Date("2026-01-01"), completion_known,
#'          !is.na(steps_since_complete)) |>
#'   group_by(since = pmin(steps_since_complete, 5L, na.rm = TRUE)) |>
#'   summarise(sends = n(), completion = mean(as.integer(
#'     label %in% c("completed", "web_complete")), na.rm = TRUE)) |>
#'   arrange(since) |>
#'   collect()
#'
#' # Reply on the next campaign after a no-reply, by channel switch (2025+
#' # campaigns only)
#' disposition_steps(dataset, date_from = "2025-01-01", columns = "neighbors",
#'                   neighbor_columns = "survey_mode") |>
#'   filter(label == "non_response", !is.na(next_label)) |>
#'   group_by(survey_mode, next_survey_mode) |>
#'   summarise(n = n(), reply_next = mean(as.integer(
#'     !next_label %in% c("non_response", "undeliverable")), na.rm = TRUE)) |>
#'   collect()
#'
#' # Reply rate on the next campaign after k non-responses in a row
#' steps |>
#'   filter(label == "non_response", !is.na(next_label)) |>
#'   group_by(run = pmin(run_length, 5L, na.rm = TRUE)) |>
#'   summarise(n = n(), reply_next = mean(as.integer(
#'     !next_label %in% c("non_response", "undeliverable")), na.rm = TRUE)) |>
#'   arrange(run) |>
#'   collect()
#' }
#' @export
disposition_steps <- function(dataset, phones = NULL, sample_frac = NULL,
                              campaign_ids = NULL, date_from = NULL, date_to = NULL,
                              columns = c("neighbors", "so_far", "run_length", "path_so_far"),
                              pattern = NULL,
                              terminal = c("raw", "engaged"),
                              undeliverable_codes = .DISPOSITION_UNDELIVERABLE_CODES,
                              neighbor_columns = NULL) {
  fn <- "disposition_steps"
  terminal <- match.arg(terminal)
  .disposition_lazy_check(fn)
  if (!is.null(columns) &&
        (!is.character(columns) || anyNA(columns) || !all(columns %in% .DISPOSITION_STEP_GROUPS))) {
    stop_s160(sprintf("`columns` must be NULL or any of %s%s.",
                      paste(.DISPOSITION_STEP_GROUPS, collapse = ", "), .not_got(columns)),
              fn = fn)
  }
  pattern_re <- if (!is.null(pattern)) .disposition_pattern_compile(pattern, fn)
  codes <- .disposition_check_terminal_codes(undeliverable_codes, fn)
  sample_frac <- .disposition_check_sample_frac(sample_frac, fn)
  phones <- .disposition_request_phones(phones)

  con <- .disposition_lazy_con()
  source <- .disposition_lazy_source(con, dataset, fn)
  neighbor_columns <- .disposition_check_neighbors(neighbor_columns, source$cols, columns, fn)
  scope <- .disposition_scope_filters(con, source, campaign_ids, date_from, date_to, fn)
  windowed <- .disposition_steps_windowed_sql(con, source, phones, sample_frac, columns, terminal,
                                              codes, scope, !is.null(pattern_re), neighbor_columns)
  sql <- paste0("SELECT ", paste(.disposition_step_select(con, source$cols, columns, pattern_re,
                                                         neighbor_columns),
                                 collapse = ", "),
                " FROM (", windowed, ")")
  dplyr::tbl(con, dbplyr::sql(sql))
}

# The steps table before its output projection: the base rows (label letter
# `s160_code`, every stored column, `completion_known`) with the window columns
# of the requested groups and the neighbor lags. disposition_steps() selects its
# public columns from it; the completion model (R/completion_model.R) its
# history features, so both read one definition of a phone's history.
.disposition_steps_windowed_sql <- function(con, source, phones, sample_frac, columns, terminal,
                                            codes, pre, need_pattern = FALSE,
                                            neighbor_columns = NULL) {
  base <- .disposition_base_sql(con, source, phones, sample_frac,
                                .disposition_label_sql(con, source, terminal, codes), pre = pre)
  need_path <- any(c("run_length", "path_so_far", "history", "context") %in% columns) ||
    need_pattern
  paste0("SELECT *, ", paste(c(.disposition_step_windows(columns, need_path),
                               .disposition_neighbor_windows(con, neighbor_columns)),
                             collapse = ", "),
         " FROM (", base, ") ", .disposition_sequence_window(source))
}
