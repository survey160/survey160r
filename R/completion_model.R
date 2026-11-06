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

# The factor levels the two fits use. The completion model never sees
# "T2W external" (completion is not recorded there); the LOI reference band is
# the commonest one, so the other bands read as multipliers against it.
.COMPLETION_MODES <- c("Live", "T2W", "T2W external")
.COMPLETION_TOPICS <- c("Brand", "Electoral", "Policy", "unknown")
.COMPLETION_LOI_BREAKS <- c(0, 5, 10, 15, 20, 31)
.COMPLETION_LOI_REFERENCE <- "(10,15]"

# Mode: the tracker's, falling back to the app's survey_mode (sms -> Live,
# t2w -> T2W); a text-to-web external survey_mode always wins, since the
# tracker calls those "T2W" and completion is never recorded for them.
.completion_mode <- function(tracker_mode, survey_mode) {
  fallback <- c(sms = "Live", t2w = "T2W", t2w_external = "T2W external")
  out <- unname(fallback[as.character(survey_mode)])
  tracked <- !is.na(tracker_mode) & tracker_mode %in% .COMPLETION_MODES
  out[tracked] <- tracker_mode[tracked]
  external <- !is.na(survey_mode) & survey_mode == "t2w_external"
  out[external] <- "T2W external"
  out
}

.completion_topic <- function(x) {
  x <- as.character(x)
  ifelse(!is.na(x) & x %in% .COMPLETION_TOPICS, x, "unknown")
}

# LOI in minutes -> band factor with the reference band first and `unknown`
# (missing, zero, negative or over 31 minutes) last. A zero LOI is a tracker
# placeholder, not a survey length, so it is unknown despite the first band's
# closed lower bound.
.completion_loi_band <- function(loi) {
  loi <- as.numeric(loi)
  loi[!is.na(loi) & loi <= 0] <- NA_real_
  band <- as.character(cut(loi, .COMPLETION_LOI_BREAKS, include.lowest = TRUE))
  band[is.na(band)] <- "unknown"
  factor(band, levels = .completion_loi_levels())
}

.completion_loi_levels <- function() {
  all <- levels(cut(1, .COMPLETION_LOI_BREAKS, include.lowest = TRUE))
  c(.COMPLETION_LOI_REFERENCE, setdiff(all, .COMPLETION_LOI_REFERENCE), "unknown")
}

# Logit with the rate clamped away from 0 and 1 (a 0-complete sample would
# otherwise score -Inf).
.completion_logit <- function(p) {
  stats::qlogis(pmin(pmax(p, 1e-4), 1 - 1e-4))
}

# P(complete | previous three letters), one table per calendar year from the
# second year in the data to the year after the last: the table for year Y is
# estimated on the known-completion rows dated before Y, so a campaign scored
# with its own year's table never sees its own rows, and the last table (every
# row) scores a survey planned for after the data ends.
.completion_context_tables <- function(con, history_sql) {
  years <- DBI::dbGetQuery(con, paste0(
    "SELECT min(year(disposition_date)) AS first, max(year(disposition_date)) AS last FROM (",
    history_sql, ")"))
  sql <- paste0(
    "SELECT y.year, h.context, avg(CAST(h.letter = 'C' AS DOUBLE)) AS p_complete, ",
    "CAST(count(*) AS INTEGER) AS n ",
    "FROM (", history_sql, ") h ",
    "JOIN (SELECT CAST(range AS INTEGER) AS year FROM range(", years$first + 1L, ", ",
    years$last + 2L, ")) y ON year(h.disposition_date) < y.year ",
    "WHERE h.completion_known GROUP BY 1, 2 ORDER BY 1, 2")
  out <- DBI::dbGetQuery(con, sql)
  out$year <- as.integer(out$year)
  out
}

# One row per campaign: dates, counts, the survey dimensions (tracker values
# resolved to the model's levels) and the sample-history features scored on the
# context table of the campaign's start year (a context unseen before that year
# takes the year's first-contact rate; a campaign in the first year of data,
# with no table, gets NA).
.completion_campaign_table <- function(con, history_sql, contexts) {
  ctx <- .disposition_lazy_register(con, "contexts", contexts)
  sql <- paste0(
    "WITH h AS (", history_sql, "), ",
    "c AS (SELECT campaign_id, min(disposition_date) AS first_date FROM h GROUP BY 1) ",
    "SELECT h.campaign_id, min(h.disposition_date) AS first_date, ",
    "max(h.disposition_date) AS last_date, CAST(count(*) AS INTEGER) AS sent, ",
    "CAST(sum(CAST(h.opted AS INTEGER)) AS INTEGER) AS optins, ",
    "CAST(sum(CAST(h.letter = 'C' AS INTEGER)) AS INTEGER) AS completes, ",
    "bool_and(h.completion_known) AS completion_known, ",
    "any_value(h.survey_mode) AS survey_mode, any_value(h.tracker_mode) AS tracker_mode, ",
    "any_value(h.tracker_topic) AS tracker_topic, any_value(h.tracker_loi) AS tracker_loi, ",
    "avg(coalesce(ctx.p_complete, first.p_complete)) AS mean_p, ",
    "avg(CAST(h.prior_completes > 0 AS DOUBLE)) AS share_prior_completers, ",
    "avg(CAST(h.n_prior >= 3 AND h.prior_replies = 0 AS DOUBLE)) AS share_cold ",
    "FROM h JOIN c ON h.campaign_id = c.campaign_id ",
    "LEFT JOIN ", ctx, " ctx ON ctx.year = year(c.first_date) AND ctx.context = h.context ",
    "LEFT JOIN ", ctx, " first ON first.year = year(c.first_date) AND first.context = '___' ",
    "GROUP BY 1 ORDER BY 1")
  d <- DBI::dbGetQuery(con, sql)
  data.frame(
    campaign_id = d$campaign_id, first_date = as.Date(d$first_date),
    last_date = as.Date(d$last_date), year = as.integer(format(as.Date(d$first_date), "%Y")),
    sent = d$sent, optins = d$optins, completes = d$completes,
    completion_known = d$completion_known,
    mode = .completion_mode(d$tracker_mode, d$survey_mode),
    topic = .completion_topic(d$tracker_topic),
    loi = suppressWarnings(as.numeric(d$tracker_loi)),
    sample_score = .completion_logit(d$mean_p),
    share_prior_completers = d$share_prior_completers, share_cold = d$share_cold,
    stringsAsFactors = FALSE)
}
