# Completion estimate: fit two campaign-level linear models (opt-ins per sent,
# completes per sent) from the disposition projection. The per-row history
# (letter, previous three letters, prior counts) is one DuckDB window pass over
# the lazy-reader plumbing in disposition_steps.R; a second pass groups it to one
# row per campaign; the fits are ordinary least squares on the logit of each
# rate. completion_estimate() (completion_estimate.R) scores a planned survey on
# the same context tables.

# The per-row history: one row per stored (phone, campaign) with the collapsed
# letter (T -> E, I -> R, W -> C: the model reads terminated as engaged,
# ineligible as refused and a web complete as a complete), whether the row is an
# opt-in (letter O or C), the running counts of prior contacts / completes /
# replies, the previous three letters as `context` (`_` when there is none) and
# whether the row is the phone's latest, plus the previous row's date and
# campaign (`prev_date`, `prev_campaign`: NULL on a first contact). The optional tracker columns and
# survey_mode come through as NULL when the file lacks them. `phones` scopes the
# read to a sample (the estimate path); NULL reads every phone (the fit path).
# `before` (a Date) keeps only rows dated before it: the history a planned
# start would see, so a past campaign's own outcome never scores its sample.
.completion_history_sql <- function(con, source, phones = NULL, before = NULL) {
  q <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  pre <- if (!is.null(before)) {
    paste("disposition_date <", DBI::dbQuoteString(con, format(before, "%Y-%m-%d")))
  }
  base <- .disposition_base_sql(
    con, source, phones, NULL,
    .disposition_label_sql(con, source, "raw", .DISPOSITION_UNDELIVERABLE_CODES), pre = pre)
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
    "lead(phone) OVER w IS NULL AS is_last, ",
    "lag(disposition_date) OVER w AS prev_date, lag(campaign_id) OVER w AS prev_campaign ",
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

# P(complete | previous three letters, survey mode) and P(reply | ...), one
# table per calendar year from the second year in the data to the year after
# the last: the table for year Y is estimated on the rows dated before Y, so a
# campaign scored with its own year's table never sees its own rows, and the
# last table (every row) scores a survey planned for after the data ends.
# `p_complete` averages the known-completion rows only (NA in a cell with none,
# which the lookups treat as a missing cell); `p_reply` every row, so a
# text-to-web external campaign's history counts for it. Each year has a block
# per survey mode (sms, t2w: a context completes at a very different rate under
# a live texter and on the web) and a pooled block (`survey_mode` NA) the
# lookups fall back to.
.completion_context_tables <- function(con, history_sql, fn = NULL) {
  years <- DBI::dbGetQuery(con, paste0(
    "SELECT min(year(disposition_date)) AS first, max(year(disposition_date)) AS last FROM (",
    history_sql, ")"))
  if (is.na(years$first)) {
    stop_s160("the projection has no disposition rows to fit on.", fn = fn)
  }
  block <- function(mode_col, where) {
    paste0("SELECT y.year, h.context, ", mode_col, " AS survey_mode, ",
           "avg(CASE WHEN h.completion_known THEN CAST(h.letter = 'C' AS DOUBLE) END) AS p_complete, ",
           "avg(CAST(h.letter IN ('E', 'O', 'R', 'C') AS DOUBLE)) AS p_reply, ",
           "CAST(count(*) AS INTEGER) AS n ",
           "FROM (", history_sql, ") h ",
           "JOIN (SELECT CAST(range AS INTEGER) AS year FROM range(", years$first + 1L, ", ",
           years$last + 2L, ")) y ON year(h.disposition_date) < y.year", where,
           " GROUP BY 1, 2, 3")
  }
  sql <- paste0(block("CAST(h.survey_mode AS VARCHAR)", " WHERE h.survey_mode IS NOT NULL"),
                " UNION ALL ", block("NULL", ""), " ORDER BY 1, 2, 3")
  out <- DBI::dbGetQuery(con, sql)
  out$year <- as.integer(out$year)
  out$survey_mode <- as.character(out$survey_mode)
  out
}

# One probability column (`p_complete`, `p_reply`) for each (context, survey
# mode) on one year's table: the mode's own block, else the pooled block, else
# the pooled first-contact rate (a cell whose value is NA counts as missing).
.completion_context_lookup <- function(contexts, year, context, survey_mode,
                                       column = "p_complete") {
  ctx <- contexts[contexts$year == year, , drop = FALSE]
  key <- paste(ctx$context, ctx$survey_mode)
  values <- ctx[[column]]
  p <- values[match(paste(context, survey_mode), key)]
  pooled <- values[match(paste(context, NA), key)]
  p[is.na(p)] <- pooled[is.na(p)]
  p[is.na(p)] <- values[match(paste("___", NA), key)]
  p
}

# One row per campaign: dates, counts (sent, opt-ins, completes, replies,
# engaged), the survey dimensions (tracker values resolved to the model's
# levels) and the sample-history features scored on the context table of the
# campaign's start year with the lookup order of .completion_context_lookup (a
# campaign in the first year of data, with no table, gets NA): `sample_score`
# from `p_complete`, `reply_score` from `p_reply`. `years_since_contact` is the
# mean, over the numbers with a prior contact before the campaign's start (an
# overlapping campaign's later row does not count), of the years from that
# contact to the start;
# `prior_optin_score` the logit of the mean opt-in rate of the campaign
# each number was last in (campaigns started before this one). Both are NA when
# no number in the sample was contacted before.
.completion_campaign_table <- function(con, history_sql, contexts) {
  ctx <- .disposition_lazy_register(con, "contexts", contexts)
  sql <- paste0(
    "WITH h AS (", history_sql, "), ",
    "c AS (SELECT campaign_id, min(disposition_date) AS first_date, ",
    "avg(CAST(opted AS DOUBLE)) AS optin_rate FROM h GROUP BY 1) ",
    "SELECT h.campaign_id, min(h.disposition_date) AS first_date, ",
    "max(h.disposition_date) AS last_date, CAST(count(*) AS INTEGER) AS sent, ",
    "CAST(sum(CAST(h.opted AS INTEGER)) AS INTEGER) AS optins, ",
    "CAST(sum(CAST(h.letter = 'C' AS INTEGER)) AS INTEGER) AS completes, ",
    "CAST(sum(CAST(h.letter IN ('E', 'O', 'R', 'C') AS INTEGER)) AS INTEGER) AS replies, ",
    "CAST(sum(CAST(h.letter = 'E' AS INTEGER)) AS INTEGER) AS engaged, ",
    "bool_and(h.completion_known) AS completion_known, ",
    "any_value(h.survey_mode) AS survey_mode, any_value(h.tracker_mode) AS tracker_mode, ",
    "any_value(h.tracker_topic) AS tracker_topic, any_value(h.tracker_loi) AS tracker_loi, ",
    "avg(coalesce(own.p_complete, pooled.p_complete, first.p_complete)) AS mean_p, ",
    "avg(coalesce(own.p_reply, pooled.p_reply, first.p_reply)) AS mean_p_reply, ",
    "avg(CAST(h.prior_completes > 0 AS DOUBLE)) AS share_prior_completers, ",
    "avg(CAST(h.n_prior >= 3 AND h.prior_replies = 0 AS DOUBLE)) AS share_cold, ",
    "avg(CASE WHEN h.prev_date < c.first_date ",
    "THEN datediff('day', h.prev_date, c.first_date) END) / 365.25 AS years_since_contact, ",
    "avg(CAST(coalesce(h.prev_date < c.first_date AND h.prev_date >= c.first_date - INTERVAL ",
    .COMPLETION_RECENT_DAYS, " DAY, FALSE) AS DOUBLE)) AS share_recent_contact, ",
    "avg(pc.optin_rate) AS prior_optin_rate ",
    "FROM h JOIN c ON h.campaign_id = c.campaign_id ",
    "LEFT JOIN c pc ON pc.campaign_id = h.prev_campaign AND pc.first_date < c.first_date ",
    "LEFT JOIN ", ctx, " own ON own.year = year(c.first_date) AND own.context = h.context ",
    "AND own.survey_mode = CAST(h.survey_mode AS VARCHAR) ",
    "LEFT JOIN ", ctx, " pooled ON pooled.year = year(c.first_date) ",
    "AND pooled.context = h.context AND pooled.survey_mode IS NULL ",
    "LEFT JOIN ", ctx, " first ON first.year = year(c.first_date) AND first.context = '___' ",
    "AND first.survey_mode IS NULL ",
    "GROUP BY 1 ORDER BY 1")
  d <- DBI::dbGetQuery(con, sql)
  data.frame(
    campaign_id = d$campaign_id, first_date = as.Date(d$first_date),
    last_date = as.Date(d$last_date), year = as.integer(format(as.Date(d$first_date), "%Y")),
    sent = d$sent, optins = d$optins, completes = d$completes, replies = d$replies,
    engaged = d$engaged, completion_known = d$completion_known,
    mode = .completion_mode(d$tracker_mode, d$survey_mode),
    topic = .completion_topic(d$tracker_topic),
    loi = suppressWarnings(as.numeric(d$tracker_loi)),
    sample_score = .completion_logit(d$mean_p),
    reply_score = .completion_logit(d$mean_p_reply),
    share_prior_completers = d$share_prior_completers, share_cold = d$share_cold,
    years_since_contact = d$years_since_contact,
    share_recent_contact = d$share_recent_contact,
    prior_optin_score = .completion_logit(d$prior_optin_rate),
    stringsAsFactors = FALSE)
}

# A sample contact within this many days before a campaign's start is "recent";
# a campaign whose sample is mostly recent contacts (share over
# .COMPLETION_RESEND_SHARE) is a re-send of a list.
.COMPLETION_RECENT_DAYS <- 30L
.COMPLETION_RESEND_SHARE <- 0.5

# The two sample-size bands the band sd is also kept by: sends under / at or
# over `size_break` (small samples spread about twice as much).
.COMPLETION_SIZE_LABELS <- c("under", "over")
.completion_size_band <- function(n, size_break) {
  ifelse(n >= size_break, "over", "under")
}

# The registry of targets a model can fit, one entry per rate: the label
# print() uses, the campaign-table `count` the rate divides by sent, an optional
# `require` column every training campaign must have TRUE, the formula on the
# logit, the mode levels the model sees and the per-phone `score` it reads (the
# context-table column and the campaign / sample term it becomes). One shared
# design (.completion_design) sets the factor levels, so an estimate row builds
# the same model matrix the fit did. completion_model_fit(targets =) picks the
# entries to fit; the output columns of completion_estimate() follow them.
.COMPLETION_DESIGN_TERMS <- paste(
  "topic + loi_band + %s + share_prior_completers + share_cold + years_since_contact")
.COMPLETION_MODELS <- list(
  optin = list(
    label = "opt-in", count = "optins",
    formula = "logit ~ mode + sample_score + prior_optin_score",
    modes = .COMPLETION_MODES, score = c(context = "p_complete", term = "sample_score")),
  completion = list(
    label = "completion", count = "completes", require = "completion_known",
    formula = paste("logit ~ mode +", sprintf(.COMPLETION_DESIGN_TERMS, "sample_score")),
    modes = .COMPLETION_MODES[1:2], score = c(context = "p_complete", term = "sample_score")),
  reply = list(
    label = "reply", count = "replies",
    formula = paste("logit ~ mode +", sprintf(.COMPLETION_DESIGN_TERMS, "reply_score")),
    modes = .COMPLETION_MODES, score = c(context = "p_reply", term = "reply_score")),
  engage = list(
    label = "engagement", count = "engaged",
    formula = paste("logit ~ mode +", sprintf(.COMPLETION_DESIGN_TERMS, "reply_score")),
    modes = .COMPLETION_MODES, score = c(context = "p_reply", term = "reply_score")))
.COMPLETION_DEFAULT_TARGETS <- c("optin", "completion")

# Band widths on the logit: the 50% and 95% multiplicative bands are
# p x/÷ exp(z * residual sd).
.COMPLETION_Z <- c(lo50 = -0.674, hi50 = 0.674, lo95 = -1.96, hi95 = 1.96)

# The training rows of one model: campaigns with enough sends, started before
# `train_until`, settled (last activity `settle_days` before the data end), with
# a context table for their year (the target's score term not NA), a resolved
# mode among the target's levels, the target's `require` column TRUE (known
# completion for the completion model) and at least one of its count (zero
# completes is an unrecorded campaign). Adds `rate` (the count per sent) and
# `logit`.
.completion_fit_rows <- function(ct, target = names(.COMPLETION_MODELS), train_until, min_sent,
                                 settle_days, data_end) {
  target <- match.arg(target)
  spec <- .COMPLETION_MODELS[[target]]
  count <- ct[[spec$count]]
  keep <- ct$sent >= min_sent & ct$first_date < train_until &
    ct$last_date <= data_end - settle_days & !is.na(ct[[spec$score[["term"]]]]) &
    ct$mode %in% spec$modes & count > 0
  if (!is.null(spec$require)) keep <- keep & ct[[spec$require]]
  rows <- ct[keep & !is.na(keep), , drop = FALSE]
  rows$rate <- count[keep & !is.na(keep)] / rows$sent
  rows$logit <- .completion_logit(rows$rate)
  rownames(rows) <- NULL
  rows
}

# Set the factor columns a model's formula reads, with the levels fixed at fit
# time so an estimate row (one level each) still expands to every column.
.completion_design <- function(rows, target) {
  rows$mode <- factor(rows$mode, levels = .COMPLETION_MODELS[[target]]$modes)
  rows$topic <- factor(.completion_topic(rows$topic), levels = .COMPLETION_TOPICS)
  rows$loi_band <- .completion_loi_band(rows$loi)
  rows
}

# A numeric term NA on a row (a sample with no prior contact) takes the
# training mean of that term, at fit time and on an estimate row.
.completion_fill_means <- function(design, means) {
  for (term in intersect(names(means), names(design))) {
    design[[term]][is.na(design[[term]])] <- means[[term]]
  }
  design
}

# Fit on the explicit model matrix (not a formula on the frame: lm() drops a
# factor level no training row has, and the model keeps every level as a term).
.completion_fit_lm <- function(rows, target, means) {
  design <- .completion_fill_means(.completion_design(rows, target), means)
  x <- .completion_model_matrix(.COMPLETION_MODELS[[target]]$formula, design)
  fit <- stats::lm(design$logit ~ x - 1)
  names(fit$coefficients) <- colnames(x)
  fit
}

.completion_model_matrix <- function(formula, design) {
  stats::model.matrix(stats::as.formula(sub("^logit", "", formula)), design)
}

# The coefficient table the model object stores instead of the lm: every term
# of the model matrix, with an aliased term (a level absent from the training
# rows) at estimate 0 and no standard error, so an estimate on that level is
# visible rather than impossible.
.completion_coefficients <- function(fit) {
  estimate <- stats::coef(fit)
  se <- summary(fit)$coefficients[, "Std. Error"]
  names(se) <- sub("^x", "", names(se))
  data.frame(term = names(estimate),
             estimate = ifelse(is.na(estimate), 0, estimate),
             std_error = unname(se[names(estimate)]),
             stringsAsFactors = FALSE, row.names = NULL)
}

# The training mean of each numeric term and the training share of each factor
# level (names `mode.T2W`, `topic.Brand`, `loi_band.(10,15]`): the centre the
# estimate's multipliers are read against.
.completion_training_means <- function(rows, target) {
  d <- .completion_design(rows, target)
  terms <- attr(stats::terms(stats::as.formula(.COMPLETION_MODELS[[target]]$formula)),
                "term.labels")
  out <- list()
  for (t in terms) {
    if (is.factor(d[[t]])) {
      share <- prop.table(table(d[[t]]))
      out[paste0(t, ".", names(share))] <- as.numeric(share)
    } else {
      out[[t]] <- mean(d[[t]], na.rm = TRUE)
    }
  }
  unlist(out)
}

# Linear predictor from the stored coefficient table: the model matrix rows
# (.completion_model_matrix on a design frame) times the estimates.
.completion_predict_logit <- function(model, x) {
  terms <- model$coefficients$term
  as.numeric(x[, terms, drop = FALSE] %*% model$coefficients$estimate)
}

# The number of columns a model's matrix has: a fit needs more rows than that.
.completion_n_terms <- function(target) {
  spec <- .COMPLETION_MODELS[[target]]
  row <- data.frame(mode = spec$modes[1L], topic = "Brand", loi = 12, sample_score = 0,
                    reply_score = 0, share_prior_completers = 0, share_cold = 0,
                    years_since_contact = 0, prior_optin_score = 0, stringsAsFactors = FALSE)
  ncol(.completion_model_matrix(spec$formula, .completion_design(row, target)))
}

# Residual sd per mode: NA when fewer than 2 rows of that mode.
.completion_sd_by_mode <- function(residuals, mode, modes) {
  vapply(modes, function(m) {
    r <- residuals[!is.na(mode) & mode == m]
    if (length(r) < 2L) NA_real_ else stats::sd(r)
  }, numeric(1))
}

# Residual sd per mode and sample-size band: one row per (mode, under/over),
# `sd` NA with fewer than 2 rows in the cell, `n` the rows in it.
.completion_sd_by_size <- function(residuals, mode, sent, modes, size_break) {
  band <- .completion_size_band(sent, size_break)
  cells <- expand.grid(mode = modes, size = .COMPLETION_SIZE_LABELS, stringsAsFactors = FALSE)
  cells$sd <- NA_real_
  cells$n <- 0L
  for (i in seq_len(nrow(cells))) {
    r <- residuals[!is.na(mode) & mode == cells$mode[i] & band == cells$size[i]]
    cells$n[i] <- length(r)
    if (length(r) >= 2L) cells$sd[i] <- stats::sd(r)
  }
  cells
}

# The re-send summary of a fit: how many training campaigns of each mode had a
# sample mostly contacted in the .COMPLETION_RECENT_DAYS before their start,
# and the median of their actual-to-fitted ratio (exp of the median residual;
# NA for a mode with no such campaign). The estimate quotes it when a sample
# looks like a re-send.
.completion_resend_summary <- function(residuals, rows, modes) {
  recent <- !is.na(rows$share_recent_contact) &
    rows$share_recent_contact > .COMPLETION_RESEND_SHARE
  campaigns <- vapply(modes, function(m) sum(recent & rows$mode == m), integer(1))
  ratio <- vapply(modes, function(m) {
    r <- residuals[recent & rows$mode == m]
    if (length(r) == 0L) NA_real_ else exp(stats::median(r))
  }, numeric(1))
  list(threshold = .COMPLETION_RESEND_SHARE, campaigns = campaigns, ratio = ratio)
}

# Fit one model: the shipped coefficients on every training row before
# `train_until`, and a second fit ending at `holdout_from` scored on the
# campaigns in between for the holdout metrics and the band's sd.
.completion_fit_model <- function(ct, target, train_until, holdout_from, min_sent, settle_days,
                                  data_end, size_break = 50000, fn) {
  spec <- .COMPLETION_MODELS[[target]]
  rows <- .completion_fit_rows(ct, target, train_until, min_sent, settle_days, data_end)
  if (nrow(rows) == 0L) {
    stop_s160(sprintf(paste0(
      "no campaign passes the training filters for the %s model (at least %s sends, started ",
      "before %s, last activity by %s, a context table for its year, at least one %s%s)."),
      spec$label, format(min_sent), format(train_until), format(data_end - settle_days),
      spec$label, if (!is.null(spec$require)) " with known completion" else ""), fn = fn)
  }
  means <- .completion_training_means(rows, target)
  fit <- .completion_fit_lm(rows, target, means)
  in_sample_sd <- .completion_sd_by_mode(stats::residuals(fit), rows$mode, spec$modes)
  in_sample_size <- .completion_sd_by_size(stats::residuals(fit), rows$mode, rows$sent, spec$modes,
                                           size_break)
  resend <- .completion_resend_summary(stats::residuals(fit), rows, spec$modes)
  hold_rows <- .completion_fit_rows(ct, target, holdout_from, min_sent, settle_days, data_end)
  score <- rows[rows$first_date >= holdout_from, , drop = FALSE]
  holdout <- list(n = nrow(score), r2_logit = NA_real_, median_ape = NA_real_,
                  within_50 = NA_real_)
  holdout_sd <- in_sample_sd * NA_real_
  holdout_size <- in_sample_size
  holdout_size$sd <- NA_real_
  min_fit <- .completion_n_terms(target)
  if (nrow(hold_rows) >= min_fit && nrow(score) >= 2L) {
    hold_means <- .completion_training_means(hold_rows, target)
    hold_model <- list(
      formula = spec$formula,
      coefficients = .completion_coefficients(.completion_fit_lm(hold_rows, target, hold_means)))
    pred <- .completion_predict_logit(hold_model, .completion_model_matrix(
      spec$formula, .completion_fill_means(.completion_design(score, target), hold_means)))
    ape <- abs(stats::plogis(pred) - score$rate) / score$rate
    holdout$r2_logit <- stats::cor(pred, score$logit)^2
    holdout$median_ape <- stats::median(ape)
    holdout$within_50 <- mean(ape <= 0.5)
    holdout_sd <- .completion_sd_by_mode(score$logit - pred, score$mode, spec$modes)
    holdout_size <- .completion_sd_by_size(score$logit - pred, score$mode, score$sent, spec$modes,
                                           size_break)
  } else {
    holdout$warning <- sprintf(paste0(
      "%s model: the holdout window from %s has %d campaign(s) to score (at least 2) and %d ",
      "to fit on (at least %d, one per term); holdout metrics and the residual sd are NA, so ",
      "estimates carry no bands. Use a longer `holdout_months` or a later `train_until`."),
      spec$label, format(holdout_from), nrow(score), nrow(hold_rows), min_fit)
  }
  residual_sd <- ifelse(is.na(in_sample_sd) | is.na(holdout_sd), NA_real_,
                        pmax(in_sample_sd, holdout_sd))
  names(residual_sd) <- spec$modes
  # the same rule per (mode, size band): the larger of the two sds, NA when either is
  residual_sd_size <- in_sample_size
  residual_sd_size$sd <- ifelse(is.na(in_sample_size$sd) | is.na(holdout_size$sd), NA_real_,
                                pmax(in_sample_size$sd, holdout_size$sd))
  list(coefficients = .completion_coefficients(fit), formula = spec$formula,
       training_means = means, residual_sd = residual_sd, residual_sd_size = residual_sd_size,
       resend = resend, holdout = holdout,
       counts = list(campaigns_fit = nrow(rows), campaigns_holdout_fit = nrow(hold_rows)),
       train_from = min(rows$first_date))
}

.completion_check_date <- function(x, name, fn) {
  if (is.null(x)) stop_s160(sprintf("`%s` must be a single date (a Date or \"YYYY-MM-DD\"), not NULL.", name), fn = fn)
  .disposition_date_bound(x, name, fn)
}

.completion_check_count <- function(x, name, fn) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0 || x != round(x)) {
    stop_s160(sprintf("`%s` must be a single non-negative whole number%s.", name, .not_got(x)),
              fn = fn)
  }
  as.numeric(x)
}

#' Fit the completion model from the disposition projection
#'
#' Fits one campaign-level linear model per target rate from the disposition
#' projection, each on the logit scale of the rate per sent. The default
#' \code{targets} are the opt-in rate and the completion rate; \code{"reply"}
#' (any response: engaged, opted in, refused or completed) and \code{"engage"}
#' (engaged, the first step of the funnel) can be added, and both are fitted
#' for text-to-web external surveys too. The completion, reply and engagement
#' models read the campaign's survey design (mode, topic, length of interview
#' band) and the history of its sample: the sample score (the mean
#' P(complete | the number's previous three outcomes and the survey mode) for
#' completion; the mean P(reply | ...) for reply and engagement), the share
#' with a prior complete, the share cold (three or more contacts and never a
#' reply) and the mean years since the numbers' previous contact. The opt-in
#' model reads mode, the sample score and the mean opt-in rate of the campaign
#' each number was last in. \code{\link{completion_estimate}} then scores a
#' planned survey on the same tables.
#'
#' The context tables are per calendar year and per survey mode (a live texter
#' and a web survey complete the same context at very different rates), with a
#' pooled block each lookup falls back to; \code{p_complete} averages the rows
#' with a known completion, \code{p_reply} every row. The two history features that need
#' a prior contact (years since contact, the previous campaign's opt-in rate)
#' take the training mean on a campaign whose sample has none.
#'
#' A training row is a campaign with at least \code{min_sent} sends, started
#' before \code{train_until}, whose last activity is at least
#' \code{settle_days} before the projection's last date (a campaign still in
#' field under-counts its completes), and started after the first year in the
#' data (its history table is estimated on the years before it). The opt-in
#' model takes every mode with at least one opt-in; the completion model takes
#' campaigns with known completion (not text-to-web external) and at least one
#' complete, since a campaign with none is an unrecorded one.
#'
#' The coefficients are refit on every training row; the holdout metrics and
#' the bands come from a second fit that stops \code{holdout_months} earlier and
#' is scored on the campaigns in between (the second fit needs at least one
#' campaign per model term and two to score, else the metrics and the sd are
#' NA with a warning). The band's residual sd is per mode
#' and per model: the larger of the shipped fit's in-sample sd and the holdout
#' sd for that mode. The same sd is also kept per mode and sample-size band
#' (sends under or at least \code{size_break}; campaigns under 50,000 sends
#' spread about twice as much as larger ones), and the estimate uses the band
#' of its sample when that cell has one. Residuals are skewed and the fleet
#' drifts, so the bands are approximate: on campaigns from mid 2026 the nominal
#' 95\% band covered about 94\% and the 50\% band about 70\%.
#'
#' Each model also keeps a re-send summary: how many training campaigns of each
#' mode had a sample mostly (over half) contacted in the 30 days before their
#' start, and the median ratio of their actual rate to the fitted one. A list
#' sent again within days completes well below its previous send (about 0.4
#' times on web in 2025 to 2026, about 0.9 on live) and such campaigns were
#' the most frequent large misses; the sample terms carry part of that, the
#' ratio says how much was left. \code{\link{completion_estimate}} flags such
#' a sample and quotes the summary.
#'
#' The fit runs inside DuckDB (\pkg{duckdb}, \pkg{DBI} and \pkg{dbplyr} must be
#' installed) and takes about a minute on the full projection.
#'
#' @param dataset Path to the disposition Parquet file, e.g. from
#'   \code{\link{disposition_pull}()}.
#' @param train_until Date (or date string): the fit reads the projection's
#'   rows dated before it only, so a past date refits the model as it could
#'   have been fitted then (the data end, the settle rule, the context tables
#'   and the campaign rates all stop there). Default today.
#' @param holdout_months Whole number of months before \code{train_until} the
#'   holdout window spans (default 5).
#' @param min_sent Minimum sends for a campaign to train on (default 2000).
#' @param settle_days Days a campaign's last activity must precede the data end
#'   by (default 7).
#' @param size_break Sends at which a campaign counts as a large sample for the
#'   per-size band sd (default 50000).
#' @param targets The rates to fit, distinct names among \code{"optin"},
#'   \code{"completion"}, \code{"reply"} and \code{"engage"} (default the
#'   first two). The estimate's columns follow the fitted targets.
#' @param x,object A \code{completion_model}.
#' @param ... Unused.
#' @return An object of class \code{completion_model}: a list with
#'   \code{models} (one entry per fitted target, each with the
#'   \code{coefficients} table (\code{term}, \code{estimate},
#'   \code{std_error}), the \code{formula}, the \code{training_means} the
#'   estimate's multipliers are centred on (and the fill for a feature a
#'   sample cannot provide), the per-mode \code{residual_sd}, the per mode and
#'   size band \code{residual_sd_size} table (\code{mode}, \code{size} under /
#'   over the break, \code{sd}, \code{n}), the \code{resend} summary
#'   (\code{threshold}, \code{campaigns} and \code{ratio} per mode, the
#'   ratio being actual odds over fitted odds, not a rate multiplier),
#'   the \code{holdout} metrics (\code{n}, \code{r2_logit}, \code{median_ape},
#'   \code{within_50}) and \code{counts}), the shared per-year
#'   \code{contexts} table (\code{year}, \code{context}, \code{survey_mode}
#'   with NA for the pooled block, \code{p_complete}, \code{p_reply},
#'   \code{n}), the
#'   \code{campaigns} table (\code{campaign_id}, \code{first_date},
#'   \code{optin_rate}) the estimate reads the previous campaign's opt-in rate
#'   from, the \code{windows} (\code{data_end}, \code{train_from},
#'   \code{train_until}, \code{holdout_from}), \code{counts},
#'   \code{min_sent}, \code{settle_days}, \code{size_break} and the package
#'   \code{version}.
#'   \code{print()} shows the windows, counts and holdout metrics;
#'   \code{summary()} returns the coefficients of every model as odds
#'   multipliers (one data frame: \code{model}, \code{term},
#'   \code{odds_multiplier}, \code{std_error}).
#' @seealso \code{\link{completion_estimate}} (score a planned survey),
#'   \code{\link{disposition_paths}} (the history letters)
#' @examples
#' \dontrun{
#' dataset <- disposition_pull()
#' m <- completion_model_fit(dataset)
#' m
#' summary(m)
#' }
#' @export
completion_model_fit <- function(dataset, train_until = Sys.Date(), holdout_months = 5,
                                 min_sent = 2000, settle_days = 7, size_break = 50000,
                                 targets = .COMPLETION_DEFAULT_TARGETS) {
  fn <- "completion_model_fit"
  .disposition_lazy_check(fn)
  if (!is.character(targets) || length(targets) == 0L || anyNA(targets) ||
        anyDuplicated(targets) || !all(targets %in% names(.COMPLETION_MODELS))) {
    stop_s160(sprintf("`targets` must be distinct names among %s%s.",
                      paste(shQuote(names(.COMPLETION_MODELS)), collapse = ", "),
                      .not_got(targets)), fn = fn)
  }
  train_until <- .completion_check_date(train_until, "train_until", fn)
  holdout_months <- .completion_check_count(holdout_months, "holdout_months", fn)
  if (holdout_months == 0) {
    stop_s160("`holdout_months` must be a positive whole number, not 0.", fn = fn)
  }
  min_sent <- .completion_check_count(min_sent, "min_sent", fn)
  settle_days <- .completion_check_count(settle_days, "settle_days", fn)
  size_break <- .completion_check_count(size_break, "size_break", fn)
  if (size_break == 0) stop_s160("`size_break` must be a positive whole number, not 0.", fn = fn)
  con <- .disposition_lazy_con()
  source <- .disposition_lazy_source(con, dataset, fn)
  history <- .completion_history_sql(con, source, before = train_until)
  contexts <- .completion_context_tables(con, history, fn)
  ct <- .completion_campaign_table(con, history, contexts)
  data_end <- max(ct$last_date, na.rm = TRUE)
  holdout_from <- lubridate::add_with_rollback(
    train_until, lubridate::period(-holdout_months, "months"))
  models <- lapply(stats::setNames(nm = targets), function(target) {
    .completion_fit_model(ct, target, train_until, holdout_from, min_sent, settle_days,
                          data_end, size_break, fn)
  })
  train_from <- min(do.call(c, lapply(models, function(m) m$train_from)))
  thin <- unlist(lapply(models, function(m) m$holdout$warning))
  if (length(thin) > 0L) warning(paste(thin, collapse = "\n"), call. = FALSE)
  models <- lapply(models, function(m) {
    m$holdout$warning <- NULL
    m[setdiff(names(m), "train_from")]
  })
  structure(list(
    models = models, contexts = contexts,
    campaigns = data.frame(campaign_id = ct$campaign_id, first_date = ct$first_date,
                           optin_rate = ct$optins / ct$sent),
    windows = list(data_end = data_end, train_from = train_from, train_until = train_until,
                   holdout_from = holdout_from),
    counts = list(campaigns_total = nrow(ct)), min_sent = min_sent, settle_days = settle_days,
    size_break = size_break,
    version = as.character(utils::packageVersion("survey160r"))),
    class = "completion_model")
}

#' @rdname completion_model_fit
#' @export
print.completion_model <- function(x, ...) {
  w <- x$windows
  pct <- function(v) if (is.na(v)) "NA" else sprintf("%.0f%%", 100 * v)
  num <- function(v) if (is.na(v)) "NA" else sprintf("%.2f", v)
  cat(sprintf("Completion model (survey160r %s)\n", x$version))
  cat(sprintf("Data end %s; %d campaigns in the projection\n", format(w$data_end),
              x$counts$campaigns_total))
  cat(sprintf("Trained on campaigns starting %s to %s; holdout from %s\n",
              format(w$train_from), format(w$train_until - 1L), format(w$holdout_from)))
  for (target in names(x$models)) {
    m <- x$models[[target]]
    cat(sprintf("%s model: %d campaigns fit; holdout %d campaigns: median error %s, within 50%% %s, R2 logit %s\n",
                .COMPLETION_MODELS[[target]]$label, m$counts$campaigns_fit, m$holdout$n,
                pct(m$holdout$median_ape), pct(m$holdout$within_50), num(m$holdout$r2_logit)))
    cat(sprintf("  residual sd (logit): %s\n",
                paste(names(m$residual_sd), vapply(m$residual_sd, num, character(1)),
                      collapse = ", ")))
  }
  invisible(x)
}

#' @rdname completion_model_fit
#' @export
summary.completion_model <- function(object, ...) {
  out <- lapply(names(object$models), function(target) {
    co <- object$models[[target]]$coefficients
    data.frame(model = target, term = co$term, odds_multiplier = exp(co$estimate),
               std_error = co$std_error, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}
