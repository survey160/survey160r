# completion_estimate(): score one planned survey with a completion_model
# (completion_model.R): read the sample's history from the projection, build
# the next contact's features, predict both rates and their bands.

.COMPLETION_SAMPLE_TERMS <- c("sample_score", "reply_score", "share_prior_completers", "share_cold",
                              "years_since_contact")

# The projection's survey_mode for each planned mode: the block of the context
# table the sample is scored on.
.COMPLETION_SURVEY_MODES <- c(Live = "sms", T2W = "t2w", "T2W external" = "t2w_external")

# The sample's history features for its NEXT contact: each phone's latest row
# (letter, previous three letters, prior counts) shifted one step forward, so
# the context is `latest letter || the two before it`; a never-contacted number
# is a first contact (`___`, zero counts). Only rows before `start_date` count
# (the history the planned survey would see). Scored on the context table of
# `year` for the planned mode's survey_mode (.completion_context_lookup).
# `years_since_contact` is the mean over the contacted numbers of the years
# from their latest row to `start_date`; `prior_optin_score` the logit of the
# mean opt-in rate (from the model's `campaigns` table) of the campaign each
# was last in. Either is NA when no number qualifies, and the warning says so.
.completion_sample_features <- function(con, source, phones, model, year, mode, start_date) {
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
  n_prior <- ifelse(seen, h$n_prior[idx] + 1L, 0L)
  prior_completes <- ifelse(seen, h$prior_completes[idx] + (letter == "C"), 0L)
  prior_replies <- ifelse(seen, h$prior_replies[idx] + letter %in% c("E", "O", "R", "C"), 0L)
  p <- .completion_context_lookup(model$contexts, year, context,
                                  .COMPLETION_SURVEY_MODES[[mode]])
  p_reply <- .completion_context_lookup(model$contexts, year, context,
                                        .COMPLETION_SURVEY_MODES[[mode]], "p_reply")
  days <- as.numeric(start_date - as.Date(h$disposition_date[idx[seen]]))
  years <- days / 365.25
  recent <- rep(FALSE, length(phones))
  recent[seen] <- days <= .COMPLETION_RECENT_DAYS
  prior_rate <- model$campaigns$optin_rate[match(h$campaign_id[idx[seen]],
                                                 model$campaigns$campaign_id)]
  prior_score <- NA_real_
  if (any(!is.na(prior_rate))) prior_score <- .completion_logit(mean(prior_rate, na.rm = TRUE))
  warnings <- character(0)
  if (anyNA(p)) {
    warnings <- sprintf(paste0("%d of %d numbers have a context with no row in the %d table ",
                               "for mode %s; they are left out of sample_score"),
                        sum(is.na(p)), length(p), year, mode)
  }
  if (!any(seen)) {
    warnings <- c(warnings, paste0("no number in the sample has a prior contact: ",
                                   "years_since_contact and prior_optin_score take the training mean"))
  } else if (all(is.na(prior_rate))) {
    warnings <- c(warnings, paste0("no number in the sample was last contacted by a campaign ",
                                   "the model knows: prior_optin_score takes the training mean"))
  }
  list(share_with_history = mean(seen), share_recent_contact = mean(recent),
       sample_score = .completion_logit(mean(p, na.rm = TRUE)),
       reply_score = .completion_logit(mean(p_reply, na.rm = TRUE)),
       share_prior_completers = mean(prior_completes > 0),
       share_cold = mean(n_prior >= 3 & prior_replies == 0),
       years_since_contact = if (any(seen)) mean(years) else NA_real_,
       prior_optin_score = prior_score, warnings = warnings)
}

# The context-table year for a planned start: the last table (every row) for a
# date past the data, the first table (with a warning) for one before it.
.completion_estimate_year <- function(start_date, contexts) {
  year <- as.integer(format(start_date, "%Y"))
  years <- range(contexts$year)
  warning <- character(0)
  if (year < years[1L]) {
    warning <- sprintf(paste0(
      "start_date %s is before the first context table (%d): the sample is scored with the ",
      "%d table"), format(start_date), years[1L], years[1L])
    year <- years[1L]
  }
  list(year = min(year, years[2L]), warning = warning)
}

# The survey's topic and LOI resolved to the model's levels, each fallback
# named in a warning.
.completion_estimate_levels <- function(topic, loi, fn) {
  warnings <- character(0)
  # a missing tracker value arrives as NA of any type: the same as not given
  missing <- function(x) is.null(x) || (length(x) == 1L && is.na(x))
  if (missing(topic)) {
    topic_level <- "unknown"
    warnings <- c(warnings, "topic missing: treated as unknown")
  } else {
    if (!is.character(topic) || length(topic) != 1L) {
      stop_s160(sprintf("`topic` must be a single string or NULL%s.", .not_got(topic)), fn = fn)
    }
    topic_level <- .completion_topic(topic)
    if (!identical(topic_level, topic)) {
      warnings <- c(warnings, sprintf("topic '%s' is not %s: treated as unknown", topic,
                                      paste(.COMPLETION_TOPICS[-4L], collapse = "/")))
    }
  }
  if (missing(loi)) loi <- NA_real_
  if (!is.numeric(loi) || length(loi) != 1L) {
    stop_s160(sprintf("`loi` must be a single number of minutes or NULL%s.", .not_got(loi)),
              fn = fn)
  }
  band <- .completion_loi_band(loi)
  if (is.na(loi)) {
    warnings <- c(warnings, "LOI missing: treated as unknown")
  } else if (band == "unknown") {
    warnings <- c(warnings, sprintf("LOI %s is not greater than 0 and at most 31 minutes: treated as unknown",
                                    format(loi)))
  }
  list(topic = topic_level, loi = loi, warnings = warnings)
}

# The band's sd for an estimate: the cell of the sample's size band when the
# model has one, else the mode's sd (with a warning when a size was known and
# its cell had none). NA when the mode has none either.
.completion_band_sd <- function(m, label, mode, n_phones, size_break) {
  sd <- m$residual_sd[[mode]]
  warnings <- character(0)
  if (!is.na(n_phones)) {
    size <- .completion_size_band(n_phones, size_break)
    cell <- m$residual_sd_size$sd[m$residual_sd_size$mode == mode & m$residual_sd_size$size == size]
    if (length(cell) == 1L && !is.na(cell)) {
      sd <- cell
    } else if (!is.na(sd)) {
      warnings <- sprintf(paste0(
        "%s model has no residual sd for %s samples %s %s sends: the band uses the mode's sd"),
        label, mode, if (size == "over") "of at least" else "under",
        format(size_break, big.mark = ","))
    }
  }
  list(sd = sd, warnings = warnings)
}

# The mean of the band's distribution of the rate: plogis(logit + sd Z) over a
# standard normal Z. Above the median (the point estimate) because the odds
# band is skewed: expected counts summed over many campaigns want this one.
.completion_mean_rate <- function(logit, sd) {
  if (is.na(sd)) return(stats::plogis(logit))
  stats::integrate(function(z) stats::plogis(logit + sd * z) * stats::dnorm(z), -Inf, Inf)$value
}

# One model's prediction on the estimate row: the rate, its four band edges
# (NA without a residual sd), the band's mean, the sd used, the matrix row and
# the warnings. `n_phones` (NA without a sample) picks the sd's size band.
.completion_estimate_rate <- function(model, target, design, mode, n_phones) {
  m <- model$models[[target]]
  label <- .COMPLETION_MODELS[[target]]$label
  design <- .completion_fill_means(design, m$training_means)
  vars <- all.vars(stats::as.formula(m$formula))[-1L]
  empty <- vars[vapply(vars, function(v) anyNA(design[[v]]), logical(1))]
  if (length(empty) > 0L) {
    stop_s160(sprintf(
      "%s model: the sample's %s is NA and the model has no training mean to fill it with.",
      label, paste(empty, collapse = ", ")), fn = "completion_estimate")
  }
  x <- .completion_model_matrix(m$formula, design)
  logit <- .completion_predict_logit(m, x)
  x <- x[1L, ]
  band_sd <- .completion_band_sd(m, label, mode, n_phones, model$size_break)
  sd <- band_sd$sd
  warnings <- band_sd$warnings
  # a term with no standard error is aliased: a factor level no training
  # campaign had, or a numeric feature collinear with the rest
  aliased <- m$coefficients$term[is.na(m$coefficients$std_error) &
                                   x[m$coefficients$term] != 0]
  level <- aliased %in% names(m$training_means) == FALSE
  if (any(level)) {
    warnings <- c(warnings, sprintf("%s model: no training campaign had %s; its effect is 0",
                                    label, paste(aliased[level], collapse = ", ")))
  }
  if (any(!level)) {
    warnings <- c(warnings, sprintf(
      "%s model: %s is aliased in the fit (collinear with the other features); its effect is 0",
      label, paste(aliased[!level], collapse = ", ")))
  }
  if (is.na(sd)) {
    warnings <- c(warnings, sprintf("%s model has no residual sd for mode %s: no band",
                                    .COMPLETION_MODELS[[target]]$label, mode))
  }
  band <- stats::plogis(logit + .COMPLETION_Z * sd)
  list(rate = stats::plogis(logit), lo50 = band[["lo50"]], hi50 = band[["hi50"]],
       lo95 = band[["lo95"]], hi95 = band[["hi95"]], mean = .completion_mean_rate(logit, sd),
       sd = sd, x = x, warnings = warnings)
}

# The re-send flag: a sample mostly contacted in the recent-days window before
# the start, with the model's re-send summary for the mode when it has one.
.completion_resend_warning <- function(m, share, mode, start_date, label = "completion") {
  if (is.na(share) || share <= m$resend$threshold) return(character(0))
  head <- sprintf("%.0f%% of the sample was contacted in the %d days before %s: a re-sent list; ",
                  100 * share, .COMPLETION_RECENT_DAYS, format(start_date))
  n <- m$resend$campaigns[[mode]]
  if (n == 0L) {
    return(paste0(head, sprintf(
      "no training campaign of mode %s was a re-send, so the model has no comparison for it", mode)))
  }
  paste0(head, sprintf(paste0(
    "on the %d training campaigns like it (%s) actual %s odds were %.2fx the model's ",
    "fitted odds (an odds ratio, not a rate multiplier); ",
    "the estimate is not adjusted"), n, mode, label, m$resend$ratio[[mode]]))
}

# The two drivers as odds multipliers: each term's coefficient times the estimate
# row's distance from the training mean (a numeric term) or share (a factor
# level), summed over the sample terms and over the survey terms.
.completion_multipliers <- function(m, x) {
  co <- m$coefficients[m$coefficients$term != "(Intercept)", ]
  mean_name <- vapply(co$term, function(term) {
    if (term %in% names(m$training_means)) return(term)
    factor <- c("mode", "topic", "loi_band")
    f <- factor[startsWith(term, factor)][1L]
    paste0(f, ".", substring(term, nchar(f) + 1L))
  }, character(1))
  effect <- co$estimate * (x[co$term] - m$training_means[mean_name])
  sample <- co$term %in% .COMPLETION_SAMPLE_TERMS
  c(sample = exp(sum(effect[sample])), survey = exp(sum(effect[!sample])))
}

# One prediction per fitted target on the estimate row: a target not fitted
# for the mode (completion on a text-to-web external survey) gives the empty
# prediction (every value NA) and a warning. Returns the predictions (named by
# target), the targets actually scored and the warnings.
.completion_estimate_targets <- function(model, row, mode, n_phones) {
  na <- NA_real_
  empty <- list(rate = na, lo50 = na, hi50 = na, lo95 = na, hi95 = na, mean = na, sd = na, x = NULL)
  warnings <- character(0)
  scored <- character(0)
  predictions <- lapply(stats::setNames(nm = names(model$models)), function(target) {
    spec <- .COMPLETION_MODELS[[target]]
    if (!mode %in% spec$modes) {
      warnings <<- c(warnings, sprintf("%s is not fitted for mode %s: no %s estimate",
                                       spec$label, mode, spec$label))
      return(empty)
    }
    pred <- .completion_estimate_rate(model, target, .completion_design(row, target), mode, n_phones)
    warnings <<- c(warnings, pred$warnings)
    scored <<- c(scored, target)
    pred
  })
  list(predictions = predictions, scored = scored, warnings = warnings)
}

# The nine output columns of each predicted target, in the model's order: the
# rate and its four band edges named by the target, the counts (NA without a
# sample) named by the target's count.
.completion_estimate_columns <- function(predictions, n_phones) {
  count <- function(p) if (is.na(n_phones)) NA_real_ else round(p * n_phones)
  columns <- lapply(names(predictions), function(target) {
    p <- predictions[[target]]
    out <- list(p$rate, p$lo50, p$hi50, p$lo95, p$hi95, count(p$rate), count(p$lo95), count(p$hi95),
                count(p$mean))
    names(out) <- c(paste0(target, c("_rate", "_lo50", "_hi50", "_lo95", "_hi95")),
                    paste0(.COMPLETION_MODELS[[target]]$count, c("", "_lo95", "_hi95", "_mean")))
    out
  })
  do.call(c, columns)
}

# Without a sample the score terms are the training means the estimate filled
# in: the completion model's when fitted, else the first fitted model's that
# has the term.
.completion_fill_scores <- function(model, sample) {
  fitted <- names(model$models)
  order <- c(intersect("completion", fitted), setdiff(fitted, "completion"))
  for (term in c("sample_score", "reply_score")) {
    means <- lapply(model$models[order], function(m) m$training_means[term])
    means <- unlist(means[!vapply(means, anyNA, logical(1))])
    if (length(means) > 0L) sample[[term]] <- means[[1L]]
  }
  sample
}

#' Estimate a planned survey's opt-in, completion (and reply) rates
#'
#' Scores one planned survey with a model from
#' \code{\link{completion_model_fit}}: the sample's history is read from the
#' disposition projection and turned into the next contact's features (the
#' mean P(complete | previous three outcomes) and P(reply | ...) on the
#' context table of the planned year and mode, the share with a prior
#' complete, the share cold, the mean years since the numbers' previous
#' contact and the mean opt-in rate of the campaign each was last in), the
#' survey design into the model's levels, and every rate the model was fitted
#' on is predicted with a 50\% and a 95\% band and an expected count. A sample
#' with no prior contact gets the training mean for the two features that need
#' one, with a warning.
#'
#' The sample is used exactly as given: the function does not screen it (no
#' opt-out or eligibility filtering), so the expected counts are overstated by
#' whatever share the caller would later remove. Numbers are digit-normalised
#' and de-duplicated before scoring and counting. A never-contacted number is a
#' first contact.
#'
#' The bands are multiplicative on the odds: \code{p x/÷ exp(z * sd)} with
#' \code{z = 0.674} (50\%) and \code{1.96} (95\%), where \code{sd} is the
#' model's residual sd for the survey's mode and the sample's size band (under
#' or over the fit's \code{size_break}), the mode's alone when that cell has
#' none or no sample is given. They are approximate (about 94\%
#' coverage at the nominal 95\% on campaigns from mid 2026). For a text-to-web external
#' survey completion is never recorded, so the completion columns are NA and
#' only the opt-in estimate stands.
#'
#' The point estimate is the median of the band's distribution; the
#' \code{*_mean} counts are its mean, higher because the odds band is skewed.
#' Quote one campaign with the median; add the means when summing expected
#' completes over many campaigns.
#'
#' Without \code{phones} the estimate is the survey design's alone: the sample
#' terms take the training mean (a typical campaign's mix), the count and
#' sample columns are NA and a warning says so. Its error is wider (about a
#' third more than with a list on 2026 campaigns); use it to quote before a
#' list exists and re-estimate when it arrives. A sample mostly contacted in
#' the 30 days before the start is a re-sent list and gets a warning with the
#' model's re-send summary: such lists completed well below their previous
#' send and were the most frequent large misses (see
#' \code{\link{completion_model_fit}}); the estimate is not adjusted for it.
#'
#' @param model A \code{completion_model} from \code{\link{completion_model_fit}}.
#' @param phones Character vector of the sample's phone numbers, or \code{NULL}
#'   (the default) for an estimate from the survey design alone.
#' @param mode One of \code{"Live"}, \code{"T2W"}, \code{"T2W external"}.
#' @param topic One of \code{"Brand"}, \code{"Electoral"}, \code{"Policy"};
#'   \code{NULL}, \code{NA} or another value counts as unknown, with a warning
#'   in the output.
#' @param loi Length of interview in minutes (greater than 0 and at most 31);
#'   \code{NULL}, \code{NA}, zero (a tracker placeholder), or a value outside
#'   the range counts as unknown, with a warning in the output.
#' @param dataset Path to the disposition Parquet file, e.g. from
#'   \code{\link{disposition_pull}()}, the sample's history is read from;
#'   required with \code{phones}, unused without.
#' @param start_date Planned start (Date or date string; default today): only
#'   history before it is read, and it picks the context table of its year. A
#'   year past the data uses the last table; one before the first table uses
#'   the first, with a warning.
#' @return A one-row data frame with nine columns per fitted target, in the
#'   model's order: \code{optin_rate}, \code{optin_lo50},
#'   \code{optin_hi50}, \code{optin_lo95}, \code{optin_hi95}, \code{optins},
#'   \code{optins_lo95}, \code{optins_hi95}, \code{optins_mean}; the same
#'   for completion (\code{completion_rate}, ..., \code{completes},
#'   \code{completes_lo95}, \code{completes_hi95}, \code{completes_mean}),
#'   reply (\code{reply_rate}, ..., \code{replies}, ...) and engagement
#'   (\code{engage_rate}, ..., \code{engaged}, ...) when fitted (NA with a
#'   warning for a target not fitted for the mode: completion on a text-to-web
#'   external survey); then
#'   \code{n_phones} (unique numbers),
#'   \code{share_with_history} (share of them with a prior contact),
#'   \code{share_recent_contact} (share contacted in the 30 days before the
#'   start),
#'   \code{sample_score} (logit of the mean P(complete | context, mode)),
#'   \code{reply_score} (logit of the mean P(reply | context, mode)),
#'   \code{years_since_contact} and \code{prior_optin_score} (logit of the
#'   previous campaigns' mean opt-in rate; both NA when no number has a prior
#'   contact),
#'   \code{sample_multiplier} and \code{survey_multiplier} (the completion
#'   model's sample terms and survey terms as odds multipliers against a
#'   campaign at the training mix: how much the sample and the survey design
#'   each move the completion odds), \code{band_sd} (the residual sd the
#'   completion band used) and
#'   \code{warnings} (a list column with one character vector: unknown levels,
#'   no sample, a sample under the fit's minimum sends, a re-sent sample, a
#'   sample with no prior contact or whose last campaign the model does not
#'   know, a context without a table row, a size band or a mode without an sd,
#'   a target not fitted for the mode).
#' @seealso \code{\link{completion_model_fit}},
#'   \code{\link{completion_sample_size}} (the sends a target needs)
#' @examples
#' \dontrun{
#' dataset <- disposition_pull()
#' m <- completion_model_fit(dataset)
#' sample <- read.csv("client_sample.csv")
#' completion_estimate(m, sample$phone, mode = "T2W", topic = "Policy", loi = 12,
#'                     dataset = dataset, start_date = "2026-12-01")
#' }
#' @export
completion_estimate <- function(model, phones = NULL, mode, topic = NULL, loi = NULL, dataset = NULL,
                                start_date = Sys.Date()) {
  fn <- "completion_estimate"
  if (!inherits(model, "completion_model")) {
    stop_s160(sprintf("`model` must be a completion_model from completion_model_fit()%s.",
                      .not_got(model)), fn = fn)
  }
  .disposition_lazy_check(fn)
  with_sample <- !is.null(phones)
  req <- if (is.character(phones)) .disposition_request_phones(phones) else character(0)
  if (with_sample && length(req) == 0L) {
    stop_s160(sprintf(paste0("`phones` must be a character vector with at least one phone number, ",
                             "or NULL for an estimate from the survey design alone%s."),
                      .not_got(phones)), fn = fn)
  }
  if (with_sample && is.null(dataset)) {
    stop_s160("`dataset` (the disposition Parquet the sample's history is read from) is required with `phones`.",
              fn = fn)
  }
  if (!is.character(mode) || length(mode) != 1L || !(mode %in% .COMPLETION_MODES)) {
    stop_s160(sprintf("`mode` must be one of %s%s.",
                      paste(sprintf('"%s"', .COMPLETION_MODES), collapse = ", "), .not_got(mode)),
              fn = fn)
  }
  levels <- .completion_estimate_levels(topic, loi, fn)
  start_date <- .completion_check_date(start_date, "start_date", fn)
  year <- .completion_estimate_year(start_date, model$contexts)
  warnings <- c(year$warning, levels$warnings)
  na <- NA_real_
  if (with_sample) {
    n_phones <- length(req)
    if (n_phones < model$min_sent) {
      warnings <- c(warnings, sprintf("sample has %d phones, under the %s sends the model was fitted on",
                                      n_phones, format(model$min_sent)))
    }
    con <- .disposition_lazy_con()
    source <- .disposition_lazy_source(con, dataset, fn)
    sample <- .completion_sample_features(con, source, req, model, year$year, mode, start_date)
    warnings <- c(warnings, sample$warnings)
  } else {
    n_phones <- NA_integer_
    sample <- list(share_with_history = na, share_recent_contact = na, sample_score = na,
                   reply_score = na, share_prior_completers = na, share_cold = na,
                   years_since_contact = na, prior_optin_score = na)
    warnings <- c(warnings, paste0(
      "no sample given: the sample terms take the training mean (the mix of a typical campaign), ",
      "so the estimate is the survey design's alone and its error is wider than with a list"))
  }
  row <- data.frame(mode = mode, topic = levels$topic, loi = levels$loi,
                    sample_score = sample$sample_score, reply_score = sample$reply_score,
                    share_prior_completers = sample$share_prior_completers,
                    share_cold = sample$share_cold,
                    years_since_contact = sample$years_since_contact,
                    prior_optin_score = sample$prior_optin_score, stringsAsFactors = FALSE)
  predicted <- .completion_estimate_targets(model, row, mode, n_phones)
  warnings <- c(warnings, predicted$warnings)
  # the drivers, the band's sd and the re-send comparison read the completion
  # model when it was fitted for the mode, else the last target that was
  multipliers <- c(sample = na, survey = na)
  lead <- if ("completion" %in% predicted$scored) "completion" else utils::tail(predicted$scored, 1L)
  if (identical(lead, "completion")) {
    multipliers <- .completion_multipliers(model$models$completion, predicted$predictions$completion$x)
  }
  if (length(lead) == 1L) {
    warnings <- c(warnings, .completion_resend_warning(model$models[[lead]],
                                                       sample$share_recent_contact, mode, start_date,
                                                       .COMPLETION_MODELS[[lead]]$label))
  }
  if (!with_sample) sample <- .completion_fill_scores(model, sample)
  out <- data.frame(c(
    .completion_estimate_columns(predicted$predictions, n_phones),
    list(n_phones = n_phones, share_with_history = sample$share_with_history,
         share_recent_contact = sample$share_recent_contact,
         sample_score = sample$sample_score, reply_score = sample$reply_score,
         years_since_contact = sample$years_since_contact,
         prior_optin_score = sample$prior_optin_score, sample_multiplier = multipliers[["sample"]],
         survey_multiplier = multipliers[["survey"]],
         band_sd = predicted$predictions$completion$sd %||% na)),
    stringsAsFactors = FALSE)
  out$warnings <- list(warnings)
  out
}
