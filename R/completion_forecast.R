# completion_forecast(): score one planned survey with a completion_model
# (completion_model.R): read the sample's history from the projection, build
# the next contact's features, predict both rates and their bands.

.COMPLETION_SAMPLE_TERMS <- c("sample_score", "share_prior_completers", "share_cold")

# The sample's history features for its NEXT contact: each phone's latest row
# (letter, previous three letters, prior counts) shifted one step forward, so
# the context is `latest letter || the two before it`; a never-contacted number
# is a first contact (`___`, zero counts). Scored on the context table of
# `year`, a context unseen in the table taking the year's first-contact rate.
.completion_sample_features <- function(con, source, phones, contexts, year) {
  sql <- paste0(
    "SELECT phone, letter, context, n_prior, prior_completes, prior_replies FROM (",
    .completion_history_sql(con, source, phones), ") WHERE is_last")
  h <- DBI::dbGetQuery(con, sql)
  idx <- match(phones, h$phone)
  seen <- !is.na(idx)
  letter <- h$letter[idx]
  context <- rep("___", length(phones))
  context[seen] <- paste0(letter[seen], substr(h$context[idx[seen]], 1L, 2L))
  n_prior <- ifelse(seen, h$n_prior[idx] + 1L, 0L)
  prior_completes <- ifelse(seen, h$prior_completes[idx] + (letter == "C"), 0L)
  prior_replies <- ifelse(seen, h$prior_replies[idx] + letter %in% c("E", "O", "R", "C"), 0L)
  ctx <- contexts[contexts$year == year, , drop = FALSE]
  p <- ctx$p_complete[match(context, ctx$context)]
  p[is.na(p)] <- ctx$p_complete[ctx$context == "___"]
  list(share_with_history = mean(seen),
       sample_score = .completion_logit(mean(p)),
       share_prior_completers = mean(prior_completes > 0),
       share_cold = mean(n_prior >= 3 & prior_replies == 0))
}

# The context-table year for a planned start: the last table (every row) for a
# date past the data, the first table (with a warning) for one before it.
.completion_forecast_year <- function(start_date, contexts) {
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
.completion_forecast_levels <- function(topic, loi, fn) {
  warnings <- character(0)
  if (is.null(topic)) {
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
  if (is.null(loi)) loi <- NA_real_
  if (!is.numeric(loi) || length(loi) != 1L) {
    stop_s160(sprintf("`loi` must be a single number of minutes or NULL%s.", .not_got(loi)),
              fn = fn)
  }
  band <- .completion_loi_band(loi)
  if (is.na(loi)) {
    warnings <- c(warnings, "LOI missing: treated as unknown")
  } else if (band == "unknown") {
    warnings <- c(warnings, sprintf("LOI %s is outside 0-31 minutes: treated as unknown",
                                    format(loi)))
  }
  list(topic = topic_level, loi = loi, warnings = warnings)
}

# One model's prediction on the forecast row: the rate, its four band edges
# (NA without a residual sd for the mode), the matrix row and the warnings.
.completion_forecast_rate <- function(model, target, design, mode) {
  m <- model$models[[target]]
  logit <- .completion_predict_logit(m, design)
  sd <- m$residual_sd[[mode]]
  warnings <- character(0)
  x <- .completion_model_matrix(m$formula, design)[1L, ]
  absent <- m$coefficients$term[is.na(m$coefficients$std_error) &
                                  x[m$coefficients$term] != 0]
  if (length(absent) > 0L) {
    warnings <- c(warnings, sprintf("%s model: no training campaign had %s; its effect is 0",
                                    .COMPLETION_MODELS[[target]]$label,
                                    paste(absent, collapse = ", ")))
  }
  if (is.na(sd)) {
    warnings <- c(warnings, sprintf("%s model has no residual sd for mode %s: no band",
                                    .COMPLETION_MODELS[[target]]$label, mode))
  }
  band <- stats::plogis(logit + .COMPLETION_Z * sd)
  list(rate = stats::plogis(logit), lo50 = band[["lo50"]], hi50 = band[["hi50"]],
       lo95 = band[["lo95"]], hi95 = band[["hi95"]], x = x, warnings = warnings)
}

# The two drivers as odds multipliers: each term's estimate times the forecast
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

#' Forecast a planned survey's opt-in and completion rates
#'
#' Scores one planned survey with a model from
#' \code{\link{completion_model_fit}}: the sample's history is read from the
#' disposition projection and turned into the next contact's features (the
#' mean P(complete | previous three outcomes) on the context table of the
#' planned year, the share with a prior complete, the share cold), the survey
#' design into the model's levels, and both rates are predicted with a 50\% and
#' a 95\% band and an expected count.
#'
#' The sample is used exactly as given: the function does not screen it (no
#' opt-out or eligibility filtering), so the expected counts are overstated by
#' whatever share the caller would later remove. Numbers are digit-normalised
#' and de-duplicated before scoring and counting. A never-contacted number is a
#' first contact.
#'
#' The bands are multiplicative on the odds: \code{p x/÷ exp(z * sd)} with
#' \code{z = 0.674} (50\%) and \code{1.96} (95\%), where \code{sd} is the
#' model's residual sd for the survey's mode. They are approximate (about 92\%
#' coverage at the nominal 95\% on 2026 campaigns). For a text-to-web external
#' survey completion is never recorded, so the completion columns are NA and
#' only the opt-in forecast stands.
#'
#' @param model A \code{completion_model} from \code{\link{completion_model_fit}}.
#' @param phones Character vector of the sample's phone numbers.
#' @param mode One of \code{"Live"}, \code{"T2W"}, \code{"T2W external"}.
#' @param topic One of \code{"Brand"}, \code{"Electoral"}, \code{"Policy"};
#'   \code{NULL} or another value counts as unknown, with a warning in the
#'   output.
#' @param loi Length of interview in minutes (0-31); \code{NULL}, \code{NA} or a
#'   value outside the range counts as unknown, with a warning in the output.
#' @param dataset Path to the disposition Parquet file, e.g. from
#'   \code{\link{disposition_pull}()}, the sample's history is read from.
#' @param start_date Planned start (Date or date string; default today): picks
#'   the context table of that year. A year past the data uses the last table;
#'   one before the first table uses the first, with a warning.
#' @return A one-row data frame: \code{optin_rate}, \code{optin_lo50},
#'   \code{optin_hi50}, \code{optin_lo95}, \code{optin_hi95}, \code{optins},
#'   \code{optins_lo95}, \code{optins_hi95}; the same eight for completion
#'   (\code{completion_rate}, ..., \code{completes}, \code{completes_lo95},
#'   \code{completes_hi95}); \code{n_phones} (unique numbers),
#'   \code{share_with_history} (share of them with a prior contact),
#'   \code{sample_score} (logit of the mean P(complete | context)),
#'   \code{sample_multiplier} and \code{survey_multiplier} (the completion
#'   model's sample terms and survey terms as odds multipliers against the
#'   training mix, so typical x sample x survey = the forecast), and
#'   \code{warnings} (a list column with one character vector: unknown levels,
#'   a sample under the fit's minimum sends, a mode without a band, opt-in only
#'   for text-to-web external).
#' @seealso \code{\link{completion_model_fit}}
#' @examples
#' \dontrun{
#' dataset <- disposition_pull()
#' m <- completion_model_fit(dataset)
#' sample <- read.csv("client_sample.csv")
#' completion_forecast(m, sample$phone, mode = "T2W", topic = "Policy", loi = 12,
#'                     dataset = dataset, start_date = "2026-12-01")
#' }
#' @export
completion_forecast <- function(model, phones, mode, topic = NULL, loi = NULL, dataset,
                                start_date = Sys.Date()) {
  fn <- "completion_forecast"
  if (!inherits(model, "completion_model")) {
    stop_s160(sprintf("`model` must be a completion_model from completion_model_fit()%s.",
                      .not_got(model)), fn = fn)
  }
  .disposition_lazy_check(fn)
  req <- if (is.character(phones)) .disposition_request_phones(phones) else character(0)
  if (length(req) == 0L) {
    stop_s160(sprintf("`phones` must be a character vector with at least one phone number%s.",
                      .not_got(phones)), fn = fn)
  }
  if (!is.character(mode) || length(mode) != 1L || !(mode %in% .COMPLETION_MODES)) {
    stop_s160(sprintf("`mode` must be one of %s%s.",
                      paste(sprintf('"%s"', .COMPLETION_MODES), collapse = ", "), .not_got(mode)),
              fn = fn)
  }
  levels <- .completion_forecast_levels(topic, loi, fn)
  start_date <- .completion_check_date(start_date, "start_date", fn)
  year <- .completion_forecast_year(start_date, model$contexts)
  warnings <- c(year$warning, levels$warnings)
  n_phones <- length(req)
  if (n_phones < model$min_sent) {
    warnings <- c(warnings, sprintf("sample has %d phones, under the %s sends the model was fitted on",
                                    n_phones, format(model$min_sent)))
  }
  con <- .disposition_lazy_con()
  source <- .disposition_lazy_source(con, dataset, fn)
  sample <- .completion_sample_features(con, source, req, model$contexts, year$year)
  row <- data.frame(mode = mode, topic = levels$topic, loi = levels$loi,
                    sample_score = sample$sample_score,
                    share_prior_completers = sample$share_prior_completers,
                    share_cold = sample$share_cold, stringsAsFactors = FALSE)
  optin <- .completion_forecast_rate(model, "optin", .completion_design(row, "optin"), mode)
  warnings <- c(warnings, optin$warnings)
  na <- NA_real_
  completion <- list(rate = na, lo50 = na, hi50 = na, lo95 = na, hi95 = na)
  multipliers <- c(sample = na, survey = na)
  if (mode == "T2W external") {
    warnings <- c(warnings,
                  "completion is not recorded for text-to-web external surveys: opt-in only")
  } else {
    completion <- .completion_forecast_rate(model, "completion",
                                            .completion_design(row, "completion"), mode)
    warnings <- c(warnings, completion$warnings)
    multipliers <- .completion_multipliers(model$models$completion, completion$x)
  }
  count <- function(p) round(p * n_phones)
  out <- data.frame(
    optin_rate = optin$rate, optin_lo50 = optin$lo50, optin_hi50 = optin$hi50,
    optin_lo95 = optin$lo95, optin_hi95 = optin$hi95, optins = count(optin$rate),
    optins_lo95 = count(optin$lo95), optins_hi95 = count(optin$hi95),
    completion_rate = completion$rate, completion_lo50 = completion$lo50,
    completion_hi50 = completion$hi50, completion_lo95 = completion$lo95,
    completion_hi95 = completion$hi95, completes = count(completion$rate),
    completes_lo95 = count(completion$lo95), completes_hi95 = count(completion$hi95),
    n_phones = n_phones, share_with_history = sample$share_with_history,
    sample_score = sample$sample_score, sample_multiplier = multipliers[["sample"]],
    survey_multiplier = multipliers[["survey"]])
  out$warnings <- list(warnings)
  out
}
