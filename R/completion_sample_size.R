# completion_sample_size(): the sends a target number of completes needs, from
# completion_estimate()'s rate and band read the other way round. The client
# fixes the completes; the Survey Manager has to size the sample.

# Which level of the estimate a list of `n_phones` covers for `target`
# completes: "95% band" when it reaches the target even at the 95% low end,
# "50% band" at the 50% low end, "estimate" at the point estimate, "none"
# otherwise. NA without a list; a level without a band cannot be reached.
.completion_list_covers <- function(n_phones, target, rate, lo50, lo95) {
  if (is.na(n_phones)) return(NA_character_)
  reaches <- function(p) !is.na(p) && n_phones * p >= target
  if (reaches(lo95)) return("95% band")
  if (reaches(lo50)) return("50% band")
  if (reaches(rate)) return("estimate")
  "none"
}

#' Sends needed for a target number of completes
#'
#' Reads \code{\link{completion_estimate}} the other way round: given the
#' number of completes a client needs, the sends it takes at the estimated
#' completion rate and at the low end of the 50\% and 95\% bands, so a quote
#' can carry a margin. \code{rate} sizes on another fitted target instead
#' (\code{"optin"} for a panel recruit that needs a number of opt-ins,
#' \code{"reply"} for a live texting capacity plan). Without \code{phones} the rate is the survey design's
#' alone (quote before a list exists); with a list the rate is the list's and
#' \code{list_covers} says which level the list reaches, so the Survey Manager
#' sees whether to ask for more sample. Completion is never recorded for
#' text-to-web external surveys, so that mode has a sample size on the other
#' rates only.
#'
#' How much margin each level buys, on 2025-2026 campaigns scored with the
#' model as fitted at their start: a sample sized at the point estimate
#' reached the target in 48-63\% of campaigns; at \code{sends_at_lo50} (about
#' 1.3 times the point estimate on live, 1.4-1.5 on text-to-web) in 86-90\%
#' on live and 77-79\% on text-to-web; at \code{sends_at_lo95} (about 2.1-2.4
#' times on live, about 3 on text-to-web) in 93-97\%. The text-to-web spread is
#' wider and heavier-tailed, so each of its levels buys a little less than the
#' live one at a higher cost.
#'
#' @inheritParams completion_estimate
#' @param target The number of completes (or of the \code{rate}'s count)
#'   required: a positive whole number.
#' @param rate The target to size on: \code{"completion"} (default),
#'   \code{"optin"}, \code{"reply"} or \code{"engage"}; it must be among the
#'   model's fitted targets.
#' @return A one-row data frame: \code{target}; \code{rate} (the target
#'   sized on); \code{rate_estimate}, \code{rate_lo50}, \code{rate_lo95}
#'   (the estimate and the low ends of its bands);
#'   \code{sends_at_estimate}, \code{sends_at_lo50},
#'   \code{sends_at_lo95} (the sends the target needs at each rate, rounded
#'   up; NA where the model has no band); \code{n_phones} (the list's unique
#'   numbers, NA without one); \code{list_covers} (\code{"95\% band"},
#'   \code{"50\% band"}, \code{"estimate"} or \code{"none"}: the lowest rate at
#'   which the list still reaches the target; NA without a list); and
#'   \code{warnings} (the estimate's, plus a note when the list is short of the
#'   target at the point estimate).
#' @seealso \code{\link{completion_estimate}}, \code{\link{completion_model_fit}}
#' @examples
#' \dontrun{
#' dataset <- disposition_pull()
#' options(survey160r.duckdb_memory = "16GB")   # the default 4 GB runs out
#' m <- completion_model_fit(dataset)
#' # before the list exists: how much sample to ask for
#' completion_sample_size(m, 1200, mode = "T2W", topic = "Brand", loi = 12,
#'                        start_date = "2026-12-01")
#' # when it arrives: does it cover the target?
#' sample <- read.csv("client_sample.csv")
#' completion_sample_size(m, 1200, mode = "T2W", topic = "Brand", loi = 12,
#'                        phones = sample$phone, dataset = dataset, start_date = "2026-12-01")
#' }
#' @export
completion_sample_size <- function(model, target, mode, topic = NULL, loi = NULL, phones = NULL,
                                   dataset = NULL, start_date = Sys.Date(),
                                   rate = c("completion", "optin", "reply", "engage")) {
  fn <- "completion_sample_size"
  if (!inherits(model, "completion_model")) {
    stop_s160(sprintf("`model` must be a completion_model from completion_model_fit()%s.",
                      .not_got(model)), fn = fn)
  }
  target <- .completion_check_count(target, "target", fn)
  if (target == 0) stop_s160("`target` must be a positive whole number, not 0.", fn = fn)
  choices <- eval(formals(completion_sample_size)$rate)
  rate <- tryCatch(match.arg(rate), error = function(e) {
    stop_s160(sprintf("`rate` must be one of %s%s.", paste0("'", choices, "'", collapse = ", "),
                      .not_got(rate)), fn = fn)
  })
  if (!rate %in% names(model$models)) {
    stop_s160(sprintf("`rate` must be one of the model's fitted targets (%s)%s.",
                      paste0("'", names(model$models), "'", collapse = ", "), .not_got(rate)), fn = fn)
  }
  spec <- .COMPLETION_MODELS[[rate]]
  if (is.character(mode) && length(mode) == 1L && mode %in% .COMPLETION_MODES &&
        !mode %in% spec$modes) {
    stop_s160(sprintf("%s is not fitted for mode %s, so there is no %s rate to size a sample with.",
                      spec$label, mode, spec$label), fn = fn)
  }
  fc <- completion_estimate(model, phones, mode, topic, loi, dataset, start_date)
  sends <- function(p) {
    if (!is.finite(p) || p <= 0) return(NA_real_)
    n <- ceiling(target / p)
    if (is.finite(n)) n else NA_real_
  }
  point <- fc[[paste0(rate, "_rate")]]
  lo50 <- fc[[paste0(rate, "_lo50")]]
  lo95 <- fc[[paste0(rate, "_lo95")]]
  covers <- .completion_list_covers(fc$n_phones, target, point, lo50, lo95)
  warnings <- fc$warnings[[1]]
  if (identical(covers, "none")) {
    warnings <- c(warnings, sprintf(
      "the list's %s numbers are short of %s %s at the estimated rate (%s needed)",
      format(fc$n_phones, big.mark = ","), format(target, big.mark = ","), spec$count,
      format(sends(point), big.mark = ",")))
  }
  out <- data.frame(
    target = target, rate = rate, rate_estimate = point, rate_lo50 = lo50, rate_lo95 = lo95,
    sends_at_estimate = sends(point), sends_at_lo50 = sends(lo50), sends_at_lo95 = sends(lo95),
    n_phones = fc$n_phones, list_covers = covers, stringsAsFactors = FALSE)
  out$warnings <- list(warnings)
  out
}
