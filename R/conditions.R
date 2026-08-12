# Standardized errors for survey160r.
#
# Every error in the package raises with call. = FALSE, so the message -- not
# R's "Error in f(x):" banner -- carries the whole story. To keep that message
# useful and uniform, the package follows one convention:
#
#   * Exported, user-facing functions PREFIX the message with "<fn>: " so a
#     call. = FALSE error still names the call that failed. Pass fn = "<name>".
#   * Internal helpers stay bare (fn = NULL) -- their name would mean nothing to
#     the caller who never invoked them directly.
#   * Config validation keeps its "config:" domain prefix (see latency_config.R)
#     rather than a function name: it points at the offending input regardless
#     of which entry point (latency_run / latency_report / ...) reached it.
#   * Names of arguments and config keys are wrapped in `backticks`. Values
#     (paths, ids, column lists) follow a colon with no trailing period;
#     complete sentences end with a period.
#
# Where the prefix lives: a check written directly in an exported function's
# body carries that function's `fn`. A REUSED validator called by several
# exported functions -- validate_campaign_id(), resolve_bucket(),
# check_gcs_ready()/check_api_ready(), api_do_auth(), get_credential(), and the
# private .disposition_* input validators -- raises BARE: its message states a
# self-describing invariant (e.g. "`campaign_id` must be a single value") and a
# single caller's name would be arbitrary (which of the callers?) rather than
# informative. This mirrors stop_not_initialized() below.
#
# The helpers below centralize the recurring shapes so that grammar stays
# uniform and the duplicated raise-and-format logic lives in one place.

# Prepend the "<fn>: " prefix for an exported caller; bare when fn is NULL.
.error_prefix <- function(msg, fn) {
  if (is.null(fn)) msg else paste0(fn, ": ", msg)
}

# Raise a standardized survey160r error (see convention above).
stop_s160 <- function(msg, fn = NULL) {
  stop(.error_prefix(msg, fn), call. = FALSE)
}

# Validate `x` is a single, non-empty (non-whitespace) string. `name` is the
# argument's name, rendered in backticks. Returns `x` invisibly on success.
check_nonempty_string <- function(x, name, fn = NULL) {
  # nzchar(NA_character_) is TRUE by default, so guard NA explicitly.
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop_s160(sprintf("`%s` must be a non-empty string.", name), fn = fn)
  }
  invisible(x)
}

# Validate `x` is a single, positive, finite number (rejecting NA/NaN/Inf --
# an Inf timeout, for one, would turn a bounded poll into an unbounded loop).
check_positive_number <- function(x, name, fn = NULL) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
    stop_s160(sprintf("`%s` must be a positive number.", name), fn = fn)
  }
  invisible(x)
}

# Validate `x` is a data frame. `name` is the argument's name, backticked.
check_data_frame <- function(x, name, fn = NULL) {
  if (!is.data.frame(x)) {
    stop_s160(sprintf("`%s` must be a data frame.", name), fn = fn)
  }
  invisible(x)
}

# "<service> not initialized. Run <init_fn>() first." Raised from the private
# readiness checks on behalf of whichever exported call needs auth, so it stays
# bare and names the fix rather than the failing helper.
stop_not_initialized <- function(service, init_fn) {
  stop_s160(sprintf("%s not initialized. Run %s() first.", service, init_fn))
}

# "<subject> not found: <value>" -- value-bearing, no trailing period.
stop_not_found <- function(subject, value, fn = NULL) {
  stop_s160(sprintf("%s not found: %s", subject, value), fn = fn)
}

# "Failed to <action>: <cause>" -- value-bearing, no trailing period.
stop_failed <- function(action, cause, fn = NULL) {
  stop_s160(sprintf("Failed to %s: %s", action, cause), fn = fn)
}
