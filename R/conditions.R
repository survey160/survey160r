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
# exported functions -- validate_campaign_id(), resolve_dataset(),
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

# Build (do not raise) a classed survey160r condition. `subclass` is the specific
# class a caller dispatches on (e.g. "s160_not_found", "s160_http_error"); every
# survey160r error also carries "s160_error" so a caller can catch the whole
# family, then "error"/"condition" so base handlers still see an error. `...`
# becomes readable condition fields (e.g. status = for an HTTP error). Raise it
# with stop(). The message still flows through .error_prefix, so the "<fn>: "
# convention and the exact wording are unchanged -- only the *class* is new,
# which lets control flow dispatch on the class instead of grepping the message.
s160_condition <- function(msg, subclass, fn = NULL, ...) {
  structure(
    list(message = .error_prefix(msg, fn), call = NULL, ...),
    class = c(subclass, "s160_error", "error", "condition")
  )
}

# Raise a standardized survey160r error (see convention above). Every raise
# goes through s160_condition(), so `tryCatch(..., s160_error = )` catches any
# error the package itself raises (and never one from R or a dependency). An
# optional `subclass` adds a more specific class in front of "s160_error".
stop_s160 <- function(msg, fn = NULL, subclass = NULL) {
  stop(s160_condition(msg, subclass, fn = fn))
}

# Raise a classed HTTP error carrying the numeric `status`, so a caller can
# dispatch on the code (s160_api_campaign_get maps 400/404 to not-found) rather
# than grepping the status text. Internal (bare), matching s160_api_request's
# raise site.
stop_http_error <- function(status, msg, fn = NULL) {
  stop(s160_condition(msg, "s160_http_error", fn = fn, status = as.integer(status)))
}

# Validate `x` is a single, non-empty (non-whitespace) string. `name` is the
# argument's name, rendered in backticks. Returns `x` invisibly on success.
check_nonempty_string <- function(x, name, fn = NULL) {
  # nzchar(NA_character_) is TRUE by default, so guard NA explicitly.
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) {
    stop_s160(sprintf("`%s` must be a non-empty string%s.", name, .not_got(x)),
              fn = fn)
  }
  invisible(x)
}

# Validate `x` is a single, positive, finite number (rejecting NA/NaN/Inf --
# an Inf timeout, for one, would turn a bounded poll into an unbounded loop).
check_positive_number <- function(x, name, fn = NULL) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
    stop_s160(sprintf("`%s` must be a positive number%s.", name, .not_got(x)),
              fn = fn)
  }
  invisible(x)
}

# Validate `x` is a data frame. `name` is the argument's name, backticked.
check_data_frame <- function(x, name, fn = NULL) {
  if (!is.data.frame(x)) {
    stop_s160(sprintf("`%s` must be a data frame%s.", name, .not_got(x)),
              fn = fn)
  }
  invisible(x)
}

# Validate that `x` is exactly TRUE or FALSE, else stop with the standard
# "<arg> must be a single TRUE or FALSE" message. Shared by .gcs_pull_cached()
# for the *_pull() helpers' `refresh` and `progress` flags -- kept out of the
# function body so their cyclomatic complexity stays under the linter cap.
.require_single_logical <- function(x, arg, fn) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop_s160(sprintf("`%s` must be a single TRUE or FALSE%s.", arg, .not_got(x)),
              fn = fn)
  }
  invisible(x)
}

# "<service> not initialized. Run <init_fn>() first." Raised from the private
# readiness checks on behalf of whichever exported call needs auth, so it stays
# bare and names the fix rather than the failing helper.
stop_not_initialized <- function(service, init_fn) {
  stop_s160(sprintf("%s not initialized. Run %s() first.", service, init_fn))
}

# "<subject> not found: <value>" -- value-bearing, no trailing period. Classed
# `s160_not_found` so a boundary raise (e.g. download_with_verify translating a
# GCS 404) can be caught by class at the callers rather than re-grepping "404".
stop_not_found <- function(subject, value, fn = NULL) {
  stop(s160_condition(sprintf("%s not found: %s", subject, value),
                      "s160_not_found", fn = fn))
}

# "Failed to <action>: <cause>" -- value-bearing, no trailing period.
stop_failed <- function(action, cause, fn = NULL) {
  stop_s160(sprintf("Failed to %s: %s", action, cause), fn = fn)
}

# Describe a value for a "must be X, not <this>" error: what the caller actually
# passed, compactly, so the fix is obvious without re-inspecting the argument.
# A scalar is shown literally (strings quoted, cut at 40 characters); anything
# else by shape -- "a character vector of length 3", "a data frame (10 x 4)",
# "a list of length 2", "a function", "an object of class <cls>".
.describe_value <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.function(x)) return("a function")
  if (is.data.frame(x)) {
    return(sprintf("a data frame (%d x %d)", nrow(x), ncol(x)))
  }
  if (is.atomic(x) && length(x) == 1L) {
    if (is.na(x)) return("NA")
    if (is.character(x)) {
      shown <- if (nchar(x) > 40L) paste0(substr(x, 1L, 37L), "...") else x
      return(deparse(shown))
    }
    return(format(x))
  }
  if (is.atomic(x) || is.list(x)) {
    kind <- if (is.list(x)) "list" else paste(class(x)[[1L]], "vector")
    if (length(x) == 0L) return(sprintf("an empty %s", kind))
    return(sprintf("a %s of length %d", kind, length(x)))
  }
  sprintf("an object of class <%s>", paste(class(x), collapse = "/"))
}

# ", not <value>" -- the tail of a "`arg` must be X, not <value>." message.
.not_got <- function(x) {
  paste0(", not ", .describe_value(x))
}

# "a, b, c, ... (+N more)" -- the first `n` of a character vector for a message
# that wants to show what is available without dumping a 200-column header.
.preview_values <- function(x, n = 5L) {
  x <- as.character(x)
  if (length(x) == 0L) return("none")
  shown <- x[seq_len(min(n, length(x)))]
  extra <- length(x) - length(shown)
  paste0(paste(shown, collapse = ", "),
         if (extra > 0L) sprintf(", ... (+%d more)", extra) else "")
}

# "`<name>` has N column(s): a, b, c, ..." -- the tail of a missing-column error,
# so the caller sees what the frame actually carries next to what it lacks.
.columns_hint <- function(data, name = "data") {
  cols <- names(data)
  sprintf("`%s` has %d column(s): %s", name, length(cols), .preview_values(cols))
}
