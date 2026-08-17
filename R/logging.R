# Structured logging for survey160r.
#
# survey160r decides WHAT to log -- an event name, a severity, and structured
# fields. It does NOT decide WHERE the log goes or in what format: that is the
# host application's concern. A consumer routes the library's events by setting
#
#   options(survey160r.log_fn = function(event, level, ...) ...)
#
# In the Cloud Run producers that sink is survey160-shiny's log_event(), so the
# library's events flow into the app's GCP-structured JSON logger (one line of
# {severity, message, ...} on stderr) with no format duplicated here and no new
# dependency added to this package. With no sink configured, s160r_log() falls
# back to base message() for interactive/dev use, gated by S160_LOG_LEVEL.
#
# Stream: the fallback uses message(), which writes to stderr -- NEVER stdout.
# stdout is the data channel a library must not corrupt (a caller may capture it
# as program output). The injected sink owns its own stream.

# Severity ranks for the fallback threshold. Only used to gate the message()
# fallback against S160_LOG_LEVEL; an injected sink does its own level handling.
.s160r_log_levels <- c(debug = 1L, info = 2L, warning = 3L, error = 4L)

# Rank a level name, defaulting unknown/misspelled names to "info" (2) rather
# than erroring -- logging must never be the thing that breaks a run.
.s160r_level_rank <- function(name) {
  rank <- unname(.s160r_log_levels[tolower(name)])
  if (is.na(rank)) 2L else rank
}

# Render one structured field value for the human-readable fallback line. A
# zero-length value (NULL / character(0)) renders empty rather than erroring.
.s160r_format_field <- function(x) {
  if (length(x) == 0L) {
    ""
  } else {
    paste(as.character(x), collapse = ",")
  }
}

# Emit a structured log event. `event` is a stable dotted name (e.g.
# "gcs.read.start"); `level` is one of debug/info/warning/error; `...` are
# structured fields (paths, ids, counts). When a sink is configured via
# options(survey160r.log_fn=), the call is delegated verbatim; otherwise it
# falls back to a human-readable message() line (see the file header).
s160r_log <- function(event, level = "info", ...) {
  sink <- getOption("survey160r.log_fn")
  if (is.function(sink)) {
    sink(event, level = level, ...)
    return(invisible(NULL))
  }
  if (.s160r_level_rank(level) <
        .s160r_level_rank(Sys.getenv("S160_LOG_LEVEL", "info"))) {
    return(invisible(NULL))
  }
  fields <- list(...)
  suffix <- if (length(fields) > 0L) {
    rendered <- vapply(fields, .s160r_format_field, character(1))
    paste0(" ", paste(names(fields), rendered, sep = "=", collapse = " "))
  } else {
    ""
  }
  message(sprintf("[%s] %s%s", toupper(level), event, suffix))
  invisible(NULL)
}
