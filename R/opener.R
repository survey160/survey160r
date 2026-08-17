# Opening-question resolution -- the single source of truth shared by the latency
# summary (summary_aggregate.R) and the disposition transform
# (disposition_aggregate.R). Both views key their contacted / replied / consent
# signals on the campaign's opener SET rather than a hardcoded "intro", so a
# non-intro (FIRSTNET) or bilingual (intro + intro_sp / intro_latinos) campaign is
# measured, not dropped -- and because both call these helpers, the two views
# cannot drift out of alignment.

# The opening question(s) of a flow: the intro-family (grep "^intro(_|$)") of the
# `questions` vector -- a routed campaign has several (intro + intro_sp /
# intro_latinos, via v2 initialconditionals) -- else the single first question
# (e.g. "FIRSTNET"), else "intro" (a minimal export with no discovered opener).
# For a pure-intro campaign the set is exactly {"intro"}, so every downstream
# signal is byte-identical to the old hardcoded behaviour.
.opening_questions <- function(questions) {
  intro_family <- grep("^intro(_|$)", questions, value = TRUE)
  if (length(intro_family) > 0L) return(intro_family)
  if (length(questions) == 0L) "intro" else questions[[1L]]
}

# The opener set resolved straight from a campaign frame or raw header: discover
# the flow's questions, then take the opening set. The "from data" convenience
# used by the disposition transform and the latency funnel filters (the latency
# config path resolves from config$flow$questions instead).
.discover_openers <- function(data) {
  .opening_questions(latency_discover_questions(data))
}

# The closing question(s) of a flow: the close-family (grep "^close(_|$)") of the
# `questions` vector. Name-agnostic like the opener set -- a bilingual campaign
# ends on close + close_sp / close_latinos, each language's completers on its own
# branch, so the union is the SMS-completion set (matching the app's
# phonelist.complete). Falls back to "close" when the flow has no close-family
# question, so the mask reads an absent id.close.scriptDate null-safely (all
# FALSE). For a single-close campaign the set is {"close"} -- byte-identical to
# the old hardcoded id.close.scriptDate read.
.closing_questions <- function(questions) {
  close_family <- grep("^close(_|$)", questions, value = TRUE)
  if (length(close_family) > 0L) close_family else "close"
}

# The per-recipient opener send/reply timestamp: parse each opener's
# id.<q>.<field> column null-safely (absent -> all-NA, length nrow) and coalesce
# across the set. Each recipient hit exactly one opener, so coalesce yields that
# recipient's timestamp; it preserves POSIXct/UTC (all inputs are UTC, matching
# parse_s160_timestamps_chr). `field` is "scriptDate" (send) or "batchDate" (reply).
.question_timestamp <- function(data, openers, field) {
  n <- nrow(data)
  ts_list <- lapply(openers, function(q) {
    col <- sprintf("id.%s.%s", q, field)
    if (col %in% names(data)) {
      parse_s160_timestamps_chr(data[[col]])
    } else {
      rep(as.POSIXct(NA, tz = "UTC"), n)
    }
  })
  do.call(dplyr::coalesce, ts_list)
}

# TRUE where the recipient has an id.<q>.<field> event for ANY question in the
# set -- the disjunction over the set. !is.na(coalesce(...)) is exactly the OR of
# the per-question presence masks. Generic over any question set (openers for the
# contacted/replied signals, the close family for completion).
.question_events <- function(data, questions, field) {
  !is.na(.question_timestamp(data, questions, field))
}

# TRUE where the recipient reached the survey close: any close-family scriptDate
# is present. The SMS-completion signal, name-agnostic over close / close_sp /
# close_latinos so a bilingual campaign's Spanish completers are counted (the
# close-side analogue of the opener set).
.reached_close <- function(data, questions) {
  .question_events(data, .closing_questions(questions), "scriptDate")
}

# Default opt-in population: the opening question set's accepted answer is "Yes"
# -- a disjunction over the openers' finalText columns, restricted to those
# PRESENT in `available` so an absent routed branch doesn't trip
# validate_columns_present() / .mask_opt_in's missing-column guard or the
# population eval. For a pure-intro campaign this is exactly `.default_population`.
.opener_population <- function(openers, available) {
  cols <- sprintf("id.%s.finalText", openers)
  present <- cols[cols %in% available]
  if (length(present) == 0L) present <- cols[1L]
  paste(sprintf("%s == \"Yes\"", present), collapse = " | ")
}

# v2 CSV headers arrive dot-form (id.<q>.field, as the readers munge them via
# make.names) OR raw bracket-form (id[<q>]field). latency_discover_questions()
# accepts both, so normalize a raw header to dot-form before .opener_population()
# resolves finalText columns -- otherwise a raw bilingual header matches no
# dot-form finalText column and the population collapses to the first opener,
# silently dropping later branches. Dot-form names pass through unchanged.
.dot_form_headers <- function(cols) {
  sub("^id\\[([A-Za-z0-9_]+)\\]([A-Za-z0-9_]+)$", "id.\\1.\\2", cols)
}

# The opt-in / consent mask: TRUE where the recipient passes the population
# filter (default id.<opener>.finalText == "Yes"). Null-safe -- a population that
# references a genuinely-absent data column yields all-FALSE rather than an eval
# error. A referenced symbol is "absent" only if it is neither a data column nor
# resolvable in baseenv() (population_filter_mask binds columns with parent =
# baseenv(), so base symbols T/F/pi/Inf and function names still resolve -- a
# valid filter such as `col == T` must not be zeroed). A population that will not
# PARSE still raises via population_filter_mask (the "not valid R" error).
.population_mask <- function(data, population) {
  vars <- tryCatch(all.vars(parse(text = population)), error = function(e) NULL)
  if (!is.null(vars)) {
    missing <- setdiff(vars, names(data))
    missing <- missing[!vapply(missing, exists, logical(1),
                               envir = baseenv(), inherits = TRUE)]
    if (length(missing) > 0L) {
      return(rep(FALSE, nrow(data)))
    }
  }
  population_filter_mask(data, population)
}

# The per-recipient funnel masks (sent / engaged / opted-in), computed once and
# identically for the latency summary (build_summary_frame) and the disposition
# transform (disposition_run), so the two views cannot report a different funnel.
# Each keys on the opener SET:
#   sent     = received ANY opener send   (opener scriptDate present)
#   engaged  = replied to ANY opener AND was sent (a reply presupposes a send)
#   opted_in = passed the opt-in population AND was sent
# `send` (the coalesced opener scriptDate) is returned so the latency view can
# bucket its summary by send date/hour; the disposition view uses only the masks.
.funnel_masks <- function(data, openers, population) {
  send <- .question_timestamp(data, openers, "scriptDate")
  reply <- .question_timestamp(data, openers, "batchDate")
  sent <- !is.na(send)
  list(
    send = send,
    sent = sent,
    engaged = !is.na(reply) & sent,
    opted_in = .population_mask(data, population) & sent
  )
}
