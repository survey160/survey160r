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
# parse_campaign_timestamps). `field` is "scriptDate" (send) or "batchDate" (reply).
.question_timestamp <- function(data, openers, field) {
  ts_list <- lapply(openers, function(q) {
    .column_timestamps(data, sprintf("id.%s.%s", q, field))
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

# The terminal branches of a flow -- the steps the opener routes a NON-opt-in
# answer to: refusal, ineligible, opt-out. Reaching one is a hard stop, not
# consent. Name-matched (^refus / ^inelig / ^opt[-_]?out), case-insensitive, so a
# bilingual/casual campaign is covered regardless of the answer TEXT.
.terminal_questions <- function(questions) {
  grep("^(refus|inelig|opt[-_]?out|optout)", questions,
       ignore.case = TRUE, value = TRUE)
}

# The continuation steps -- every question that is neither an opener nor a
# terminal branch (the survey body + the close family). Reaching ANY of them
# means the opener routed the recipient FORWARD, i.e. they consented. Falls back
# to the close family when the flow has no other continuation (a T2W / short
# campaign whose opener routes straight to close), so the mask is never empty.
.continuation_questions <- function(questions) {
  cont <- setdiff(questions,
                  c(.opening_questions(questions), .terminal_questions(questions)))
  if (length(cont) > 0L) cont else .closing_questions(questions)
}

# The routing-based opt-in / consent mask: TRUE where the recipient reached a
# continuation step (the opener routed them forward). This is the DEFAULT consent
# signal -- language- and phrasing-agnostic, unlike the legacy
# id.<opener>.finalText == "Yes" text match, which silently reads 0 for a campaign
# whose opt-in answer is "im down" / "en español" / a templated value. A caller
# that needs a specific answer still passes an explicit `population` filter.
.reached_continuation <- function(data, questions) {
  .question_events(data, .continuation_questions(questions), "scriptDate")
}

# The custom-population consent mask: TRUE where the recipient passes an explicit
# `population` filter (the routing default in .funnel_masks needs no population;
# this is only reached when a caller supplies one). Null-safe -- a population that
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
#   opted_in = reached a continuation step (the opener routed them forward) AND
#              was sent -- OR passed an explicit `population` filter when one is
#              given (the custom-consent override).
# `send` (the coalesced opener scriptDate) is returned so the latency view can
# bucket its summary by send date/hour; the disposition view uses only the masks.
# NOTE both consumers additionally OR the mode-dependent `completed` signal into
# opted_in (a completion is an opt-in), so the reported opted_in is >= completed
# in every mode; that fold lives in the consumers because completion is
# mode-dependent (computed there), not in this routing/population mask.
.funnel_masks <- function(data, openers, questions, population = NULL) {
  send <- .question_timestamp(data, openers, "scriptDate")
  reply <- .question_timestamp(data, openers, "batchDate")
  sent <- !is.na(send)
  opted <- if (is.null(population)) {
    .reached_continuation(data, questions)
  } else {
    .population_mask(data, population)
  }
  list(
    send = send,
    sent = sent,
    engaged = !is.na(reply) & sent,
    opted_in = opted & sent
  )
}
