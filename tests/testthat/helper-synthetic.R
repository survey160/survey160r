# Synthetic campaign-export generator -- shared by the parity tests and by
# scripts/bench.R (which sources this file). Deterministic for a given seed.
#
# Shape mirrors a real Survey160 export read through s160_read_csv(): one row
# per recipient, `campaignid`, a unique ten-digit `phone`, and for every
# question in the flow the `id.<q>.scriptDate` (send) / `id.<q>.batchDate`
# (reply) / `id.<q>.finalText` columns, with timestamps in the export's
# "YYYY-mm-dd HH:MM:SS.ffffffZ" string form. The funnel drops off question by
# question, a share of replies is blank (never answered), a share of sends is
# unparseable text (a parse failure), and the terminal / support columns the
# transforms read (refusal, ineligible, close text, web_complete, error_code,
# carrier) are present. `bilingual = TRUE` adds an `intro_sp` opener that
# routes a slice of recipients, so the opener-SET code paths run.
#
# Returns a plain data.frame with character timestamp columns (pass it through
# synthetic_parse_timestamps() for the POSIXct form a reader would produce).
synthetic_export <- function(n = 2000L, n_questions = 6L, seed = 42L,
                             campaign_id = 1980L, bilingual = FALSE,
                             drop_off = 0.8, blank_reply = 0.05,
                             garbage = 0.005, start = "2026-03-01 14:00:00",
                             span_days = 5) {
  set.seed(seed)
  stopifnot(n_questions >= 2L)
  questions <- c("intro", sprintf("q%d", seq_len(n_questions - 2L)), "close")
  base <- as.POSIXct(start, tz = "UTC") + stats::runif(n, 0, span_days * 86400)
  d <- data.frame(
    campaignid = rep(campaign_id, n),
    phone = sprintf("%010.0f", 2e9 + sample(7.99e9, n)),
    stringsAsFactors = FALSE
  )
  reach <- rep(TRUE, n)
  at <- base
  for (i in seq_along(questions)) {
    q <- questions[[i]]
    if (i > 1L) reach <- reach & (stats::runif(n) < drop_off)
    send <- at
    send[!reach] <- NA
    reply <- send + stats::rexp(n, 1 / 120)
    reply[stats::runif(n) < blank_reply] <- NA
    d[[sprintf("id.%s.scriptDate", q)]] <- synthetic_format_timestamps(send)
    d[[sprintf("id.%s.batchDate", q)]] <- synthetic_format_timestamps(reply)
    d[[sprintf("id.%s.finalText", q)]] <- ifelse(reach, "Yes", "")
    at <- reply + stats::rexp(n, 1 / 60)
    at[is.na(at)] <- send[is.na(at)] + 300
  }
  if (garbage > 0 && n_questions >= 3L) {
    bad <- sample(n, max(1L, as.integer(n * garbage)))
    d[["id.q1.scriptDate"]][bad] <- "not a timestamp"
  }
  if (bilingual) {
    routed <- stats::runif(n) < 0.1
    d[["id.intro_sp.scriptDate"]] <- ifelse(routed, d[["id.intro.scriptDate"]], "")
    d[["id.intro_sp.batchDate"]] <- ifelse(routed, d[["id.intro.batchDate"]], "")
  }
  d[["id.refusal.scriptDate"]] <- ifelse(stats::runif(n) < 0.02,
                                         synthetic_format_timestamps(base + 500), "")
  d[["id.ineligible.scriptDate"]] <- ifelse(stats::runif(n) < 0.03,
                                            synthetic_format_timestamps(base + 600), "")
  d[["id.intro.scriptText"]] <- rep("Hi, reply YES to take part", n)
  d[["id.close.scriptText"]] <- rep("Thanks!", n)
  d[["web_complete"]] <- rep("", n)
  d[["error_code"]] <- ifelse(stats::runif(n) < 0.01, "4780", "")
  d[["carrier"]] <- sample(c("AT&T", "Verizon", "T-Mobile", ""), n, replace = TRUE)
  d
}

# Export-format timestamp strings ("YYYY-mm-dd HH:MM:SS.ffffffZ"); NA -> "".
synthetic_format_timestamps <- function(x) {
  out <- rep("", length(x))
  ok <- !is.na(x)
  out[ok] <- paste0(format(x[ok], "%Y-%m-%d %H:%M:%OS6", tz = "UTC"), "Z")
  out
}

# The POSIXct form of a synthetic export: every id.<q>.scriptDate / batchDate
# column decoded exactly as the readers' `timestamps = "POSIXct"` does.
synthetic_parse_timestamps <- function(d) {
  survey160r:::.parse_export_timestamps(d)
}

# A synthetic disposition projection: `n_rows` (phone, campaign_id) records
# over `n_phones` distinct phones, with 0/1 funnel flags, an NA-completed
# campaign (the t2w_external case), a carrier dimension and dates.
synthetic_disposition <- function(n_rows = 5000L, n_phones = 2000L, seed = 7L) {
  set.seed(seed)
  phones <- sprintf("%010.0f", 2e9 + sample(7.99e9, n_phones))
  d <- data.frame(
    phone = phones[sample(n_phones, n_rows, replace = TRUE)],
    campaign_id = sample(1000:1040, n_rows, replace = TRUE),
    sent = rep(1L, n_rows),
    engaged = stats::rbinom(n_rows, 1, 0.3),
    opted_in = stats::rbinom(n_rows, 1, 0.1),
    completed = stats::rbinom(n_rows, 1, 0.05),
    web_complete = stats::rbinom(n_rows, 1, 0.01),
    refused = stats::rbinom(n_rows, 1, 0.02),
    ineligible = stats::rbinom(n_rows, 1, 0.02),
    terminated = stats::rbinom(n_rows, 1, 0.04),
    error = ifelse(stats::runif(n_rows) < 0.02, "4780", NA_character_),
    carrier = sample(c("AT&T", "Verizon", NA), n_rows, replace = TRUE),
    disposition_date = as.Date("2026-01-01") + sample(0:200, n_rows, replace = TRUE),
    stringsAsFactors = FALSE
  )
  d$completed[d$campaign_id == 1001L] <- NA_integer_
  d
}
