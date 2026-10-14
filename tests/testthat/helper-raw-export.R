# Synthetic RAW campaign export -- the bytes a Survey160 `<id>_raw_data_download.csv`
# holds before any reader touches it. helper-synthetic.R's synthetic_export()
# builds the already-read, dot-named frame; this one builds the file, so the
# readers (fread and the read.csv fallback), the header munging and the
# timestamp decoding run on the same shape they meet in production.
#
# The shape is taken from a profile of the 1,468 exports in the historical
# backfill corpus (no real value is copied -- every phone, name and text here
# is made up):
#   * every field is double-quoted, header included; LF line endings, with
#     ~9% of files CRLF (`eol = "\r\n"`);
#   * leading columns campaignid, phone, userid, status, attempts,
#     outgoing_phone, ms_sid, status_disposition, error_code, web_complete,
#     text_complete, then 11 `id[<q>]<field>` columns per question;
#   * the panel's own columns are appended after the flow, and nearly every
#     file repeats `phone` there (1464 of 1468), many repeat `userid` too --
#     the readers make them phone.1 / userid.1;
#   * `status` mixes words (limbo, open, closed, terminated, assigned) with
#     carrier error codes (4720, 4753, ...); `error_code` holds a code, "",
#     "None" or "NA"; `userid` is "", "None" or login-<campaign>-<n>;
#   * timestamps are "YYYY-mm-dd HH:MM:SS.<fraction>Z" with trailing zeros of
#     the microseconds trimmed (1-6 digits), "" when not sent, and a literal
#     "NA" in a few older files;
#   * script texts are multilingual (Spanish, Vietnamese, Chinese, Korean),
#     carry commas, doubled quotes and the odd embedded line break, and panel
#     columns carry literal "NA" values;
#   * `carrier` is mostly a carrier name but some files reuse the column for
#     a survey answer.
#
# The flow: every recipient is sent the intro (an errored send carries an
# error status/code); a share replies; a "Yes" walks q1 .. q<n> to `close`,
# a "No" gets `refusal`, a screen-out gets `ineligible`. Deterministic for a
# given seed. Returns the file's lines (header first, no trailing EOL);
# write_raw_export() writes them.
synthetic_raw_export <- function(n = 40L, n_body = 2L, seed = 1L,
                                 campaign_id = 1980L,
                                 start = "2026-05-13 19:00:00",
                                 eol = "\n", na_timestamps = 0,
                                 embedded_newline = TRUE,
                                 extra_questions = character(0)) {
  set.seed(seed)
  body <- if (n_body > 0L) sprintf("q%d", seq_len(n_body)) else character(0)
  questions <- c("intro", body, extra_questions, "refusal", "ineligible", "close")
  fields <- c("scriptDate", "scriptText", "scriptAgentUserId", "batchDate",
              "batchText", "batchAgentUserId", "finalDate", "finalType",
              "finalValue", "finalText", "finalAgentUserId")
  texts <- c(
    intro = "Hi, it's Ana with the Community Survey, reply 1) Yes 2) No (or QUIT) 3) En español",
    q1 = "Q1: How would you rate your area, \"good\" or \"poor\"?",
    q2 = "Q2: ¿Cómo calificaría su barrio? Responda 1-5",
    refusal = "No problem, thanks for your time.",
    ineligible = "Thanks! This survey is for adults 18+ only.",
    close = paste0("Thanks for taking part! Trả lời để nhận,",
                   " 回覆即可 / 번을 보내주시면")
  )
  text_for <- function(q) if (q %in% names(texts)) texts[[q]] else sprintf("%s: next question?", q)

  base <- as.POSIXct(start, tz = "UTC") + stats::runif(n, 0, 3 * 86400)
  phone <- sprintf("%010.0f", 2e9 + sample(7.99e9, n))
  errored <- stats::runif(n) < 0.08
  code <- sample(c("4720", "4753", "4700", "30003"), n, replace = TRUE)
  replied <- !errored & stats::runif(n) < 0.6
  answer <- ifelse(replied, sample(c("yes", "no", "screen"), n, replace = TRUE,
                                   prob = c(0.7, 0.15, 0.15)), "")

  ts <- function(x) {
    out <- rep("", length(x))
    ok <- !is.na(x)
    micro <- sprintf("%06d", sample(1:999999, sum(ok), replace = TRUE))
    out[ok] <- paste0(format(x[ok], "%Y-%m-%d %H:%M:%S", tz = "UTC"), ".",
                      sub("0+$", "", micro), "Z")
    out
  }
  cell <- matrix("", n, length(questions) * length(fields))
  colnames(cell) <- paste0("id[", rep(questions, each = length(fields)), "]",
                           rep(fields, length(questions)))
  put <- function(q, field, rows, value) {
    cell[rows, sprintf("id[%s]%s", q, field)] <<- value
  }
  at <- base
  put("intro", "scriptDate", TRUE, ts(at))
  put("intro", "scriptText", TRUE, text_for("intro"))
  reply_at <- at + stats::rexp(n, 1 / 90)
  put("intro", "batchDate", replied, ts(reply_at[replied]))
  put("intro", "batchText", replied,
      c(yes = "1", no = "No", screen = "Si")[answer[replied]])
  put("intro", "finalText", replied,
      c(yes = "Yes", no = "No", screen = "Yes")[answer[replied]])
  put("intro", "finalType", replied, "intro")
  # Yes -> the body questions, one after another, each answered after a delay.
  walk <- answer == "yes"
  at <- reply_at
  for (q in c(body, extra_questions)) {
    sent_at <- at + stats::rexp(n, 1 / 20)
    put(q, "scriptDate", walk, ts(sent_at[walk]))
    put(q, "scriptText", walk, text_for(q))
    walk <- walk & stats::runif(n) < 0.85
    at <- sent_at + stats::rexp(n, 1 / 120)
    put(q, "batchDate", walk, ts(at[walk]))
    put(q, "batchText", walk, sample(c("1", "2", "Good", "Được"), sum(walk), TRUE))
    put(q, "finalText", walk, "Yes")
  }
  terminal <- function(q, rows) {
    sent_at <- at + stats::rexp(n, 1 / 15)
    put(q, "scriptDate", rows, ts(sent_at[rows]))
    put(q, "scriptText", rows, text_for(q))
  }
  terminal("close", walk)
  terminal("refusal", answer == "no")
  terminal("ineligible", answer == "screen")
  put("close", "finalType", walk, "closed")
  if (embedded_newline && n >= 3L) {
    # One reply spans two lines inside its quotes, as a few exports do.
    put("intro", "batchText", 3L, "Yes\nplease")
  }
  if (na_timestamps > 0) {
    blank <- which(!replied)
    pick <- blank[stats::runif(length(blank)) < na_timestamps]
    put("intro", "batchDate", pick, "NA")
  }

  status <- ifelse(errored, code, sample(c("limbo", "open", "closed", "assigned"),
                                         n, replace = TRUE, prob = c(0.6, 0.2, 0.1, 0.1)))
  status[answer %in% c("no", "screen")] <- "terminated"
  lead <- cbind(
    campaignid = as.character(campaign_id),
    phone = phone,
    userid = sample(c("", "None", sprintf("login-%d-%d", campaign_id, 1:3)), n, TRUE),
    status = status,
    attempts = sample(c("5", "5", "5", "4", ""), n, TRUE),
    outgoing_phone = sprintf("%010.0f", 2e9 + sample(7.99e9, n)),
    ms_sid = sprintf("MG%030d", seq_len(n)),
    status_disposition = ifelse(errored, "limbo", sample(
      c("limbo", "open", "closing question", "recipient termination"), n, TRUE)),
    error_code = ifelse(errored, code, sample(c("None", "", "NA"), n, TRUE,
                                              prob = c(0.5, 0.45, 0.05))),
    web_complete = ifelse(stats::runif(n) < 0.05, "1", "0"),
    text_complete = ifelse(walk, "1", "0")
  )
  tail <- cbind(
    phone = phone,
    name = sample(c("Ana", "Bao", "Chen", "Désirée", "O'Neil, Jr."), n, TRUE),
    id = sprintf("R%06d", seq_len(n)),
    carrier = sample(c("Verizon", "AT&T", "T-Mobile", "Metro PCS", "Strongly approve", "NA"),
                     n, TRUE, prob = c(0.35, 0.3, 0.2, 0.1, 0.03, 0.02)),
    state = sample(c("NY", "CA", "TX", "NA"), n, TRUE),
    dup_phones = "0",
    rand = sprintf("%.15f", stats::runif(n)),
    userid = "",
    deactivationDate = "NA"
  )
  all <- cbind(lead, cell, tail)
  quote <- function(x) paste0("\"", gsub("\"", "\"\"", x, fixed = TRUE), "\"")
  rows <- apply(all, 1L, function(r) paste(quote(r), collapse = ","))
  c(paste(quote(colnames(all)), collapse = ","), unname(rows))
}

# Write synthetic_raw_export() lines to `path` byte-for-byte (UTF-8, the
# given end-of-line, no re-encoding) and return the path.
write_raw_export <- function(lines, path = tempfile(fileext = ".csv"), eol = "\n") {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = eol), eol))), con)
  path
}
