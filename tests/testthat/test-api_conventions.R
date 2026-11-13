# The argument conventions in CLAUDE.md, checked over every export so a new
# function or argument cannot drift from them unnoticed.

.exports <- function() {
  ex <- getNamespaceExports("survey160r")
  sort(ex[!grepl("^(print|summary|format)\\.", ex)])
}

.formals_of <- function(name) formals(get(name, asNamespace("survey160r")))

.has_default <- function(fm, arg) !identical(fm[[arg]], quote(expr = ))

test_that("the first argument is the data, under one of the names the conventions give", {
  first <- vapply(.exports(), function(f) {
    fm <- .formals_of(f)
    if (length(fm) == 0L) "" else names(fm)[1L]
  }, character(1))
  allowed <- c("", "x", "dataset", "data", "sample", "model", "campaign_id", "config",
               "consolidated", "path", "env", "date_from", "bucket", "refresh", "available",
               "questions")
  bad <- first[!first %in% allowed]
  expect_identical(unname(bad), character(0), info = paste(names(bad), bad, collapse = "; "))
})

test_that("after the second slot every argument has a default, except the listed required options", {
  required_options <- c(completion_estimate = "mode", completion_sample_size = "mode",
                        completion_score = "dataset",
                        question_timestamps = "question", latency_validate_config = "data")
  bad <- character(0)
  for (f in .exports()) {
    fm <- .formals_of(f)
    if (length(fm) < 3L) next
    for (arg in names(fm)[-(1:2)]) {
      if (arg == "...") next
      if (!.has_default(fm, arg) && !identical(unname(required_options[f]), arg)) {
        bad <- c(bad, paste0(f, "(", arg, ")"))
      }
    }
  }
  expect_identical(bad, character(0))
})

test_that("`...` is only a pass-through, never a function's own options", {
  dots <- Filter(function(f) "..." %in% names(.formals_of(f)), .exports())
  expect_setequal(dots, c("latency_run", "s160_api_campaign_results",
                          "s160_gcs_campaign_results_read", "s160_read_csv"))
})

test_that("an enumerated argument lists its choices in the signature and match.arg()s them", {
  # a literal c("a", "b") default, or a `.*_CHOICES` constant, is a choice vector
  is_choice <- function(d) {
    (is.call(d) && identical(d[[1L]], as.name("c")) && length(d) > 2L &&
       all(vapply(as.list(d)[-1L], is.character, logical(1)))) ||
      (is.symbol(d) && grepl("_CHOICES$", as.character(d)))
  }
  # a default that is a set of names, not a choice among them
  set_defaults <- "disposition_steps(columns)"
  found <- character(0)
  for (f in .exports()) {
    fm <- .formals_of(f)
    body_text <- paste(deparse(body(get(f, asNamespace("survey160r")))), collapse = "\n")
    for (arg in names(fm)) {
      if (!.has_default(fm, arg) || !is_choice(fm[[arg]])) next
      if (paste0(f, "(", arg, ")") %in% set_defaults) next
      found <- c(found, paste0(f, "(", arg, ")"))
      # match.arg(), or the readers' .check_<arg>_arg() helper that does the same
      expect_true(grepl(sprintf("match.arg\\(%s\\)|\\.check_%s_arg\\(", arg, arg), body_text),
                  label = paste0(f, "(", arg, ") is match.arg()'d"))
    }
  }
  # the ones the conventions name, so the detector itself is checked
  expect_true(all(c("disposition_funnel(grain)", "disposition_paths(terminal)",
                    "s160_read_csv(timestamps)", "disposition_pull(env)",
                    "completion_sample_size(rate)") %in% found))
})

test_that("the signatures are unchanged (new arguments go last; update the snapshot deliberately)", {
  sig <- vapply(.exports(), function(f) {
    s <- paste(deparse(args(get(f, asNamespace("survey160r")))), collapse = " ")
    gsub("\\s+", " ", sub("\\s*NULL\\s*$", "", sub("^function ", "", s)))
  }, character(1))
  expect_snapshot(cat(paste0(names(sig), sig, collapse = "\n")))
})
