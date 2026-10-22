# disposition_paths(): one row per phone, its campaigns as a letter path, and the
# sequence-pattern syntax. Fixtures: sequence_records() (helper-sequence.R).

test_that("paths: one row per phone, letters in time order, run-length shape", {
  skip_if_no_sequence()
  d <- collect_sorted(disposition_paths(write_sequence_parquet()))
  expect_named(d, c("phone", "n_campaigns", "path", "shape", "first_date", "last_date",
                    "n_unknown_completion"))
  expect_equal(d$phone, paste0("201555010", 1:5))
  expect_equal(d$path, c("NEC", "XNNT", "OW", "EN", "RI"))
  expect_equal(d$shape, c("N+E+C+", "X+N+T+", "O+W+", "E+N+", "R+I+"))
  expect_equal(d$n_campaigns, c(3, 4, 2, 2, 2))
  expect_equal(d$first_date, as.Date(c("2026-01-01", "2026-01-01", "2026-02-01", "2026-03-01",
                                       "2026-01-01")))
  expect_equal(d$last_date, as.Date(c("2026-03-01", "2026-03-01", "2026-02-01", "2026-03-01",
                                      "2026-02-01")))
  expect_equal(d$n_unknown_completion, c(0L, 0L, 0L, 1L, 0L))
})

test_that("each phone's path equals the last step's history in disposition_steps()", {
  skip_if_no_sequence()
  p <- write_sequence_parquet()
  paths <- collect_sorted(disposition_paths(p))
  steps <- collect_sorted(dplyr::filter(disposition_steps(p), step == n_campaigns))
  expect_equal(paths$path, steps$path_so_far)
})

test_that("a pattern adds the match flag, positions and dates", {
  skip_if_no_sequence()
  d <- collect_sorted(disposition_paths(write_sequence_parquet(), "non_response > engaged > completed"))
  expect_equal(d$matched, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(d$match_start, c(1L, NA, NA, NA, NA))
  expect_equal(d$match_end, c(3L, NA, NA, NA, NA))
  expect_equal(d$match_start_date[1], as.Date("2026-01-01"))
  expect_equal(d$match_end_date[1], as.Date("2026-03-01"))
  # the first (leftmost, shortest) match: the N run inside XNNT
  d <- collect_sorted(disposition_paths(write_sequence_parquet(), "N{2}", phones = "2015550102"))
  expect_equal(c(d$match_start, d$match_end), c(2L, 3L))
  expect_equal(d$match_start_date, as.Date("2026-01-15"))
  # letters and names are the same pattern; anchors
  p <- write_sequence_parquet()
  m <- function(pattern) collect_sorted(disposition_paths(p, pattern))$matched
  expect_equal(m("N > C"), m("non_response > completed"))
  expect_equal(m("^engaged"), c(FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(m("non_response$"), c(FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(m("opted_in >> web_complete"), c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(m("refused|ineligible+$"), c(FALSE, FALSE, FALSE, FALSE, TRUE))
})

test_that("the date window shapes the path; phones scope it", {
  skip_if_no_sequence()
  p <- write_sequence_parquet()
  d <- collect_sorted(disposition_paths(p, date_from = "2026-02-01"))
  expect_equal(d$path, c("EC", "NT", "OW", "E", "I"))
  d <- collect_sorted(disposition_paths(p, date_to = "2026-01-31", phones = c("2015550101", "2015550102")))
  expect_equal(d$path, c("N", "XN"))
  expect_equal(nrow(collect_sorted(disposition_paths(p, sample_frac = 1, terminal = "engaged",
                                                     undeliverable_codes = NULL))), 5L)
})

test_that("the pattern syntax compiles to an RE2 regex over the letters", {
  compile <- function(x) .disposition_pattern_compile(x, "disposition_paths")
  expect_equal(compile("non_response > engaged|opted_in > completed|web_complete"),
               "[N].*[EO].*[CW]")
  expect_equal(compile("N >> E >> C|W"), "[N][E][CW]")
  expect_equal(compile("  non_response|undeliverable{3,}$ "), "[NX]{3,}$")
  expect_equal(compile("^c > * > n+"), "^[C].*..*[N]+")
  expect_equal(compile("W|completed"), "[CW]")
  expect_equal(compile("Refused {2,4}"), "[R]{2,4}")
  expect_equal(compile("* | N"), ".")
})

test_that("bad patterns say what is wrong and show the syntax", {
  compile <- function(x) .disposition_pattern_compile(x, "disposition_paths")
  expect_error(compile(""), "`pattern` must be a single non-empty string", class = "s160_error")
  expect_error(compile(c("N", "C")), "`pattern` must be a single non-empty string", class = "s160_error")
  expect_error(compile("compleated"), "unknown label `compleated`.*Steps are label names", class = "s160_error")
  expect_error(compile("N >"), "has an empty step", class = "s160_error")
  expect_error(compile("N > > C"), "has an empty step", class = "s160_error")
  expect_error(compile("N $ C"), "may use `\\^` / `\\$` only at its start / end", class = "s160_error")
  expect_error(compile("N{4,2}"), "repeat `\\{4,2\\}` outside 0-1000 or with min > max", class = "s160_error")
  expect_error(compile("N{1001}"), "outside 0-1000", class = "s160_error")
  expect_error(compile("N!"), "has a step it cannot read \\(`N!`\\)", class = "s160_error")
  skip_if_no_sequence()
  expect_error(disposition_paths(write_sequence_parquet(), "X > Y"),
               "disposition_paths: `pattern` has an unknown label `Y`", class = "s160_error")
})
