# Coverage for R/s160_gcs.R::s160_read_csv. Local-source sibling of
# s160_gcs_campaign_results_read(hash = TRUE) -- reads + stamps the same provenance attrs.

test_that("s160_read_csv stamps source_csv_hash and source_csv_path", {
  tmp <- withr::local_tempfile(lines = c("a,b", "1,2", "3,4"), fileext = ".csv")

  data <- s160_read_csv(tmp)

  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 2L)
  expect_match(attr(data, "source_csv_hash"), "^sha256:")
  expect_equal(attr(data, "source_csv_path"), tmp)
  # stringsAsFactors defaults to FALSE.
  expect_type(data$a, "integer")
})

test_that("s160_read_csv errors when the file is missing", {
  expect_error(
    s160_read_csv("/path/does/not/exist.csv"),
    "file not found"
  )
})

test_that("s160_read_csv forwards `...` to read.csv (e.g., sep)", {
  tmp <- withr::local_tempfile(lines = c("a\tb", "1\t2"), fileext = ".tsv")

  data <- s160_read_csv(tmp, sep = "\t")

  expect_equal(names(data), c("a", "b"))
  expect_equal(data$a, 1L)
  expect_equal(data$b, 2L)
})

test_that("s160_read_csv result is consumable by latency_run downstream", {
  # Use the same synthetic fixture the algorithm tests rely on. The
  # reader's job is to produce a stamped data frame; the algorithm
  # surfaces source_csv_hash / source_csv_path on result$meta.
  fx_path <- test_path("fixtures/synthetic.csv")
  data <- s160_read_csv(fx_path)
  result <- latency_run(campaign_id = 1, data = data)
  expect_equal(result$meta$source_csv_path, fx_path)
  expect_match(result$meta$source_csv_hash, "^sha256:")
})

test_that("s160_read_csv hash = FALSE skips the hashing pass", {
  tmp <- withr::local_tempfile(lines = c("a,b", "1,2"), fileext = ".csv")

  data <- s160_read_csv(tmp, hash = FALSE)

  expect_true(is.na(attr(data, "source_csv_hash")))
  expect_equal(attr(data, "source_csv_path"), tmp)
})

test_that("s160_read_csv rejects a non-scalar-logical hash", {
  tmp <- withr::local_tempfile(lines = c("a,b", "1,2"), fileext = ".csv")

  expect_error(s160_read_csv(tmp, hash = "FALSE"), "single TRUE or FALSE")
  expect_error(s160_read_csv(tmp, hash = c(TRUE, FALSE)), "single TRUE or FALSE")
  expect_error(s160_read_csv(tmp, hash = NA), "single TRUE or FALSE")
})

test_that("s160_csv_header makes colliding munged names unique", {
  tmp <- withr::local_tempfile(fileext = ".csv")
  # Both raw headers munge to "id.q1.scriptDate"; check.names = TRUE on the
  # body read would uniquify them, so the header peek must do the same (the
  # exact .N suffix placement is make.unique's business -- assert uniqueness).
  writeLines(c("id[q1]scriptDate,id.q1.scriptDate,campaignid", "a,b,5"), tmp)

  hdr <- s160_csv_header(tmp)
  expect_length(hdr, 3L)
  expect_false(anyDuplicated(hdr) > 0L)
  expect_contains(hdr, "campaignid")
  expect_true(all(grepl("^id\\.q1\\.scriptDate", hdr[1:2])))
})

test_that("s160_read_csv columns = projects to the requested columns", {
  tmp <- withr::local_tempfile(lines = c("campaignid,extra,userid", "5,junk,7"), fileext = ".csv")

  data <- s160_read_csv(tmp, columns = c("campaignid", "userid"))

  expect_equal(names(data), c("campaignid", "userid"))
})

test_that("s160_csv_header returns munged names and errors on missing file", {
  tmp <- withr::local_tempfile(lines = c("id[q1]scriptDate,campaignid", "2026-01-01,5"), fileext = ".csv")

  expect_equal(s160_csv_header(tmp), c("id.q1.scriptDate", "campaignid"))
  expect_error(s160_csv_header("/no/such/file.csv"), "file not found")
})

test_that("s160_csv_header fallback (no data.table) returns munged names", {
  tmp <- withr::local_tempfile(lines = c("id[q1]scriptDate,campaignid", "2026-01-01,5"), fileext = ".csv")

  stub_no_data_table()
  expect_equal(s160_csv_header(tmp), c("id.q1.scriptDate", "campaignid"))
})

test_that(".file_sha256 matches digest on every path: sha256sum, tools, digest", {
  path <- test_path("fixtures", "synthetic.csv")
  expected <- digest::digest(file = path, algo = "sha256")
  expect_equal(survey160r:::.file_sha256(path), expected)
  # No system sha256sum -> tools::sha256sum, then (no tools) digest.
  in_r <- survey160r:::.file_sha256
  mockery::stub(in_r, ".file_sha256_native", NULL)
  expect_equal(in_r(path), expected)
  mockery::stub(in_r, "get0", NULL)
  expect_equal(in_r(path), expected)
})

test_that(".file_sha256_native parses the tool's digest line or declines (mocked tool)", {
  hex <- strrep("ab", 32L)
  native <- function(out) {
    f <- survey160r:::.file_sha256_native
    mockery::stub(f, "Sys.which", "/usr/bin/sha256sum")
    mockery::stub(f, "system2", out)
    f
  }
  expect_equal(native(paste0(hex, "  a.csv"))("a.csv"), hex)
  expect_equal(native(paste0("\\", hex, "  a\\nb.csv"))("a\nb.csv"), hex)  # escaped name
  expect_null(native(structure(character(0), status = 1L))("a.csv"))
  expect_null(native("not a digest")("a.csv"))
  expect_null(native(function(...) stop("spawn failed"))("a.csv"))
  no_tool <- survey160r:::.file_sha256_native
  mockery::stub(no_tool, "Sys.which", "")
  expect_null(no_tool("a.csv"))
  # A relative name starting with "-" reaches the tool as "./-...", never as an
  # option or (for "-") standard input.
  seen <- NULL
  f <- native(function(cmd, args, ...) {
    seen <<- args
    paste0(hex, "  x")
  })
  for (name in c("-", "-b")) {
    f(name)
    expect_identical(seen, shQuote(paste0("./", name)))
  }
})

test_that(".file_sha256_native matches digest with the real tool", {
  skip_if(!nzchar(Sys.which("sha256sum")), "no system sha256sum")
  skip_on_os("windows")   # backslash is a path separator there, not a name character
  dir <- withr::local_tempdir()
  odd <- file.path(dir, "a b\\c 'q'.csv")   # space, backslash, quote: escaped output
  writeLines("x,y", odd)
  expect_equal(survey160r:::.file_sha256_native(odd),
               digest::digest(file = odd, algo = "sha256"))
  expect_null(survey160r:::.file_sha256_native(file.path(dir, "missing.csv")))
  for (name in c("-", "-b")) {
    writeLines(name, file.path(dir, name))
    expect_equal(withr::with_dir(dir, survey160r:::.file_sha256_native(name)),
                 digest::digest(file = file.path(dir, name), algo = "sha256"))
  }
})

test_that("timestamps = \"POSIXct\" decodes the id.* timestamp columns at read time", {
  fx_path <- test_path("fixtures", "synthetic.csv")
  chr <- s160_read_csv(fx_path)
  parsed <- s160_read_csv(fx_path, timestamps = "POSIXct")
  ts_cols <- grep("^id\\..+\\.(scriptDate|batchDate)$", names(chr), value = TRUE)
  expect_true(length(ts_cols) > 0L)
  for (col in ts_cols) {
    expect_s3_class(parsed[[col]], "POSIXct")
    expect_equal(attr(parsed[[col]], "tzone"), "UTC")
    expect_equal(as.numeric(parsed[[col]]),
                 as.numeric(parse_campaign_timestamps(chr[[col]])))
  }
  # Non-timestamp columns and the provenance attributes are untouched.
  other <- setdiff(names(chr), ts_cols)
  expect_identical(parsed[other], chr[other])
  expect_identical(attr(parsed, "source_csv_hash"), attr(chr, "source_csv_hash"))
  expect_identical(attr(parsed, "source_csv_path"), attr(chr, "source_csv_path"))
  # The default keeps the strings.
  expect_identical(s160_read_csv(fx_path, timestamps = "character"), chr)
})

test_that("timestamps = \"POSIXct\" gives the same latency and disposition results, parse failures included", {
  fx_path <- test_path("fixtures", "synthetic.csv")
  # Inject an unparseable (non-blank) timestamp into the raw CSV text, so the
  # reader cannot infer that column as POSIXct (fread keeps a column with any
  # unparseable cell as character) and a parse failure is counted downstream.
  lines <- readLines(fx_path)
  header <- strsplit(lines[[1L]], ",", fixed = TRUE)[[1L]]
  at <- match("id.intro.batchDate", header)
  row2 <- strsplit(lines[[2L]], ",", fixed = TRUE)[[1L]]
  row2[[at]] <- "not a timestamp"
  lines[[2L]] <- paste(row2, collapse = ",")
  tmp <- withr::local_tempfile(lines = lines, fileext = ".csv")
  chr <- s160_read_csv(tmp)
  expect_type(chr$id.intro.batchDate, "character")
  parsed <- s160_read_csv(tmp, timestamps = "POSIXct")
  expect_identical(attr(parsed$id.intro.batchDate, "parse_failed"),
                   c(TRUE, rep(FALSE, nrow(parsed) - 1L)))
  run_at <- as.POSIXct("2026-01-01", tz = "UTC")
  a <- latency_run(1L, chr, field_timezone = "America/New_York", run_at = run_at)
  b <- latency_run(1L, parsed, field_timezone = "America/New_York", run_at = run_at)
  expect_identical(a$consolidated, b$consolidated)
  expect_identical(a$diagnostics, b$diagnostics)
  expect_identical(a$latency_frame, b$latency_frame)
  expect_equal(a$diagnostics$parse_failures_per_column[["id.intro.batchDate"]], 1L)
})

test_that("timestamps rejects anything but character / POSIXct", {
  fx_path <- test_path("fixtures", "synthetic.csv")
  expect_error(s160_read_csv(fx_path, timestamps = "Date"),
               "`timestamps` must be \"character\" or \"POSIXct\"")
  expect_error(s160_read_csv(fx_path, timestamps = TRUE),
               "`timestamps` must be")
})
