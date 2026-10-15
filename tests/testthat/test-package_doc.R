# The package help page (?survey160r) is the grouped index of the whole API --
# the r-universe manual lists it first. Every export must appear on it.

package_page <- function() {
  source_rd <- test_path("..", "..", "man", "survey160r-package.Rd")
  if (file.exists(source_rd)) return(tools::parse_Rd(source_rd))  # source tree
  tools::Rd_db("survey160r")[["survey160r-package.Rd"]]          # installed (R CMD check)
}

test_that("every exported function is listed on the package help page", {
  page <- paste(capture.output(tools::Rd2txt(package_page())), collapse = "\n")
  missing <- Filter(function(fn) !grepl(fn, page, fixed = TRUE),
                    getNamespaceExports("survey160r"))
  expect_identical(missing, character(0))
})
