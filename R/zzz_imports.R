# Centralized roxygen-driven imports.

#' @importFrom rlang .data
#' @importFrom utils head
NULL

# Startup hint: duckdb is a Suggests, but the disposition readers are far lighter
# with it (it keeps a large projection read out of R's memory). Point attached
# users at it once, on library()/require() only (never for programmatic
# survey160r:: use), and stay silent once it is installed.
.onAttach <- function(libname, pkgname) {
  if (!requireNamespace("duckdb", quietly = TRUE)) {
    packageStartupMessage(
      "survey160r: install.packages(\"duckdb\") for low-memory, much faster ",
      "disposition reads (the full projection otherwise loads into memory).")
  }
}
