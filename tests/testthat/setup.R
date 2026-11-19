# Force deprecation warnings to fire on EVERY call during the test run.
#
# lifecycle::deprecate_warn() and rlang::warn(.frequency=) throttle repeats to
# once per R session per call site, keyed on a session-global store. That makes
# expect_warning() order-dependent across the suite (a second test hitting the
# same deprecation would see nothing). Setting the verbosity options defeats the
# throttle so every deprecation test is deterministic; reset after the suite.
withr::local_options(
  lifecycle_verbosity = "warning",
  rlib_warning_verbosity = "verbose",
  .local_envir = testthat::teardown_env()
)

# One time zone listing per test process. duckdb checks the session time zone
# against OlsonNames() on every connection it opens, OlsonNames() re-reads the
# time zone database directory on every call (a few milliseconds), and the
# disposition readers open a connection per call -- several hundred over the
# suite. The listing cannot change while the tests run, so the default call is
# answered from the first result; a call with arguments goes to the real one.
local({
  real_olson_names <- base::OlsonNames
  listing <- NULL
  testthat::local_mocked_bindings(
    OlsonNames = function(...) {
      if (...length() > 0L) return(real_olson_names(...))
      if (is.null(listing)) listing <<- real_olson_names()
      listing
    },
    .package = "base",
    .env = testthat::teardown_env()
  )
})
