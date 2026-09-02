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
