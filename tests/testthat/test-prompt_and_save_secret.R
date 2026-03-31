test_that("prompts for secret and appends to .Renviron when file does not exist", {
  local_mocked_bindings(
    readline = function(...) "prompted-secret",
    file.exists = function(...) FALSE
  )

  captured_cat <- NULL
  local_mocked_bindings(
    cat = function(...) {
      captured_cat <<- list(...)
    }
  )

  suppressMessages(survey160r:::prompt_and_save_secret())

  expect_true(grepl("S160_GCS_CLIENT_SECRET=prompted-secret", captured_cat[[1]]))
  expect_true(captured_cat$append)
})

test_that("strips existing secret from .Renviron before writing", {
  local_mocked_bindings(
    readline = function(...) "new-secret",
    file.exists = function(...) TRUE,
    readLines = function(...) c("OTHER_VAR=keep", "S160_GCS_CLIENT_SECRET=old", "ANOTHER=also_keep")
  )

  written_lines <- NULL
  local_mocked_bindings(
    writeLines = function(lines, ...) {
      written_lines <<- lines
    },
    cat = function(...) NULL
  )

  suppressMessages(survey160r:::prompt_and_save_secret())

  expect_equal(written_lines, c("OTHER_VAR=keep", "ANOTHER=also_keep"))
})

test_that("errors when readline returns empty", {
  local_mocked_bindings(readline = function(...) "")

  expect_error(
    suppressMessages(survey160r:::prompt_and_save_secret()),
    "Client secret cannot be empty"
  )
})
