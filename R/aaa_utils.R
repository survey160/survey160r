# Shared internal utilities

# Null-coalescing operator. Used package-wide.
`%||%` <- function(a, b) if (is.null(a)) b else a

# Digit-normalize a phone for matching: strip non-digits, then drop a leading US
# country code so an 11-digit "1NXXNXXXXXX" matches a stored 10-digit number.
# Blank/NA -> NA. Shared by every screen (disposition_screen, opt_out_screen) so
# a sample matches the two datasets identically.
.normalize_phone <- function(x) {
  x <- gsub("[^0-9]", "", as.character(x))
  x[!nzchar(x)] <- NA_character_
  eleven <- !is.na(x) & nchar(x) == 11L & startsWith(x, "1")
  x[eleven] <- substr(x[eleven], 2L, 11L)
  x
}

# Prompt for a value and persist it to ~/.Renviron.
# When secret = TRUE, uses masked input if available.
prompt_and_save_renviron <- function(var_name, prompt_msg, secret = FALSE) { # nocov start
  message(prompt_msg)
  if (secret && requireNamespace("askpass", quietly = TRUE)) {
    value <- askpass::askpass(paste0(var_name, ": "))
    if (is.null(value)) stop(paste(var_name, "cannot be empty."), call. = FALSE)
  } else {
    if (secret) message("(Tip: install the 'askpass' package for masked input.)")
    value <- readline(paste0(var_name, ": "))
  }
  if (value == "") {
    stop(paste(var_name, "cannot be empty."), call. = FALSE)
  }
  renviron_path <- path.expand("~/.Renviron")
  if (file.exists(renviron_path)) {
    lines <- readLines(renviron_path, warn = FALSE)
    lines <- lines[!grepl(paste0("^", var_name, "="), lines)]
    writeLines(lines, renviron_path)
  }
  cat(paste0(var_name, "=", value, "\n"),
      file = renviron_path, append = TRUE)
  args <- list(value)
  names(args) <- var_name
  do.call(Sys.setenv, args)
  message(sprintf("Saved %s to ~/.Renviron.", var_name))
  value
} # nocov end
