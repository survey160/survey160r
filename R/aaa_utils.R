# Shared internal utilities

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
