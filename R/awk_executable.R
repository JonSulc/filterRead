#' Find the awk executables installed
#'
#' @return Named character vector of the paths found, one element per
#'   executable, named by executable.
#' @export
#' @examples
#' find_awk_executables()
find_awk_executables <- function() {
  found <- Sys.which(c("mawk", "gawk", "awk", "original-awk"))
  found[nzchar(found)]
}

#' Set the awk executable a read runs
#'
#' The setting persists across sessions.
#'
#' @param executable The awk to run: a name found on `PATH`, or a path to one.
#'   Where `validate` is FALSE, any shell fragment that invokes awk, such as
#'   `"busybox awk"`. NULL clears the setting.
#' @param validate Whether to check that `executable` is installed.
#' @return The configuration, invisibly.
#' @export
set_awk <- function(
  executable,
  validate = TRUE
) {
  config <- .filterread_env$config
  config$awk <- if (!is.null(executable)) {
    resolve_awk_fragment(executable, validate)
  }
  save_config(config)
  .filterread_env$config <- config
  invisible(config)
}

#' Resolve an executable to the fragment that invokes it
#'
#' @param executable The awk to run: a name found on `PATH`, or a path to one.
#'   Where `validate` is FALSE, any shell fragment that invokes awk.
#' @inheritParams set_awk
#' @return The fragment to store.
#' @keywords internal
resolve_awk_fragment <- function(
  executable,
  validate
) {
  if (
    !is.character(executable) || length(executable) != 1L ||
      is.na(executable) || !nzchar(executable)
  ) {
    stop(
      "The awk executable must be one character string.",
      call. = FALSE
    )
  }
  if (!validate) {
    return(executable)
  }
  if (!nzchar(Sys.which(executable))) {
    stop(
      "No awk executable found for ",
      encodeString(executable, quote = "\""),
      ". Pass validate = FALSE to store it anyway.",
      call. = FALSE
    )
  }
  if (grepl("[[:space:]]", executable)) {
    return(shQuote(executable))
  }
  executable
}

#' Get the awk executable a read runs
#'
#' The configured executable, or `mawk` where it is installed, or `awk`.
#' Whether mawk is installed is checked once a session.
#'
#' @return Character string: the shell fragment a read invokes awk with.
#' @export
get_awk <- function() {
  configured <- .filterread_env$config$awk
  if (!is.null(configured)) {
    return(configured)
  }
  if (is.null(.filterread_env$mawk_found)) {
    .filterread_env$mawk_found <- nzchar(Sys.which("mawk"))
  }
  if (.filterread_env$mawk_found) "mawk" else "awk"
}
