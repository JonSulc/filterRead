# What awk executables are known to handle differently than R, each as an awk
# expression that is 1 where the executable agrees with R on the data as fread
# reads it, named for the divergence it exercises. An element split() produces
# is typed the way a field is, so the hex probe runs in BEGIN with no input.
AWK_R_DIVERGENCE_PROBES <- c(
  "an interval `{n}` in a regex pattern" = '"aa" ~ /^a{2}$/',
  "a POSIX bracket class `[[:alpha:]]` in a regex pattern" =
    '"a" ~ /^[[:alpha:]]$/',
  "a hex value such as `0x10` in a field, read as a number" =
    'split("0x10", f) && f[1] < 5'
)

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
#' The awk executable is run once, to check that it runs awk and to report the
#' known divergences between it and R. The setting persists across sessions.
#'
#' @param executable The awk to run: a name found on `PATH`, or a path to one.
#'   Where `quote` is FALSE, shell text that invokes awk, such as
#'   `"LC_ALL=C gawk"`. NULL clears the setting.
#' @param quote Whether to quote `executable` for the shell, taking it as one
#'   path or name.
#' @return The configuration, invisibly.
#' @export
set_awk <- function(
  executable,
  quote = TRUE
) {
  config <- .filterread_env$config
  if (is.null(executable)) {
    config$awk <- NULL
    .filterread_env$config <- config
    .filterread_env$awk <- NULL
    return(invisible(config))
  }

  awk_executable <- resolve_awk_executable(executable, quote)
  hint <- if (!quote) {
    "\nQuote any path it holds, or pass quote = TRUE."
  } else if (grepl("[[:space:]]", executable)) {
    "\nIf it is shell text rather than one path or name, pass quote = FALSE."
  }
  divergences <- tryCatch(
    probe_awk(awk_executable),
    error = function(e) stop(conditionMessage(e), hint, call. = FALSE)
  )

  config$awk <- awk_executable
  .filterread_env$config <- config
  .filterread_env$awk <- awk_executable
  report_awk_r_divergences(awk_executable, divergences)
  invisible(config)
}

#' Resolve the awk executable a caller names
#'
#' @inheritParams set_awk
#' @return The awk executable as the shell invokes it: `executable`, quoted
#'   where `quote`.
#' @keywords internal
resolve_awk_executable <- function(
  executable,
  quote
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
  if (quote) shQuote(executable) else executable
}

#' Probe an awk executable for its divergences from R
#'
#' Runs the awk executable under `bash -c`, as a read does, over one awk
#' program that prints a sentinel and then one line per divergence probe.
#'
#' @param awk_executable The awk executable, as the shell invokes it.
#' @return Character vector naming the divergences the awk executable shows,
#'   empty where it agrees with R on every probe. Errors where it does not
#'   print the sentinel and one answer per probe.
#' @keywords internal
probe_awk <- function(awk_executable) {
  program <- paste0(
    "BEGIN {\n  print \"filterRead-probe\"\n",
    paste0("  print (", AWK_R_DIVERGENCE_PROBES, ")\n", collapse = ""),
    "}"
  )
  cmd <- paste(awk_executable, shQuote(program), "; true")
  stderr_file <- tempfile()
  on.exit(unlink(stderr_file), add = TRUE)
  output <- suppressWarnings(system2(
    "bash", c("-c", shQuote(cmd)),
    stdout = TRUE, stderr = stderr_file
  ))
  answers <- output[-1L]
  answers_each_probe <- identical(output[1L], "filterRead-probe") &&
    length(answers) == length(AWK_R_DIVERGENCE_PROBES) &&
    all(answers %in% c("0", "1"))
  if (!answers_each_probe) {
    printed <- c(readLines(stderr_file), output)
    stop(
      "Could not run the awk executable\n  ", awk_executable, "\n",
      if (length(printed) == 0L) {
        "It printed nothing."
      } else {
        paste(printed, collapse = "\n")
      },
      call. = FALSE
    )
  }
  names(AWK_R_DIVERGENCE_PROBES)[answers == "0"]
}

#' Report the divergences between an awk executable and R
#'
#' @inheritParams probe_awk
#' @param divergences Character vector naming the divergences, as
#'   `probe_awk()` returns them.
#' @return NULL, invisibly. Emits a message where `divergences` is non-empty.
#' @keywords internal
report_awk_r_divergences <- function(
  awk_executable,
  divergences
) {
  if (length(divergences) == 0L) {
    return(invisible())
  }
  message(
    "The awk executable\n  ", awk_executable, "\n",
    "handles the following differently than R:\n",
    paste0("  - ", divergences, "\n", collapse = ""),
    "A condition relying on one may not select the records it selects in R. ",
    "If\nthat matters for your data, call set_awk() to set an awk executable ",
    "that\nhandles them as R does; it reports any known divergence between R ",
    "and the\nexecutable it sets."
  )
}

#' Get the awk executable a read runs
#'
#' The configured executable, or `mawk` where it is installed, or `awk`. The
#' answer is resolved once a session, and resolving it reports the known
#' divergences between it and R.
#'
#' @return Character string: the awk executable, as the shell invokes it.
#' @export
get_awk <- function() {
  configured <- .filterread_env$config$awk
  if (!is.null(.filterread_env$awk)) {
    return(.filterread_env$awk)
  }
  awk_executable <- configured %||%
    if (nzchar(Sys.which("mawk"))) "mawk" else "awk"
  # get_awk() builds commands, some of which are returned rather than run, so
  # an awk executable that does not run is warned of rather than raised; a
  # read that runs it fails with the shell's own error.
  tryCatch(
    report_awk_r_divergences(awk_executable, probe_awk(awk_executable)),
    error = function(e) warning(conditionMessage(e), call. = FALSE)
  )
  .filterread_env$awk <- awk_executable
  awk_executable
}
