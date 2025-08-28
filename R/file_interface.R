#' @import data.table

#' @export
new_file_interface <- function(
    filename,
    extra_column_names_dt = NULL,
    standard_names_dt = summary_stats_standard_names_dt,
    ieugwas_parsing = TRUE) {
  stopifnot(is.character(filename))
  stopifnot(file.exists(filename))
  standard_names_dt <- list(
    standard_names_dt,
    extra_column_names_dt
  ) |>
    data.table::rbindlist(fill = TRUE, use.names = TRUE)
  finterface <- structure(
    list(
      filename = filename,
      gzipped = is_gzipped(filename)
    ),
    class = c("file_interface", "character")
  )

  # Detect comment and drop prefixes
  finterface$comment_prefix <- detect_comment_prefix(finterface)
  finterface$trim_prefix <- detect_trim_prefix(finterface, finterface$comment_prefix)

  finterface$sep <- get_file_separator(finterface)

  # Check if IEUGWAS format and add parsing info if so
  if (ieugwas_parsing && is_ieugwas_file(finterface)) {
    standard_names_dt <- list(
      standard_names_dt,
      get_ieugwas_column_parsing(finterface)
    ) |>
      data.table::rbindlist(fill = TRUE, use.names = TRUE)
  }

  finterface$column_info <- get_column_info(
    finterface,
    standard_names_dt = standard_names_dt
  )

  finterface$needs_rsid_matching <- needs_rsid_matching(finterface)

  finterface
}

#' @export
is_file_interface <- function(finterface) inherits(finterface, "file_interface")

is_gzipped <- function(
    filename) {
  stringr::str_detect(filename, "[.]gz$")
}

get_file_separator <- function(
    finterface) {
  dt_output <- head(finterface, nlines = 1L, verbose = TRUE) |>
    capture.output() |>
    stringr::str_match("sep='([^']+)'")
  dt_output[!is.na(dt_output[, 1L]), 2L][1L]
}

detect_prefix_from_first_line <- function(finterface, skip_prefix = NULL) {
  # Get file reading command based on compression
  if (finterface$gzipped) {
    file_cmd <- sprintf("zcat %s", finterface$filename)
  } else {
    file_cmd <- sprintf("cat %s", finterface$filename)
  }

  # Build command to get the first line
  if (!is.null(skip_prefix)) {
    # Remove the ^ for grep pattern
    grep_pattern <- substr(skip_prefix, 2, nchar(skip_prefix))
    first_line_cmd <- sprintf("%s | grep -v '%s' | head -n 1", file_cmd, grep_pattern)
  } else {
    first_line_cmd <- paste(file_cmd, "| head -n 1")
  }

  first_line <- system(first_line_cmd, intern = TRUE)

  if (length(first_line) == 0 || nchar(first_line) == 0) {
    return(NULL)
  }

  # Extract non-alphanumeric prefix
  prefix_match <- regexpr("^[^A-Za-z0-9\" ]+", first_line)
  if (prefix_match > 0) {
    prefix <- substr(first_line, 1, attr(prefix_match, "match.length"))
    escaped_prefix <- gsub("([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1", prefix)
    return(paste0("^", escaped_prefix))
  }

  NULL
}

detect_comment_prefix <- function(finterface) {
  detect_prefix_from_first_line(finterface)
}

detect_trim_prefix <- function(finterface, comment_prefix) {
  detect_prefix_from_first_line(finterface, skip_prefix = comment_prefix)
}

validate_file_interface <- function(
    finterface) {
  if (!all(c("gzipped", "values_are_quoted") %chin% attributes(finterface))) {
    stop(
      "finterface is missing attributes ",
      c("gzipped", "values_are_quoted")[
        !c("gzipped", "values_are_quoted") %chin% attributes(finterface)
      ] |>
        paste(collapse(", "))
    )
  }
  stopifnot(is.logical(attr(finterface, "gzipped")))
  stopifnot(is.logical(attr(finterface, "values_are_quoted")))
  stopifnot(file.exists(finterface))
}

#' @export
head.file_interface <- function(
    finterface,
    nlines = 1,
    ...) {
  if (!"column_info" %in% names(finterface)) {
    return(
      data.table::fread(
        cmd = compile_awk_cmds(finterface,
          nlines = nlines + 1
        ),
        ...
      )
    )
  }
  data.table::fread(
    cmd = compile_awk_cmds(finterface, nlines = nlines + 1),
    col.names = {
      if (needs_rsid_matching(finterface)) {
        column_names(finterface, original = TRUE)
      } else {
        column_names(finterface)
      }
    },
    ...
  )
}

#' @export
`[.file_interface` <- function(
    finterface,
    conditions = NULL,
    ...,
    return_only_cmd = FALSE) {
  fcondition <- new_filter_condition(
    rlang::enexpr(conditions),
    finterface = finterface
  )
  command_line <- fcondition_to_awk(fcondition)

  if (return_only_cmd) {
    return(command_line)
  }

  data.table::fread(
    cmd = paste("bash -c", shQuote(command_line)),
    ...,
    col.names = {
      if (needs_rsid_matching(finterface) &
        !has_genomic_condition(fcondition)) {
        column_names(finterface, original = TRUE)
      } else {
        column_names(finterface)
      }
    }
  )
}

#' @export
print.file_interface <- function(
    finterface) {
  cat(sprintf("\"%s\"\n", finterface$filename))
  cat(sprintf(
    "Columns: %s\n",
    paste(finterface$column_info$name, collapse = ", ")
  ))
  cat(sprintf(
    "Prefixes: %s\n",
    ifelse(length(finterface$column_info$prefixes) == 0,
      "none",
      sapply(
        names(finterface$column_info$prefixes),
        \(col_name) {
          sprintf("%s - \"%s\"", col_name, finterface$column_info$prefixes[[col_name]])
        }
      ) |>
        paste(collapse = ", ")
    )
  ))
  cat(
    sprintf(
      "Gzipped: %s, Quoted: %s",
      finterface$gzipped,
      ifelse(any(unlist(finterface$column_info$quoted_values)),
        paste(
          names(finterface$column_info$quoted_values)[
            unlist(finterface$column_info$quoted_values)
          ],
          collapse = ", "
        ),
        FALSE
      )
    ),
    "\n"
  )
}
