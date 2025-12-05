#' Create a new file interface object
#'
#' Creates a file interface for efficient reading and filtering of
#' tab/comma-separated files. The interface detects file format,
#' separators, column names, and encoding automatically.
#'
#' @param filename Character string. Path to the file to read. File
#'   must exist and can be gzipped.
#' @param extra_column_names_dt Data.table with additional column name
#'   mappings to recognize. Should have columns matching the structure
#'   of standard_names_dt. Default is NULL.
#' @param standard_names_dt Data.table with standard column name
#'   mappings. Default is summary_stats_standard_names_dt.
#' @param ieugwas_parsing Logical. Whether to detect and parse
#'   IEUGWAS-formatted files with special column encodings.
#'   Default is TRUE.
#' @param build Character string specifying genome build (e.g., "b37",
#'   "b38") for genomic data. If NULL (default), attempts to detect
#'   from file.
#'
#' @return A file_interface object (list with class "file_interface")
#'   containing file metadata including filename, separator, column
#'   information, and compression status.
#'
#' @examples
#' \dontrun{
#' finterface <- new_file_interface("data.txt.gz")
#' head(finterface)
#' finterface[chr == 1 & pval < 5e-8]
#' }
#'
#' @export
new_file_interface <- function(
  filename,
  extra_column_names_dt = NULL,
  standard_names_dt = summary_stats_standard_names_dt,
  ieugwas_parsing = TRUE,
  build = NULL
) {
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
  finterface$trim_prefix <- detect_trim_prefix(finterface)

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

  if (!is.null(build)) {
    finterface$build <- build
  } else {
    finterface$build <- get_build_from_file_interface(finterface)
  }

  finterface
}

#' Check if object is a file interface
#'
#' Tests whether an object inherits from the "file_interface" class.
#'
#' @param finterface Object to test.
#'
#' @return Logical. TRUE if object is a file_interface, FALSE
#'   otherwise.
#'
#' @export
is_file_interface <- function(finterface) {
  inherits(finterface, "file_interface")
}

is_gzipped <- function(
  filename
) {
  stringr::str_detect(filename, "[.]gz$")
}

get_file_separator <- function(
  finterface
) {
  dt_output <- head(finterface, nlines = 100L, verbose = TRUE) |>
    capture.output()

  seps <- stringr::str_match(dt_output, "sep=([^ ]+)[ ]+with ([0-9]+) lines")
  seps <- seps[!is.na(seps[, 1L]), -1L, drop = FALSE]
  best_sep <- seps[
    which.max(as.numeric(seps[, 2L])),
    1L
  ]
  # '\t' is reported in hex ('0x9')
  if (grepl("^0x", best_sep)) {
    return(
      as.raw(best_sep) |>
        rawToChar()
    )
  }
  if (!grepl("^'.+'$", best_sep)) {
    stop(
      "Unsupported field separator: ", best_sep, ". ",
      "If you think this is an error, please report this."
    )
  }
  substr(best_sep, 2, nchar(best_sep) - 1)
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

detect_trim_prefix <- function(finterface) {
  detect_prefix_from_first_line(
    finterface,
    skip_prefix = finterface$comment_prefix
  )
}

validate_file_interface <- function(
  finterface
) {
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

#' Return first rows of a file interface
#'
#' S3 method for head() that reads the first n rows from a file (in addition to
#' the header) using the file interface. Column names are converted to standard
#' column names and transformed if necessary.
#'
#' @param x A file_interface object.
#' @param nlines Integer. Number of rows to read. Default is 1.
#' @param return_only_cmd Logical. If TRUE, returns the awk command
#'   string instead of executing it. Useful for debugging or piping
#'   to other commands. Default is FALSE.
#' @param ... Additional arguments passed to data.table::fread().
#'
#' @return A data.table containing the first nrows of the file.
#'
#' @examples
#' \dontrun{
#' finterface <- new_file_interface("data.txt.gz")
#' head(finterface, nlines = 10)
#' }
#'
#' @export
head.file_interface <- function(
  x,
  nlines = 1,
  return_only_cmd = FALSE,
  ...
) {
  cmd <- compile_awk_cmds(x, nlines = nlines + 1)

  if (return_only_cmd) {
    return(cmd)
  }

  if (!"column_info" %in% names(x)) {
    return(
      data.table::fread(
        cmd = paste("bash -c", shQuote(cmd)),
        ...
      )
    )
  }
  data.table::fread(
    cmd = paste("bash -c", shQuote(cmd)),
    col.names = {
      if (needs_rsid_matching(x)) {
        column_names(x, original = TRUE)
      } else {
        column_names(x)
      }
    },
    ...
  )
}

#' Subset a file interface with filtering conditions
#'
#' S3 method for `[` that reads and filters data from a file using
#' R-style conditions. Conditions are translated to awk commands for
#' efficient filtering. Supports logical expressions with standardized column
#' names and operators, including parentheses.
#'
#' @param finterface A file_interface object.
#' @param conditions R expression specifying filter conditions. Can
#'   use column names, comparison operators (&lt;, &gt;, ==, !=, etc.),
#'   and logical operators (&, |). Examples: \code{pval < 5e-8},
#'   \code{chr == 1 & pos > 1000000}. If NULL, returns all rows.
#' @param ... Additional arguments passed to data.table::fread().
#' @param return_only_cmd Logical. If TRUE, returns the awk command
#'   string instead of executing it. Useful for debugging or piping
#'   to other commands. Default is FALSE.
#'
#' @return If return_only_cmd is FALSE (default), returns a data.table
#'   with filtered rows. If return_only_cmd is TRUE, returns a
#'   character string containing the awk command.
#'
#' @examples
#' \dontrun{
#' finterface <- new_file_interface("gwas_data.txt.gz")
#'
#' # Filter by p-value
#' results <- finterface[pval < 5e-8]
#'
#' # Multiple conditions
#' results <- finterface[chr == 1 & pval < 0.001]
#'
#' # Get command without executing
#' cmd <- finterface[chr == 1, return_only_cmd = TRUE]
#' }
#'
#' @export
`[.file_interface` <- function(
  finterface,
  conditions = NULL,
  ...,
  return_only_cmd = FALSE
) {
  fcondition <- new_filter_condition(
    rlang::enexpr(conditions),
    finterface = finterface
  )
  command_line <- fcondition_to_awk(
    fcondition,
    return_only_cmd = return_only_cmd
  )

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

#' Print a file interface object
#'
#' S3 method for print() that displays a summary of the file
#' interface including filename, columns, prefixes, compression,
#' and quoting information.
#'
#' @param x A file_interface object.
#' @param ... Additional arguments (unused).
#'
#' @return Invisibly returns x. Called for side effect of printing.
#'
#' @export
print.file_interface <- function(
  x,
  ...
) {
  cat(sprintf("\"%s\"\n", x$filename))
  cat(sprintf(
    "Standardized columns: %s\n",
    paste(na.omit(x$column_info$standard_name), collapse = ", ")
  ))
  cat(sprintf(
    "Prefixes: %s\n",
    ifelse(length(x$column_info$prefixes) == 0,
      "none",
      sapply(
        names(x$column_info$prefixes),
        \(col_name) {
          sprintf("%s - \"%s\"", col_name, x$column_info$prefixes[[col_name]])
        }
      ) |>
        paste(collapse = ", ")
    )
  ))
  cat(
    sprintf(
      "Gzipped: %s, Quoted: %s\n",
      x$gzipped,
      ifelse(any(unlist(x$column_info$quoted_values)),
        paste(
          names(x$column_info$quoted_values)[
            unlist(x$column_info$quoted_values)
          ],
          collapse = ", "
        ),
        FALSE
      )
    )
  )
  cat(
    "Requires RSID matching:",
    x$needs_rsid_matching,
    "\n"
  )
  cat(
    "Genome build:",
    x$build,
    "\n"
  )
  cat(
    sprintf(
      "Commented line prefix: %s, header prefix: %s\n",
      ifelse(
        is.null(x$comment_prefix),
        "none",
        sprintf("'%s'", x$comment_prefix)
      ),
      ifelse(
        is.null(x$trim_prefix),
        "none",
        sprintf("'%s'", x$trim_prefix)
      )
    )
  )
  cat(
    sprintf(
      "Value separator: '%s'\n",
      x$sep
    )
  )
}
