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
  build = c("auto", "b36", "b37", "b38", "none")
) {
  stopifnot(is.character(filename))
  filename <- path.expand(filename)
  stopifnot(file.exists(filename))
  if (!is.null(build)) {
    build <- match.arg(build)
  }

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

  # Decide which of the leading lines is the header
  line_prefixes <- detect_line_prefixes(finterface)
  finterface$comment_prefix <- line_prefixes$comment
  finterface$trim_prefix <- line_prefixes$trim

  finterface$sep <- get_file_separator(finterface)

  # Check if IEUGWAS format and add parsing info if so
  if (ieugwas_parsing) {
    standard_names_dt <- list(
      standard_names_dt,
      get_ieugwas_parsing(finterface)
    ) |>
      data.table::rbindlist(fill = TRUE, use.names = TRUE)
  }

  finterface$column_info <- get_column_info(
    finterface,
    standard_names_dt = standard_names_dt
  )

  finterface$needs_rsid_matching <- needs_rsid_matching(finterface)

  if (is.null(build)) {} else if (build %in% c("b36", "b37", "b38")) {
    if (build == "b36" && needs_rsid_matching(finterface)) {
      warning("File ", filename, " uses RSID-based indexing")
    }
    build(finterface) <- build
  } else if (build == "infer" ||
    (build == "auto" && !needs_rsid_matching(finterface))) {
    # Only infer the build if file doesn't require RSID-matching or specified
    # (build inferrence on RSID-indexed files is slow)
    build(finterface) <- get_build_from_file_interface(finterface)
  } else {
    if (needs_rsid_matching(finterface) && build == "auto") {
      # Skip build inferrence with message
      message(
        "File has RSID-based indexing, no inherent build.",
        " To enable position-based sorting, specifying a build using the",
        " `build` parameter or set one using build<-().\n",
        "Using default build b38."
      )
    }
    build(finterface) <- "b38"
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

#' Check if a file is gzip-compressed
#'
#' @param filename Path to the file
#' @return Logical indicating whether the file has a .gz extension
#' @keywords internal
is_gzipped <- function(
  filename
) {
  stringr::str_detect(filename, "[.]gz$")
}

#' Detect the field separator of a file
#'
#' Uses data.table's fread to detect the most likely field separator
#' by analyzing the first 100 lines of the file. fread reports every
#' candidate it weighs, so the winner is picked the way fread picks it:
#' the candidate parsing the most lines, and among those, the one
#' splitting them into the most fields.
#'
#' @param finterface A file_interface object
#' @return Character string containing the detected separator
#' @keywords internal
get_file_separator <- function(
  finterface
) {
  # Detection needs fread to sniff, so ask for the automatic separator
  dt_output <- head(
    finterface,
    nlines = 100L,
    sep = "auto",
    verbose = TRUE
  ) |>
    capture.output()

  # The separator is reported either quoted (e.g. sep=',', or sep=' ' for a
  # space, whose interior space must stay inside the capture) or as hex for
  # tab (sep=0x9). Match a quoted value first, then any run of non-space
  # characters.
  seps <- stringr::str_match(
    dt_output,
    "sep=('[^']*'|[^ ]+)[ ]+with ([0-9]+) lines of ([0-9]+) fields"
  )
  seps <- seps[!is.na(seps[, 1L]), -1L, drop = FALSE]
  best_sep <- seps[
    order(-as.numeric(seps[, 2L]), -as.numeric(seps[, 3L]))[1L],
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

#' Read a file's leading lines, optionally skipping lines that match
#'
#' @param finterface A file_interface object
#' @param nlines Number of lines to read
#' @param skip_pattern Optional anchored extended regex; lines matching it are
#'   skipped before the leading lines are taken
#' @return Character vector of at most `nlines` lines, shorter when the file
#'   holds fewer
#' @keywords internal
read_leading_lines <- function(
  finterface,
  nlines = 1L,
  skip_pattern = NULL
) {
  read_cmd <- build_file_read_cmd(finterface)
  if (!is.null(skip_pattern)) {
    read_cmd <- sprintf(
      "%s | grep -E -v %s",
      read_cmd,
      shQuote(skip_pattern, type = "sh")
    )
  }
  sprintf("%s | head -n %d", read_cmd, nlines) |>
    system(intern = TRUE)
}

#' Extract a line's leading punctuation run as a regex
#'
#' Every metacharacter in the run is backslash-escaped, so the pattern matches
#' the run literally. The escaping targets extended regular expressions, the
#' dialect shared by R's `sub()` and `grep -E`.
#'
#' @param line Character vector; only its first element is examined
#' @return Anchored, escaped extended regex matching the line's leading run of
#'   punctuation, or NULL when the line starts with none
#' @keywords internal
extract_prefix_pattern <- function(line) {
  line <- head(line, 1)
  if (length(line) == 0 || nchar(line) == 0) {
    return(NULL)
  }
  prefix_match <- regexpr("^[^A-Za-z0-9\" ]+", line)
  if (prefix_match < 0) {
    return(NULL)
  }
  substr(line, 1, attr(prefix_match, "match.length")) |>
    escape_extended_regex() |>
    paste0("^", x = _)
}

#' Escape every extended-regex metacharacter in a string
#'
#' @param pattern Character string to escape
#' @return The string with each metacharacter preceded by a backslash
#' @keywords internal
escape_extended_regex <- function(pattern) {
  metacharacters <- c(
    "\\", ".", "*", "+", "?", "^", "$", "(", ")", "[", "]", "{", "}", "|"
  )
  for (metacharacter in metacharacters) {
    pattern <- gsub(
      metacharacter,
      paste0("\\", metacharacter),
      pattern,
      fixed = TRUE
    )
  }
  pattern
}

#' Compare the field shape of two lines
#'
#' The dominant separator of `candidate_header_line` decides the comparison.
#'
#' @param candidate_header_line Character string tested as the header
#' @param candidate_data_line Character string tested as the line below it
#' @return TRUE when both lines carry the header candidate's dominant
#'   separator the same number of times, FALSE when they differ, and NA when
#'   neither line carries a candidate separator
#' @keywords internal
field_shapes_agree <- function(
  candidate_header_line,
  candidate_data_line
) {
  candidate_separators <- c("\t", ",", ";", " ")
  header_counts <- stringr::str_count(
    candidate_header_line,
    stringr::fixed(candidate_separators)
  )
  data_counts <- stringr::str_count(
    candidate_data_line,
    stringr::fixed(candidate_separators)
  )
  if (max(header_counts) == 0L) {
    return(if (max(data_counts) == 0L) NA else FALSE)
  }
  dominant <- which.max(header_counts)
  header_counts[dominant] == data_counts[dominant]
}

#' Detect a file's comment and trim prefixes
#'
#' Decides which of a file's leading lines is the header, and reports the
#' prefix carried by the lines above it and the prefix carried by the header
#' itself. A prefixed first line is the header when no line follows it, or
#' when [field_shapes_agree()] finds the first line below carrying the
#' stripped line's dominant separator the same number of times. Otherwise the
#' prefix marks comments, and the header is the first line that does not carry
#' it. The trim prefix is what awk strips from the header, e.g., '#CHROM' ->
#' 'CHROM'.
#'
#' @param finterface A file_interface object
#' @return List with elements `comment` and `trim`, each an anchored regex or
#'   NULL
#' @keywords internal
detect_line_prefixes <- function(finterface) {
  leading_lines <- read_leading_lines(finterface, nlines = 2L)
  prefix <- head(leading_lines, 1) |>
    extract_prefix_pattern()
  if (is.null(prefix)) {
    return(list(comment = NULL, trim = NULL))
  }

  below <- read_leading_lines(finterface, skip_pattern = prefix)
  if (length(below) == 0) {
    if (length(leading_lines) != 1L) {
      stop(
        "Every line of ", finterface$filename,
        " carries the same prefix, leaving no header to read."
      )
    }
    return(list(comment = NULL, trim = prefix))
  }

  shapes_agree <- field_shapes_agree(sub(prefix, "", leading_lines[1]), below)
  if (is.na(shapes_agree)) {
    stop(
      "Cannot tell whether the first line of ", finterface$filename,
      " is a header or a comment: neither it nor the line below carries any",
      " of the separators this package reads (tab, comma, semicolon, space)."
    )
  }
  if (shapes_agree) {
    return(list(comment = NULL, trim = prefix))
  }

  list(comment = prefix, trim = extract_prefix_pattern(below))
}

#' Validate a file_interface object
#'
#' Checks that a file_interface has all required attributes and that the
#' referenced file exists.
#'
#' @param finterface A file_interface object to validate
#' @return NULL invisibly; stops with an error if validation fails
#' @keywords internal
validate_file_interface <- function(
  finterface
) {
  if (!all(c("gzipped", "values_are_quoted") %chin% attributes(finterface))) {
    stop(
      "finterface is missing attributes ",
      c("gzipped", "values_are_quoted")[
        !c("gzipped", "values_are_quoted") %chin% attributes(finterface)
      ] |>
        paste(collapse = ", ")
    )
  }
  stopifnot(is.logical(attr(finterface, "gzipped")))
  stopifnot(is.logical(attr(finterface, "values_are_quoted")))
  stopifnot(file.exists(finterface))
}

#' Muffle data.table's empty-input warning
#'
#' `fread` warns and returns a NULL data.table when awk emits no bytes;
#' both read paths replace that result with the read's schema.
#'
#' @param w A warning condition.
#'
#' @return NULL, invoked for the `muffleWarning` restart.
#' @keywords internal
muffle_empty_read_warning <- function(w) {
  if (grepl("has size 0", conditionMessage(w), fixed = TRUE)) {
    invokeRestart("muffleWarning")
  }
}

#' Build a zero-row result carrying a read's schema
#'
#' Stands in for the table a read returns when there is no data row to
#' read. Every column is `logical`; the package declares no type per
#' standard column.
#'
#' @param read_columns Character vector of the read's column names.
#'
#' @return A zero-row data.table with those columns.
#' @keywords internal
empty_read_result <- function(read_columns) {
  rep(list(logical(0)), length(read_columns)) |>
    stats::setNames(read_columns) |>
    data.table::setDT()
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
#' @param sep Field separator for the read. Defaults to the separator
#'   detected for the file: awk preserves it, and the handful of rows a
#'   read emits is too small a sample to detect it from, since a comma
#'   inside a single field is enough to make fread split on the wrong
#'   character.
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
  ...,
  sep = x$sep
) {
  # nlines = 0 cannot be served via awk: empty fread input warns and
  # returns NULL. column_info does not record column types, so read one
  # row to let fread infer them, then drop it.
  if (nlines == 0 && !return_only_cmd) {
    return(head(x, 1L, ...)[0])
  }

  # When column_info does not exist, head() is called for data.table::fread()
  # to parse column names, in which case we need to return one additional row
  # (the header), otherwise the header is skipped
  has_column_info <- "column_info" %in% names(x)
  effective_nlines <- nlines + !has_column_info

  cmd <- compile_awk_cmds(
    x,
    nlines = effective_nlines,
    skip_header = has_column_info,
    return_only_cmd = return_only_cmd
  )

  if (return_only_cmd) {
    return(cmd)
  }

  if (!has_column_info) {
    return(
      data.table::fread(
        cmd = paste("bash -c", shQuote(cmd)),
        sep = sep,
        ...
      )
    )
  }
  read_columns <- column_names(x, add_rsid_genomic_columns = FALSE)
  result <- withCallingHandlers(
    data.table::fread(
      cmd = paste("bash -c", shQuote(cmd)),
      header = FALSE,
      sep = sep,
      col.names = read_columns,
      colClasses = column_class_overrides(x),
      ...
    ),
    warning = muffle_empty_read_warning
  )

  # fread returns a 0-column table when the file holds no data rows,
  # leaving no row for it to infer types from.
  if (ncol(result) == 0L) {
    return(empty_read_result(read_columns))
  }
  result
}

#' Build a zero-row result for a query that matched nothing
#'
#' Takes the file's own columns from `head(finterface, 0)`, which reads
#' a real row so `fread` infers their types, and adds the columns a
#' positional query on an rsid-indexed file draws from the tabix index.
#'
#' @param finterface File interface object.
#' @param fcondition A filter_condition object.
#'
#' @return A zero-row data.table with the query's columns.
#' @keywords internal
empty_read_for_query <- function(
  finterface,
  fcondition
) {
  from_file <- head(finterface, 0)
  read_columns <- column_names(
    finterface,
    add_rsid_genomic_columns = has_genomic_condition(fcondition)
  )
  index_columns <- setdiff(read_columns, names(from_file))
  if (length(index_columns) == 0L) {
    return(from_file)
  }
  # chr and pos reach a result from the tabix index rather than from
  # the file, so no read of the file supplies their types. The index
  # holds raw chromosome names, so chr is a character string; positions
  # are always digits.
  index_types <- list(chr = character(0), pos = integer(0))
  cbind(
    data.table::setDT(index_types[index_columns]),
    from_file
  )[, .SD, .SDcols = read_columns]
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
#' @param nlines Optional integer cap on the number of matching lines to
#'   return. NULL or `integer(0)` means no cap.
#' @param ... Additional arguments passed to data.table::fread().
#' @param sep Field separator for the read. Defaults to the separator
#'   detected for the file: awk preserves it, and the handful of rows a
#'   read emits is too small a sample to detect it from, since a comma
#'   inside a single field is enough to make fread split on the wrong
#'   character.
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
  nlines = NULL,
  ...,
  sep = finterface$sep,
  return_only_cmd = FALSE
) {
  fcondition <- new_filter_condition(
    rlang::enquo(conditions),
    finterface = finterface
  )

  # Short-circuit unsatisfiable conditions: an empty included
  # genomic_regions cannot match any row regardless of non-genomic
  # constraints, so skip awk entirely.
  gregions <- fc_genomic_regions(fcondition)
  if (!return_only_cmd
    && !is.null(gregions)
    && nrow(gregions) == 0
    && is_included(gregions)) {
    return(empty_read_for_query(finterface, fcondition))
  }

  command_line <- fcondition_to_awk(
    fcondition,
    nlines = nlines,
    return_only_cmd = return_only_cmd
  )

  if (return_only_cmd) {
    return(command_line)
  }

  additional_files <- attr(command_line, "additional_files")
  if (length(additional_files) > 0) {
    on.exit(unlink(additional_files), add = TRUE)
  }

  result <- withCallingHandlers(
    data.table::fread(
      cmd = paste("bash -c", shQuote(command_line)),
      header = FALSE,
      sep = sep,
      ...,
      col.names = column_names(
        finterface,
        add_rsid_genomic_columns = has_genomic_condition(fcondition)
      ),
      colClasses = column_class_overrides(finterface)
    ),
    warning = muffle_empty_read_warning
  )

  # fread returns a 0-column table when awk emits no bytes; recover the schema
  if (ncol(result) == 0L) {
    return(empty_read_for_query(finterface, fcondition))
  }

  result
}

#' Load a full file from the file_interface
#'
#' Loads all data from the file_interface without filtering but still providing
#' post-processing (standardizing names, etc.). RSID-indexed files are not
#' re-indexed with chr or pos.
#'
#' @param file_interface A file_interface object.
#'
#' @return A data.table with the loaded data.
#'
#' @export
read_file_interface <- function(
  file_interface
) {
  file_interface[]
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
    build(x),
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

#' @export
dimnames.file_interface <- function(x) {
  list(
    NULL,
    x$column_info$name[!is.na(x$column_info$name)]
  )
}
