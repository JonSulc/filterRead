#' @import data.table

#' @export
new_file_interface <- function(
  filename,
  column_names    = summary_stats_column_names,
  prefixes        = summary_stats_prefixes,
  encoded_columns = summary_stats_encoded_columns
) {
  stopifnot(is.character(filename))
  stopifnot(file.exists(filename))
  finterface <- structure(list(filename = filename,
                               gzipped  = is_gzipped(filename)),
                          class = c("file_interface", "character"))

  finterface$sep <- get_file_separator(finterface)
  finterface$column_info <- get_column_info(
    finterface,
    column_names    = column_names,
    prefixes        = prefixes
  )

  finterface <- add_encoded_columns(finterface, encoded_columns)

  finterface
}

get_column_info <- function(
  finterface,
  column_names    = NULL,
  prefixes        = summary_stats_prefixes
) {
  file_colnames <- get_column_names(finterface) |>
    swap_in_column_names(column_names)

  finterface$column_info <- list(
    index         = setNames(seq_along(file_colnames),
                             names(file_colnames)),
    bash_index    = file_colnames
  )

  column_prefixes <- get_prefixes(finterface, file_colnames, prefixes)

  c(
    finterface$column_info,
    list(
      quoted_values    = are_values_quoted(finterface) |>
        as.list(),
      prefixes         = column_prefixes
    )
  )
}

add_encoded_columns <- function(
  finterface,
  encoded_columns
) {
  encoding_columns <- get_encoding_columns(finterface, encoded_columns)

  if (is.null(encoding_columns)) return(finterface)

  finterface$column_info$encoded_columns <- encoding_columns$encoded_column
  finterface$column_info$encoding_columns <- encoding_columns

  finterface$column_info$bash_index <- finterface$column_info$bash_index |>
    c(encoding_columns[, as.list(setNames(substitutes, encoded_column))])

  finterface
}

get_encoding_columns <- function(
  finterface,
  encoded_columns
) {
  encoded <- intersect(names(encoded_columns),
                       names(finterface$column_info$bash_index))
  if (length(encoded) == 0) {
    return()
  }

  encoded_dt <- head(finterface)[, .SD, .SDcols = encoded]

  encoding_pattern_matches <- setNames(nm = encoded) |>
    lapply(\(column_name) {
      sapply(encoded_columns[[encoded]], \(pattern) {
        if (grepl(pattern$regex, encoded_dt[[encoded]])
            # Only return as an encoding column if the values are not also in
            # another column
            & any(!names(pattern$substitutes) %in% names(finterface$column_info$index))) {
          pattern
        }
      })
    })

  # TODO Check for 0 regex matches
  # TODO Check returning multiple
  encoding_pattern_matches <- encoding_pattern_matches |>
    lapply(\(column_patterns) {
      single_match <- column_patterns[!sapply(column_patterns, is.null)]
      if (1 < length(single_match)) {
        warning("Several regex patterns match columns encoding data:\n",
                paste(sapply(single_match, \(x) x$regex), collapse = "\n"), "\n",
                "Using the first one.")
      }
      single_match[[1]]
    })

  encoded_columns <- lapply(seq_along(encoding_pattern_matches), \(i) {
    encoding_column_name <- names(encoding_pattern_matches)[i]
    encoding_column_index <- finterface$column_info$bash_index[[encoding_column_name]]
    delimiter <- encoding_pattern_matches[[i]]$delimiter

    list(
      encoding_column_name = encoding_column_name,
      pattern = sprintf(single_encoded_column_awk,
                        encoding_column_index,
                        i,
                        delimiter),
      encoded_column = encoding_pattern_matches[[i]]$names,
      substitutes = sprintf(
        "encoded%i[%i]",
        i,
        seq_along(encoding_pattern_matches[[i]]$names)
      ) |>
        setNames(encoding_pattern_matches[[i]]$names)
    )
  }) |>
    data.table::rbindlist()

  encoded_columns[!duplicated(encoded_column)]
}

swap_in_column_names <- function(
  column_indices,
  column_names = NULL
) {
  if (is.null(column_names)) return(column_indices)
  new_names <- flip_column_names(column_names)[names(column_indices)]
  to_replace <- !sapply(new_names, is.null)
  names(column_indices)[to_replace] <- new_names[to_replace]
  column_indices
}

flip_column_names <- function(
  column_names
) {
  lapply(
    names(column_names),
    \(cname) {
      setNames(
        rep(cname, length(column_names[[cname]])),
        column_names[[cname]]
      )
    }
  ) |>
    unlist() |>
    as.list()
}

is_gzipped <- function(
  filename
) {
  stringr::str_detect(filename, "[.]gz$")
}

#' @export
head.file_interface <- function(
  finterface,
  nlines = 1,
  ...
) {
  # TODO Improve handling of gzipped files (broken pipe)
  if (!"column_info" %in% names(finterface)) {
    return(
      data.table::fread(
        cmd = fhead_cmd(finterface, nlines + 1),
        ...
      )
    )
  }
  data.table::fread(
    cmd = fhead_cmd(finterface, nlines + 1),
    col.names = names(finterface$column_info$index),
    ...
  )
}

fhead_cmd <- function(
  finterface,
  nlines
) {
  if (!finterface$gzipped) {
    return(paste("head -n", nlines, finterface$filename))
  }
  paste("zcat", finterface$filename, "| head -n", nlines)
}

are_values_quoted <- function(
  finterface
) {
  quoted_values <- head(finterface, nlines = 1, quote = "") |>
    sapply(stringr::str_detect, "\"")
  names(quoted_values) <- names(quoted_values) |>
    stringr::str_replace_all("\"", "")
  quoted_values
}

check_quotes <- function(
  value,
  value_needs_to_be_quoted,
  base_enquote = TRUE
) {
  if (is.null(value_needs_to_be_quoted)) {
    value_needs_to_be_quoted <- FALSE
  }

  number_of_quotes <- value_needs_to_be_quoted + (!all(is.numeric(value)) & base_enquote)

  if (number_of_quotes == 0) return(value)
  if (number_of_quotes == 1) return(paste0("\"", value, "\""))
  paste0("\"\\\"", value, "\\\"\"")
}

get_prefixes <- function(
  finterface,
  file_colnames,
  prefixes = NULL,
  nrows_to_check = 500
) {
  intersect(
    names(file_colnames),
    names(prefixes)
  ) |>
    setNames(nm = _) |>
    lapply(
      \(colname) {
        check_single_column_prefix(
          finterface     = finterface,
          file_colname   = file_colnames[[colname]],
          prefix         = prefixes[[colname]],
          nrows_to_check = nrows_to_check
        )
      }
    ) |>
    {\(x) x[!sapply(x, is.null)]}()
}

check_single_column_prefix <- function(
  finterface,
  file_colname,
  prefix,
  nrows_to_check = 500
) {
  awk_script <- sprintf(
    "NR > 1 && %s !~ (\"^[\\\"]?%s\") { exit 1 }",
    file_colname,
    prefix
  )
  if (is.null(nrows_to_check)) {
    cmd <- wrap_first_awk(
      awk_script,
      finterface = finterface
    )
  } else {
    cmd <- fhead_cmd(finterface, nrows_to_check) |>
      paste("|", wrap_next_awk(awk_script, finterface = finterface))
  }
  if (system(cmd) == 1) return()
  prefix
}

is_value_numeric <- function(
    value
) {
  !is.na(as.numeric(value)) |>
    suppressWarnings()
}

get_file_separator <- function(
  finterface
) {
  dt_output <- head(finterface, nlines = 1, verbose = TRUE) |>
    capture.output() |>
    stringr::str_match("sep='([^']+)'")
  dt_output[!is.na(dt_output[, 1]), 2][1]
}

get_column_names <- function(
  finterface
) {
  column_names <- head(finterface) |>
    names() |>
    get_indices_from_column_names()
}

validate_file_interface <- function(
  finterface
) {
  if (!all(c("gzipped", "values_are_quoted") %chin% attributes(finterface))) {
    stop("finterface is missing attributes ",
         c("gzipped", "values_are_quoted")[
           !c("gzipped", "values_are_quoted") %chin% attributes(finterface)
         ] |>
           paste(collapse(", ")))
  }
  stopifnot(is.logical(attr(finterface, "gzipped")))
  stopifnot(is.logical(attr(finterface, "values_are_quoted")))
  stopifnot(file.exists(finterface))
}

#' @export
`[.file_interface` <- function(
  finterface,
  conditions,
  ...,
  return_only_cmd = FALSE
) {
  command_line <- new_filter_condition(
    rlang::enexpr(conditions),
    finterface = finterface
  ) |>
    as_command_line(
      finterface = finterface
    )

  if (return_only_cmd) return(command_line)
  lapply(
    command_line,
    \(cmd) {
      data.table::fread(
        cmd = cmd,
        ...,
        col.names = names(head(finterface, 0))
      )
    }
  ) |>
    data.table::rbindlist()
}

#' @export
print.file_interface <- function(
  finterface
) {
  cat(sprintf("\"%s\"\n", finterface$filename))
  cat(sprintf("Columns: %s\n",
              paste(names(finterface$column_info$index), collapse = ", ")))
  cat(sprintf(
    "Prefixes: %s\n",
    ifelse(length(finterface$column_info$prefixes) == 0,
           "none",
           sapply(names(finterface$column_info$prefixes),
                  \(col_name) {
                    sprintf("%s - \"%s\"", col_name, finterface$column_info$prefixes[[col_name]])
                  }) |>
             paste(collapse = ", "))
  ))
  cat(sprintf(
    "Gzipped: %s, Quoted: %s",
    finterface$gzipped,
    ifelse(any(unlist(finterface$column_info$quoted_values)),
           paste(names(finterface$column_info$quoted_values)[
             unlist(finterface$column_info$quoted_values)
           ],
                 collapse = ", "),
           FALSE)
  ),
      "\n")
}
