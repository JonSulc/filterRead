#' @import data.table

#' @export
new_file_interface <- function(
  filename,
  standard_names_dt = summary_stats_standard_names_dt
) {
  stopifnot(is.character(filename))
  stopifnot(file.exists(filename))
  finterface <- structure(list(filename = filename,
                               gzipped  = is_gzipped(filename)),
                          class = c("file_interface", "character"))

  finterface$sep <- get_file_separator(finterface)
  finterface$column_info <- get_column_info(
    finterface,
    standard_names_dt = standard_names_dt
  )

  finterface
}

get_column_info <- function(
  finterface,
  standard_names_dt = summary_stats_standard_names_dt
) {
  column_info <- data.table::data.table(
    input_name = head(finterface) |> names()
  )[
    ,
    c(.SD,
      .(input_index = seq_len(.N),
        bash_index = paste0("$", seq_len(.N))))
  ]
  column_info <- standard_names_dt[
    column_info,
    on = "input_name"
  ]

  data_first_row <- head(finterface)

  column_info <- column_info[
    ,
    {
      if (is.na(pattern[[1]])) {
        .SD
      } else {
        row_match <- .SD[sapply(.SD$regex, grepl, data_first_row[[input_name]])]
        if (nrow(row_match) == 0) {
          row_match <- .SD[1]
          row_match[
            ,
            c("pattern", "regex", "encoded_names", "substitutes", "delimiter") := .(
              NA_character_, NA_character_, list(), list(), NA_character_
            )
          ]
        } else if (1 < nrow(row_match)) {
          warning("Column ", input_name, " matches multiple regex patterns:\n",
                  paste(row_match$regex, collapse = "\n"), "\n\n",
                  "Using first.")
          row_match <- row_match[1]
        } else {
          row_match
        }
      }
    },
    by = input_name
  ]

  column_info[
    ,
    quoted := are_values_quoted(finterface)[input_name]
  ][]
  to_check <- head(finterface, nrows_to_check)

  column_info[
    !is.na(possible_prefixes),
    prefix := check_single_column_prefix(finterface,
                                         bash_index = bash_index,
                                         possible_prefixes = possible_prefixes),
    by = bash_index
  ][
    !sapply(regex, is.na),
    previous_non_encoded_index := data.table::shift(input_index, n = 1L, fill = 1L)
  ][
    !sapply(encoded_names, is.null),
    encoded_column_index := seq_len(.N)
  ][]
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
    cmd = compile_awk_cmds(finterface, nlines + 1),
    col.names = column_names(finterface),
    ...
  )
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

check_single_column_prefix <- function(
  finterface,
  bash_index,
  possible_prefixes,
  nrows_to_check = 500
) {
  if (length(bash_index) == 0 || length(possible_prefixes) == 0) return()
  awk_script <- sprintf(
    "NR > 1 && %s !~ (\"^[\\\"]?%s\") { exit 1 }",
    bash_index,
    possible_prefixes
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
  possible_prefixes
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

column_names <- function(
  finterface,
  original = FALSE
) {
  if (original | !"column_info" %in% names(finterface))
    return(finterface$column_info$input_name)

  finterface$column_info[
    ,
    {
      if (length(encoded_names[[1]]) == 0) {
        standard_name
      } else {
        encoded_names
      }
    },
    by = input_index
  ]$V1
}
