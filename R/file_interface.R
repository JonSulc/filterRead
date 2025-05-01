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
  standard_names_dt = summary_stats_standard_names_dt,
  nrows_to_check = 500
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
    ,
    prefix := character(0)
  ][
    !is.na(possible_prefixes),
    prefix := check_single_column_prefix(finterface,
                                         bash_index = bash_index,
                                         possible_prefixes = possible_prefixes),
    by = bash_index
  ][
    !sapply(encoded_names, is.null),
    encoded_column_index := seq_len(.N)
  ][
    ,
    encoding_column := NA_character_
  ][]

  if (any(!sapply(column_info$regex, is.na))) {
    column_info[
      !sapply(regex, is.na),
      split_encoding_column := sprintf(
        "split(%s, encoded%i, \"%s\")",
        bash_index,
        encoded_column_index,
        delimiter
      )
    ][
      !sapply(regex, is.na),
      recode_columns := sprintf(
        "%s = %s",
        bash_index,
        sprintf("encoded%i[%i]", encoded_column_index, seq_along(encoded_names[[1]])) |>
          paste(collapse = " OFS ")
      ),
      by = bash_index
    ][]
  }

  column_info <- column_info[
    ,
    {
      if (is.null(encoded_names[[1]])) {
        .SD
      } else {
        list(
          .SD,
          .(standard_name         = unlist(encoded_names),
            regex                 = regex,
            delimiter             = delimiter,
            input_index           = input_index,
            bash_index            = sprintf("encoded%i[%i]",
                                            encoded_column_index,
                                            seq_along(encoded_names[[1]])),
            quoted                = FALSE,
            encoding_column       = input_name,
            split_encoding_column = split_encoding_column,
            recode_columns        = recode_columns) |>
            data.table::as.data.table()
        ) |>
          data.table::rbindlist(fill = TRUE, use.names = TRUE)
      }
    },
    by = seq_len(nrow(column_info))
  ][
    ,
    -"seq_len"
  ]
  column_info[
    ,
    name := data.table::fcoalesce(standard_name, input_name)
  ][]
}

is_gzipped <- function(
  filename
) {
  stringr::str_detect(filename, "[.]gz$")
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

check_single_column_prefix <- function(
  finterface,
  bash_index,
  possible_prefixes,
  nrows_to_check = 500L
) {
  if (length(bash_index) == 0L || length(possible_prefixes) == 0L) return()
  awk_script <- sprintf(
    "'NR > 1 && %s !~ (\"^[\\\"]?%s\") { exit 1 }'",
    bash_index,
    possible_prefixes
  )
  if (is.null(nrows_to_check)) {
    cmd <- wrap_first_awk(
      awk_script,
      finterface = finterface
    )
  } else {
    cmd <- awk_load_file_cmd(finterface, nlines = nrows_to_check + 1L) |>
      paste(awk_script)
  }
  if (system(cmd) == 1L) return()
  if (length(possible_prefixes) == 1) return(possible_prefixes)
  sapply(possible_prefixes, \(prefix) {
    check_single_column_prefix(finterface, bash_index, prefix, nrows_to_check)
  })
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
  dt_output <- head(finterface, nlines = 1L, verbose = TRUE) |>
    capture.output() |>
    stringr::str_match("sep='([^']+)'")
  dt_output[!is.na(dt_output[, 1L]), 2L][1L]
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
head.file_interface <- function(
    finterface,
    nlines = 1,
    ...
) {
  # TODO Improve handling of gzipped files (broken pipe)
  if (!"column_info" %in% names(finterface)) {
    return(
      data.table::fread(
        cmd = compile_awk_cmds(finterface,
                               nlines = nlines + 1),
        ...
      )
    )
  }
  data.table::fread(
    cmd = compile_awk_cmds(finterface, nlines = nlines + 1),
    col.names = column_names(finterface),
    ...
  )
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
    fcondition_to_awk(
      finterface = finterface
    )

  if (return_only_cmd) return(command_line)
  lapply(
    command_line,
    \(cmd) {
      data.table::fread(
        cmd = cmd,
        ...,
        col.names = column_names(finterface)
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
              paste(finterface$column_info$name, collapse = ", ")))
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
  if (original)
    return(finterface$column_info$input_name)

  finterface$column_info[
    sapply(encoded_names, is.null),
    data.table::fcoalesce(standard_name, input_name)
  ]
}
