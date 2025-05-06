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

#' @export
is_file_interface <- function(finterface) inherits(finterface, "file_interface")

get_column_info <- function(
  finterface,
  standard_names_dt = summary_stats_standard_names_dt,
  nrows_to_check = 500
) {
  column_info <- get_base_column_info(
    finterface,
    standard_names_dt = standard_names_dt
  )

  data_to_check <- head(finterface, nrows_to_check)
  column_info <- filter_regex_matches(column_info, data_to_check)

  add_quoted_column(column_info, finterface)

  add_prefix_column(column_info, data_to_check)

  add_encoding_columns(column_info)

  column_info <- expand_encoded_columns(column_info)

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
  rsid_condition  = NULL, # data.table with chr, start, stop
  ...,
  return_only_cmd = FALSE
) {
  command_line <- new_filter_condition(
    rlang::enexpr(conditions),
    finterface = finterface
  ) |>
    fcondition_to_awk(
      rsid_condition = rsid_condition
    )

  if (return_only_cmd) return(command_line)

  data.table::fread(
    cmd = paste("bash -c", shQuote(command_line)),
    ...,
    col.names = column_names(finterface)
  )
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
  original     = FALSE,
  rsid_parsing = TRUE
) {
  if (original)
    return(finterface$column_info$input_name)

  finterface$column_info[
    sapply(encoded_names, is.null),
    data.table::fcoalesce(standard_name, input_name)
  ]
}
