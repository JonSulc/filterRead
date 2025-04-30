#' @import data.table

# TODO to be removed once replaced and test-covered
single_encoded_column_awk <- "split(%s, encoded%s, \"%s\");\n"


encoded_column_awk_wrapper <- "BEGIN{OFS=FS} {
    %s
    if (%s) {
        print $0
    }
}"

setup_column_array <- function(
  bash_index,
  array_name,
  delimiter = " ",
  array_length = 1
) {
  sprintf(
    "split(%s, %s, \"%s\")
    %s = %s",
    bash_index,
    array_name,
    delimiter,
    bash_index,
    sprintf("%s[%i]", array_name, seq_len(array_length)) |>
      paste(collapse = " OFS ")
  )
}

setup_variable_array <- function(
  values,
  filename,
  filename_handle = "testfile",
  variable_handle = "test"
) {
  writeLines(as.character(values), filename)
  "if (FILENAME == %s) {
    %s[$0] = 1
    next
  }" |>
    sprintf(
      filename_handle,
      variable_handle
    )
}

fcondition_to_awk <- function(
  fcondition,
  finterface
) {
  awk_condition_list <- with(
    finterface$column_info$bash_index,
    eval(fcondition)
  )
  compile_awk_cmds(
    finterface       = finterface,
    awk_condition    = awk_condition_list$condition,
    variable_arrays  = awk_condition_list$variable_arrays,
    additional_files = awk_condition_list$additional_files,
    column_arrays    = awk_condition_list$column_arrays,
    return_line      = awk_condition_list$return_line
  )
}

compile_awk_cmds <- function(
  finterface,
  awk_condition    = NULL,
  variable_arrays  = NULL,
  additional_files = NULL,
  column_arrays    = get_awk_column_arrays(finterface),
  return_line      = get_awk_return_line(finterface),
  nlines           = NULL
) {
  load_file <- awk_load_file_cmd(finterface,
                                 additional_files = additional_files,
                                 nlines = nlines)
  if (is.null(awk_condition)
      & is.null(variable_arrays)
      & is.null(column_arrays)
      & return_line == "print $0")
    return(load_file)

  if (is.null(awk_condition)) {
    condition_block <- return_line
  } else {
    condition_block <- sprintf(
      "if (%s) {
    %s
  }",
      awk_condition,
      return_line
    )
  }

  awk_begin_code <-
    "'BEGIN{
  FS = \"%s\"
  OFS = FS
} {
  %s
  %s
  %s
}'" |>
    sprintf(
      finterface$sep,
      paste(variable_arrays, collapse = "\n"),
      paste(column_arrays, collapse = "\n"),
      condition_block
    )
  awk_final_filename <- ifelse(finterface$gzipped | !is.null(nlines),
                               "",
                               finterface$filename)

  paste(load_file, awk_begin_code, awk_final_filename)
}


awk_load_file_cmd <- function(
  finterface,
  nlines  = NULL,
  additional_files = list()
) {
  if (finterface$gzipped) {
    return(
      sprintf(
        "zcat %s%s | awk %s",
        finterface$filename,
        ifelse(is.null(nlines),
               "",
               sprintf(" | head -n %i", nlines)),
        paste(additional_files, collapse = " ")
      )
    )
  }
  if (is.null(nlines)) {
    return(
      sprintf("awk %s",
              paste(additional_files, collapse = " "))
    )
  }
  sprintf(
    "head -n %i %s | awk %s",
    nlines,
    finterface$filename,
    paste(additional_files, collapse = " ")
  )
}

get_awk_return_line <- function(
  finterface
) {
  return("print $0")
  if (all(sapply(finterface$column_info$regex, is.na))) return("print $0")
  column_decoding <- finterface$column_info[
    !sapply(regex, is.na),
    sprintf(
      "        for (i = %s; i < %s; i++) {
          printf \"%%s%%s\", $i, OFS
        }
        printf \"%s\", %s",
      previous_non_encoded_index,
      input_index,
      rep("%s", length(encoded_names[[1]])) |>
        paste(collapse = "%s"),
      sprintf("encoded%i[%i]",
              encoded_column_index,
              seq_along(encoded_names[[1]])) |>
        paste(collapse = ", OFS, ")
    ),
    by = input_index
  ]$V1
  column_decoding |>
    paste(collapse = "\n        print OFS\n") |>
    paste("\n        print\n")
}


get_awk_column_arrays <- function(
  finterface
) {
  finterface$column_info[
    !sapply(encoded_names, is.null),
    setup_column_array(
      bash_index,
      paste0("encoded", encoded_column_index),
      delimiter,
      length(encoded_names[[1]])
    ),
    by = bash_index
  ]$V1
}
