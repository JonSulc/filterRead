#' @import data.table

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
  variable_handle
) {
  writeLines(as.character(values), filename)
  "if (FILENAME == \"%s\") {
    %s[$0] = 1
    next
  }" |>
    sprintf(
      filename,
      variable_handle
    )
}

eval_fcondition <- function(
  fcondition,
  finterface
) {
  with(
    finterface$column_info[
      !sapply(name, is.na),
      setNames(bash_index, name)
    ] |> as.list(),
    eval(fcondition)
  )
}

fcondition_to_awk <- function(
  fcondition,
  finterface
) {
  awk_condition_list <- eval_fcondition(fcondition, finterface)
  compile_awk_cmds(
    finterface       = finterface,
    awk_condition    = awk_condition_list$condition,
    variable_arrays  = awk_condition_list$variable_arrays,
    additional_files = awk_condition_list$additional_files
  )
}

compile_awk_cmds <- function(
  finterface,
  awk_condition    = NULL,
  variable_arrays  = NULL,
  additional_files = list(),
  column_arrays    = get_awk_column_arrays(finterface),
  nlines           = NULL
) {
  only_read <- is.null(awk_condition) &
    is.null(variable_arrays) &
    is.null(column_arrays)
  load_file <- awk_load_file_cmd(finterface,
                                 nlines = nlines,
                                 only_read = only_read)
  if (only_read)
    return(load_file)

  if (is.null(awk_condition)) {
    condition_block <- "print $0"
  } else {
    # TODO Move column split blocks to output if not used in condition
    condition_block <- sprintf(
      "if (%s) {
    print $0
  }",
      awk_condition
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
  awk_final_filename <- c(
    additional_files,
    ifelse(finterface$gzipped | !is.null(nlines),
           "",
           finterface$filename)
  ) |>
    paste(collapse = " ")

  paste(load_file, awk_begin_code, awk_final_filename)
}


awk_load_file_cmd <- function(
  finterface,
  nlines  = NULL,
  only_read = FALSE
) {
  if (finterface$gzipped) {
    return(
      c(
        "zcat",
        finterface$filename,
        ifelse(is.null(nlines),
               "",
               sprintf("| head -n %i", nlines)),
        ifelse(only_read,
               "",
               "| awk")
      ) |>
        paste(collapse = " ")
    )
  }
  if (is.null(nlines)) {
    if (only_read) {
      return(paste("cat", finterface$filename))
    }
    return("awk")
  }
  c(
    "head -n",
    nlines,
    finterface$filename,
    ifelse(only_read,
           "",
           "| awk")
  ) |>
    paste(collapse = " ")
}


get_awk_column_arrays <- function(
  finterface
) {
  if (!"column_info" %in% names(finterface)) return()
  if (all(sapply(finterface$column_info$encoded_names, is.null))) return()
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
