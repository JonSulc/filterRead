#' @import data.table

setup_column_array <- function(
    bash_index,
    array_name,
    delimiter = " ",
    array_length = 1) {
  sprintf(
    "%s  \n%s",
    awk_split_column(
      bash_index = bash_index,
      array_name = array_name,
      delimiter = delimiter
    ),
    awk_combine_split_for_output(
      bash_index = bash_index,
      array_name = array_name,
      array_length = array_length
    )
  )
}

awk_split_column <- function(
    bash_index,
    array_name,
    delimiter) {
  sprintf(
    "split(%s, %s, \"%s\")",
    bash_index,
    array_name,
    delimiter
  )
}
awk_combine_split_for_output <- function(
    bash_index,
    array_name,
    array_length) {
  sprintf(
    "%s = %s",
    bash_index,
    sprintf("%s[%i]", array_name, seq_len(array_length)) |>
      paste(collapse = " OFS ")
  )
}

setup_variable_array <- function(
    values,
    filename,
    variable_handle) {
  writeLines(as.character(values), filename)
  sprintf("if (FILENAME == \"%s\")", filename) |>
    paste(as_block(sprintf("%s[$0] = 1\nnext", variable_handle)))
}

eval_fcondition <- function(
    fcondition,
    finterface) {
  if (length(fcondition) == 0) {
    return()
  }
  environment <- get_file_interface(fcondition)$column_info[
    !sapply(name, is.na),
    setNames(bash_index, name)
  ] |>
    as.list()
  if (needs_rsid_matching(finterface)) {
    environment <- c(
      environment,
      list(or_filter_condition = or_filter_condition_rsid)
    )
  }
  with(
    environment,
    eval(fcondition)
  )
}

fcondition_to_awk <- function(
    fcondition) {
  column_arrays <- get_awk_column_arrays(
    get_file_interface(fcondition),
    fcondition = fcondition
  )

  fcondition_and_rsid_to_awk(fcondition) |>
    compile_awk_cmds(
      finterface = get_file_interface(fcondition),
      column_arrays_before_conditions = column_arrays$before_if,
      column_arrays_after_conditions = column_arrays$after_if
    )
}

fcondition_and_rsid_to_awk <- function(
    fcondition,
    rsid_bash_index = get_file_interface(fcondition)$column_info[
      "rsid",
      bash_index,
      on = "name"
    ],
    index = 0,
    chr_names = chromosome_names) {
  if (!needs_rsid_matching(get_file_interface(fcondition)) ||
    (is.null(attr(fcondition, "genomic_range")) &&
      is_single_genomic_range_block(fcondition))) {
    return(eval_fcondition(
      fcondition,
      get_file_interface(fcondition)
    ) |>
      data.table::as.data.table())
  }
  if (!is_single_genomic_range_block(fcondition)) {
    stopifnot(fcondition[[1]] == as.symbol("or_filter_condition"))
    to_return <- fcondition_and_rsid_to_awk(fcondition[[2]],
      rsid_bash_index,
      index     = index,
      chr_names = chr_names
    )
    return(rbind(
      to_return,
      fcondition_and_rsid_to_awk(fcondition[[3]],
        rsid_bash_index,
        index     = index + 1,
        chr_names = chr_names
      )
    ))
  }
  attr(fcondition, "genomic_range")[
    ,
    .(
      index = index,
      awk_code_block = paste0(
        "if (NR == FNR%s) {\n",
        "  rsid%i[$3]=$1 OFS $2\n",
        "}"
      ) |>
        sprintf(
          ifelse(index == 0,
            "",
            paste0(" + ", index)
          ),
          index
        ),
      print_prefix = sprintf(
        "rsid%i[%s] OFS ",
        index,
        rsid_bash_index
      ),
      process_substitution = get_tabix_process_substitution(
        chr = chr,
        start = start,
        end = end,
        chr_names = chr_names
      ),
      rsid_condition = sprintf("%s in rsid%i", rsid_bash_index, index)
    )
  ] |>
    cbind(data.table::as.data.table(
      eval_fcondition(
        fcondition,
        get_file_interface(fcondition)
      )
    ))
}

compile_awk_cmds <- function(
    finterface,
    fcondition_awk_dt = NULL,
    column_arrays_before_conditions = NULL,
    column_arrays_after_conditions = get_awk_column_arrays(finterface)$after_if,
    nlines = NULL) {
  if (is.null(fcondition_awk_dt) &&
    is.null(column_arrays_before_conditions) &&
    is.null(column_arrays_after_conditions)) {
    only_read <- TRUE
  } else {
    only_read <- is.null(fcondition_awk_dt$condition) &&
      is.null(fcondition_awk_dt$variable_arrays) &&
      is.null(column_arrays_before_conditions) &&
      is.null(column_arrays_after_conditions) &&
      is.null(fcondition_awk_dt$awk_code_block)
  }

  load_file <- awk_load_file_cmd(finterface,
    nlines = nlines,
    only_read = only_read
  )
  if (only_read) {
    return(load_file)
  }

  main_file_code <- wrap_main_file_code(
    fcondition_awk_dt               = fcondition_awk_dt,
    column_arrays_before_conditions = column_arrays_before_conditions,
    column_arrays_after_conditions  = column_arrays_after_conditions
  )

  full_code_block <- wrap_full_code_block(
    fcondition_awk_dt,
    main_file_code = main_file_code
  )

  begin_code_block <-
    "BEGIN{
  OFS = \"%s\"
}" |>
    sprintf(finterface$sep)

  awk_code <- sprintf(
    "'%s'",
    paste(begin_code_block, full_code_block)
  )

  awk_final_filename <- c(
    {
      if (is.null(fcondition_awk_dt$process_substitution)) {
        NULL
      } else {
        paste(
          "FS=\"\\t\"",
          paste(fcondition_awk_dt$process_substitution, collapse = " ")
        )
      }
    },
    fcondition_awk_dt$additional_files,
    paste(
      sprintf("FS=\"%s\"", finterface$sep),
      ifelse(finterface$gzipped | !is.null(nlines),
        "",
        finterface$filename
      )
    )
  ) |>
    paste(collapse = " ")

  paste(load_file, awk_code, awk_final_filename)
}

as_block <- function(
    code) {
  paste0(
    "{\n",
    "  %s\n",
    "}"
  ) |>
    sprintf(increase_indent(code))
}

wrap_condition_block <- function(
    condition = NULL,
    column_arrays_after_conditions = NULL,
    print_prefix = NULL,
    rsid_condition = NULL,
    ...) {
  print_line <- sprintf(
    "print %s$0",
    ifelse(is.null(print_prefix),
      "",
      print_prefix
    )
  )

  if (is.null(rsid_condition)) {
    condition_block <- "%s"
  } else {
    condition_block <- "if (%s) {
  %%s
}" |>
      sprintf(rsid_condition)
  }

  if (is.null(condition)) {
    inner_block <- c(column_arrays_after_conditions, print_line) |>
      paste(collapse = "\n")
  } else {
    inner_block <- sprintf(
      "if (%s) {
  %s
}",
      condition,
      c(column_arrays_after_conditions, print_line) |>
        paste(collapse = "\n") |>
        increase_indent()
    )
  }
  sprintf(
    condition_block,
    ifelse(is.null(rsid_condition),
      inner_block,
      increase_indent(inner_block)
    )
  )
}

increase_indent <- function(
    code) {
  gsub("\n", "\n  ", code)
}

wrap_main_file_code <- function(
    fcondition_awk_dt,
    column_arrays_before_conditions,
    column_arrays_after_conditions) {
  if (is.null(fcondition_awk_dt)) {
    condition_block <- wrap_condition_block(
      column_arrays_after_conditions = column_arrays_after_conditions
    )
  } else if (nrow(fcondition_awk_dt) == 0) {
    condition_block <- "print $0"
  } else {
    condition_block <- fcondition_awk_dt[
      ,
      do.call(
        wrap_condition_block,
        c(
          .SD,
          .(column_arrays_after_conditions = column_arrays_after_conditions)
        )
      ),
      by = seq_len(nrow(fcondition_awk_dt))
    ]$V1 |>
      paste(collapse = "\nelse ")
  }

  full_code <- c(column_arrays_before_conditions, condition_block) |>
    paste(collapse = "\n  ")

  if (!"additional_files" %in% names(fcondition_awk_dt) &
    !"process_substitution" %in% names(fcondition_awk_dt)) {
    return(full_code)
  }

  as_block(full_code)
}

wrap_full_code_block <- function(
    fcondition_awk_dt,
    main_file_code) {
  c(
    fcondition_awk_dt$awk_code_block,
    unlist(fcondition_awk_dt$variable_arrays),
    main_file_code
  ) |>
    paste(collapse = "\nelse ") |>
    as_block()
}


awk_load_file_cmd <- function(
    finterface,
    nlines = NULL,
    only_read = FALSE) {
  if (finterface$gzipped) {
    if (is.null(nlines)) {
      cmd <- sprintf("zcat %s", finterface$filename)
    } else {
      cmd <- paste(
        "bash -c",
        sprintf(
          "head -n %i < <(zcat %s 2>/dev/null)",
          nlines,
          finterface$filename
        ) |>
          shQuote()
      )
    }
    if (!only_read) {
      cmd <- paste(cmd, "| awk")
    }
    return(cmd)
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
      "| awk"
    )
  ) |>
    paste(collapse = " ")
}

get_awk_column_arrays <- function(
    finterface,
    fcondition = NULL) {
  if (!"column_info" %in% names(finterface)) {
    return()
  }
  if (all(sapply(finterface$column_info$encoded_names, is.null))) {
    return()
  }

  required_for_if <- finterface$column_info[
    get_used_columns(fcondition, finterface),
    encoding_column |> na.omit() |> c(),
    on = "name"
  ]

  before_if <- finterface$column_info[!sapply(encoded_names, is.null)][
    required_for_if,
    {
      if (.N != 0) {
        awk_split_column(
          bash_index = bash_index,
          array_name = paste0("encoded", encoded_column_index),
          delimiter = delimiter
        )
      } else {
        character(0)
      }
    },
    by = bash_index,
    on = "name"
  ]$V1
  after_if <- finterface$column_info[!sapply(encoded_names, is.null)][
    !required_for_if,
    {
      if (.N != 0) {
        split_encoding_column
        # awk_split_column(
        #   bash_index = bash_index,
        #   array_name = paste0("encoded", encoded_column_index),
        #   delimiter = delimiter
        # )
      } else {
        character(0)
      }
    },
    by = bash_index,
    on = "name"
  ]$V1 |>
    c(finterface$column_info[
      !sapply(encoded_names, is.null),
      {
        if (.N != 0) {
          recode_columns
          # awk_combine_split_for_output(
          #   bash_index   = bash_index,
          #   array_name   = paste0("encoded", encoded_column_index),
          #   array_length = length(encoded_names[[1]])
          # )
        }
      },
      by = bash_index
    ]$V1)

  list(
    before_if = before_if,
    after_if = after_if
  )
}
