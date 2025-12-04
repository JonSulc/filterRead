setup_column_array <- function(
  bash_index,
  array_name,
  delimiter = " ",
  array_length = 1
) {
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
  delimiter
) {
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
  array_length
) {
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
  variable_handle
) {
  writeLines(as.character(values), filename)
  sprintf("if (FILENAME == \"%s\")", filename) |>
    paste(as_block(sprintf("%s[$0] = 1\nnext", variable_handle)))
}

eval_fcondition <- function(
  fcondition,
  finterface
) {
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
  fcondition,
  return_only_cmd = FALSE
) {
  column_arrays <- get_awk_column_arrays(
    get_file_interface(fcondition),
    fcondition = fcondition
  )

  fcondition_and_rsid_to_awk(fcondition) |>
    compile_awk_cmds(
      finterface = get_file_interface(fcondition),
      column_arrays_before_conditions = column_arrays$before_if,
      column_arrays_after_conditions = column_arrays$after_if,
      return_only_cmd = return_only_cmd
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
  chr_names = chromosome_names
) {
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
  nlines = NULL,
  return_only_cmd = FALSE
) {
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

  load_file <- awk_load_file_cmd(
    finterface,
    nlines = nlines,
    only_read = only_read
  )
  if (only_read) {
    return(load_file)
  }

  main_file_code <- wrap_main_file_code(
    finterface                      = finterface,
    fcondition_awk_dt               = fcondition_awk_dt,
    column_arrays_before_conditions = column_arrays_before_conditions,
    column_arrays_after_conditions  = column_arrays_after_conditions,
    nlines                          = nlines
  )

  full_code_block <- wrap_full_code_block(
    fcondition_awk_dt,
    main_file_code = main_file_code
  )

  # Determine if we need command-line FS
  # (multiple files with different separators)
  use_command_line_fs <- !is.null(fcondition_awk_dt$process_substitution)

  begin_code_block <- build_awk_begin_block(
    finterface$sep,
    nlines,
    use_command_line_fs
  )

  # Add comment prefix handling pattern
  comment_prefix_pattern <- build_comment_filter_pattern(
    finterface$comment_prefix
  )

  awk_script <- paste(
    c(begin_code_block, comment_prefix_pattern, full_code_block),
    collapse = "\n"
  )

  # In the case of long awk scripts, passing it to command line can be
  # problematic, so we write it to a temporary file by default
  # TODO: improve cleanup of temporary files
  if (return_only_cmd) {
    awk_code <- sprintf("'%s'", awk_script)
  } else {
    temp_awk_file <- tempfile(fileext = ".awk")
    writeLines(
      paste0(awk_script, "\n"),
      temp_awk_file
    )

    awk_code <- sprintf(
      "-f %s",
      temp_awk_file
    )
  }


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
    {
      if (use_command_line_fs) {
        paste(
          sprintf("FS=\"%s\"", finterface$sep),
          ifelse(finterface$gzipped | !is.null(nlines),
            "",
            finterface$filename
          )
        )
      } else {
        # Only omit filename if using pipeline (gzipped or has prefixes)
        has_prefixes <- !is.null(finterface$comment_prefix) ||
          !is.null(finterface$trim_prefix)
        ifelse(finterface$gzipped || has_prefixes,
          "",
          finterface$filename
        )
      }
    }
  ) |>
    paste(collapse = " ")

  paste(load_file, awk_code, awk_final_filename)
}

as_block <- function(
  code
) {
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
  nlines = NULL,
  ...
) {
  print_line <- sprintf(
    "print %s$0",
    ifelse(is.null(print_prefix),
      "",
      print_prefix
    )
  )

  if (!is.null(nlines)) {
    print_line <- sprintf("if (++output_lines <= max_lines) %s; else exit", print_line)
  }

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
  code
) {
  gsub("\n", "\n  ", code)
}

wrap_main_file_code <- function(
  finterface,
  fcondition_awk_dt,
  column_arrays_before_conditions,
  column_arrays_after_conditions,
  nlines = NULL
) {
  if (is.null(fcondition_awk_dt)) {
    condition_block <- wrap_condition_block(
      column_arrays_after_conditions = column_arrays_after_conditions,
      nlines = nlines
    )
  } else if (nrow(fcondition_awk_dt) == 0) {
    if (is.null(nlines)) {
      condition_block <- "print $0"
    } else {
      condition_block <- "if (++output_lines <= max_lines) print $0; else exit"
    }
  } else {
    condition_block <- fcondition_awk_dt[
      ,
      do.call(
        wrap_condition_block,
        c(
          .SD,
          .(
            column_arrays_after_conditions = column_arrays_after_conditions,
            nlines = nlines
          )
        )
      ),
      by = seq_len(nrow(fcondition_awk_dt))
    ]$V1 |>
      paste(collapse = "\nelse ")
  }

  # Add trim prefix processing to the code
  trim_prefix_code <- build_trim_prefix_code(finterface$trim_prefix)

  full_code <- c(
    trim_prefix_code, column_arrays_before_conditions,
    condition_block
  ) |>
    paste(collapse = "\n  ")

  if (!"additional_files" %in% names(fcondition_awk_dt) &
    !"process_substitution" %in% names(fcondition_awk_dt)) {
    return(full_code)
  }

  as_block(full_code)
}

wrap_full_code_block <- function(
  fcondition_awk_dt,
  main_file_code
) {
  c(
    fcondition_awk_dt$awk_code_block,
    unlist(fcondition_awk_dt$variable_arrays),
    main_file_code
  ) |>
    paste(collapse = "\nelse ") |>
    as_block()
}


# Level 1: Primitive Builders (No Dependencies)

# Helper function to escape special regex characters for AWK patterns
escape_awk_regex <- function(pattern) {
  if (is.null(pattern)) {
    return(NULL)
  }
  escaped <- pattern
  escaped <- gsub("/", "\\\\/", escaped) # / -> \/
  escaped <- gsub("\\[", "\\\\[", escaped) # [ -> \[
  escaped <- gsub("\\]", "\\\\]", escaped) # ] -> \]
  escaped <- gsub("\\(", "\\\\(", escaped) # ( -> \(
  escaped <- gsub("\\)", "\\\\)", escaped) # ) -> \)
  escaped <- gsub("\\{", "\\\\{", escaped) # { -> \{
  escaped <- gsub("\\}", "\\\\}", escaped) # } -> \}
  escaped <- gsub("\\+", "\\\\+", escaped) # + -> \+
  escaped <- gsub("\\?", "\\\\?", escaped) # ? -> \?
  escaped <- gsub("\\|", "\\\\|", escaped) # | -> \|
  escaped
}

build_file_read_cmd <- function(finterface) {
  if (finterface$gzipped) {
    sprintf("zcat %s", finterface$filename)
  } else {
    sprintf("cat %s", finterface$filename)
  }
}

build_comment_filter_pattern <- function(comment_prefix) {
  if (is.null(comment_prefix)) {
    return(NULL)
  }
  escaped_prefix <- escape_awk_regex(comment_prefix)
  sprintf("/%s/ { next }", escaped_prefix)
}

build_trim_prefix_code <- function(trim_prefix) {
  if (is.null(trim_prefix)) {
    return(NULL)
  }
  escaped_prefix <- escape_awk_regex(trim_prefix)
  sprintf('gsub(/%s/, "", $0)', escaped_prefix)
}

build_line_limit_code <- function(nlines) {
  if (is.null(nlines)) {
    return(NULL)
  }
  sprintf("if (++output_lines <= max_lines) print $0; else exit")
}

build_awk_begin_block <- function(sep, nlines = NULL, use_command_line_fs = FALSE) {
  # If no sep provided and no nlines needed, no BEGIN block required
  if (is.null(sep) && is.null(nlines)) {
    return(NULL)
  }

  begin_parts <- c()

  # Add FS/OFS if sep is provided and not using command line FS
  if (!is.null(sep) && !use_command_line_fs) {
    begin_parts <- c(
      begin_parts,
      sprintf("FS = \"%s\"", sep),
      sprintf("OFS = \"%s\"", sep)
    )
  } else if (!is.null(sep)) {
    # Only set OFS in BEGIN, FS will be set on command line
    begin_parts <- c(begin_parts, sprintf("OFS = \"%s\"", sep))
  }

  # Add line counting variables if nlines is provided
  if (!is.null(nlines)) {
    begin_parts <- c(
      begin_parts,
      "output_lines = 0",
      sprintf("max_lines = %i", nlines)
    )
  }

  sprintf("BEGIN{\n  %s\n}", paste(begin_parts, collapse = "\n  "))
}

# Level 2: Component Assemblers
build_read_only_awk_script <- function(finterface, nlines = NULL) {
  parts <- c(
    build_awk_begin_block(finterface$sep, nlines),
    build_comment_filter_pattern(finterface$comment_prefix)
  )

  # Build main block content
  main_block_parts <- c(
    build_trim_prefix_code(finterface$trim_prefix),
    build_line_limit_code(nlines) %||% "print $0"
  )

  if (length(main_block_parts) > 0) {
    main_block <- sprintf("{\n  %s\n}", paste(main_block_parts, collapse = "\n  "))
    parts <- c(parts, main_block)
  }

  # Combine all parts into final awk script
  sprintf("awk '%s'", paste(parts, collapse = "\n"))
}

build_pipeline_cmd <- function(file_cmd, awk_script) {
  paste(file_cmd, "|", awk_script)
}

# Level 3: High-Level Interfaces
awk_load_file_cmd <- function(
  finterface,
  nlines = NULL,
  only_read = FALSE
) {
  has_prefixes <- !is.null(finterface$comment_prefix) ||
    !is.null(finterface$trim_prefix)
  needs_processing <- has_prefixes || !is.null(nlines)

  if (only_read) {
    # For only_read cases with processing needed, build complete pipeline
    if (needs_processing) {
      file_cmd <- build_file_read_cmd(finterface)
      awk_script <- build_read_only_awk_script(finterface, nlines)
      # return()
      return(build_pipeline_cmd(file_cmd, awk_script))
    }
    return(build_file_read_cmd(finterface))
  }
  if (needs_processing && !finterface$gzipped) {
    return("awk")
  }
  build_file_read_cmd(finterface) |>
    paste("| awk")
}

get_awk_column_arrays <- function(
  finterface,
  fcondition = NULL
) {
  if (!"column_info" %in% names(finterface)) {
    return()
  }
  if (all(sapply(finterface$column_info$encoded_names, is.null))) {
    if (all(sapply(finterface$column_info$add_prefix, is.na))) {
      return()
    }
    return(
      list(
        after_if = finterface$column_info[
          !is.na(add_prefix),
          sprintf('%s = "%s"%s', bash_index, add_prefix, bash_index)
        ]
      )
    )
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
      } else {
        character(0)
      }
    },
    by = bash_index,
    on = "name"
  ]$V1 |>
    c(
      finterface$column_info[
        !sapply(encoded_names, is.null),
        {
          if (.N != 0) {
            recode_columns
          }
        },
        by = bash_index
      ]$V1,
      finterface$column_info[
        !is.na(add_prefix),
        sprintf('%s = "%s"%s', bash_index, add_prefix, bash_index)
      ]
    )

  list(
    before_if = before_if,
    after_if = after_if
  )
}
