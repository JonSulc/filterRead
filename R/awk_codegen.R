# =============================================================================
# Awk Command Assembly
# =============================================================================
# These functions assemble the final awk command from components: BEGIN block,
# condition blocks, file handling, etc.

#' Check if this is a simple read with no filtering/processing
#'
#' @param fcondition_awk_dt data.table from fcondition_and_rsid_to_awk
#' @param column_arrays_before_conditions Awk code for pre-condition splits
#' @param column_arrays_after_conditions Awk code for post-condition processing
#' @return TRUE if no filtering needed, FALSE otherwise
#' @keywords internal
is_simple_read <- function(
  fcondition_awk_dt,
  column_arrays_before_conditions,
  column_arrays_after_conditions
) {
  if (is.null(fcondition_awk_dt) &&
    is.null(column_arrays_before_conditions) &&
    is.null(column_arrays_after_conditions)) {
    return(TRUE)
  }
  is.null(fcondition_awk_dt$condition) &&
    is.null(fcondition_awk_dt$variable_arrays) &&
    is.null(column_arrays_before_conditions) &&
    is.null(column_arrays_after_conditions) &&
    is.null(fcondition_awk_dt$awk_code_block)
}

#' Build awk file arguments string
#'
#' Assembles file arguments: process substitutions (tabix), additional files
#' (%in% arrays), and main data file with appropriate separators.
#'
#' @param finterface File interface
#' @param fcondition_awk_dt data.table with process_substitution, additional_files
#' @param use_command_line_fs Whether to set FS on command line
#' @return File arguments string for awk command
#' @keywords internal
build_awk_file_args <- function(
  finterface,
  fcondition_awk_dt,
  use_command_line_fs
) {
  c(
    # Process substitutions (tabix queries) with tab separator
    if (!is.null(fcondition_awk_dt$process_substitution)) {
      paste(
        "FS=\"\\t\"",
        paste(fcondition_awk_dt$process_substitution, collapse = " ")
      )
    },
    # Additional temp files (%in% value arrays)
    fcondition_awk_dt$additional_files,
    # Reset FS to file's separator before reading main file
    if (use_command_line_fs) {
      sprintf("FS=\"%s\"", finterface$sep)
    },
    wrap_filename(finterface)
  ) |>
    paste(collapse = " ")
}

#' Compile complete awk command from components
#'
#' Assembles all awk components (BEGIN block, conditions, column processing)
#' into a runnable command. Handles both simple reads and complex filtered
#' queries with RSID matching.
#'
#' @param finterface File interface with filename, separator, prefixes
#' @param fcondition_awk_dt data.table from fcondition_and_rsid_to_awk with
#'   condition, variable_arrays, awk_code_block, process_substitution, etc.
#' @param column_arrays_before_conditions Awk code for column splits needed
#'   before evaluating conditions
#' @param column_arrays_after_conditions Awk code for column processing after
#'   condition passes (for output formatting)
#' @param nlines Optional limit on number of output lines
#' @param return_only_cmd If TRUE, return awk inline; if FALSE, use temp file
#'
#' @return Character string with complete awk command
#' @keywords internal
compile_awk_cmds <- function(
  finterface,
  fcondition_awk_dt = NULL,
  column_arrays_before_conditions = NULL,
  column_arrays_after_conditions = get_awk_column_arrays(finterface)$after_if,
  nlines = NULL,
  return_only_cmd = FALSE
) {
  # Simple read: just cat/zcat the file (with optional prefix handling)
  if (is_simple_read(
    fcondition_awk_dt,
    column_arrays_before_conditions,
    column_arrays_after_conditions
  )) {
    return(awk_load_file_cmd(finterface, nlines = nlines, only_read = TRUE))
  }

  # Build main processing code (conditions, column transforms, output)
  main_file_code <- wrap_main_file_code(
    finterface                      = finterface,
    fcondition_awk_dt               = fcondition_awk_dt,
    column_arrays_before_conditions = column_arrays_before_conditions,
    column_arrays_after_conditions  = column_arrays_after_conditions,
    nlines                          = nlines
  )

  # Wrap with RSID loading blocks if needed
  full_code_block <- wrap_full_code_block(
    fcondition_awk_dt,
    main_file_code = main_file_code
  )

  # Determine if we need command-line FS
  # (when reading multiple files with different separators, e.g., tabix + tsv)
  use_command_line_fs <- !is.null(fcondition_awk_dt$process_substitution)

  # Build BEGIN block and assemble complete awk script
  awk_script <- paste(
    c(
      build_awk_begin_block(finterface$sep, nlines, use_command_line_fs),
      build_comment_filter_pattern(finterface$comment_prefix),
      full_code_block
    ),
    collapse = "\n"
  )

  # Write to temp file for long scripts, or use inline
  if (return_only_cmd) {
    awk_code <- sprintf("'%s'", awk_script)
  } else {
    temp_awk_file <- tempfile(fileext = ".awk")
    writeLines(paste0(awk_script, "\n"), temp_awk_file)
    awk_code <- sprintf("-f %s", temp_awk_file)
  }

  # Build file arguments and assemble final command
  awk_file_args <- build_awk_file_args(
    finterface,
    fcondition_awk_dt,
    use_command_line_fs
  )
  paste("awk", awk_code, awk_file_args)
}

# =============================================================================
# Awk Code Block Helpers
# =============================================================================
# These functions format and structure awk code blocks.

#' Wrap code in awk block syntax { ... }
#'
#' @param code Awk code string to wrap
#' @return Code wrapped in braces with proper indentation
#' @keywords internal
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

#' Build awk conditional output block
#'
#' Generates awk code that: (1) checks conditions, (2) processes columns,
#' (3) prints output. Handles optional RSID conditions, line limits, and
#' output prefixes.
#'
#' @param condition Awk condition string (e.g., "$5 < 5e-8")
#' @param column_arrays_after_conditions Awk code for post-condition
#'   column processing
#' @param print_prefix Optional prefix to add to output (for RSID chr:pos)
#' @param rsid_condition Optional RSID membership condition
#' @param nlines Optional output line limit
#' @param ... Additional ignored parameters
#'
#' @return Awk code block with nested conditionals
#' @keywords internal
wrap_condition_block <- function(
  condition = NULL,
  column_arrays_after_conditions = NULL,
  print_prefix = NULL,
  rsid_condition = NULL,
  nlines = NULL,
  ...
) {
  # Build print statement with optional prefix
  print_line <- sprintf(
    "print %s$0",
    ifelse(is.null(print_prefix),
      "",
      print_prefix
    )
  )

  # Add line limit check if needed
  if (length(nlines) != 0) {
    print_line <- sprintf(
      "if (++output_lines <= max_lines) %s; else exit",
      print_line
    )
  }

  # RSID condition wrapper (outer layer)
  if (is.null(rsid_condition)) {
    condition_block <- "%s"
  } else {
    condition_block <- "if (%s) {
  %%s
}" |>
      sprintf(rsid_condition)
  }

  # Regular condition wrapper (inner layer)
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

  # Combine blocks with proper indentation
  sprintf(
    condition_block,
    ifelse(is.null(rsid_condition),
      inner_block,
      increase_indent(inner_block)
    )
  )
}

#' Add indentation to multiline code
#' 
#' This simply increases the indent of all new lines in the code (not the first
#' line). This helps readability when checking the awk code.
#'
#' @param code Awk code string (may contain newlines)
#' @return Code with each line indented by 2 spaces
#' @keywords internal
increase_indent <- function(
  code
) {
  gsub("\n", "\n  ", code)
}

#' Build awk code for main file processing
#'
#' Assembles awk code for processing the main data file: prefix trimming,
#' column array setup, and condition blocks. For multi-file processing
#' (RSID matching), wraps code in a block with else chains.
#'
#' @param finterface File interface with trim_prefix, etc.
#' @param fcondition_awk_dt data.table with condition info per block
#' @param column_arrays_before_conditions Column splits before conditions
#' @param column_arrays_after_conditions Column processing after conditions
#' @param nlines Optional output line limit
#'
#' @return Awk code string for main file processing
#' @keywords internal
wrap_main_file_code <- function(
  finterface,
  fcondition_awk_dt,
  column_arrays_before_conditions,
  column_arrays_after_conditions,
  nlines = NULL
) {
  # Build condition block(s) based on fcondition_awk_dt rows
  if (is.null(fcondition_awk_dt) || nrow(fcondition_awk_dt) == 0) {
    condition_block <- wrap_condition_block(
      column_arrays_after_conditions = column_arrays_after_conditions,
      nlines = nlines
    )
  } else {
    # Multiple condition blocks (e.g., from OR of genomic regions)
    # Each row becomes an else-if branch
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

  # Add trim prefix processing (remove line prefixes like "chr")
  trim_prefix_code <- build_trim_prefix_code(finterface$trim_prefix)

  # Combine: prefix trim -> column splits -> condition blocks
  full_code <- c(
    trim_prefix_code, column_arrays_before_conditions,
    condition_block
  ) |>
    paste(collapse = "\n  ")

  # Wrap in block if multi-file processing (for else chain with RSID blocks)
  if (!"additional_files" %in% names(fcondition_awk_dt) &
    !"process_substitution" %in% names(fcondition_awk_dt)) {
    return(full_code)
  }

  as_block(full_code)
}

#' Combine all awk code blocks into final structure
#'
#' Assembles RSID loading blocks, variable array blocks, and main file
#' processing into a single awk code block with else chain.
#'
#' @param fcondition_awk_dt data.table with awk_code_block, variable_arrays
#' @param main_file_code Awk code for main file processing
#'
#' @return Complete awk code block wrapped in { }
#' @keywords internal
wrap_full_code_block <- function(
  fcondition_awk_dt,
  main_file_code
) {
  # Order: RSID blocks -> variable arrays -> main file code
  # Connected with "else" for multi-file processing
  c(
    fcondition_awk_dt$awk_code_block,
    unlist(fcondition_awk_dt$variable_arrays),
    main_file_code
  ) |>
    paste(collapse = "\nelse ") |>
    as_block()
}


# =============================================================================
# Primitive Builders (No Dependencies)
# =============================================================================
# These functions generate individual awk code fragments for specific tasks.
# They have no dependencies on other awk.R functions.

#' Escape special regex characters for awk patterns
#'
#' Escapes characters that have special meaning in awk regex patterns:
#' /, [, ], (, ), {, }, +, ?, |
#'
#' @param pattern Character string to escape
#' @return Escaped pattern safe for use in awk regex, or NULL if input is NULL
#' @keywords internal
escape_awk_regex <- function(pattern) {
  if (is.null(pattern)) {
    return(NULL)
  }
  # Characters that need escaping for awk regex: / [ ] ( ) { } + ? |
  chars_to_escape <- c("/", "[", "]", "(", ")", "{", "}", "+", "?", "|")
  for (char in chars_to_escape) {
    pattern <- gsub(char, paste0("\\", char), pattern, fixed = TRUE)
  }
  pattern
}

#' Build shell command to read file (cat or zcat)
#'
#' @param finterface File interface with filename and gzipped flag
#' @return Shell command string: "cat file" or "zcat file.gz"
#' @keywords internal
build_file_read_cmd <- function(finterface) {
  if (finterface$gzipped) {
    sprintf("zcat %s", finterface$filename)
  } else {
    sprintf("cat %s", finterface$filename)
  }
}

#' Build awk pattern to skip comment lines
#'
#' @param comment_prefix Prefix that marks comment lines (e.g., "#")
#' @return Awk pattern like `/^#/ { next }`, or NULL if no prefix
#' @keywords internal
build_comment_filter_pattern <- function(comment_prefix) {
  if (is.null(comment_prefix)) {
    return(NULL)
  }
  escaped_prefix <- escape_awk_regex(comment_prefix)
  sprintf("/%s/ { next }", escaped_prefix)
}

#' Build awk code to remove line prefix
#'
#' @param trim_prefix Prefix to remove from each line
#' @return Awk gsub() call to remove prefix, or NULL if no prefix
#' @keywords internal
build_trim_prefix_code <- function(trim_prefix) {
  if (is.null(trim_prefix)) {
    return(NULL)
  }
  escaped_prefix <- escape_awk_regex(trim_prefix)
  sprintf('gsub(/%s/, "", $0)', escaped_prefix)
}

#' Build awk code for line limit enforcement
#'
#' @param nlines Maximum number of lines to output
#' @return Awk if statement for line limiting, or NULL if no limit
#' @keywords internal
build_line_limit_code <- function(nlines) {
  if (length(nlines) == 0) {
    return(NULL)
  }
  sprintf("if (++output_lines <= max_lines) print $0; else exit")
}

#' Build awk BEGIN block
#'
#' Creates BEGIN block with field separator setup and optional line counting
#' variables for output limiting.
#'
#' @param sep Field separator character
#' @param nlines Optional output line limit
#' @param use_command_line_fs If TRUE, FS is set on command line (for
#'   multi-file processing with different separators)
#'
#' @return Awk BEGIN block string, or NULL if not needed
#' @keywords internal
build_awk_begin_block <- function(
  sep,
  nlines = NULL,
  use_command_line_fs = FALSE
) {
  # If no sep provided and no nlines needed, no BEGIN block required
  if (is.null(sep) && length(nlines) == 0) {
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
  if (length(nlines) != 0) {
    begin_parts <- c(
      begin_parts,
      "output_lines = 0",
      sprintf("max_lines = %i", nlines)
    )
  }

  sprintf("BEGIN{\n  %s\n}", paste(begin_parts, collapse = "\n  "))
}

# =============================================================================
# Component Assemblers
# =============================================================================
# These functions combine primitive builders into larger components.

#' Build read-only awk script (no filtering)
#'
#' Creates a complete awk command for reading a file without filtering,
#' but with optional line limiting and prefix handling.
#'
#' @param finterface File interface with sep, comment_prefix, trim_prefix
#' @param nlines Optional output line limit
#'
#' @return Complete awk command string
#' @keywords internal
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
    main_block <- sprintf(
      "{\n  %s\n}",
      paste(main_block_parts, collapse = "\n  ")
    )
    parts <- c(parts, main_block)
  }

  # Combine all parts into final awk script
  sprintf(
    "awk '%s' %s",
    paste(parts, collapse = "\n"),
    wrap_filename(finterface)
  )
}

#' Wrap filename for awk input
#'
#' For gzipped files, wraps in process substitution: <(zcat file.gz)
#' For regular files, returns filename as-is.
#'
#' @param finterface File interface with filename and gzipped flag
#' @return Filename or process substitution string
#' @keywords internal
wrap_filename <- function(finterface) {
  if (!finterface$gzipped) {
    return(finterface$filename)
  }
  sprintf("<(zcat %s)", finterface$filename)
}

# =============================================================================
# High-Level Interfaces
# =============================================================================

#' Get file loading command (entry point for simple reads)
#'
#' Determines the appropriate command for loading a file. For simple reads
#' without processing, returns cat/zcat. For reads requiring prefix handling
#' or line limits, returns a complete awk command.
#'
#' @param finterface File interface object
#' @param nlines Optional output line limit
#' @param only_read If TRUE, this is a read-only operation (no filtering)
#'
#' @return Command string: "cat file", "zcat file.gz", or complete awk command
#' @keywords internal
awk_load_file_cmd <- function(
  finterface,
  nlines = NULL,
  only_read = FALSE
) {
  has_prefixes <- !is.null(finterface$comment_prefix) ||
    !is.null(finterface$trim_prefix)
  needs_processing <- has_prefixes || length(nlines) != 0

  if (only_read) {
    # For only_read cases with processing needed, build complete pipeline
    if (needs_processing) {
      return(build_read_only_awk_script(finterface, nlines))
    }
    # Simple case: just cat/zcat the file
    return(build_file_read_cmd(finterface))
  }
  # For filtered reads, caller will build the full awk command
  "awk"
}

#' Determine column array setup for encoded/prefixed columns
#'
#' Analyzes file interface to determine which columns need splitting (for
#' encoded values like "A;B") or prefix addition. Returns awk code to be
#' executed before and after condition evaluation.
#'
#' Encoded columns are split into arrays before condition checks (if needed
#' for filtering), then recombined after. Prefixes are added after filtering.
#'
#' @param finterface File interface with column_info containing encoding info
#' @param fcondition Optional filter condition (to determine which encoded
#'   columns are needed for condition evaluation)
#'
#' @return List with:
#'   - before_if: awk split() calls for encoded columns used in conditions
#'   - after_if: awk code for column recoding and prefix addition
#'   Or NULL if no special column handling needed.
#' @keywords internal
get_awk_column_arrays <- function(
  finterface,
  fcondition = NULL
) {
  if (!"column_info" %in% names(finterface)) {
    return()
  }

  # Check if any columns need encoding or prefix handling
  if (all(sapply(finterface$column_info$encoded_names, is.null))) {
    # No encoded columns - only handle prefix addition if needed
    if (all(sapply(finterface$column_info$add_prefix, is.na))) {
      return()
    }
    # Generate prefix addition code: $1 = "chr"$1
    return(
      list(
        after_if = finterface$column_info[
          !is.na(add_prefix),
          sprintf('%s = "%s"%s', bash_index, add_prefix, bash_index)
        ]
      )
    )
  }

  # Determine which encoded columns are used in conditions
  # These need to be split BEFORE condition evaluation
  required_for_if <- finterface$column_info[
    get_used_columns(fcondition, finterface),
    encoding_column |> na.omit() |> c(),
    on = "name"
  ]

  # Split columns needed for condition evaluation BEFORE the if
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

  # Split remaining encoded columns AFTER condition passes (for output)
  # Also add column recoding and prefix addition
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
      # Recode columns (rearrange array elements)
      finterface$column_info[
        !sapply(encoded_names, is.null),
        {
          if (.N != 0) {
            recode_columns
          }
        },
        by = bash_index
      ]$V1,
      # Add prefixes (e.g., "chr" to chromosome column)
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
