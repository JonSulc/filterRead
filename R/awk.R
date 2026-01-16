# =============================================================================
# Column Array Setup Functions
# =============================================================================
# These functions generate awk code for handling encoded/delimited columns.
# When a column contains multiple values separated by a delimiter (e.g., "A;B"),
# these functions split the column into an array, process it, and reassemble
# the values for output.

#' Generate awk code to split and recombine a delimited column
#'
#' Creates awk code that: (1) splits a column by delimiter into an array,
#' and (2) recombines the array elements for output using OFS as separator.
#' Used for columns that contain multiple encoded values.
#'
#' @param bash_index The awk column reference (e.g., "$1", "$2")
#' @param array_name Name for the awk array to store split values
#' @param delimiter Character(s) used to split the column (default: " ")
#' @param array_length Number of elements expected in the split array
#'
#' @return Character string containing awk code for split and recombine
#' @keywords internal
setup_column_array <- function(
  bash_index,
  array_name,
  delimiter = " ",
  array_length = 1
) {
  # Output format: "split($1, arr, \" \")\n$1 = arr[1] OFS arr[2]"
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

#' Generate awk split() function call
#'
#' Creates awk code to split a column value into an array using a delimiter.
#'
#' @param bash_index The awk column reference (e.g., "$1")
#' @param array_name Name for the awk array to receive split values
#' @param delimiter Delimiter character for splitting
#'
#' @return Character string like `split($1, arr, ";")`
#' @keywords internal
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

#' Generate awk code to reassemble split array elements for output
#'
#' Creates assignment that recombines array elements using OFS (output field
#' separator). This restores the column to a single value after processing.
#'
#' @param bash_index The awk column reference (e.g., "$1")
#' @param array_name Name of the awk array containing split values
#' @param array_length Number of elements to combine
#'
#' @return Character string like `$1 = arr[1] OFS arr[2] OFS arr[3]`
#' @keywords internal
awk_combine_split_for_output <- function(
  bash_index,
  array_name,
  array_length
) {
  # Generates: "$1 = arr[1] OFS arr[2]" for array_length=2
  sprintf(
    "%s = %s",
    bash_index,
    sprintf("%s[%i]", array_name, seq_len(array_length)) |>
      paste(collapse = " OFS ")
  )
}

# =============================================================================
# Variable Array Functions (for %in% operator)
# =============================================================================
# These functions support the R %in% operator by writing values to a temp file
# and generating awk code that loads them into an associative array.

#' Generate awk code to load values from file into associative array
#'
#' For `%in%` operations, writes R values to a temp file and generates awk code
#' that reads the file and stores values in an associative array for O(1) lookup.
#' The array is populated when awk reads the temp file, before processing the
#' main data file.
#'
#' @param values Vector of values to match against
#' @param filename Path to temp file where values will be written
#' @param variable_handle Name for the awk associative array
#'
#' @return Character string with awk code block for loading the array
#' @keywords internal
#'
#' @details
#' The generated awk code uses FILENAME to detect when reading the temp file,
#' stores each line as an array key with value 1, then skips to next line.
#' Example output: `if (FILENAME == "/tmp/file123") { var123[$0] = 1; next }`
setup_variable_array <- function(
  values,
  filename,
  variable_handle
) {
  # Write values to temp file (one per line)
  writeLines(as.character(values), filename)
  # Generate awk code: when reading this file, store each line in array
  sprintf("if (FILENAME == \"%s\")", filename) |>
    paste(as_block(sprintf("%s[$0] = 1\nnext", variable_handle)))
}

# =============================================================================
# Column Index Mapping
# =============================================================================

#' Create named list mapping column names to awk column references
#'
#' Extracts column name to bash index mapping from file interface, creating
#' an environment-like structure for evaluating R filter expressions as awk.
#' For RSID-indexed files, also adds special `or_filter_condition` function.
#'
#' @param finterface A file_interface object with column_info
#'
#' @return Named list where names are column names (chr, pos, pval, etc.)
#'   and values are awk column refs ($1, $2, etc.). For RSID files, includes
#'   `or_filter_condition` pointing to `or_filter_condition_rsid`.
#' @keywords internal
get_column_indices <- function(
  finterface
) {
  # Build list: list(chr = "$1", pos = "$2", pval = "$5", ...)
  column_indices <- finterface$column_info[
    !sapply(name, is.na),
    setNames(bash_index, name)
  ] |>
    as.list()
  # RSID-indexed files need special OR handling for genomic region queries
  if (needs_rsid_matching(finterface)) {
    column_indices <- c(
      column_indices,
      list(or_filter_condition = or_filter_condition_rsid)
    )
  }
  column_indices
}

# =============================================================================
# Filter Condition Evaluation (R expression -> awk condition)
# =============================================================================
# These functions convert R filter_condition objects into awk condition strings.
# They handle both regular conditions (pval < 5e-8) and genomic region filters.

#' Evaluate filter condition with genomic regions to awk code
#'
#' Main entry point for converting a filter_condition (with embedded genomic
#' regions) to awk condition strings. Handles both composite conditions
#' (AND/OR trees) and atomic conditions.
#'
#' @param fcondition A filter_condition object (call-like structure)
#' @param finterface File interface for column mappings
#' @param column_indices Named list mapping column names to awk refs
#' @param env Environment for evaluating non-column variables
#'
#' @return List with components:
#'   - condition: awk condition string (e.g., "$5 < 5e-8 && $1 == 1")
#'   - variable_arrays: awk code for loading %in% arrays
#'   - additional_files: temp files that need to be passed to awk
#' @keywords internal
eval_fcondition_w_gregions <- function(
  fcondition,
  finterface = get_file_interface(fcondition),
  column_indices = get_column_indices(finterface),
  env = parent.frame()
) {
  # For non-AND blocks (i.e., OR at top level), recursively process children
  if (!is_and_block(fcondition)) {
    return(
      do.call(
        get(fcondition[[1]]),
        lapply(
          fcondition[-1],
          eval_fcondition_w_gregions,
          column_indices = column_indices,
          env = env
        )
      )
    )
  }
  # For AND blocks, evaluate non-genomic conditions first
  awk_conditions <- eval_fcondition(
    fcondition,
    column_indices = column_indices,
    env = env
  )
  # Then add genomic region conditions (chr == X && pos >= Y && pos <= Z)
  awk_conditions$condition <- c(
    awk_conditions$condition,
    eval_genomic_regions_as_fc(
      genomic_regions(fcondition) |>
        post_process(finterface = finterface),
      finterface = finterface,
      column_indices = column_indices
    )
  )
  # Combine all conditions with &&
  if (!is.null(awk_conditions$condition)) {
    awk_conditions$condition <- awk_conditions$condition |>
      paste(collapse = " && ")
  }
  awk_conditions
}

#' Evaluate non-genomic filter conditions to awk
#'
#' Converts filter_condition expressions (excluding genomic regions) to awk
#' conditions by evaluating them in an environment where column names resolve
#' to awk column references.
#'
#' @param fcondition A filter_condition object (excluding genomic parts)
#' @param finterface File interface for column mappings
#' @param column_indices Named list mapping column names to awk refs
#' @param env Environment for evaluating non-column R variables
#'
#' @return List with condition string and optional variable_arrays/files
#' @keywords internal
eval_fcondition <- function(
  fcondition,
  finterface = get_file_interface(fcondition),
  column_indices = get_column_indices(finterface),
  env = parent.frame()
) {
  if (length(fcondition) == 0) {
    return()
  }
  # Evaluate fcondition in environment where chr="$1", pos="$2", etc.
  # post_process handles quote escaping and prefix handling
  with(
    column_indices,
    post_process(fcondition, env = env) |>
      eval()
  )
}

#' Convert genomic_regions to awk filter conditions
#'
#' Translates a genomic_regions data.table into awk conditions. Each region
#' becomes a condition like `$1 == "chr1" && 1000 <= $2 && $2 <= 2000`,
#' combined with OR for multiple regions.
#'
#' @param gregions A genomic_regions object (data.table with chr/start/end)
#' @param finterface File interface (used to check RSID matching)
#' @param column_indices Named list with chr and pos column refs
#'
#' @return Character string with awk condition, or NULL if empty/RSID file
#' @keywords internal
eval_genomic_regions_as_fc <- function(
  gregions,
  finterface,
  column_indices
) {
  # Skip for NULL regions or RSID-indexed files (handled differently)
  if (is.null(gregions) || needs_rsid_matching(finterface)) {
    return()
  }
  # Convert each row to an awk condition, combine with ||
  gregions_fcs <- gregions[
    ,
    single_genomic_region_to_fc(
      .(chr = chr, start = start, end = end),
      column_indices
    ),
    by = .I # Could be improved if vectorized
  ][, -"I"] |>
    unlist()
  if (!is.null(gregions_fcs)) {
    gregions_fcs <- gregions_fcs |>
      paste(collapse = " || ")
  }
  # Wrap multiple regions in parentheses for correct precedence
  if (1 < nrow(gregions)) {
    sprintf("(%s)", gregions_fcs)
  } else {
    gregions_fcs
  }
}

#' Convert single genomic region to awk condition
#'
#' Creates awk condition for one region: chr equality and position bounds.
#' NA values in chr/start/end are skipped (allowing partial constraints).
#'
#' @param gregion_row List with chr, start, end (possibly NA)
#' @param column_indices Named list with chr and pos column refs
#'
#' @return Character string like `$1 == "1" && 1000 <= $2 && $2 <= 2000`
#' @keywords internal
single_genomic_region_to_fc <- function(
  gregion_row,
  column_indices
) {
  # Build condition components, skipping NA constraints
  all_fc <- c(
    if (!is.na(gregion_row$chr)) {
      eq_filter_condition(column_indices$chr, gregion_row$chr)
    },
    if (!is.na(gregion_row$start)) {
      lte_filter_condition(gregion_row$start, column_indices$pos)
    },
    if (!is.na(gregion_row$end)) {
      lte_filter_condition(column_indices$pos, gregion_row$end)
    }
  )
  if (is.null(all_fc)) {
    return()
  }
  paste(all_fc, collapse = " && ")
}

# =============================================================================
# Main Entry Points (filter_condition -> awk command)
# =============================================================================

#' Convert filter condition to complete awk command
#'
#' Main entry point for translating a filter_condition into a runnable awk
#' command string. Handles column array setup, condition evaluation, and
#' command assembly.
#'
#' @param fcondition A filter_condition object
#' @param return_only_cmd If TRUE, return awk code inline; if FALSE (default),
#'   write awk script to temp file and use `-f`
#'
#' @return Character string with complete awk command ready for execution
#' @keywords internal
fcondition_to_awk <- function(
  fcondition,
  return_only_cmd = FALSE
) {
  # Determine which columns need array splitting (for encoded columns)
  column_arrays <- get_awk_column_arrays(
    get_file_interface(fcondition),
    fcondition = fcondition
  )

  # Convert condition to awk and assemble full command
  compile_awk_cmds(
    finterface = get_file_interface(fcondition),
    fcondition_awk_dt = fcondition_and_rsid_to_awk(fcondition),
    column_arrays_before_conditions = column_arrays$before_if,
    column_arrays_after_conditions = column_arrays$after_if,
    return_only_cmd = return_only_cmd
  )
}

#' Convert filter condition with RSID matching to awk data.table
#'
#' Handles files that use RSID-based indexing (files without chr/pos columns
#' that require dbSNP lookup). For RSID files with genomic conditions, uses
#' tabix to query dbSNP and creates awk code to match RSIDs.
#'
#' @param fcondition A filter_condition object
#' @param rsid_bash_index Awk column ref for RSID column (e.g., "$3")
#' @param index Counter for multiple genomic blocks (for unique array names)
#'
#' @return data.table with columns:
#'   - index: block index for unique naming
#'   - awk_code_block: awk code to load RSID array from dbSNP
#'   - print_prefix: awk code to prepend chr/pos to output
#'   - process_substitution: tabix query command
#'   - rsid_condition: awk condition to check RSID membership
#'   - condition: additional non-genomic conditions
#'   - variable_arrays, additional_files: from eval_fcondition_w_gregions
#' @keywords internal
fcondition_and_rsid_to_awk <- function(
  fcondition,
  rsid_bash_index = get_file_interface(fcondition)$column_info[
    "rsid",
    bash_index,
    on = "name"
  ],
  index = 0
) {
  # RSID-based indexing should only be applied if required, i.e.,
  # if the file is RSID-indexed and there is a genomic condition applied
  rsid_indexed <- needs_rsid_matching(get_file_interface(fcondition))
  full_genome_condition <- is_full_genome(
    genomic_regions(fcondition, recursive = TRUE)
  )
  no_genome_condition <- has_no_gregions(fcondition)

  # Standard path: no RSID matching needed
  if (!rsid_indexed || full_genome_condition || no_genome_condition) {
    return(eval_fcondition_w_gregions(
      fcondition,
      finterface = get_file_interface(fcondition)
    ) |>
      data.table::as.data.table())
  }

  # Recursive case: handle OR of multiple genomic blocks
  if (!is_single_genomic_block(fcondition)) {
    stopifnot(fcondition[[1]] == as.symbol("or_filter_condition"))
    return(rbind(
      fcondition_and_rsid_to_awk(
        fcondition[[2]],
        rsid_bash_index,
        index = index
      ),
      fcondition_and_rsid_to_awk(
        fcondition[[3]],
        rsid_bash_index,
        index = index + 1
      )
    ))
  }

  # Single genomic block with RSID matching:
  # 1. Query dbSNP via tabix for RSIDs in the region
  # 2. Generate awk code to load RSIDs into array
  # 3. Generate condition to filter by RSID membership
  genomic_regions(fcondition)[
    ,
    .(
      index = index,
      # awk code block loads RSID -> chr:pos mapping from dbSNP tabix output
      # NR == FNR detects when reading the first file (dbSNP output)
      awk_code_block = paste0(
        "if (NR == FNR%s) {\n",
        "  rsid%i[$3]=$1 OFS $2\n",  # rsid array: RSID -> "chr\tpos"
        "}"
      ) |>
        sprintf(
          ifelse(index == 0,
            "",
            paste0(" + ", index)
          ),
          index
        ),
      # Prefix to prepend chr/pos to output lines
      print_prefix = sprintf(
        "rsid%i[%s] OFS ",
        index,
        rsid_bash_index
      ),
      # Tabix query as bash process substitution
      process_substitution = get_tabix_process_substitution(
        chr = chr,
        start = start,
        end = end,
        dbsnp_filename = get_dbsnp_filename(
          build = get_file_interface(fcondition)$build,
          full_path = TRUE
        )
      ),
      # Condition: only print if RSID exists in loaded array
      rsid_condition = sprintf("%s in rsid%i", rsid_bash_index, index)
    )
  ] |>
    cbind(data.table::as.data.table(
      eval_fcondition_w_gregions(
        fcondition,
        finterface = get_file_interface(fcondition)
      )
    ))
}

# =============================================================================
# Awk Command Assembly
# =============================================================================
# These functions assemble the final awk command from components: BEGIN block,
# condition blocks, file handling, etc.

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
  # Check if this is a simple read (no filtering/processing)
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

  # Simple read: just cat/zcat the file (with optional prefix handling)
  if (only_read) {
    return(
      awk_load_file_cmd(
        finterface,
        nlines = nlines,
        only_read = only_read
      )
    )
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

  # Build BEGIN block (FS, OFS, line counting vars)
  begin_code_block <- build_awk_begin_block(
    finterface$sep,
    nlines,
    use_command_line_fs
  )

  # Add comment prefix handling pattern (skip lines starting with #, etc.)
  comment_prefix_pattern <- build_comment_filter_pattern(
    finterface$comment_prefix
  )

  # Assemble complete awk script
  awk_script <- paste(
    c(begin_code_block, comment_prefix_pattern, full_code_block),
    collapse = "\n"
  )

  # For long scripts, write to temp file to avoid command-line length limits
  # TODO: improve cleanup of temporary files
  if (return_only_cmd) {
    awk_code <- sprintf("'%s'", awk_script)
  } else {
    temp_awk_file <- tempfile(fileext = ".awk")
    writeLines(
      paste0(awk_script, "\n"),
      temp_awk_file
    )
    awk_code <- sprintf("-f %s", temp_awk_file)
  }

  # Build file arguments list:
  # 1. Process substitutions (tabix queries) with tab separator
  # 2. Additional temp files (%in% value arrays)
  # 3. Main data file (with appropriate separator)
  awk_final_filenames <- c(
    {
      if (is.null(fcondition_awk_dt$process_substitution)) {
        NULL
      } else {
        # Tabix output is always tab-separated
        paste(
          "FS=\"\\t\"",
          paste(fcondition_awk_dt$process_substitution, collapse = " ")
        )
      }
    },
    fcondition_awk_dt$additional_files,
    {
      # Reset FS to file's separator before reading main file
      if (use_command_line_fs) {
        sprintf("FS=\"%s\"", finterface$sep)
      }
    },
    wrap_filename(finterface)
  ) |>
    paste(collapse = " ")

  paste("awk", awk_code, awk_final_filenames)
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
  if (!is.null(nlines)) {
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
  } else if (nrow(fcondition_awk_dt) == 0) {
    # Simple case: just print (with optional line limit)
    if (is.null(nlines)) {
      condition_block <- "print $0"
    } else {
      condition_block <- "if (++output_lines <= max_lines) print $0; else exit"
    }
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
  escaped <- pattern
  # Escape each special regex character
  escaped <- gsub("/", "\\\\/", escaped)   # / -> \/
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
  if (is.null(nlines)) {
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
build_awk_begin_block <- function(sep, nlines = NULL, use_command_line_fs = FALSE)
{
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

#' Build pipeline command (file | awk)
#'
#' @param file_cmd Shell command to read file
#' @param awk_script Awk script/command
#' @return Piped command string
#' @keywords internal
build_pipeline_cmd <- function(file_cmd, awk_script) {
  paste(file_cmd, "|", awk_script)
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
  needs_processing <- has_prefixes || !is.null(nlines)

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
