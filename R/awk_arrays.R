# =============================================================================
# Column Array Setup Functions
# =============================================================================
# These functions generate awk code for handling encoded/delimited columns.
# When a column contains multiple values separated by a delimiter (e.g., "A:B"),
# these functions create awk code to split the column into an array, process it,
# and reassemble the values for output.
# Example:
# # Before conditions:
# split($1, arr, \":\")
# # Before printing:
# $1 = arr[1] OFS arr[2]

#' Generate awk code to split and recombine a delimited column
#'
#' Creates awk code that: (1) splits a column by delimiter into an array,
#' and (2) recombines the array elements for output using OFS as separator.
#' Used for columns that contain multiple encoded values.
#'
#' @param bash_index The awk column reference (e.g., "$1", "$2")
#' @param array_name Name for the awk array to store split values
#' @param delimiter String used to split the column (default: " ")
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
  # Output format: "split($1, arr, \"<delimiter>\")\n$1 = arr[1] OFS arr[2]"
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
#' @param bash_index The awk column reference (e.g., "$1", "$2")
#' @param array_name Name for the awk array to store split values
#' @param delimiter String used to split the column
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
#' separator). This adds the decoded values as new columns in the output.
#'
#' @param bash_index The awk column reference (e.g., "$1", "$2")
#' @param array_name Name of the awk array containing split values
#' @param array_length Number of elements expected in the array
#'
#' @return Character string like `$1 = arr[1] OFS arr[2] OFS arr[3]`
#' @keywords internal
awk_combine_split_for_output <- function(
  bash_index,
  array_name,
  array_length
) {
  # Generates: "$1 = arr[1] OFS arr[2]" for array_length = 2
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
#' that reads the file and stores values in an associative array for O(1)
#' lookup. The array is populated when awk reads the temp file, before
#' processing the main data file.
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

