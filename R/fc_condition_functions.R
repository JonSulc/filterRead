# =============================================================================
# Filter Condition Functions (Awk Condition Builders)
# =============================================================================
# These functions are the atomic building blocks for constructing awk
# condition strings. Each function corresponds to an R comparison operator
# and returns a list with:
#   - condition: awk condition string (e.g., "$5 < 5e-8")
#   - variable_arrays: (optional) awk code for loading %in% value arrays
#   - additional_files: (optional) temp files to pass to awk
#
# These functions are called during filter_condition evaluation when
# R expressions are translated to awk syntax.

#' Build awk less-than condition
#'
#' @param var1 Left operand (column ref or value)
#' @param var2 Right operand (column ref or value)
#' @return List with `condition` element: "var1 < var2"
#' @keywords internal
lt_filter_condition <- function(
  var1,
  var2
) {
  list(condition = sprintf("%s < %s", var1, var2))
}

#' Build awk less-than-or-equal condition
#'
#' @param var1 Left operand (column ref or value)
#' @param var2 Right operand (column ref or value)
#' @return List with `condition` element: "var1 <= var2"
#' @keywords internal
lte_filter_condition <- function(
  var1,
  var2
) {
  list(condition = sprintf("%s <= %s", var1, var2))
}

#' Build awk greater-than condition
#'
#' @param var1 Left operand (column ref or value)
#' @param var2 Right operand (column ref or value)
#' @return List with `condition` element: "var1 > var2"
#' @keywords internal
gt_filter_condition <- function(
  var1,
  var2
) {
  list(condition = sprintf("%s > %s", var1, var2))
}

#' Build awk greater-than-or-equal condition
#'
#' @param var1 Left operand (column ref or value)
#' @param var2 Right operand (column ref or value)
#' @return List with `condition` element: "var1 >= var2"
#' @keywords internal
gte_filter_condition <- function(
  var1,
  var2
) {
  list(condition = sprintf("%s >= %s", var1, var2))
}

#' Build awk equality condition
#'
#' @param var1 Left operand (column ref or value)
#' @param var2 Right operand (column ref or value)
#' @return List with `condition` element: "var1 == var2"
#' @keywords internal
eq_filter_condition <- function(
  var1,
  var2
) {
  list(condition = sprintf("%s == %s", var1, var2))
}

#' Build awk membership condition (for R %in% operator)
#'
#' Creates an awk condition that checks if a column value exists in a set
#' of values. Values are written to a temp file and loaded into an awk
#' associative array for O(1) lookup.
#'
#' @param column_name Awk column reference (e.g., "$3")
#' @param values Vector of values to check membership against
#' @param filename Path for temp file (defaults to tempfile())
#'
#' @return List with:
#'   - condition: awk membership check "(column_name in array)"
#'   - variable_arrays: awk code to load values from file into array
#'   - additional_files: temp file path to pass to awk
#' @keywords internal
in_filter_condition <- function(
  column_name,
  values,
  filename = tempfile()
) {
  filename_handle <- basename(filename)
  # "var" is pasted instead of subbed to avoid issues if the filename does not
  # contain "file"
  variable_handle <- paste0("var", gsub("file", "", filename_handle))

  list(
    variable_arrays = setup_variable_array(values,
      filename        = filename,
      variable_handle = variable_handle
    ),
    condition = sprintf("(%s in %s)", column_name, variable_handle),
    additional_files = filename
  )
}

#' Combine two conditions with AND (&&)
#'
#' @param fcondition1 First condition list
#' @param fcondition2 Second condition list
#' @return Combined condition list with merged arrays/files
#' @keywords internal
and_filter_condition <- function(
  fcondition1,
  fcondition2
) {
  combine_filter_condition(fcondition1, fcondition2, "&&")
}

#' Combine two conditions with OR (||)
#'
#' @param fcondition1 First condition list
#' @param fcondition2 Second condition list
#' @return Combined condition list with merged arrays/files
#' @keywords internal
or_filter_condition <- function(
  fcondition1,
  fcondition2
) {
  combine_filter_condition(fcondition1, fcondition2, "||")
}

#' Combine two conditions for RSID-indexed files
#'
#' Special OR handling for RSID-based files. Instead of combining into a
#' single awk condition, keeps conditions separate for multi-file processing
#' where each condition corresponds to a different tabix query.
#'
#' @param fcondition1 First condition (or condition list)
#' @param fcondition2 Second condition (or condition list)
#' @return List with `condition` as unnamed vector of conditions
#' @keywords internal
or_filter_condition_rsid <- function(
  fcondition1,
  fcondition2
) {
  list(condition = unname(c(fcondition1, fcondition2)))
}

#' Wrap condition in parentheses
#'
#' Adds parentheses around a condition for correct operator precedence.
#'
#' @param fcondition Condition list with `condition` element
#' @return Same list with condition wrapped in parentheses
#' @keywords internal
lp_filter_condition <- function(
  fcondition
) {
  fcondition$condition <- sprintf("(%s)", fcondition$condition)
  fcondition
}

#' Generic condition combiner
#'
#' Combines two condition lists with a specified operator, merging their
#' variable_arrays and additional_files.
#'
#' @param fcondition1 First condition list
#' @param fcondition2 Second condition list
#' @param operation Awk operator string ("&&" or "||")
#'
#' @return Combined condition list with:
#'   - condition: combined condition string
#'   - variable_arrays: merged from both conditions
#'   - additional_files: merged from both conditions
#' @keywords internal
combine_filter_condition <- function(
  fcondition1,
  fcondition2,
  operation
) {
  list(
    variable_arrays = c(
      fcondition1$variable_array,
      fcondition2$variable_array
    ),
    condition = paste(
      fcondition1$condition,
      operation,
      fcondition2$condition
    ),
    additional_files = c(
      fcondition1$additional_files,
      fcondition2$additional_files
    )
  )
}
