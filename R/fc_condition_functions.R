#' @import data.table

lt_filter_condition <- function(
    var1,
    var2) {
  list(condition = sprintf("%s < %s", var1, var2))
}
lte_filter_condition <- function(
    var1,
    var2) {
  list(condition = sprintf("%s <= %s", var1, var2))
}
gt_filter_condition <- function(
    var1,
    var2) {
  list(condition = sprintf("%s > %s", var1, var2))
}
gte_filter_condition <- function(
    var1,
    var2) {
  list(condition = sprintf("%s >= %s", var1, var2))
}
eq_filter_condition <- function(
    var1,
    var2) {
  list(condition = sprintf("%s == %s", var1, var2))
}
in_filter_condition <- function(
    column_name,
    values,
    filename = tempfile()) {
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

and_filter_condition <- function(
    fcondition1,
    fcondition2) {
  combine_filter_condition(fcondition1, fcondition2, "&&")
}

or_filter_condition <- function(
    fcondition1,
    fcondition2) {
  combine_filter_condition(fcondition1, fcondition2, "||")
}
or_filter_condition_rsid <- function(
    fcondition1,
    fcondition2) {
  list(condition = unname(c(fcondition1, fcondition2)))
}

lp_filter_condition <- function(
    fcondition) {
  fcondition$condition <- sprintf("(%s)", fcondition$condition)
  fcondition
}
