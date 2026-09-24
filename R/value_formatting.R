# Value Post-Processing ----
# These functions transform R values to match file format expectations:
# - Spelling a value as the file may hold it
# - Handling quoted values (escaping quotes for awk)

#' Apply post-processing to values for condition matching
#'
#' Orchestrates spelling and quote handling based on column metadata. A
#' literal arrives in the package's spelling or in the file's, so any prefix
#' the column declares is dropped from it before it is spelled for the file.
#'
#' @param values Vector of values to process
#' @param column_symbol Symbol representing the column being matched
#' @param finterface File interface with column metadata
#' @param to_write If TRUE, use write mode for quotes (for temp files)
#' @param resolve_quotes_function Quote handling function to use
#'
#' @return Processed values ready for awk condition or file writing, one per
#'   spelling the file may hold them in
#' @keywords internal
resolve_post_processing <- function(
  values,
  column_symbol,
  finterface,
  to_write = FALSE,
  resolve_quotes_function = ifelse(
    to_write,
    resolve_quotes_to_write,
    resolve_quotes
  )
) {
  column_name <- as.character(column_symbol)
  if (!column_name %in% finterface$column_info$name) {
    return(values)
  }

  column <- finterface$column_info[
    column_name,
    .(standard_name, quoted, possible_prefixes, dropped_prefix),
    on = "name"
  ]

  # Allele columns are upper-cased in awk, so a literal compared against one
  # is upper-cased too.
  if (isTRUE(column$standard_name %in% ALLELE_STANDARD_NAMES) &&
    is.character(values)) {
    values <- toupper(values)
  }

  # A column declaring nothing leaves the values as they came, which is what
  # keeps a numeric literal numeric for resolve_quotes().
  values <- Reduce(
    \(carried, prefix) drop_prefix(carried, prefix),
    column$possible_prefixes[[1]],
    init = values
  )

  resolve_spelling(values, column) |>
    resolve_quotes_function(column$quoted)
}

#' Spell values as the file may hold them
#'
#' A chromosome is returned bare and again under every prefix its column
#' declares. Any other column's values are returned under its `dropped_prefix`,
#' or unchanged where that is NA.
#'
#' @param values Values in the package's spelling.
#' @param column The `column_info` row of the column being matched.
#'
#' @return The values, once per spelling.
#' @keywords internal
resolve_spelling <- function(
  values,
  column
) {
  if (isTRUE(column$standard_name == "chr")) {
    return(c(
      values,
      unlist(lapply(
        column$possible_prefixes[[1]], paste0, values, recycle0 = TRUE
      ))
    ))
  }
  if (is.na(column$dropped_prefix)) {
    return(values)
  }
  paste0(column$dropped_prefix, values, recycle0 = TRUE)
}

#' Format values for awk condition (with quote escaping)
#'
#' Prepares values for use in awk equality conditions. Handles:
#' - Numeric values: passed through unchanged
#' - String values: wrapped in double quotes
#' - Quoted column values: escaped for nested quotes in awk
#'
#' @param values Vector of values
#' @param quoted TRUE if column values are quoted in the file
#'
#' @return Formatted values for awk condition syntax
#' @keywords internal
#'
#' @details
#' For quoted columns, generates awk syntax like `"\"value\""` to match
#' values that appear as `"value"` in the file.
resolve_quotes <- function(
  values,
  quoted
) {
  if (!isTRUE(quoted)) {
    if (all(is.numeric(values))) {
      return(values)
    }
    # Simple case: wrap in quotes for awk string comparison
    return(sprintf("\"%s\"", values))
  }
  # Quoted column: need to match "value" in file
  # Strip any existing quotes, then add escaped quotes
  values <- gsub("^(?:\")?(?:\\\")?(.*)(?:\\\")?(?:\")?$", "\\1", values)
  sprintf(
    "\"\\\"%s\\\"\"",
    values
  )
}

#' Format values for writing to temp file
#'
#' Prepares values for writing to temp files (used by %in% operator).
#' For quoted columns, ensures values are properly quoted to match
#' file contents.
#'
#' @param values Vector of values
#' @param quoted TRUE if column values are quoted in the file
#'
#' @return Formatted values for temp file writing
#' @keywords internal
resolve_quotes_to_write <- function(
  values,
  quoted
) {
  if (!isTRUE(quoted)) {
    return(values)
  }
  # Add quotes if not already quoted
  data.table::fifelse(
    grepl("^\".*\"$", values),
    as.character(values),
    sprintf("\"%s\"", values)
  )
}
