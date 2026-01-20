# =============================================================================
# Value Post-Processing
# =============================================================================
# These functions transform R values to match file format expectations:
# - Adding prefixes (e.g., "chr" for chromosome values)
# - Handling quoted values (escaping quotes for awk)

#' Apply post-processing to values for condition matching
#'
#' Orchestrates prefix and quote handling based on column metadata.
#'
#' @param values Vector of values to process
#' @param column_symbol Symbol representing the column being matched
#' @param finterface File interface with column metadata
#' @param to_write If TRUE, use write mode for quotes (for temp files)
#' @param check_quotes_function Quote handling function to use
#'
#' @return Processed values ready for awk condition or file writing
#' @keywords internal
check_post_processing <- function(
  values,
  column_symbol,
  finterface,
  to_write = FALSE,
  check_quotes_function = ifelse(
    to_write,
    check_quotes_to_write,
    check_quotes
  )
) {
  column_name <- as.character(column_symbol)
  if (!column_name %in% finterface$column_info$name) {
    return(values)
  }

  # Get column metadata for post-processing
  post_processing_to_check <- finterface$column_info[
    column_name,
    .(quoted, prefix),
    on = "name"
  ]

  # Apply prefix first, then quote handling
  values |>
    check_prefix(post_processing_to_check$prefix) |>
    check_quotes_function(post_processing_to_check$quoted)
}

#' Add prefix to values if missing
#'
#' Ensures values have the expected prefix (e.g., "chr" for chromosomes).
#' Values already having the prefix are unchanged.
#'
#' @param values Vector of values
#' @param prefix Prefix to add (or NA if none needed)
#'
#' @return Values with prefix added where missing
#' @keywords internal
#'
#' @examples
#' # check_prefix(c("1", "chr2"), "chr") returns c("chr1", "chr2")
check_prefix <- function(
  values,
  prefix
) {
  if (is.na(prefix)) {
    return(values)
  }
  data.table::fifelse(
    grepl(paste0("^", prefix), values),
    as.character(values),
    paste0(prefix, values)
  )
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
check_quotes <- function(
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
check_quotes_to_write <- function(
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
