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
#' @param resolve_quotes_function Quote handling function to use
#'
#' @return Processed values ready for awk condition or file writing
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

  # Get column metadata for post-processing
  post_processing_to_check <- finterface$column_info[
    column_name,
    .(standard_name, quoted, observed_prefixes),
    on = "name"
  ]

  # Allele columns are upper-cased in awk so RHS literals must match.
  if (isTRUE(post_processing_to_check$standard_name %in%
    ALLELE_STANDARD_NAMES) && is.character(values)) {
    values <- toupper(values)
  }

  if (isTRUE(post_processing_to_check$standard_name == "chr")) {
    values <- drop_chr_prefix(values)
  }

  observed_prefixes <- post_processing_to_check$observed_prefixes[[1]]
  if (1 < length(observed_prefixes)) {
    stop(
      "Filtering on ", column_name, " is not supported: its values do not ",
      "agree on a prefix (some carry ",
      paste(encodeString(setdiff(observed_prefixes, ""), quote = "\""),
            collapse = " or "),
      ", some carry none), so a literal has no single spelling to match them.",
      call. = FALSE
    )
  }

  values |>
    resolve_prefix(observed_prefixes) |>
    resolve_quotes_function(post_processing_to_check$quoted)
}

#' Add prefix to values if missing
#'
#' Ensures values have the expected prefix (e.g., "chr" for chromosomes).
#' Values already having the prefix are unchanged.
#'
#' @param values Vector of values
#' @param prefix Prefix to add. `NA`, `""` and a zero-length vector each mean
#'   there is none to add, and the values are returned unchanged.
#'
#' @return Values with prefix added where missing
#' @keywords internal
#'
#' @examples
#' # resolve_prefix(c("1", "chr2"), "chr") returns c("chr1", "chr2")
resolve_prefix <- function(
  values,
  prefix
) {
  if (length(prefix) == 0L || is.na(prefix) || prefix == "") {
    return(values)
  }
  data.table::fifelse(
    values == "" | grepl(paste0("^", prefix), values),
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
