# =============================================================================
# Filter Condition Internal (R Expression -> Filter Condition Conversion)
# =============================================================================
# These functions convert R expressions (like `pval < 5e-8`) into
# filter_condition objects that can later be evaluated to awk code.
#
# The conversion process:
# 1. R expression is parsed
# 2. Operators are replaced with filter_condition function calls:
#    - `(` -> lp_filter_condition (left paren)
#    - `&` -> and_filter_condition
#    - `|` -> or_filter_condition
#    - `<`, `<=`, `>`, `>=` -> chainable operators
#    - `==` -> eq_filter_condition
#    - `%in%` -> in_filter_condition
# 3. Values are processed (quotes added, prefixes applied)
# 4. Result is a call object with filter_condition class

#' Convert parenthesized expression to filter condition
#'
#' Transforms `(expr)` into `lp_filter_condition(expr)` for precedence control.
#'
#' @param fcall R call object starting with `(`
#' @param ... Additional arguments passed to new_filter_condition
#'
#' @return filter_condition call with lp_filter_condition class
#' @keywords internal
lp_to_fc <- function(
  fcall,
  ...
) {
  stopifnot(fcall[[1]] == as.symbol("("))
  fcall[[1]] <- as.symbol("lp_filter_condition")
  fcall[[2]] <- new_filter_condition(fcall[[2]], ...)
  class(fcall) <- c("lp_filter_condition", class(fcall)) |>
    unique()
  fcall
}

#' Wrap filter condition in parentheses
#'
#' Creates a new lp_filter_condition wrapping an existing filter_condition.
#' Preserves build and genomic_regions attributes.
#'
#' @param fcall An existing filter_condition object
#'
#' @return New filter_condition wrapped in lp_filter_condition
#' @keywords internal
lp_wrap_fcondition <- function(
  fcall
) {
  fcondition <- rlang::expr(lp_filter_condition())
  fcondition[[2]] <- fcall
  fcondition <- as_filter_condition(fcondition)
  class(fcondition) <- c("lp_filter_condition", class(fcondition)) |>
    unique()
  attr(fcondition, "build") <- attr(fcall, "build")
  genomic_regions(fcondition) <- genomic_regions(fcall)
  fcondition
}

#' Convert AND expression to filter condition
#'
#' Transforms `expr1 & expr2` by recursively converting both operands
#' and combining with `&` operator (which creates and_filter_condition).
#'
#' @param fcall R call object with `&` operator
#' @param ... Additional arguments passed to new_filter_condition
#'
#' @return Combined filter_condition with AND semantics
#' @keywords internal
and_to_fc <- function(
  fcall,
  ...
) {
  new_filter_condition(fcall[[2]], ...) &
    new_filter_condition(fcall[[3]], ...)
}

#' Convert OR expression to filter condition
#'
#' Transforms `expr1 | expr2` by recursively converting both operands
#' and combining with `|` operator (which creates or_filter_condition).
#'
#' @param fcall R call object with `|` operator
#' @param ... Additional arguments passed to new_filter_condition
#'
#' @return Combined filter_condition with OR semantics
#' @keywords internal
or_to_fc <- function(
  fcall,
  ...
) {
  new_filter_condition(fcall[[2]], ...) |
    new_filter_condition(fcall[[3]], ...)
}

#' Convert comparison operators to filter conditions
#'
#' Handles `<`, `<=`, `>`, `>=` operators by replacing with corresponding
#' filter_condition function names.
#'
#' @param fcall R call object with comparison operator
#' @param ... Ignored (for interface consistency)
#'
#' @return filter_condition call with appropriate class
#' @keywords internal
chainable_to_fc <- function(
  fcall,
  ...
) {
  # Map R operators to filter_condition function symbols
  fcall[[1]] <- list(
    "<"  = as.symbol("lt_filter_condition"),
    "<=" = as.symbol("lte_filter_condition"),
    ">"  = as.symbol("gt_filter_condition"),
    ">=" = as.symbol("gte_filter_condition")
  )[[as.character(fcall[[1]])]]
  class(fcall) <- c(
    as.character(fcall[[1]]),
    class(fcall)
  ) |> unique()
  fcall
}

#' Convert equality expression to filter condition
#'
#' Handles `==` operator with special processing for column values:
#' - Identifies which operand is a column reference
#' - Applies quote escaping and prefix handling to the value
#' - For genomic columns (chr, pos), preserves for later genomic_regions
#'   processing
#'
#' @param fcall R call object with `==` operator
#' @param finterface_env Environment containing file interface
#' @param env Environment for evaluating non-column expressions
#' @param ... Ignored
#'
#' @return eq_filter_condition call with processed values
#' @keywords internal
eq_to_fc <- function(
  fcall,
  finterface_env,
  env,
  ...
) {
  stopifnot(fcall[[1]] == as.symbol("=="))
  fcall[[1]] <- as.symbol("eq_filter_condition")

  # Identify column vs value and apply post-processing to value
  # To work with genomic_regions, post-processing will have to be done later
  if (is_column_symbol(fcall[[2]], finterface_env$finterface) &&
    !is_column_symbol(fcall[[3]], finterface_env$finterface)) {
    # Left is column, right is value
    fcall[[3]] <- eval(fcall[[3]], env) |>
      check_post_processing(fcall[[2]], finterface_env$finterface)
  } else if (is_column_symbol(fcall[[3]], finterface_env$finterface) &&
    !is_column_symbol(fcall[[2]], finterface_env$finterface)) {
    # Right is column, left is value
    fcall[[2]] <- eval(fcall[[2]], env) |>
      check_post_processing(fcall[[3]], finterface_env$finterface)
  }

  class(fcall) <- c("eq_filter_condition", class(fcall)) |>
    unique()

  fcall
}

#' Convert %in% expression to filter condition
#'
#' Handles `%in%` operator for membership testing. Values are written
#' to a temp file and loaded into an awk associative array.
#'
#' @param fcall R call object with `%in%` operator
#' @param finterface_env Environment containing file interface
#' @param env Environment for evaluating the values vector
#' @param ... Ignored
#'
#' @return in_filter_condition call with processed values
#' @keywords internal
in_to_fc <- function(
  fcall,
  finterface_env,
  env,
  ...
) {
  fcall[[1]] <- as.symbol("in_filter_condition")

  # Evaluate values and apply post-processing (quotes/prefixes for file write)
  fcall[[3]] <- eval(fcall[[3]], env) |>
    check_post_processing(
      fcall[[2]],
      finterface_env$finterface,
      to_write = TRUE
    )

  class(fcall) <- c("in_filter_condition", class(fcall)) |>
    unique()

  fcall
}

#' Check if expression is a column name
#'
#' Tests if a call/symbol represents a recognized column name in the file.
#'
#' @param fcall Expression to test
#' @param finterface File interface with column_info
#'
#' @return TRUE if fcall is a single symbol matching a column name
#' @keywords internal
is_column_symbol <- function(
  fcall,
  finterface
) {
  if (length(fcall) != 1) {
    return(FALSE)
  }
  as.character(fcall) %in% finterface$column_info$name
}

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

#' Evaluate non-column symbols in filter condition
#'
#' For comparison operators, evaluates any operand that is not a column
#' reference. Column references are kept as symbols for later evaluation
#' in the awk column environment.
#'
#' @param fcondition A filter_condition call
#' @param finterface File interface for column detection
#' @param env Environment for evaluating expressions
#'
#' @return filter_condition with non-column values evaluated
#' @keywords internal
evaluate_non_column_variables <- function(
  fcondition,
  finterface,
  env
) {
  # Evaluate all arguments except the function name (first element)
  fcondition[-1] <- lapply(
    fcondition[-1],
    \(x) {
      if (is_column_symbol(x, finterface)) return(x)
      eval(x, env)
    }
  )
  fcondition
}
