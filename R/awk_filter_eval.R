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
  if (!non_genomic_is_and_block(fcondition)) {
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
  # Then add genomic region conditions (chr == X && Y <= pos && pos <= Z)
  awk_conditions$condition <- c(
    awk_conditions$condition,
    eval_genomic_regions_from_fc(
      fcondition,
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

#' Convert genomic_regions from filter_condition to awk
#'
#' Translates a genomic_regions from the filter_condition into awk conditions.
#' Each region becomes a condition like
#' `$1 == "chr1" && 1000 <= $2 && $2 <= 2000`, combined with OR for multiple
#' regions.
#'
#' @param gregions A genomic_regions object (data.table with chr/start/end)
#' @param finterface File interface (used to check RSID matching)
#' @param column_indices Named list with chr and pos column refs
#'
#' @return Character string with awk condition, or NULL if empty/RSID file
#' @keywords internal
eval_genomic_regions_from_fc <- function(
  fcondition,
  finterface,
  column_indices
) {
  gregions <- genomic_regions(fcondition) |>
    post_process(finterface = finterface)
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
