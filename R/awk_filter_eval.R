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
#'
#' @return List with components:
#'   - condition: awk condition string (e.g., "$5 < 5e-8 && $1 == 1")
#'   - variable_arrays: awk code for loading %in% arrays
#'   - additional_files: temp files that need to be passed to awk
#' @keywords internal
eval_fcondition_w_gregions <- function(
  fcondition,
  finterface = get_file_interface(fcondition),
  column_indices = get_column_indices(finterface)
) {
  # For non-AND blocks (i.e., OR at top level), recursively process children
  if (!non_genomic_is_and_block(fcondition)) {
    return(
      do.call(
        get(fcondition[[1]]),
        lapply(
          fcondition[-1],
          eval_fcondition_w_gregions,
          column_indices = column_indices
        )
      )
    )
  }
  # For AND blocks, evaluate non-genomic conditions first
  awk_conditions <- eval_fcondition(
    fcondition,
    column_indices = column_indices
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
  # Drop empty fragments before combining: paste on a list containing
  # zero-length elements emits the literal "character(0)" because R coerces
  # each empty vector to a string. Empty conditions reduce to NULL so
  # downstream codegen can omit the wrapping `if (...)`.
  awk_conditions$condition <- unlist(awk_conditions$condition)
  if (length(awk_conditions$condition) == 0) {
    awk_conditions$condition <- NULL
  } else {
    awk_conditions$condition <- paste(awk_conditions$condition,
      collapse = " && "
    )
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
#'
#' @return List with condition string and optional variable_arrays/files
#' @keywords internal
eval_fcondition <- function(
  fcondition,
  finterface = get_file_interface(fcondition),
  column_indices = get_column_indices(finterface)
) {
  if (length(fcondition) == 0) {
    return()
  }
  # The atomic constructors (in_filter_condition, eq_filter_condition, ...)
  # and the &/| combinators are unexported, so they only resolve in this
  # package's namespace. quo_squash flattens the post-processed tree --
  # including the nested quosures of composite conditions -- into a bare
  # expression so eval_tidy honors `env` instead of each quosure's captured
  # environment (which under library() cannot see the constructors). Column
  # symbols still resolve through the `column_indices` data mask; all
  # user-supplied values were reduced to literals during construction.
  rlang::eval_tidy(
    rlang::quo_squash(post_process(fcondition)),
    data = column_indices,
    env = asNamespace("filterRead")
  )
}

#' Convert genomic_regions from filter_condition to awk
#'
#' Translates a genomic_regions from the filter_condition into awk conditions.
#' Each region becomes a condition like
#' `$1 == "chr1" && 1000 <= $2 && $2 <= 2000`, combined with OR for multiple
#' regions.
#'
#' @param fcondition A filter_condition with embedded genomic_regions
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
  gregions <- fc_genomic_regions(fcondition) |>
    post_process(finterface = finterface)
  # Skip for NULL regions or RSID-indexed files (handled differently)
  if (is.null(gregions) || needs_rsid_matching(finterface)) {
    return()
  }
  # Empty regions are a degenerate case:
  # - Included with 0 rows = match nothing (awk false constant "0")
  # - Excluded with 0 rows = no constraint to apply (drop the condition)
  if (nrow(gregions) == 0) {
    if (is_included(gregions)) return("0")
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
  if (is.null(gregions_fcs)) {
    return()
  }
  gregions_fcs <- paste(gregions_fcs, collapse = " || ")
  # Wrap multiple regions in parentheses for correct precedence
  if (1 < nrow(gregions)) {
    gregions_fcs <- sprintf("(%s)", gregions_fcs)
  }
  # Excluded regions invert the match: keep rows that fall outside them
  if (!is_included(gregions)) {
    gregions_fcs <- sprintf("!(%s)", gregions_fcs)
  }
  gregions_fcs
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
  nlines = NULL,
  return_only_cmd = FALSE
) {
  # Determine which columns need array splitting (for encoded columns)
  column_arrays <- get_awk_column_arrays(
    get_file_interface(fcondition),
    fcondition = fcondition
  )

  # Convert condition to awk and assemble full command. The awk dt also
  # carries any tempfiles created for %in% matching; expose them on the
  # returned command so callers can clean up after fread consumes them.
  fcondition_awk_dt <- fcondition_and_rsid_to_awk(fcondition)
  cmd <- compile_awk_cmds(
    finterface = get_file_interface(fcondition),
    fcondition_awk_dt = fcondition_awk_dt,
    column_arrays_before_conditions = column_arrays$before_if,
    column_arrays_after_conditions = column_arrays$after_if,
    nlines = nlines,
    return_only_cmd = return_only_cmd
  )
  attr(cmd, "additional_files") <- c(
    unlist(fcondition_awk_dt$additional_files),
    attr(cmd, "additional_files")
  )
  cmd
}
