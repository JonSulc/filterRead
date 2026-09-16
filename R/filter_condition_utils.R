#' Check if a filter condition is an AND block
#'
#' An AND block contains only AND operations (no OR at any level, including
#' its genomic constraint).
#'
#' @param fcondition A filter_condition object
#' @return Logical indicating whether the condition is an AND block
#' @keywords internal
is_and_block <- function(
  fcondition
) {
  if (!is.filter_condition(fcondition)) {
    return(TRUE)
  }

  gconstraint <- genomic_constraint(fcondition)
  if (!is.null(gconstraint) && 1 < nrow(gconstraint)) {
    return(FALSE)
  }

  if (length(fcondition) == 0) {
    return(TRUE)
  }

  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(FALSE)
  }

  all(sapply(fcondition[-1], is_and_block))
}

#' Check if non-genomic parts of a condition form an AND block
#'
#' Similar to is_and_block but ignores the region set, only checking
#' the structure of non-genomic conditions.
#'
#' @param fcondition A filter_condition object
#' @return Logical indicating whether non-genomic conditions are an AND block
#' @keywords internal
non_genomic_is_and_block <- function(
  fcondition
) {
  if (!is.call(fcondition)) {
    return(TRUE)
  }

  if (length(fcondition) == 0) {
    return(TRUE)
  }

  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(FALSE)
  }

  all(sapply(fcondition[-1], non_genomic_is_and_block))
}

#' Check whether a genomic constraint restricts nothing
#'
#' A constraint restricts nothing when it is absent or is a full region set.
#' Either way it narrows no read.
#'
#' @param genomic_constraint A region_set, or NULL for a full region set
#' @return Logical indicating whether the constraint narrows nothing
#' @seealso [is_overconstrained()], the opposite pole.
#' @keywords internal
is_unconstrained <- function(
  genomic_constraint
) {
  is.null(genomic_constraint) || is_full_region_set(genomic_constraint)
}

#' Check whether a genomic constraint admits no position
#'
#' A constraint admits no position when it is present and includes no genomic
#' position.
#'
#' @param genomic_constraint A region_set, or NULL for a full region set
#' @return Logical indicating whether the constraint admits no position
#' @seealso [is_unconstrained()], the opposite pole.
#' @keywords internal
is_overconstrained <- function(
  genomic_constraint
) {
  !is.null(genomic_constraint) && is_empty_region_set(genomic_constraint)
}

#' Combine genomic constraints, treating NULL as a full region set
#'
#' @param left,right A region_set, or NULL for a full region set
#' @return A region_set object, or NULL: for `and_constraints()` when
#'   neither side constrains, for `or_constraints()` when either side does not.
#' @keywords internal
and_constraints <- function(
  left,
  right
) {
  if (is.null(left)) {
    return(right)
  }
  if (is.null(right)) {
    return(left)
  }
  left & right
}

#' @rdname and_constraints
#' @keywords internal
or_constraints <- function(
  left,
  right
) {
  if (is.null(left) || is.null(right)) {
    return(NULL)
  }
  left | right
}

#' Compare two genomic constraints, treating NULL as a full region set
#'
#' An absent constraint and an explicit full region set select the same
#' positions, so they compare equal.
#'
#' @param left,right A region_set, or NULL for a full region set
#' @return Logical indicating whether both constraints cover the same positions
#' @keywords internal
eq_constraints <- function(
  left,
  right
) {
  if (is.null(left)) {
    return(is_unconstrained(right))
  }
  if (is.null(right)) {
    return(is_unconstrained(left))
  }
  left == right
}

#' Get the genomic constraint of a filter condition
#'
#' The coordinate restriction the condition contributes on its own, beyond
#' whatever the conditions it combines contribute.
#'
#' @param fcondition A filter_condition object
#' @return A region_set object, or NULL when the condition restricts no
#'   coordinates
#' @seealso [source_regions()] for what the condition as a whole covers.
#' @keywords internal
genomic_constraint <- function(
  fcondition
) {
  stopifnot(is.filter_condition(fcondition))
  attr(fcondition, "genomic_constraint")
}

#' Set the genomic constraint of a filter condition
#'
#' @param fcondition A filter_condition object
#' @param value A region_set, or NULL for a full region set
#' @return The modified filter_condition
#' @keywords internal
`genomic_constraint<-` <- function(
  fcondition,
  value
) {
  attr(fcondition, "genomic_constraint") <- value
  fcondition
}

#' Check if a filter condition contains genomic constraints
#'
#' @param fcondition A filter_condition object
#' @return Logical indicating whether chromosome or position conditions exist
#' @keywords internal
has_genomic_condition <- function(
  fcondition
) {
  has_chromosome_condition(fcondition) | has_position_condition(fcondition)
}

#' Check if a filter condition contains chromosome constraints
#'
#' @param fcondition A filter_condition object
#' @return Logical indicating whether chromosome conditions exist
#' @keywords internal
has_chromosome_condition <- function(
  fcondition
) {
  if (is.filter_condition(fcondition)) {
    if (any(!is.na(genomic_constraint(fcondition)$chr))) {
      return(TRUE)
    }
  }
  if (rlang::is_quosure(fcondition)) {
    fcondition <- strip_parentheses(fcondition, recursive = FALSE)
    if (!rlang::is_quosure(fcondition)) {
      return(has_chromosome_condition(fcondition))
    }
    # grepl / %like% on chr cannot be enumerated into a tabix range, so
    # they must fall through to awk as a non-genomic regex filter.
    if (is_regex_filter_condition(fcondition)) {
      return(FALSE)
    }
    return(any(sapply(fcondition[-1], has_chromosome_condition) |> unlist()))
  }
  if (length(fcondition) == 0) {
    return(FALSE)
  }
  fcondition == as.symbol("chr")
}

#' Check if a filter condition contains position constraints
#'
#' @param fcondition A filter_condition object
#' @return Logical indicating whether position conditions exist
#' @keywords internal
has_position_condition <- function(
  fcondition
) {
  if (is.filter_condition(fcondition)) {
    gconstraint <- genomic_constraint(fcondition)
    if (any(1L < gconstraint$start, !is.na(gconstraint$end))) {
      return(TRUE)
    }
  }
  if (rlang::is_quosure(fcondition)) {
    fcondition <- strip_parentheses(fcondition, recursive = FALSE)
    if (!rlang::is_quosure(fcondition)) {
      return(has_position_condition(fcondition))
    }
    # grepl / %like% on pos cannot be enumerated into a tabix range, so
    # they must fall through to awk as a non-genomic regex filter.
    if (is_regex_filter_condition(fcondition)) {
      return(FALSE)
    }
    return(any(sapply(fcondition[-1], has_position_condition) |> unlist()))
  }
  if (length(fcondition) == 0) {
    return(FALSE)
  }
  fcondition == as.symbol("pos")
}

#' Check whether a filter condition selects by coordinate alone
#'
#' A pure genomic condition restricts records by coordinate and nothing else,
#' so its genomic constraint is the whole of what it narrows.
#'
#' @param fcondition A filter_condition object
#' @return Logical indicating whether the condition selects by coordinate alone
#' @keywords internal
is_pure_genomic_condition <- function(
  fcondition
) {
  if (length(fcondition) == 0) {
    return(TRUE)
  }
  if (as.character(fcondition[[1]]) %in%
    c("and_filter_condition", "or_filter_condition")) {
    return(all(sapply(fcondition[2:3], is_pure_genomic_condition)))
  }
  if (as.character(fcondition[[1]]) == "in_filter_condition") {
    return(is_genomic_symbol(fcondition[[2]]))
  }
  if (as.character(fcondition[[1]]) %in% c(
    "lt_filter_condition", "lte_filter_condition",
    "gt_filter_condition", "gte_filter_condition",
    "eq_filter_condition", "neq_filter_condition"
  )) {
    return(
      is_genomic_symbol(fcondition[[2]]) | is_genomic_symbol(fcondition[[3]])
    )
  }
  if (fcondition[[1]] == as.symbol("lp_filter_condition")) {
    return(is_pure_genomic_condition(fcondition[[2]]))
  }
  is_genomic_symbol(fcondition)
}

#' Check if a symbol is a genomic column reference
#'
#' @param symbol A symbol to check
#' @return Logical indicating whether the symbol is chr or pos
#' @keywords internal
is_genomic_symbol <- function(
  symbol
) {
  rlang::get_expr(symbol) == as.symbol("chr") |
    rlang::get_expr(symbol) == as.symbol("pos")
}

#' Split genomic conditions from a filter condition
#'
#' Separates genomic (chr, pos) conditions into the genomic constraint
#' while keeping non-genomic conditions in the filter expression.
#'
#' @param fcondition A filter_condition object
#' @param build Optional genome build
#' @return Modified filter_condition with genomic conditions extracted
#' @keywords internal
split_genomic_conditions <- function(
  fcondition,
  build = NULL
) {
  build <- build %||% build(fcondition)
  if (length(fcondition) == 0) {
    return(fcondition)
  }
  fcondition <- strip_parentheses(fcondition)
  if (!has_genomic_condition(fcondition)) {
    genomic_constraint(fcondition) <- NULL
    return(fcondition)
  }
  if (is_pure_genomic_condition(fcondition)) {
    return(
      pure_genomic_condition(
        genomic_constraint = as_region_set(fcondition, build = build),
        build = build,
        context = attr(fcondition, "context")
      )
    )
  }
  stop("This should not be occurring")
}

#' @export
length.filter_condition <- function(x) {
  if (!inherits(x, "quosure")) {
    return(0)
  }
  rlang::get_expr(x) |>
    length()
}

#' Remove parenthesis wrappers from a filter condition
#'
#' Unwraps lp_filter_condition (parenthesis) nodes from a filter condition.
#' This always removes all parentheses at the highest level.
#' If recursive = TRUE, parentheses within the filter_condition are removed as
#' well.
#'
#' @param fcondition A filter_condition object
#' @param recursive Logical, whether to recursively strip from nested conditions
#' @return Filter condition with parentheses removed
#' @keywords internal
strip_parentheses <- function(
  fcondition,
  recursive = TRUE
) {
  if (length(fcondition) <= 1) {
    return(fcondition)
  }
  if (fcondition[[1]] == as.symbol("lp_filter_condition")) {
    child <- fcondition[[2]]
    genomic_constraint(child) <- genomic_constraint(fcondition)
    return(strip_parentheses(child, recursive = recursive))
  }
  if (recursive) {
    if (fcondition[[1]] == as.symbol("and_filter_condition")) {
      stripped <- strip_parentheses(fcondition[[2]], recursive = recursive) &
        strip_parentheses(fcondition[[3]], recursive = recursive)
      genomic_constraint(stripped) <- genomic_constraint(fcondition)
      return(stripped)
    }
    if (fcondition[[1]] == as.symbol("or_filter_condition")) {
      stripped <- strip_parentheses(fcondition[[2]], recursive = recursive) |
        strip_parentheses(fcondition[[3]], recursive = recursive)
      genomic_constraint(stripped) <- genomic_constraint(fcondition)
      return(stripped)
    }
  }
  fcondition
}

#' Check whether a filter condition has a single genomic scope
#'
#' One region set governs every record the condition can select, so its
#' source regions apply to the condition as a whole rather than separately
#' to each condition it combines.
#'
#' @param fcondition A filter_condition object
#' @return Logical indicating whether one region set governs every record
#' @keywords internal
has_single_genomic_scope <- function(
  fcondition
) {
  if (!rlang::is_quosure(fcondition)) {
    return(TRUE)
  }
  if (is_and_block(fcondition)) {
    return(TRUE)
  }
  if (is_pure_genomic_condition(fcondition)) {
    return(TRUE)
  }
  if (!has_chromosome_condition(fcondition) &&
    !has_position_condition(fcondition)) {
    return(TRUE)
  }
  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(
      has_single_genomic_scope(fcondition[[2]]) &&
        has_single_genomic_scope(fcondition[[3]]) &&
        eq_constraints(
          combined_constraint(fcondition[[2]]),
          combined_constraint(fcondition[[3]])
        )
    )
  }
  FALSE
}

#' Evaluate non-column symbols in filter condition
#'
#' For comparison operators, evaluates any operand that is not a column
#' reference. Column references are kept as symbols for later evaluation
#' in the awk column environment.
#'
#' @param fcondition A filter_condition call
#' @param finterface File interface for column detection
#'
#' @return filter_condition with non-column values evaluated
#' @keywords internal
evaluate_non_column_variables <- function(
  fcondition,
  finterface
) {
  # Evaluate all arguments except the function name (first element)
  fcondition[-1] <- lapply(
    fcondition[-1],
    \(fx) {
      if (is_column_symbol(fx, finterface)) {
        return(fx)
      }
      eval(fx, rlang::get_env(fcondition))
    }
  )
  fcondition
}
