is_and_block <- function(
  fcondition
) {
  if (!inherits(fcondition, "call")) {
    return(TRUE)
  }

  if (!is.null(genomic_regions(fcondition))) {
    if (1 < nrow(genomic_regions(fcondition))) {
      return(FALSE)
    }
  }

  if (length(fcondition) == 0) {
    return(TRUE)
  }

  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(FALSE)
  }

  all(sapply(fcondition[-1], is_and_block))
}

non_genomic_is_and_block <- function(
  fcondition
) {
  if (!inherits(fcondition, "call")) {
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

genomic_regions <- function(
  fcondition,
  recursive = FALSE
) {
  if (!recursive || !is_composite_filter_condition(fcondition)) {
    return(attr(fcondition, "genomic_regions"))
  }
  if (fcondition[[1]] == as.symbol("lp_filter_condition")) {
    # fcondition is (
    return(
      attr(fcondition, "genomic_regions") &
        genomic_regions(fcondition[[2]], recursive = TRUE)
    )
  } else if (fcondition[[1]] == as.symbol("and_filter_condition")) {
    # fcondition is & or |
    return(
      attr(fcondition, "genomic_regions") &
        genomic_regions(fcondition[[2]], recursive = TRUE) &
        genomic_regions(fcondition[[3]], recursive = TRUE)
    )
  } else if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(
      attr(fcondition, "genomic_regions") &
        (genomic_regions(fcondition[[2]], recursive = TRUE) |
          genomic_regions(fcondition[[3]], recursive = TRUE))
    )
  }
  stop(
    "Unrecognized composite filter_condition: ",
    as.character(fcondition[[1]])
  )
}
has_no_gregions <- function(
  fcondition
) {
  gregions <- genomic_regions(fcondition, recursive = TRUE)
  is.null(gregions) || is_full_genome(gregions)
}

`genomic_regions<-` <- function(
  fcondition,
  value
) {
  attr(fcondition, "genomic_regions") <- value
  fcondition
}

has_genomic_condition <- function(
  fcondition
) {
  has_chromosome_condition(fcondition) | has_position_condition(fcondition)
}

has_chromosome_condition <- function(
  fcondition
) {
  if (!is.null(genomic_regions(fcondition))) {
    if (any(!is.na(genomic_regions(fcondition)$chr))) {
      return(TRUE)
    }
  }
  if (is.call(fcondition)) {
    while (fcondition[[1]] == as.symbol("lp_filter_condition")) {
      fcondition <- fcondition[[2]]
    }
    return(any(sapply(fcondition[-1], has_chromosome_condition) |> unlist()))
  }
  if (length(fcondition) == 0) {
    return(FALSE)
  }
  fcondition == as.symbol("chr")
}

has_position_condition <- function(
  fcondition
) {
  if (!is.null(genomic_regions(fcondition))) {
    if (any(genomic_regions(fcondition)[, c(!is.na(start), !is.na(end))])) {
      return(TRUE)
    }
  }
  if (is.call(fcondition)) {
    while (fcondition[[1]] == as.symbol("lp_filter_condition")) {
      fcondition <- fcondition[[2]]
    }
    return(any(sapply(fcondition[-1], has_position_condition) |> unlist()))
  }
  if (length(fcondition) == 0) {
    return(FALSE)
  }
  fcondition == as.symbol("pos")
}

has_non_genomic_condition <- function(
  fcondition
) {
  if (length(fcondition) == 0) {
    return(FALSE)
  }
  if (as.character(fcondition[[1]]) %in% c("and_filter_condition", "or_filter_condition")) {
    return(any(sapply(fcondition[2:3], has_non_genomic_condition)))
  }
  if (as.character(fcondition[[1]]) == "in_filter_condition") {
    return(!is_genomic_symbol(fcondition[[2]]))
  }
  if (as.character(fcondition[[1]]) %in% c(
    "lt_filter_condition", "lte_filter_condition",
    "gt_filter_condition", "gte_filter_condition",
    "eq_filter_condition"
  )) {
    return(!is_genomic_symbol(fcondition[[2]]) & !is_genomic_symbol(fcondition[[3]]))
  }
  if (fcondition[[1]] == as.symbol("lp_filter_condition")) {
    return(has_non_genomic_condition(fcondition[[2]]))
  }
  !is_genomic_symbol(fcondition)
}
is_genomic_symbol <- function(
  symbol
) {
  symbol == as.symbol("chr") | symbol == as.symbol("pos")
}

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
    genomic_regions(fcondition) <- full_genomic_regions(
      build = build
    )
    return(fcondition)
  }
  if (!has_non_genomic_condition(fcondition)) {
    return(
      empty_filter_condition(
        build = build,
        genomic_regions = as_genomic_regions(fcondition, build = build),
        finterface = attr(fcondition, "finterface_env")
      )
    )
  }
  stop("This should not be occurring")
  # Contains mix of genomic and non-genomic conditions
  if (fcondition[[1]] == as.symbol("and_filter_condition")) {
    return(
      split_genomic_conditions(fcondition[[2]], build = build) &
        split_genomic_conditions(fcondition[[3]], build = build)
    )
  }
  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(
      split_genomic_conditions(fcondition[[2]], build = build) |
        split_genomic_conditions(fcondition[[3]], build = build)
    )
  }
  fcondition
}

get_chr_condition <- function(
  fcondition
) {
  stopifnot(is_and_block(fcondition))
  fcondition <- strip_parentheses(fcondition)
  if (fcondition[[1]] == as.symbol("and_filter_condition")) {
    return(
      get_chr_condition(fcondition[[2]]) & get_chr_condition(fcondition[[3]])
    )
  }
  if (fcondition[[2]] == as.symbol("chr") | fcondition[[3]] == as.symbol("chr")) {
    return(fcondition)
  }
  NULL
}
get_pos_condition <- function(
  fcondition
) {
  stopifnot(is_and_block(fcondition))
  fcondition <- strip_parentheses(fcondition)
  if (fcondition[[1]] == as.symbol("and_filter_condition")) {
    return(
      get_pos_condition(fcondition[[2]]) & get_pos_condition(fcondition[[3]])
    )
  }
  if (fcondition[[2]] == as.symbol("pos") | fcondition[[3]] == as.symbol("pos")) {
    return(fcondition)
  }
  NULL
}
# Operator mappings for position condition conversion
# When pos is on left (pos OP val): list(field, adjustment)
# When pos is on right (val OP pos): the comparison is effectively flipped
POS_OP_LEFT_MAP <- list(
  lt_filter_condition = list(field = "end", adj = -1L),
  lte_filter_condition = list(field = "end", adj = 0L),
  gt_filter_condition = list(field = "start", adj = 1L),
  gte_filter_condition = list(field = "start", adj = 0L)
)
POS_OP_RIGHT_MAP <- list(
  lt_filter_condition = list(field = "start", adj = 1L),
  lte_filter_condition = list(field = "start", adj = 0L),
  gt_filter_condition = list(field = "end", adj = -1L),
  gte_filter_condition = list(field = "end", adj = 0L)
)

# Helper to create genomic region data.table from field name and value
make_pos_region <- function(field, value) {
  if (field == "start") {
    data.table::data.table(start = value)
  } else {
    data.table::data.table(end = value)
  }
}

pos_condition_to_genomic_region <- function(
  fcondition
) {
  fcondition <- strip_parentheses(fcondition)
  op_name <- as.character(fcondition[[1]])

  # Handle pos on left side (pos OP val)
  if (fcondition[[2]] == as.symbol("pos")) {
    val <- fcondition[[3]]
    if (op_name %in% names(POS_OP_LEFT_MAP)) {
      m <- POS_OP_LEFT_MAP[[op_name]]
      return(make_pos_region(m$field, val + m$adj))
    }
    if (op_name %in% c("eq_filter_condition", "in_filter_condition")) {
      return(data.table::data.table(start = val, end = val))
    }
  }

  # Handle pos on right side (val OP pos)
  if (fcondition[[3]] == as.symbol("pos")) {
    val <- fcondition[[2]]
    if (op_name %in% names(POS_OP_RIGHT_MAP)) {
      m <- POS_OP_RIGHT_MAP[[op_name]]
      return(make_pos_region(m$field, val + m$adj))
    }
    if (op_name %in% c("eq_filter_condition", "in_filter_condition")) {
      return(data.table::data.table(start = val, end = val))
    }
  }

  # Handle composite conditions
  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(
      lapply(fcondition[-1], pos_condition_to_genomic_region) |>
        data.table::rbindlist(fill = TRUE, use.names = TRUE)
    )
  }
  if (fcondition[[1]] == as.symbol("and_filter_condition")) {
    to_return <- lapply(fcondition[-1], pos_condition_to_genomic_region) |>
      data.table::rbindlist(fill = TRUE, use.names = TRUE)
    return(
      to_return[
        ,
        .(
          start = {
            if (any(!is.na(to_return$start))) {
              max(start, na.rm = TRUE)
            } else {
              NA_real_
            }
          },
          end = {
            if (any(!is.na(to_return$end))) {
              min(end, na.rm = TRUE)
            } else {
              NA_real_
            }
          }
        )
      ]
    )
  }
  NULL
}

strip_parentheses <- function(
  fcondition,
  recursive = TRUE
) {
  if (length(fcondition) == 0) {
    return(fcondition)
  }
  if (fcondition[[1]] == as.symbol("lp_filter_condition")) {
    return(strip_parentheses(fcondition[[2]], recursive = recursive))
  }
  if (recursive) {
    if (fcondition[[1]] == as.symbol("and_filter_condition")) {
      return(
        strip_parentheses(fcondition[[2]], recursive = recursive) &
          strip_parentheses(fcondition[[3]], recursive = recursive)
      )
    }
    if (fcondition[[1]] == as.symbol("or_filter_condition")) {
      return(
        strip_parentheses(fcondition[[2]], recursive = recursive) |
          strip_parentheses(fcondition[[3]], recursive = recursive)
      )
    }
  }
  fcondition
}

is_single_genomic_block <- function(
  fcondition
) {
  # Determine whether all non-genomic conditions apply to the same genomic range
  if (!is.call(fcondition)) {
    return(TRUE)
  }
  if (is_and_block(fcondition)) {
    return(TRUE)
  }
  if (!has_non_genomic_condition(fcondition)) {
    return(TRUE)
  }
  if (!has_chromosome_condition(fcondition) &&
    !has_position_condition(fcondition)) {
    return(TRUE)
  }
  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(
      is_single_genomic_block(fcondition[[2]]) &&
        is_single_genomic_block(fcondition[[3]]) &&
        identical(
          genomic_regions(fcondition[[2]]),
          genomic_regions(fcondition[[3]])
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
      if (is_column_symbol(x, finterface)) {
        return(x)
      }
      eval(x, env)
    }
  )
  fcondition
}
