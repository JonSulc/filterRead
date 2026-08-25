#' @export
`&.filter_condition` <- function(
  fcondition1,
  fcondition2
) {
  if (length(fcondition1) == 0) {
    genomic_constraint(fcondition2) <-
      and_constraints(
        genomic_constraint(fcondition1), genomic_constraint(fcondition2)
      )
    return(fcondition2)
  }
  if (length(fcondition2) == 0) {
    genomic_constraint(fcondition1) <-
      and_constraints(
        genomic_constraint(fcondition1), genomic_constraint(fcondition2)
      )
    return(fcondition1)
  }

  stopifnot(identical(
    attr(fcondition1, "context")$finterface,
    attr(fcondition2, "context")$finterface
  ))

  if (fcondition1[[1]] == as.symbol("or_filter_condition")) {
    return(fcondition1[[2]] & fcondition2 | fcondition1[[3]] & fcondition2)
  }
  if (fcondition2[[1]] == as.symbol("or_filter_condition")) {
    return(fcondition1 & fcondition2[[2]] | fcondition1 & fcondition2[[3]])
  }

  fcondition <- rlang::quo(and_filter_condition()) |>
    as_filter_condition()
  combined_regions <- and_constraints(
    genomic_constraint(fcondition1), genomic_constraint(fcondition2)
  )
  genomic_constraint(fcondition1) <- NULL
  genomic_constraint(fcondition2) <- NULL
  fcondition[2:3] <- list(fcondition1, fcondition2)
  attr(fcondition, "context") <- attr(fcondition1, "context")
  genomic_constraint(fcondition) <- combined_regions

  if (!identical(build(fcondition1), build(fcondition2))) {
    warning(
      "filter_conditions being combine do not have the same build, ",
      " using ", build(fcondition1)
    )
  }


  build(fcondition) <- build(fcondition1) %||% build(fcondition2)
  class(fcondition) <- c("and_filter_condition", class(fcondition)) |>
    unique()

  fcondition
}

#' @export
`|.filter_condition` <- function(
  fcondition1,
  fcondition2
) {
  stopifnot(identical(
    attr(fcondition1, "context")$finterface,
    attr(fcondition2, "context")$finterface
  ))

  if (length(fcondition1) == 0 && length(fcondition2) == 0) {
    genomic_constraint(fcondition1) <- or_constraints(
      genomic_constraint(fcondition1), genomic_constraint(fcondition2)
    )
    return(fcondition1)
  }

  fcondition <- rlang::quo(or_filter_condition()) |>
    as_filter_condition()
  fcondition[2:3] <- list(fcondition1, fcondition2)
  attr(fcondition, "context") <- attr(fcondition1, "context")
  genomic_constraint(fcondition) <- NULL

  if (!identical(build(fcondition1), build(fcondition2))) {
    warning(
      "filter_conditions being combine do not have the same build, ",
      " using ", build(fcondition1)
    )
  }

  build(fcondition) <- build(fcondition1) %||% build(fcondition2)
  class(fcondition) <- c("or_filter_condition", class(fcondition)) |>
    unique()

  fcondition
}
