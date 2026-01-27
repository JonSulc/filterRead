#' @export
`&.filter_condition` <- function(
  fcondition1,
  fcondition2
) {
  if (length(fcondition1) == 0) {
    genomic_regions(fcondition2) <-
      genomic_regions(fcondition1) & genomic_regions(fcondition2)
    return(fcondition2)
  }
  if (length(fcondition2) == 0) {
    genomic_regions(fcondition1) <-
      genomic_regions(fcondition1) & genomic_regions(fcondition2)
    return(fcondition1)
  }

  stopifnot(identical(
    attr(fcondition1, "finterface_env"),
    attr(fcondition2, "finterface_env")
  ))

  if (fcondition1[[1]] == as.symbol("or_filter_condition")) {
    return(fcondition1[[2]] & fcondition2 | fcondition1[[3]] & fcondition2)
  }
  if (fcondition2[[1]] == as.symbol("or_filter_condition")) {
    return(fcondition1 & fcondition2[[2]] | fcondition1 & fcondition2[[3]])
  }

  fcondition <- rlang::quo(and_filter_condition()) |>
    as_filter_condition()
  fcondition[2:3] <- list(fcondition1, fcondition2)
  attr(fcondition, "finterface_env") <- attr(fcondition1, "finterface_env")
  genomic_regions(fcondition) <- genomic_regions(fcondition1) & genomic_regions(fcondition2)

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
    attr(fcondition1, "finterface_env"),
    attr(fcondition2, "finterface_env")
  ))

  if (length(fcondition1) == 0 && length(fcondition2) == 0) {
    genomic_regions(fcondition1) <- genomic_regions(fcondition1) |
      genomic_regions(fcondition2)
    return(fcondition1)
  }

  fcondition <- rlang::quo(or_filter_condition()) |>
    as_filter_condition()
  fcondition[2:3] <- list(fcondition1, fcondition2)
  attr(fcondition, "finterface_env") <- attr(fcondition1, "finterface_env")
  if (is_single_genomic_block(fcondition)) {
    if (length(fcondition1) == 0) {
      fcondition <- fcondition2
    } else if (length(fcondition2) == 0) {
      fcondition <- fcondition1
    }
    genomic_regions(fcondition) <- rbind(
      genomic_regions(fcondition1),
      genomic_regions(fcondition2)
    )
  } else {
    genomic_regions(fcondition) <- full_genomic_regions(
      build = build(fcondition1)
    )
  }

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
