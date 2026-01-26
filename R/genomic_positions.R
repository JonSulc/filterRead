#' Create a new genomic_positions object
#'
#' A genomic_positions object represents single-base positions (start == end).
#'
#' @param chr Character vector of chromosome names
#' @param pos Integer vector of positions
#' @param ... Additional arguments passed to new_genomic_regions
#' @return A genomic_positions object
#' @keywords internal
new_genomic_positions <- function(
  chr = character(),
  pos = integer(),
  ...
) {
  new_genomic_regions(
    chr = chr,
    start = pos,
    end = pos,
    ...
  ) |>
    as_genomic_positions()
}

#' Coerce to genomic_positions
#'
#' @param genomic_positions_dt A data.table or genomic_regions object
#' @param ... Additional arguments passed to as_genomic_regions
#' @return A genomic_positions object
#' @keywords internal
as_genomic_positions <- function(
  genomic_positions_dt,
  ...
) {
  genomic_positions <- as_genomic_regions(
    genomic_positions_dt,
    ...
  )
  data.table::setattr(
    genomic_positions,
    "class",
    c("genomic_positions", class(genomic_positions)) |>
      unique()
  )
  genomic_positions
}

#' Test if an object is a genomic_positions
#'
#' @param x Object to test
#' @return Logical indicating whether x inherits from genomic_positions
#' @keywords internal
is_genomic_positions <- function(
  x
) {
  inherits(x, "genomic_positions")
}
