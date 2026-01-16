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

is_genomic_positions <- function(
  x
) {
  inherits(x, "genomic_positions")
}
