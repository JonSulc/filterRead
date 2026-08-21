# The canonical form of a genomic_regions
#
# Every object is reduced to this form on construction: rows sorted and
# pairwise disjoint. Row count and row shape are therefore facts about the set
# and how it is stored, which is what lets `==` compare by rows, `compact()`
# choose by row count, and the tabix builder emit one token per row.

#' Reduce a genomic_regions to its canonical form
#'
#' @param gregions A genomic_regions object
#' @return A genomic_regions covering the same positions, in canonical form
#' @keywords internal
normalize_genomic_regions <- function(
  gregions
) {
  merge_contiguous_regions(gregions)
}

#' Merge overlapping and contiguous genomic regions
#'
#' Combines regions that overlap or are adjacent (end + 1 == start) into
#' single regions. A region leaving a position unconstrained reaches the
#' start or the end of its chromosome, so it merges with whatever it meets
#' there.
#'
#' @param gregions A genomic_regions object
#' @return genomic_regions with overlapping regions merged
#' @keywords internal
merge_contiguous_regions <- function(
  gregions
) {
  if (nrow(gregions) == 0) {
    return(gregions)
  }
  replace_na_with_sentinel(gregions) |>
    # The merging below requires sorted rows
    data.table::setkey(chr, start, end) |>
    _[
      ,
      c(
        .SD,
        # Where the largest end so far + 1 is less than the start, the rows
        # are disjoint and a new group begins; otherwise the row meets or
        # overlaps that group and joins it, to be merged into one region
        .(group = cumsum(
          cummax(data.table::shift(end + 1L, fill = 0L)) < start
        ))
      ),
      by = chr
    ][
      ,
      .(
        chr = chr,
        start = min(start),
        end = max(end)
      ),
      by = .(chr, group)
    ][
      ,
      .SD,
      .SDcols = -"group"
    ] |>
    replace_sentinel_with_na() |>
    as_genomic_regions(
      build = build(gregions),
      complement = is_complement(gregions),
      normalize_regions = FALSE
    )
}
