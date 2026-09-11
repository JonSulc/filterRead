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
  merge_contiguous_regions(gregions) |>
    subtract_unconstrained_chr_coverage()
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

#' Remove from the named rows what the unconstrained-chromosome rows cover
#'
#' A row whose `chr` is NA covers every chromosome, so a named row keeps only
#' the positions that row leaves. Trimming can split a named row, so the
#' result may carry more rows than the input.
#'
#' The complement bit does not enter into it. Reducing the rows as a set is
#' right for a set stored as its complement too, since De Morgan makes the
#' complement of a union the intersection of the complements.
#'
#' @param gregions A genomic_regions object with merged rows
#' @return A genomic_regions with pairwise disjoint rows
#' @keywords internal
subtract_unconstrained_chr_coverage <- function(
  gregions
) {
  if (!anyNA(gregions$chr)) {
    return(gregions)
  }
  rows <- data.table::data.table(
    chr = gregions$chr,
    start = gregions$start,
    end = gregions$end
  )
  unconstrained <- rows[is.na(chr)]
  list(
    unconstrained,
    subtract_regions_dt(rows[!is.na(chr)], unconstrained)
  ) |>
    data.table::rbindlist() |>
    as_genomic_regions(
      build = build(gregions),
      complement = is_complement(gregions),
      normalize_regions = FALSE
    )
}

#' Subtract one set of region rows from another
#'
#' Removes the coverage of `to_remove` from the rows of `regions_dt`. A row
#' is kept when `to_remove` does not reach it, dropped when `to_remove`
#' covers it, trimmed when the two overlap at one end, and split into two
#' when an interval of `to_remove` lands inside it. A row of `to_remove`
#' whose `chr` is NA applies to every row.
#'
#' Both arguments are plain region tables of `chr`, `start` and `end`, read
#' as the positions they cover — there is no complement bit — and the result
#' is one too, with its rows in no particular order.
#'
#' @param regions_dt,to_remove data.tables of region rows
#' @return The rows of `regions_dt` that `to_remove` does not cover
#' @keywords internal
subtract_regions_dt <- function(
  regions_dt,
  to_remove
) {
  if (nrow(regions_dt) == 0L || nrow(to_remove) == 0L) {
    return(regions_dt[, .(chr, start, end)])
  }
  rows <- data.table::data.table(
    chr = regions_dt$chr,
    start = data.table::fcoalesce(regions_dt$start, START_SENTINEL),
    end = data.table::fcoalesce(regions_dt$end, END_SENTINEL)
  )
  for (index in seq_len(nrow(to_remove))) {
    rows <- subtract_interval_coverage(rows, to_remove[index])
  }
  rows[
    end == END_SENTINEL,
    end := NA_integer_
  ][]
}

#' Remove one interval's coverage from a set of region rows
#'
#' Every row the interval reaches contributes the part of itself before the
#' interval and the part after, and the empty ones are dropped. Rows the
#' interval does not reach pass through untouched, so carving the intervals
#' one at a time is correct even when they overlap each other.
#'
#' @param rows Region rows carrying sentinel values in place of NA
#' @param interval A one-row data.table of `chr`, `start` and `end`
#' @return The rows with the interval's coverage removed
#' @keywords internal
subtract_interval_coverage <- function(
  rows,
  interval
) {
  cut_start <- data.table::fcoalesce(interval$start, START_SENTINEL)
  cut_end <- data.table::fcoalesce(interval$end, END_SENTINEL)
  reaches <- cut_start <= rows$end & rows$start <= cut_end
  if (is.na(interval$chr)) {
    carved <- reaches
  } else {
    if (any(reaches & is.na(rows$chr))) {
      stop(
        "Cannot subtract a named chromosome from a region covering every ",
        "chromosome without enumerating the chromosomes."
      )
    }
    carved <- reaches & rows$chr == interval$chr
  }
  touched <- rows[carved]
  list(
    rows[!carved],
    touched[, .(chr, start, end = pmin(end, cut_start - 1L))][start <= end],
    touched[, .(chr, start = pmax(start, cut_end + 1L), end)][start <= end]
  ) |>
    data.table::rbindlist()
}
