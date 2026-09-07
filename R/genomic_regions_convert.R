#' Convert objects to genomic_regions
#'
#' Generic function to convert various objects (data.frames, comparisons on
#' `chr` or `pos`) to genomic_regions format.
#'
#' @seealso [source_regions()] for the regions a filter_condition can match
#'   within.
#'
#' @param x Object to convert
#' @param build Reference build the coordinates are expressed in ("b36",
#'   "b37", "b38", or a synonym).
#' @param complement Whether the rows describe the coordinates the set
#'   excludes (TRUE) rather than those it covers (FALSE). NULL inherits the
#'   input's `complement` attribute, defaulting to FALSE.
#' @param normalize_regions If TRUE, reduce the regions to their canonical
#'   form: sorted and pairwise disjoint.
#' @param ... Additional arguments passed to methods
#' @return A genomic_regions object
#' @export
as_genomic_regions <- function(
  x,
  build = NULL,
  ...
) {
  UseMethod("as_genomic_regions")
}

#' @export
#' @rdname as_genomic_regions
as_genomic_regions.data.frame <- function(
  x,
  build = NULL,
  complement = NULL,
  normalize_regions = TRUE,
  ...
) {
  missing_cols <- setdiff(c("chr", "start", "end"), names(x))
  if (length(missing_cols) != 0L) {
    stop(
      "`as_genomic_regions()` requires columns `chr`, `start`, and `end`; ",
      "missing: ", paste(missing_cols, collapse = ", "), ".\n",
      "To build regions from vectors, use `genomic_regions()`."
    )
  }
  build <- build %||% build(x)
  complement <- complement %||% is_complement(x) %||% FALSE
  gregions <- data.table::as.data.table(x) |>
    data.table::copy()
  validate_genomic_regions_dt(gregions)
  for (col in intersect(c("start", "end"), names(gregions))) {
    if (!is.integer(gregions[[col]])) {
      data.table::set(gregions, j = col, value = as.integer(gregions[[col]]))
    }
  }
  if (!is.null(build)) {
    set_build(gregions, build)
  }
  set_complement(gregions, complement)
  data.table::setattr(
    gregions,
    "class",
    c("genomic_regions", class(gregions)) |>
      unique()
  )
  data.table::setkey(gregions, chr, start, end)

  if (!normalize_regions) return(gregions)

  normalize_genomic_regions(gregions)
}
#' @export
as_genomic_regions.eq_filter_condition <- function(
  x,
  build = NULL,
  ...
) {
  build <- build %||% build(x)
  if (length(x) == 0) {
    NextMethod()
  }
  if (x[[2]] == as.symbol("chr")) {
    return(
      new_genomic_regions(chr = x[[3]], build = build)
    )
  }
  if (x[[3]] == as.symbol("chr")) {
    return(
      new_genomic_regions(chr = x[[2]], build = build)
    )
  }
  if (x[[2]] == as.symbol("pos")) {
    return(new_genomic_regions(
      start = x[[3]],
      end = x[[3]],
      build = build
    ))
  }
  if (x[[3]] == as.symbol("pos")) {
    return(new_genomic_regions(
      start = x[[2]],
      end = x[[2]],
      build = build
    ))
  }
  full_genomic_regions(build = build)
}
#' @export
as_genomic_regions.neq_filter_condition <- function(
  ...
) {
  !as_genomic_regions.eq_filter_condition(...)
}
#' @export
as_genomic_regions.in_filter_condition <- function(
  x,
  build = NULL,
  ...
) {
  build <- build %||% build(x)
  if (length(x) == 0) {
    NextMethod()
  }
  # `%in%` always has the column on the left: in_filter_condition(col, set).
  # Each member becomes a region: a chromosome for `chr`, a single-position
  # interval for `pos`. Combined with the rest of the condition through the
  # usual genomic-regions set operations.
  if (x[[2]] == as.symbol("chr")) {
    return(new_genomic_regions(chr = x[[3]], build = build))
  }
  if (x[[2]] == as.symbol("pos")) {
    return(new_genomic_regions(
      start = x[[3]],
      end = x[[3]],
      build = build
    ))
  }
  full_genomic_regions(build = build)
}
#' @export
as_genomic_regions.lt_filter_condition <- function(
  x,
  build = NULL,
  ordered_chr = 1:22,
  ...
) {
  build <- build %||% build(x)
  if (length(x) == 0) {
    NextMethod()
  }
  if (x[[2]] == as.symbol("pos")) {
    return(
      new_genomic_regions(
        end = x[[3]] - 1,
        build = build
      )
    )
  }
  if (x[[3]] == as.symbol("pos")) {
    return(
      new_genomic_regions(
        start = x[[2]] + 1,
        build = build
      )
    )
  }
  if (x[[2]] == as.symbol("chr")) {
    # chr < x[[3]]
    if (x[[3]] %in% ordered_chr) {
      return(
        new_genomic_regions(
          chr = ordered_chr[
            ordered_chr < x[[3]]
          ],
          build = build
        )
      )
    }
  } else if (x[[3]] == as.symbol("chr")) {
    # x[[2]] < chr
    if (x[[2]] %in% ordered_chr) {
      return(
        new_genomic_regions(
          chr = ordered_chr[
            x[[2]] < ordered_chr
          ],
          build = build
        )
      )
    }
  }
  full_genomic_regions(build = build)
}
#' @export
as_genomic_regions.lte_filter_condition <- function(
  x,
  build = NULL,
  ordered_chr = 1:22,
  ...
) {
  build <- build %||% build(x)
  if (length(x) == 0) {
    NextMethod()
  }
  if (x[[2]] == as.symbol("pos")) {
    return(
      new_genomic_regions(
        end = x[[3]],
        build = build
      )
    )
  }
  if (x[[3]] == as.symbol("pos")) {
    return(
      new_genomic_regions(
        start = x[[2]],
        build = build
      )
    )
  }
  if (x[[2]] == as.symbol("chr")) {
    # chr <= x[[3]]
    if (x[[3]] %in% ordered_chr) {
      return(
        new_genomic_regions(
          chr = ordered_chr[
            ordered_chr <= x[[3]]
          ],
          build = build
        )
      )
    }
  } else if (x[[3]] == as.symbol("chr")) {
    # x[[2]] <= chr
    if (x[[2]] %in% ordered_chr) {
      return(
        new_genomic_regions(
          chr = ordered_chr[
            x[[2]] <= ordered_chr
          ],
          build = build
        )
      )
    }
  }
  full_genomic_regions(build = build)
}
#' @export
as_genomic_regions.gt_filter_condition <- function(
  x,
  build = NULL,
  ordered_chr = 1:22,
  ...
) {
  build <- build %||% build(x)
  if (length(x) == 0) {
    NextMethod()
  }
  if (x[[2]] == as.symbol("pos")) {
    return(
      new_genomic_regions(
        start = x[[3]] + 1,
        build = build
      )
    )
  }
  if (x[[3]] == as.symbol("pos")) {
    return(
      new_genomic_regions(
        end = x[[2]] - 1,
        build = build
      )
    )
  }
  if (x[[2]] == as.symbol("chr")) {
    # chr > x[[3]]
    if (x[[3]] %in% ordered_chr) {
      return(
        new_genomic_regions(
          chr = ordered_chr[
            ordered_chr > x[[3]]
          ],
          build = build
        )
      )
    }
  } else if (x[[3]] == as.symbol("chr")) {
    # x[[2]] > chr
    if (x[[2]] %in% ordered_chr) {
      return(
        new_genomic_regions(
          chr = ordered_chr[
            x[[2]] > ordered_chr
          ],
          build = build
        )
      )
    }
  }
  full_genomic_regions(build = build)
}
#' @export
as_genomic_regions.gte_filter_condition <- function(
  x,
  build = NULL,
  ordered_chr = 1:22,
  ...
) {
  build <- build %||% build(x)
  if (length(x) == 0) {
    NextMethod()
  }
  if (x[[2]] == as.symbol("pos")) {
    return(
      new_genomic_regions(
        start = x[[3]],
        build = build
      )
    )
  }
  if (x[[3]] == as.symbol("pos")) {
    return(
      new_genomic_regions(
        end = x[[2]],
        build = build
      )
    )
  }
  if (x[[2]] == as.symbol("chr")) {
    # chr >= x[[3]]
    if (x[[3]] %in% ordered_chr) {
      return(
        new_genomic_regions(
          chr = ordered_chr[
            ordered_chr >= x[[3]]
          ],
          build = build
        )
      )
    }
  } else if (x[[3]] == as.symbol("chr")) {
    # x[[2]] >= chr
    if (x[[2]] %in% ordered_chr) {
      return(
        new_genomic_regions(
          chr = ordered_chr[
            x[[2]] >= ordered_chr
          ],
          build = build
        )
      )
    }
  }
  full_genomic_regions(build = build)
}
#' Regions a filter condition can match within
#'
#' The smallest set of regions covering every row the condition could select
#' when its non-genomic constraints are ignored. Combines the condition's own
#' genomic constraint with those of every condition it combines.
#'
#' @param fcondition A filter_condition object
#' @return A genomic_regions object, in the build the condition carries
#' @export
source_regions <- function(
  fcondition
) {
  stopifnot(is.filter_condition(fcondition))
  combined_constraint(fcondition) %||%
    full_genomic_regions(build = build(fcondition))
}

#' Combine the genomic constraints of a condition and everything it combines
#'
#' @param fcondition A filter_condition object
#' @return A genomic_regions object, or NULL when nothing is constrained
#' @keywords internal
combined_constraint <- function(
  fcondition
) {
  if (length(fcondition) <= 1) {
    return(genomic_constraint(fcondition))
  }
  if (fcondition[[1]] == as.symbol("lp_filter_condition")) {
    return(and_constraints(
      genomic_constraint(fcondition),
      combined_constraint(fcondition[[2]])
    ))
  }
  if (fcondition[[1]] == as.symbol("and_filter_condition")) {
    return(and_constraints(
      genomic_constraint(fcondition),
      and_constraints(
        combined_constraint(fcondition[[2]]),
        combined_constraint(fcondition[[3]])
      )
    ))
  }
  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(and_constraints(
      genomic_constraint(fcondition),
      or_constraints(
        combined_constraint(fcondition[[2]]),
        combined_constraint(fcondition[[3]])
      )
    ))
  }
  genomic_constraint(fcondition)
}
