#' Convert objects to region_set
#'
#' Generic function to convert various objects (data.frames, comparisons on
#' `chr` or `pos`) to region_set format.
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
#' @param canonicalize If TRUE, reduce the regions to their canonical
#'   form: sorted and pairwise disjoint.
#' @param ... Additional arguments passed to methods
#' @return A region_set object
#' @export
as_region_set <- function(
  x,
  build = NULL,
  ...
) {
  UseMethod("as_region_set")
}

#' @export
#' @rdname as_region_set
as_region_set.data.frame <- function(
  x,
  build = NULL,
  complement = NULL,
  canonicalize = TRUE,
  ...
) {
  build <- build %||% build(x)
  complement <- complement %||% is_complement(x) %||% FALSE
  regions <- data.table::as.data.table(x) |>
    data.table::copy()
  if ("chr" %in% names(regions) && !is.character(regions$chr)) {
    data.table::set(
      regions,
      j = "chr",
      value = as.character(regions$chr)
    )
  }
  for (col in intersect(c("start", "end"), names(regions))) {
    value <- regions[[col]]
    if (!is.integer(value)) {
      as_integer <- suppressWarnings(as.integer(value))
      # An NA introduced by conversion should not read as an open end,
      # so leave a column the conversion cannot read for the validator to name
      if (!any(!is.na(value) & is.na(as_integer))) {
        data.table::set(regions, j = col, value = as_integer)
      }
    }
  }
  if (!is.null(build)) {
    set_build(regions, build)
  }
  set_complement(regions, complement)
  data.table::setattr(
    regions,
    "class",
    c("region_set", class(regions)) |>
      unique()
  )
  validate_region_set(regions)
  data.table::setkey(regions, chr, start, end)

  if (!canonicalize) return(regions)

  canonicalize_region_set(regions)
}
#' @export
as_region_set.eq_filter_condition <- function(
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
      new_region_set(chr = x[[3]], build = build)
    )
  }
  if (x[[3]] == as.symbol("chr")) {
    return(
      new_region_set(chr = x[[2]], build = build)
    )
  }
  if (x[[2]] == as.symbol("pos")) {
    return(new_region_set(
      start = x[[3]],
      end = x[[3]],
      build = build
    ))
  }
  if (x[[3]] == as.symbol("pos")) {
    return(new_region_set(
      start = x[[2]],
      end = x[[2]],
      build = build
    ))
  }
  full_region_set(build = build)
}
#' @export
as_region_set.neq_filter_condition <- function(
  ...
) {
  !as_region_set.eq_filter_condition(...)
}
#' @export
as_region_set.in_filter_condition <- function(
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
  # usual region set operations.
  if (x[[2]] == as.symbol("chr")) {
    return(new_region_set(chr = x[[3]], build = build))
  }
  if (x[[2]] == as.symbol("pos")) {
    return(new_region_set(
      start = x[[3]],
      end = x[[3]],
      build = build
    ))
  }
  full_region_set(build = build)
}
#' @export
as_region_set.lt_filter_condition <- function(
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
      general_rule_to(x[[3]] - 1, build = build)
    )
  }
  if (x[[3]] == as.symbol("pos")) {
    return(
      general_rule_from(x[[2]] + 1, build = build)
    )
  }
  if (x[[2]] == as.symbol("chr")) {
    # chr < x[[3]]
    if (x[[3]] %in% ordered_chr) {
      return(
        new_region_set(
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
        new_region_set(
          chr = ordered_chr[
            x[[2]] < ordered_chr
          ],
          build = build
        )
      )
    }
  }
  full_region_set(build = build)
}
#' @export
as_region_set.lte_filter_condition <- function(
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
      general_rule_to(x[[3]], build = build)
    )
  }
  if (x[[3]] == as.symbol("pos")) {
    return(
      general_rule_from(x[[2]], build = build)
    )
  }
  if (x[[2]] == as.symbol("chr")) {
    # chr <= x[[3]]
    if (x[[3]] %in% ordered_chr) {
      return(
        new_region_set(
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
        new_region_set(
          chr = ordered_chr[
            x[[2]] <= ordered_chr
          ],
          build = build
        )
      )
    }
  }
  full_region_set(build = build)
}
#' @export
as_region_set.gt_filter_condition <- function(
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
      general_rule_from(x[[3]] + 1, build = build)
    )
  }
  if (x[[3]] == as.symbol("pos")) {
    return(
      general_rule_to(x[[2]] - 1, build = build)
    )
  }
  if (x[[2]] == as.symbol("chr")) {
    # chr > x[[3]]
    if (x[[3]] %in% ordered_chr) {
      return(
        new_region_set(
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
        new_region_set(
          chr = ordered_chr[
            x[[2]] > ordered_chr
          ],
          build = build
        )
      )
    }
  }
  full_region_set(build = build)
}
#' @export
as_region_set.gte_filter_condition <- function(
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
      general_rule_from(x[[3]], build = build)
    )
  }
  if (x[[3]] == as.symbol("pos")) {
    return(
      general_rule_to(x[[2]], build = build)
    )
  }
  if (x[[2]] == as.symbol("chr")) {
    # chr >= x[[3]]
    if (x[[3]] %in% ordered_chr) {
      return(
        new_region_set(
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
        new_region_set(
          chr = ordered_chr[
            x[[2]] >= ordered_chr
          ],
          build = build
        )
      )
    }
  }
  full_region_set(build = build)
}
#' Regions a filter condition can match within
#'
#' The smallest set of regions covering every record the condition could
#' select when its non-genomic constraints are ignored. Combines the
#' condition's own genomic constraint with those of every condition it
#' combines.
#'
#' @param fcondition A filter_condition object
#' @return A region_set object, in the build the condition carries
#' @export
source_regions <- function(
  fcondition
) {
  stopifnot(is.filter_condition(fcondition))
  combined_constraint(fcondition) %||%
    full_region_set(build = build(fcondition))
}

#' Combine the genomic constraints of a condition and everything it combines
#'
#' @param fcondition A filter_condition object
#' @return A region_set object, or NULL when nothing is constrained
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
