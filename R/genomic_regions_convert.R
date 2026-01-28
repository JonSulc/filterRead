#' Convert objects to genomic_regions
#'
#' Generic function to convert various objects (data.frames, filter_conditions)
#' to genomic_regions format.
#'
#' @param x Object to convert
#' @param build Genome build (e.g., "hg19", "hg38")
#' @param ... Additional arguments passed to methods
#' @return A genomic_regions object
#' @keywords internal
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
  ...
) {
  build <- build %||% build(x)
  genomic_regions <- data.table::as.data.table(x) |>
    data.table::copy()
  validate_genomic_regions_dt(genomic_regions)
  build(genomic_regions) <- build
  data.table::setattr(
    genomic_regions,
    "class",
    c("genomic_regions", class(genomic_regions)) |>
      unique()
  )
  data.table::setkey(genomic_regions, chr, start, end)
  genomic_regions
}
#' @export
as_genomic_regions.and_filter_condition <- function(
  x,
  build = NULL,
  ...
) {
  build <- build %||% build(x)
  as_genomic_regions(x[[2]], build = build, ...) &
    as_genomic_regions(x[[3]], build = build, ...)
}
#' @export
as_genomic_regions.or_filter_condition <- function(
  x,
  build = NULL,
  ...
) {
  build <- build %||% build(x)
  as_genomic_regions(x[[2]], build = build, ...) |
    as_genomic_regions(x[[3]], build = build, ...)
}
#' @export
as_genomic_regions.lp_filter_condition <- function(
  x,
  build = NULL,
  ...
) {
  build <- build %||% build(x)
  as_genomic_regions(x[[2]], build = build, ...)
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
#' @export
as_genomic_regions.filter_condition <- function(
  x,
  build = NULL,
  ordered_chr = 1:22,
  ...
) {
  if (!is.null(genomic_regions(x))) {
    return(genomic_regions(x))
  }
  full_genomic_regions(build = build)
}
