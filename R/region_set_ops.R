# Sentinel value for NA replacement during overlap operations
# Every site that substitutes it in or back out must agree on the value:
# replace_na_with_sentinel and replace_sentinel_with_na, and subtract_rule_set
# and subtract_rule, which substitute inline. It covers the end axis of the
# foverlaps join; chr joins on equality and a start is always a position.
#
# END_SENTINEL is calibrated so that `END_SENTINEL + 1L` equals
# .Machine$integer.max exactly — no overflow. Every arithmetic site that
# adds to `end` uses `+ 1L`:
# - data.table::foverlaps adds 1 internally to convert to half-open ranges
# - merge_rules: shift(end + 1L, fill = 0)
# - subtract_rule: pmax(start, cut_end + 1L)
# - expand_region_set (negation): start = end + 1L
# Any future code that adds more than 1 to `end` must lower this sentinel
# accordingly, otherwise it will silently overflow to NA on sentinel rules.
END_SENTINEL <- .Machine$integer.max - 1L

#' Liftover region_set to a different genome build
#'
#' Converts genomic coordinates from one reference genome to another
#' using UCSC chain files. Handles regions that span chain boundaries
#' by splitting them.
#'
#' @param x A region_set object
#' @param target Target genome build (e.g., "hg38") or a chain data.table
#' @param ... Ignored
#'
#' @return region_set with coordinates in target build
#' @export
liftover.region_set <- function(
  x,
  target,
  ...
) {
  stopifnot(
    is.character(target) ||
      data.table::is.data.table(target) ||
      is.null(target)
  )
  # Normalize build name if target is a character string
  if (is.character(target)) {
    target <- normalize_build(target, allow_null = FALSE)
  }
  if (nrow(x) == 0) {
    target_build <- if (is.character(target)) target else attr(target, "to")
    x <- data.table::copy(x)
    if (!is.null(target_build)) {
      set_build(x, target_build)
    }
    return(x)
  }
  if (data.table::is.data.table(target)) {
    chain <- target
    target <- attr(chain, "to")
  } else {
    if (is.null(build(x))) {
      if (is.null(target)) {
        return(x)
      }
      warning(
        "No build configured for region_set, ",
        "returning original object"
      )
      return(x)
    }
    if (is.null(target)) {
      warning(
        "No build target specified, returning original object"
      )
      return(x)
    }
    if (build(x) == target) {
      return(x)
    }
    chain <- get_shared_chain(
      from = build(x),
      to = target
    )
  }

  expanded_x <- copy_region_set(x)

  if (!is.character(expanded_x$chr)) {
    expanded_x[
      ,
      chr := as.character(chr)
    ][]
  }

  expanded_x <- expanded_x[
    ,
    c(
      .SD,
      # This is required for data.table's 'by' grouping.
      # Using is.na(chr) directly leads data.table to auto-exclude chr from .SD
      list(chr_is_na = is.na(chr))
    )
  ][
    ,
    {
      if (all(is.na(chr))) {
        expanded_x[
          is.na(chr),
          c(
            .(
              chr = unique(chain$chr)
            ),
            .SD
          ),
          .SD = -"chr"
        ]
      } else {
        .SD
      }
    },
    by = chr_is_na
  ][, -"chr_is_na"]
  expanded_x[
    !grepl("^chr", chr),
    chr := paste0("chr", chr)
  ][]

  lift_chain_overlap(
    # An open end is one the overlap join has no notion of; the sentinel
    # states it as a concrete interval.
    replace_na_with_sentinel(expanded_x),
    chain
  ) |>
    as_region_set(
      build = build(target),
      complement = is_complement(x)
    )
}

#' @export
`[.region_set` <- function(
  regions,
  ...
) {
  gr_attributes <- attributes(regions)
  NextMethod() |>
    data.table::setattr("build", gr_attributes$build)
}

# Set Operations
#
# region_set supports set operations for combining regions:
# - Union (+ or |): combine all regions from both sets
# - Intersection (&): find overlapping portions

#' Union of region_set
#'
#' Combines two region sets, merging the rules that overlap or abut.
#'
#' @param e1,e2 region_set object
#' @return Combined region_set with merged overlaps
#' @export
`+.region_set` <- function(
  e1,
  e2
) {
  if (is_complement(e1)) {
    if (is_complement(e2)) {
      return(!(!e1 & !e2))
    }
    return(
      expand_region_set(e1) + e2
    )
  }
  if (is_complement(e2)) {
    return(
      e1 + expand_region_set(e2)
    )
  }
  rbind(e1, e2)
}

#' @export
`!.region_set` <- function(
  x
) {
  x <- data.table::copy(x)
  set_complement(x, !is_complement(x))
  x
}

#' Whether a region_set stores its coordinates or their complement
#'
#' `set_complement()` modifies `regions` in place.
#'
#' @param regions A region_set object
#' @param value TRUE if the rules are stored as the set's complement
#' @return `is_complement()` returns whether the rules are stored as the set's
#'   complement; `set_complement()` returns `regions`, invisibly.
#' @keywords internal
is_complement <- function(regions) {
  attr(regions, "complement")
}
#' @rdname is_complement
#' @keywords internal
set_complement <- function(regions, value) {
  stopifnot(is.logical(value))
  data.table::setattr(regions, "complement", value)
  invisible(regions)
}

#' @export
`-.region_set` <- function(
  e1,
  e2 = NULL
) {
  if (missing(e2)) {
    return(!e1)
  }
  e1 & !e2
}

#' @export
`==.region_set` <- function(
  e1,
  e2
) {
  e1 <- expand_region_set(e1)
  e2 <- liftover(e2, build(e1)) |>
    expand_region_set()

  if (nrow(e1) != nrow(e2)) {
    return(FALSE)
  }

  identical(data.table::as.data.table(e1), data.table::as.data.table(e2))
}

expand_region_set <- function(
  regions,
  ordered_chr = 1:22
) {
  if (!is_complement(regions)) {
    return(regions)
  }
  if (nrow(regions) == 0) {
    return(full_region_set(build = build(regions)))
  }
  if (1 < nrow(regions)) {
    return(
      lapply(
        seq_len(nrow(regions)),
        \(index) {
          expand_region_set(regions[index], ordered_chr)
        }
      ) |>
        Reduce(f = "&")
    )
  }
  if (is.na(regions$chr)) {
    full_chrs <- empty_region_set(build = build(regions))
  } else {
    full_chrs <- new_region_set(
      chr = setdiff(ordered_chr, drop_prefix(regions$chr)),
      build = build(regions)
    )
  }
  if (1 < regions$start) {
    before_fragment <- data.table::data.table(
      chr = regions$chr,
      start = 1L,
      end = regions$start - 1
    )
  } else {
    before_fragment <- data.table::data.table(
      chr = character(0),
      start = integer(0),
      end = integer(0)
    )
  }
  if (!is.na(regions$end)) {
    after_fragment <- data.table::data.table(
      chr = regions$chr,
      start = regions$end + 1,
      end = NA_integer_
    )
  } else {
    after_fragment <- data.table::data.table(
      chr = character(0),
      start = integer(0),
      end = integer(0)
    )
  }
  list(
    before_fragment,
    after_fragment
  ) |>
    data.table::rbindlist() |>
    as_region_set(build = build(regions)) |>
    rbind(full_chrs)
}

#' Combine region_set by rows
#'
#' @param ... region_set objects to combine
#' @param canonicalize If TRUE, reduce the regions to their canonical
#'   form: sorted and pairwise disjoint.
#' @return Combined region_set
#' @export
rbind.region_set <- function(
  ...,
  canonicalize = TRUE
) {
  dots <- list(...)
  if (!all(sapply(dots, is_complement) == is_complement(dots[[1]]))) {
    stop(
      "Attempting to rbind region_set objects that are stored differently ",
      "(one holding its coordinates, the other their complement)."
    )
  }
  # Check if all inputs have the required columns
  # (waldo may pass modified objects without these columns)
  required_cols <- c("chr", "start", "end")
  has_required_cols <- vapply(
    dots,
    function(x) all(required_cols %in% names(x)),
    logical(1)
  )
  if (!all(has_required_cols)) {
    # Fall back to simple rbindlist for waldo (testthat) comparison
    return(data.table::rbindlist(dots))
  }
  dots |>
    lapply(
      liftover,
      target = build(dots[[1]])
    ) |>
    data.table::rbindlist(use.names = TRUE, fill = TRUE) |>
    as_region_set(
      build = build(dots[[1]]),
      complement = is_complement(dots[[1]]),
      canonicalize = canonicalize
    )
}

#' Union operator for region_set
#'
#' Alias for `+.region_set` - combines regions from both sets.
#' When used as filter conditions, OR means "match any of these regions".
#'
#' @param e1,e2 region_set objects
#' @return Combined region_set
#' @name or-.region_set
#' @export
`|.region_set` <- `+.region_set`

#' Intersection of region_set
#'
#' Finds the positions two region sets both select.
#' Uses data.table::foverlaps for efficient overlap detection.
#'
#' Handles special cases:
#' - NA chromosome (any chr) intersected with specific chr
#' - Empty regions (returns empty)
#' - Different builds (auto-lifts second argument)
#'
#' @param e1,e2 region_set objects
#' @return region_set containing only overlapping portions
#' @export
`&.region_set` <- function(
  e1,
  e2
) {
  # Ensure same build
  if (!identical(build(e1), build(e2))) {
    warning(
      "The region_set objects do not have the same build, ",
      "converting the second."
    )
    e2 <- liftover(e2, build(e1))
  }

  if (is_complement(e1)) {
    if (is_complement(e2)) {
      return(rbind(e1, e2))
    }
    return(
      expand_region_set(e1) & e2
    )
  }
  if (is_complement(e2)) {
    return(
      e1 & expand_region_set(e2)
    )
  }

  # Intersection with empty set = empty set
  if (is.null(e1) || is.null(e2) || nrow(e1) == 0 || nrow(e2) == 0) {
    return(empty_region_set(build = build(e1)))
  }

  # Handle mixed NA chromosomes by splitting and recursing
  if (chr_has_mixed_na(e1)) {
    return((e1[is.na(chr)] & e2) + (e1[!is.na(chr)] & e2))
  }
  if (chr_has_mixed_na(e2)) {
    return((e1 & e2[is.na(chr)]) + (e1 & e2[!is.na(chr)]))
  }

  e1_safe <- copy_region_set(e1)
  e2_safe <- copy_region_set(e2)

  # Expand NA chr to match specific chromosomes from other set
  if (is.na(e1_safe$chr[1]) != is.na(e2_safe$chr[1])) {
    if (is.na(e1_safe$chr[1])) {
      e1_safe <- expand_na_chr(e1_safe, e2_safe)
    } else {
      e2_safe <- expand_na_chr(e2_safe, e1_safe)
    }
  }

  data.table::foverlaps(
    # foverlaps rejects an NA end
    replace_na_with_sentinel(e1_safe),
    replace_na_with_sentinel(e2_safe),
    nomatch = 0
  )[
    ,
    c(
      .SD,
      list(
        start = pmax(start, i.start, na.rm = TRUE),
        end = pmin(end, i.end, na.rm = TRUE)
      )
    ),
    .SDcols = -c("start", "end", "i.start", "i.end")
  ] |>
    replace_sentinel_with_na() |>
    as_region_set(build = build(e1))
}

#' Check if region_set has both NA and non-NA chromosomes
#'
#' @param regions A region_set object
#' @return TRUE if some rules have NA chr and others don't
#' @keywords internal
chr_has_mixed_na <- function(
  regions
) {
  length(unique(is.na(regions$chr))) == 2
}

#' Expand NA chromosome regions to all chromosomes in reference
#'
#' When a region_set has NA chromosome (meaning "any"), this expands
#' it to explicitly list all chromosomes found in the second region_set.
#'
#' @param regions1 region_set with NA chr (to expand)
#' @param regions2 Second region_set (for chromosome list)
#'
#' @return Expanded region_set with explicit chromosomes
#' @keywords internal
expand_na_chr <- function(
  regions1,
  regions2
) {
  stopifnot(build(regions1) == build(regions2))
  stopifnot(all(is.na(regions1$chr)))
  # Cross join: each rule of regions1 x each unique chr from regions2
  regions1[
    ,
    .(
      chr = unique(regions2$chr)
    ) |>
      c(.SD),
    .SDcols = -"chr",
    by = .I
  ][, -"I"] |>
    as_region_set(build = build(regions1))
}

# Sentinel Value Handling for Overlap Operations
#
# An overlap operation over region sets — data.table::foverlaps under
# `&`, the non-equi join under a lift — has no notion of an open end, and
# foverlaps rejects an NA in an interval column outright. An open end is
# replaced with a sentinel value, which states it as a concrete interval,
# and can be substituted back for NA once the overlap is done.

#' Replace NA values with sentinel values for overlap operations
#'
#' Converts an NA end to `end_sentinel`, the concrete bound an overlap
#' operation reads in its place.
#'
#' @param regions A region_set with a potentially NA end
#' @param end_sentinel Value to use for NA end
#'
#' @return Copy with NAs replaced by sentinels, keyed on `chr`, `start`
#'   and `end`, the key `data.table::foverlaps` requires of its `y`.
#' @keywords internal
replace_na_with_sentinel <- function(
  regions,
  end_sentinel = END_SENTINEL
) {
  copy_region_set(regions)[
    is.na(end),
    end := end_sentinel
  ][] |>
    data.table::setkey("chr", "start", "end")
}

#' Restore NA values from sentinel values after an overlap operation
#'
#' Converts `end_sentinel` back to NA after overlap operations.
#'
#' @param regions A region_set carrying sentinel values
#' @param end_sentinel Value used for NA end
#'
#' @return Copy with sentinels replaced by NAs
#' @keywords internal
replace_sentinel_with_na <- function(
  regions,
  end_sentinel = END_SENTINEL
) {
  copy_region_set(regions)[
    end == end_sentinel,
    end := NA_integer_
  ][] |>
    # The comparisons above leave a secondary index on the result
    data.table::setindexv(NULL) |>
    data.table::setkey("chr", "start", "end")
}

#' Return the smallest equivalent region_set representation
#'
#' Compares the input against both stored forms — its rules expanded, and the
#' complement of its negation expanded — and returns whichever has the fewest
#' rows. Output is semantically equivalent to the input - it represents the
#' same set of genomic positions.
#'
#' @param regions A region_set object
#' @return A region_set selecting the same positions as `regions`, with the
#'   fewest rows
#' @export
#' @examples
#' \dontrun{
#' regions <- new_region_set(chr = 1:20, build = "b38")
#' compact(regions)
#' }
compact <- function(regions) {
  expanded_regions <- expand_region_set(regions)
  expanded_complement <- !expand_region_set(!regions)
  if (nrow(expanded_complement) < nrow(expanded_regions) &&
    nrow(expanded_complement) < nrow(regions)) {
    return(expanded_complement)
  }
  if (nrow(expanded_regions) < nrow(regions)) {
    return(expanded_regions)
  }
  regions
}
