# Sentinel values for NA replacement during foverlaps operations
# These must stay in sync between replace_na_with_sentinel and
# replace_sentinel_with_na
CHR_SENTINEL <- ""
START_SENTINEL <- 0L
# Need to subtract 1 because foverlaps adds 1
END_SENTINEL <- .Machine$integer.max - 1L

#' Liftover genomic_regions to a different genome build
#'
#' Converts genomic coordinates from one reference genome to another
#' using UCSC chain files. Handles regions that span chain boundaries
#' by splitting them.
#'
#' @param x A genomic_regions object
#' @param target Target genome build (e.g., "hg38")
#' @param chain_dt Chain data.table
#' @param ... Ignored
#'
#' @return genomic_regions with coordinates in target build
#' @export
liftover.genomic_regions <- function(
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
    if (is.character(target)) {
      build(x) <- target
    } else {
      build(x) <- attr(target, "to")
    }
    return(x)
  }
  if (data.table::is.data.table(target)) {
    chain_dt <- target
    target <- attr(chain_dt, "to")
  } else {
    if (is.null(build(x))) {
      if (is.null(target)) {
        return(x)
      }
      warning(
        "No build configured for genomic_regions, ",
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
    chain_dt <- get_chain_dt(
      from = build(x),
      to = target
    )
  }

  expanded_x <- copy_genomic_regions(x)

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
              chr = unique(chain_dt$chr)
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

  data.table::foverlaps(
    # foverlaps doesn't handle NAs
    replace_na_with_sentinel(
      expanded_x,
      start_sentinel = 1L
    ),
    chain_dt,
    nomatch = 0
  )[
    ,
    .(
      chr = new_chr,
      start = start,
      end = end,
      offset = offset,
      rev = rev,
      i.start = pmax(start, i.start),
      i.end = pmin(end, i.end)
    )
  ][
    ,
    .(
      chr = chr,
      start = data.table::fifelse(
        rev,
        end - offset - (i.end - start),
        i.start - offset
      ),
      end = data.table::fifelse(
        rev,
        end - offset - (i.start - start),
        i.end - offset
      )
    )
  ] |>
    as_genomic_regions(build = build(target))
}

#' @export
`[.genomic_regions` <- function(
  gregions,
  ...
) {
  gr_attributes <- attributes(gregions)
  NextMethod() |>
    data.table::setattr("build", gr_attributes$build)
}

# Set Operations
#
# genomic_regions supports set operations for combining regions:
# - Union (+ or |): combine all regions from both sets
# - Intersection (&): find overlapping portions

#' Union of genomic_regions
#'
#' Combines two sets of genomic regions, merging overlapping/contiguous
#' regions.
#'
#' @param e1,e2 genomic_regions object
#' @return Combined genomic_regions with merged overlaps
#' @export
`+.genomic_regions` <- function(
  e1,
  e2
) {
  if (!is_included(e1)) {
    if (!is_included(e2)) {
      return(!(!e1 & !e2))
    }
    return(
      expand_genomic_regions(e1) + e2
    )
  }
  if (!is_included(e2)) {
    return(
      e1 + expand_genomic_regions(e2)
    )
  }
  rbind(e1, e2) |>
    merge_contiguous_regions()
}

#' @export
`!.genomic_regions` <- function(
  x
) {
  is_included(x) <- !is_included(x)
  x
}
is_included <- function(gregions) {
  attr(gregions, "include")
}
`is_included<-` <- function(gregions, value) {
  stopifnot(is.logical(value))
  attr(gregions, "include") <- value
  gregions
}

#' @export
`-.genomic_regions` <- function(
  e1,
  e2 = NULL
) {
  if (missing(e2)) {
    return(!e1)
  }
  e1 & !e2
}

#' @export
`==.genomic_regions` <- function(
  e1,
  e2
) {
  e1 <- expand_genomic_regions(e1)
  e2 <- liftover(e2, build(e1)) |>
    expand_genomic_regions()

  if (nrow(e1) != nrow(e2)) {
    return(FALSE)
  }

  identical(data.table::as.data.table(e1), data.table::as.data.table(e2))
}

expand_genomic_regions <- function(
  gregions,
  ordered_chr = 1:22
) {
  if (is_included(gregions)) {
    return(gregions)
  }
  if (nrow(gregions) == 0) {
    return(full_genomic_regions(build = build(gregions)))
  }
  if (1 < nrow(gregions)) {
    return(
      lapply(
        seq_len(nrow(gregions)),
        \(index) {
          expand_genomic_regions(gregions[index], ordered_chr)
        }
      ) |>
        Reduce(f = "&")
    )
  }
  full_chrs <- new_genomic_regions(
    chr = setdiff(ordered_chr, drop_chr_prefix(gregions$chr)),
    build = build(gregions)
  )
  if (!is.na(gregions$start) && 1 < gregions$start) {
    include_before <- data.table::data.table(
      chr = gregions$chr,
      start = 1,
      end = gregions$start - 1
    )
  } else {
    include_before <- data.table::data.table(
      chr = character(0),
      start = integer(0),
      end = integer(0)
    )
  }
  if (!is.na(gregions$end)) {
    include_after <- data.table::data.table(
      chr = gregions$chr,
      start = gregions$end + 1,
      end = NA_integer_
    )
  } else {
    include_after <- data.table::data.table(
      chr = character(0),
      start = integer(0),
      end = integer(0)
    )
  }
  list(
    include_before,
    include_after
  ) |>
    data.table::rbindlist() |>
    as_genomic_regions(build = build(gregions)) |>
    rbind(full_chrs)
}

#' Combine genomic_regions by rows
#'
#' @param ... genomic_regions objects to combine
#' @return Combined genomic_regions
#' @export
rbind.genomic_regions <- function(
  ...,
  merge_contiguous = TRUE
) {
  dots <- list(...)
  if (!all(sapply(dots, is_included) == is_included(dots[[1]]))) {
    stop(
      "Attempting to rbind genomic_regions of different types ",
      "(both including and excluding the described regions)."
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
    as_genomic_regions(
      build = build(dots[[1]]),
      include = is_included(dots[[1]]),
      merge_contiguous = merge_contiguous
    )
}

#' Union operator for genomic_regions
#'
#' Alias for `+.genomic_regions` - combines regions from both sets.
#' When used as filter conditions, OR means "match any of these regions".
#'
#' @param e1,e2 genomic_regions objects
#' @return Combined genomic_regions
#' @export
`|.genomic_regions` <- `+.genomic_regions`

#' Intersection of genomic_regions
#'
#' Finds overlapping portions between two sets of genomic regions.
#' Uses data.table::foverlaps for efficient overlap detection.
#'
#' Handles special cases:
#' - NA chromosome (any chr) intersected with specific chr
#' - Empty regions (returns empty)
#' - Different builds (auto-lifts second argument)
#'
#' @param e1,e2 genomic_regions objects
#' @return genomic_regions containing only overlapping portions
#' @export
`&.genomic_regions` <- function(
  e1,
  e2
) {
  # Ensure same build
  if (!identical(build(e1), build(e2))) {
    warning(
      "The genomic_regions do not have the same build, ",
      "converting the second."
    )
    e2 <- liftover(e2, build(e1))
  }

  if (!is_included(e1)) {
    if (!is_included(e2)) {
      return(
        rbind(e1, e2) |>
          merge_contiguous_regions()
      )
    }
    return(
      expand_genomic_regions(e1) & e2
    )
  }
  if (!is_included(e2)) {
    return(
      e1 & expand_genomic_regions(e2)
    )
  }

  # Intersection with empty set = empty set
  if (is.null(e1) || is.null(e2) || nrow(e1) == 0 || nrow(e2) == 0) {
    return(empty_genomic_regions(build = build(e1)))
  }

  # Handle mixed NA chromosomes by splitting and recursing
  if (chr_has_mixed_na(e1)) {
    return((e1[is.na(chr)] & e2) + (e1[!is.na(chr)] & e2))
  }
  if (chr_has_mixed_na(e2)) {
    return((e1 & e2[is.na(chr)]) + (e1 & e2[!is.na(chr)]))
  }

  # Prepare inputs and compute intersection
  prepared <- prepare_for_intersection(e1, e2)
  compute_intersection(prepared$e1, prepared$e2, build = build(e1))
}

# Helper Functions for Intersection

#' Prepare genomic_regions pair for intersection
#'
#' Copies inputs, expands NA chromosomes if needed, and replaces NAs with
#' sentinel values for foverlaps compatibility.
#'
#' @param e1,e2 genomic_regions objects (same build, no mixed NA)
#' @return List with prepared e1 and e2
#' @keywords internal
prepare_for_intersection <- function(e1, e2) {
  e1_safe <- copy_genomic_regions(e1)
  e2_safe <- copy_genomic_regions(e2)

  # Expand NA chr to match specific chromosomes from other set
  if (is.na(e1_safe$chr[1]) != is.na(e2_safe$chr[1])) {
    if (is.na(e1_safe$chr[1])) {
      e1_safe <- expand_na_chr(e1_safe, e2_safe)
    } else {
      e2_safe <- expand_na_chr(e2_safe, e1_safe)
    }
  }

  # Replace NAs with sentinels (foverlaps doesn't handle NAs)
  list(
    e1 = replace_na_with_sentinel(e1_safe),
    e2 = replace_na_with_sentinel(e2_safe)
  )
}

#' Compute intersection using foverlaps
#'
#' @param e1,e2 Prepared genomic_regions (sentinel values, keyed)
#' @param build Build to assign to result
#' @return genomic_regions with overlapping portions
#' @keywords internal
compute_intersection <- function(e1, e2, build) {
  # TODO Handle 'include'
  data.table::foverlaps(e1, e2, nomatch = 0)[
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
    as_genomic_regions(build = build)
}

#' Check if genomic_regions has both NA and non-NA chromosomes
#'
#' @param gregions A genomic_regions object
#' @return TRUE if some rows have NA chr and others don't
#' @keywords internal
chr_has_mixed_na <- function(
  gregions
) {
  length(unique(is.na(gregions$chr))) == 2
}

#' Expand NA chromosome regions to all chromosomes in reference
#'
#' When a genomic_regions has NA chromosome (meaning "any"), this expands
#' it to explicitly list all chromosomes found in the second genomic_regions.
#'
#' @param gregions1 genomic_regions with NA chr (to expand)
#' @param gregions2 Second genomic_regions (for chromosome list)
#'
#' @return Expanded genomic_regions with explicit chromosomes
#' @keywords internal
expand_na_chr <- function(
  gregions1,
  gregions2
) {
  stopifnot(build(gregions1) == build(gregions2))
  stopifnot(all(is.na(gregions1$chr)))
  # Cross join: each row of gregions1 x each unique chr from gregions2
  gregions1[
    ,
    .(
      chr = unique(gregions2$chr)
    ) |>
      c(.SD),
    .SDcols = -"chr",
    by = .I
  ][, -"I"] |>
    as_genomic_regions(build = build(gregions1))
}

# Sentinel Value Handling for foverlaps
#
# data.table::foverlaps cannot handle NA values in any columns.
# NAs are replaced with sentinel values, which enable foverlaps and can then
# be substituted back for NA after foverlaps-dependent operations.

#' Replace NA values with sentinel values for foverlaps
#'
#' Converts NA to sentinel values that foverlaps can handle:
#' - NA chr -> empty string
#' - NA start -> 0 (from beginning)
#' - NA end -> max integer - 1 (to end)
#' The default for end subtracts 1 from max integer to avoid overflows
#' (data.table::foverlaps adds 1 internally).
#'
#' @param genomic_regions data.table with potential NAs
#' @param chr_sentinel Value to use for NA chromosomes (default: "")
#' @param start_sentinel Value to use for NA start (default: 0)
#' @param end_sentinel Value to use for NA end
#'   (default: .Machine$integer.max - 1)
#'
#' @return Copy with NAs replaced by sentinels, keyed for foverlaps
#' @keywords internal
replace_na_with_sentinel <- function(
  genomic_regions,
  chr_sentinel = CHR_SENTINEL,
  start_sentinel = START_SENTINEL,
  end_sentinel = END_SENTINEL
) {
  copy_genomic_regions(genomic_regions)[
    is.na(chr),
    chr := chr_sentinel
  ][
    is.na(start),
    start := start_sentinel
  ][
    is.na(end),
    end := end_sentinel
  ][] |>
    data.table::setkey("chr", "start", "end")
}

#' Restore NA values from sentinel values after foverlaps
#'
#' Converts sentinel values back to NA after overlap operations.
#'
#' @param genomic_regions data.table with sentinel values
#' @param chr_sentinel Value used for NA chromosomes (default: "")
#' @param start_sentinel Value used for NA start (default: 0)
#' @param end_sentinel Value used for NA end (default: .Machine$integer.max - 1)
#'
#' @return Copy with sentinels replaced by NAs
#' @keywords internal
replace_sentinel_with_na <- function(
  genomic_regions,
  chr_sentinel = CHR_SENTINEL,
  start_sentinel = START_SENTINEL,
  end_sentinel = END_SENTINEL
) {
  copy_genomic_regions(genomic_regions)[
    chr == chr_sentinel,
    chr := NA_character_
  ][
    start == start_sentinel,
    start := NA_integer_
  ][
    end == end_sentinel,
    end := NA_integer_
  ][] |>
    data.table::setkey("chr", "start", "end")
}

# Region Merging

#' Merge overlapping and contiguous genomic regions
#'
#' Combines regions that overlap or are adjacent (end+1 == start) into
#' single regions. Compares each start with end of previous row.
#'
#' Algorithm:
#' 1. Sort by chr, start, end
#' 2. For each chromosome, identify groups where start > previous end + 1
#' 3. Merge each group to (min(start), max(end))
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
  copy_genomic_regions(gregions)[
    ,
    c(
      .SD,
      # Groups of overlapping regions are obtained by counting (cumsum)
      # regions where the start is less than the end of the previous
      # region + 1 (shift(end + 1))
      .(group = cumsum(
        cummax(data.table::shift(end + 1, fill = 0)) < start
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
    # 'build' is preserved but 'include' is not
    as_genomic_regions(
      include = is_included(gregions),
      merge_contiguous = FALSE
    )
}

#' @export
compact <- function(gregions) {
  expanded_gregions <- expand_genomic_regions(gregions)
  expanded_ngregions <- !expand_genomic_regions(!gregions)
  if (nrow(expanded_ngregions) < nrow(expanded_gregions) &&
    nrow(expanded_ngregions) < nrow(gregions)) {
    return(expanded_ngregions)
  }
  if (nrow(expanded_gregions) < nrow(gregions)) {
    return(expanded_gregions)
  }
  gregions
}
