# =============================================================================
# Genomic Regions
# =============================================================================
# This module provides the genomic_regions class for representing and
# manipulating genomic coordinate ranges. Key features:
#
# - Represents regions as data.table with chr, start, end columns
# - Supports set operations: union (|/+), intersection (&)
# - Handles full-genome queries (NA values mean "any")
# - Liftover between genome builds (e.g., hg19 -> hg38)
# - Automatically merges overlapping/contiguous regions
#
# The genomic_regions class is used throughout filterRead to:
# - Extract genomic constraints from filter conditions
# - Generate tabix queries for RSID-indexed files
# - Combine multiple filtering regions efficiently

#' Create a new genomic_regions object
#'
#' Constructor for genomic_regions. Validates inputs, formats chromosome
#' names, and optionally merges contiguous regions.
#'
#' @param chr Character vector of chromosome names (NA = any chromosome)
#' @param start Integer vector of start positions (NA = from beginning)
#' @param end Integer vector of end positions (NA = to end)
#' @param build Genome build identifier ("b38", "b37", "b36", default: NULL)
#' @param chr_prefix Prefix to add to chromosome names (default: "chr")
#' @param merge_contiguous If TRUE, merge overlapping/adjacent regions
#'
#' @return A genomic_regions object (data.table with class attribute)
#' @keywords internal
new_genomic_regions <- function(
  chr = character(),
  start = integer(),
  end = integer(),
  build = NULL,
  chr_prefix = "chr",
  merge_contiguous = TRUE
) {
  if (data.table::is.data.table(chr)) {
    return(
      as_genomic_regions(chr, build = build)
    )
  }

  argument_lengths <- list(chr, start, end) |>
    sapply(length)
  nrows <- max(argument_lengths)

  if (!all(argument_lengths %in% c(0, 1, nrows))) {
    stop(
      "Invalid argument lengths. `chr`, `start`, and `end`, ",
      "if provided, must have length 1 or the same length as the longest."
    )
  }
  if (any(argument_lengths == 0) && nrows != 0) {
    if (length(chr) == 0) chr <- NA_character_
    if (length(start) == 0) start <- NA_integer_
    if (length(end) == 0) end <- NA_integer_
  }

  end_before_start <- end < start
  any_end_before_start <- end_before_start |>
    na.omit() |>
    any()
  if (any_end_before_start) {
    stop("start must be less than end.")
  }

  genomic_regions_dt <- data.table::data.table(
    chr = format_chr(chr, prefix = chr_prefix),
    start = start,
    end = end
  ) |>
    as_genomic_regions(build = build)
  if (!merge_contiguous) {
    return(genomic_regions_dt)
  }
  merge_contiguous_regions(genomic_regions_dt)
}

#' Create empty genomic_regions (no regions)
#'
#' @param build Genome build identifier
#' @return Empty genomic_regions (0 rows)
#' @keywords internal
empty_genomic_regions <- function(
  build = NULL
) {
  new_genomic_regions(build = build)
}

#' Create full-genome genomic_regions (all positions)
#'
#' Returns a genomic_regions with NA for chr, start, and end.
#'
#' @param build Genome build identifier
#' @return genomic_regions representing the entire genome
#' @keywords internal
full_genomic_regions <- function(
  build = NULL
) {
  new_genomic_regions(
    chr = NA_character_,
    build = build
  )
}

# =============================================================================
# as_genomic_regions Methods
# =============================================================================
# These methods convert various objects to genomic_regions. Each filter
# condition type has a method that extracts the genomic constraints:
# - eq_filter_condition: chr == X, pos == Y
# - lt/lte/gt/gte: pos comparisons -> start/end bounds
# - and/or: combine child regions with &/|

#' Coerce object to genomic_regions
#'
#' @param x Object to convert
#' @param build Genome build identifier
#' @param ... Additional arguments passed to methods
#' @return A genomic_regions object
#' @export
as_genomic_regions <- function(
  x,
  build = get_build(x),
  ...
) {
  UseMethod("as_genomic_regions")
}

#' @export
#' @rdname as_genomic_regions
as_genomic_regions.default <- function(
  x,
  build = get_build(x),
  ...
) {
  stopifnot(is.data.frame(x))
  genomic_regions <- data.table::as.data.table(x) |>
    data.table::copy()
  validate_genomic_regions_dt(genomic_regions)
  data.table::setattr(genomic_regions, "build", build)
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
as_genomic_regions.filter_condition <- function(
  x,
  build = get_build(x),
  ...
) {
  genomic_regions(x)
}
#' @export
as_genomic_regions.and_filter_condition <- function(
  x,
  build = get_build(x),
  ...
) {
  as_genomic_regions(x[[2]], build = build, ...) & as_genomic_regions(x[[3]], build = build, ...)
}
#' @export
as_genomic_regions.or_filter_condition <- function(
  x,
  build = get_build(x),
  ...
) {
  as_genomic_regions(x[[2]], build = build, ...) | as_genomic_regions(x[[3]], build = build, ...)
}
#' @export
as_genomic_regions.lp_filter_condition <- function(
  x,
  build = get_build(x),
  ...
) {
  as_genomic_regions(x[[2]], build = build, ...)
}
#' @export
as_genomic_regions.eq_filter_condition <- function(
  x,
  build = get_build(x),
  ...
) {
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
  new_genomic_regions(chr = NA_character_, build = build)
}
#' @export
as_genomic_regions.lt_filter_condition <- function(
  x,
  build = get_build(x),
  ordered_chr = 1:22,
  ...
) {
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
  new_genomic_regions(chr = NA_character_, build = build)
}
#' @export
as_genomic_regions.lte_filter_condition <- function(
  x,
  build = get_build(x),
  ordered_chr = 1:22,
  ...
) {
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
  new_genomic_regions(chr = NA_character_, build = build)
}
#' @export
as_genomic_regions.gt_filter_condition <- function(
  x,
  build = get_build(x),
  ordered_chr = 1:22,
  ...
) {
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
  new_genomic_regions(chr = NA_character_, build = build)
}
#' @export
as_genomic_regions.gte_filter_condition <- function(
  x,
  build = get_build(x),
  ordered_chr = 1:22,
  ...
) {
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
  new_genomic_regions(chr = NA_character_, build = build)
}

# =============================================================================
# Utility Functions
# =============================================================================

#' Check if object is genomic_regions
#'
#' @param x Object to test
#' @return TRUE if x inherits from genomic_regions
#' @keywords internal
is_genomic_regions <- function(
  x
) {
  inherits(x, "genomic_regions")
}

#' Format chromosome names with prefix
#'
#' Ensures all chromosome names have the specified prefix (e.g., "chr").
#' NA values are preserved.
#'
#' @param chr Character vector of chromosome names
#' @param prefix Prefix to add (default: "chr")
#' @return Formatted chromosome names
#' @keywords internal
format_chr <- function(
  chr,
  prefix
) {
  chr <- as.character(chr)
  data.table::fifelse(
    grepl(paste0("^", prefix), chr) | is.na(chr),
    chr,
    paste0(prefix, chr)
  )
}

#' Deep copy genomic_regions
#'
#' @param gregions A genomic_regions object
#' @return A copy with class preserved
#' @keywords internal
copy_genomic_regions <- function(gregions) {
  data.table::copy(gregions) |>
    as_genomic_regions()
}

#' Remove prefix from chromosome names
#'
#' @param chr Character vector of chromosome names
#' @param chr_prefix Prefix to remove (defaul: "chr")
#' @return Chromosome names without prefix
#' @keywords internal
drop_chr_prefix <- function(
  chr,
  chr_prefix = "chr"
) {
  gsub(paste0("^", chr_prefix), "", chr)
}

#' Check if genomic_regions represents full genome
#'
#' A full genome query has all NA values (no constraints).
#'
#' @param gregions A genomic_regions object
#' @return TRUE if gregions has no genomic constraints
#' @keywords internal
is_full_genome <- function(gregions) {
  all(is.na(gregions))
}


#' @export
print.genomic_regions <- function(
  x
) {
  NextMethod()
  if (!is.null(get_build(x))) {
    cat("Build:", get_build(x))
  }
}

# TODO: Implement other checks
#' Validate genomic_regions data.table structure
#'
#' @param genomic_regions_dt data.table to validate
#' @return Invisible input (for chaining)
#' @keywords internal
validate_genomic_regions_dt <- function(
  genomic_regions_dt
) {
  if (!data.table::is.data.table(genomic_regions_dt)) {
    stop(
      "Object does not inherit the data.table class.\n",
      "Actual class: ", class(genomic_regions_dt)
    )
  }
  invisible(genomic_regions_dt)
}

# =============================================================================
# Liftover (Coordinate Conversion Between Genome Builds)
# =============================================================================

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
  if (nrow(x) == 0) {
    if (is.character(target)) {
      attr(x, "build") <- target
    } else {
      attr(x, "build") <- attr(target, "to")
    }
    return(x)
  }
  if (data.table::is.data.table(target)) {
    chain_dt <- target
    target <- attr(chain_dt, "to")
  } else {
    if (is.null(get_build(x))) {
      if (is.null(target)) return(x)
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
    if (get_build(x) == target) {
      return(x)
    }
    chain_dt <- get_chain_dt(
      from = get_build(x),
      to = target
    )
  }

  expanded_x <- data.table::copy(x)

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
    as_genomic_regions(build = get_build(target))
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

# =============================================================================
# Set Operations
# =============================================================================
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
  rbind(e1, e2) |>
    merge_contiguous_regions()
}

#' Combine genomic_regions by rows
#'
#' @param ... genomic_regions objects to combine
#' @return Combined genomic_regions
#' @export
rbind.genomic_regions <- function(
  ...
) {
  dots <- list(...)
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
      target = get_build(dots[[1]])
    ) |>
    data.table::rbindlist() |>
    as_genomic_regions(
      build = get_build(dots[[1]])
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
  if (!identical(get_build(e1), get_build(e2))) {
    warning(
      "The genomic_regions do not have the same build, ",
      "converting the second."
    )
    e2 <- liftover(e2, get_build(e1))
  }
  # Intersection with empty set = empty set
  if (is.null(e1) || is.null(e2)) {
    return(new_genomic_regions(build = get_build(e1)))
  }
  if (nrow(e1) == 0 || nrow(e2) == 0) {
    return(new_genomic_regions(build = get_build(e1)))
  }

  # genomic_regions where is.na(chr) are to be combined with some
  # where !is.na(chr) have to be expanded to match everything instead
  if (chr_has_mixed_na(e1)) {
    return(
      (e1[is.na(chr)] & e2) + (e1[!is.na(chr)] & e2)
    )
  }
  if (chr_has_mixed_na(e2)) {
    return(
      (e1 & e2[is.na(chr)]) + (e1 & e2[!is.na(chr)])
    )
  }

  e1_safe <- data.table::copy(e1)
  e2_safe <- data.table::copy(e2)
  if (is.na(e1_safe$chr[1]) != is.na(e2_safe$chr[1])) {
    if (is.na(e1_safe$chr[1])) {
      e1_safe <- expand_na_chr(e1_safe, e2_safe)
    } else {
      e2_safe <- expand_na_chr(e2_safe, e1_safe)
    }
  }
  # NAs are not handled by foverlaps, need to replace with sentinel values and
  # restore the NAs after foverlaps
  e1_safe <- replace_na_with_sentinel(e1_safe)
  e2_safe <- replace_na_with_sentinel(e2_safe)
  data.table::foverlaps(e1_safe, e2_safe, nomatch = 0)[
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
    as_genomic_regions(build = get_build(e1_safe))
}

# =============================================================================
# Helper Functions for Intersection
# =============================================================================

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
  stopifnot(get_build(gregions1) == get_build(gregions2))
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
  ] |>
    as_genomic_regions(build = get_build(gregions1))
}

# =============================================================================
# Sentinel Value Handling for foverlaps
# =============================================================================
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
  chr_sentinel = "",
  start_sentinel = 0L,
  # Need to subtract 1 because foverlaps adds 1
  end_sentinel = .Machine$integer.max - 1L
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
  chr_sentinel = "",
  start_sentinel = 0L,
  end_sentinel = .Machine$integer.max - 1L
) {
  data.table::copy(genomic_regions)[
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

# =============================================================================
# Region Merging
# =============================================================================

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
    as_genomic_regions()
}
