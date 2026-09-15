# Region sets
#
# This module provides the region_set class for representing and
# manipulating genomic coordinate ranges. Key features:
#
# - Represents regions as data.table with chr, start, end columns
# - Supports set operations: union (|/+), intersection (&)
# - Handles full-genome queries (an NA chr or end means "any")
# - Liftover between genome builds (e.g., b37 -> b38)
# - Reduces regions to a canonical form on construction
#
# The region_set class is used throughout filterRead to:
# - Extract genomic constraints from filter conditions
# - Generate tabix queries for RSID-indexed files
# - Combine multiple filtering regions efficiently

#' Construct genomic regions for positional filtering
#'
#' Builds a `region_set` object — a set of `(chr, start, end)`
#' coordinate ranges in one reference build — from parallel vectors. Pass
#' the result as a `[` subscript to a `file_interface` to filter its rows by
#' position: `finterface[region_set(...)]`. To build regions from an
#' existing `data.table` of `chr`/`start`/`end`, use [as_region_set()].
#'
#' @param chr Character vector of chromosome names (NA = any chromosome).
#' @param start Integer vector of start positions.
#' @param end Integer vector of end positions (NA = to end).
#' @param build Reference build the coordinates are expressed in ("b36",
#'   "b37", "b38", or a synonym). Optional at construction, but positional
#'   filtering against a build-bearing file requires it (see
#'   [as_region_set()] and the file-interface `[` method).
#' @param chr_prefix Prefix to ensure on chromosome names (default "chr").
#' @param canonicalize If TRUE, reduce the regions to their canonical
#'   form: sorted and pairwise disjoint.
#' @param complement If TRUE the rows describe the coordinates the set
#'   excludes rather than those it covers.
#'
#' @return A `region_set` object.
#' @seealso [as_region_set()] to coerce an existing table.
#' @examples
#' region_set(
#'   chr = c("1", "2"),
#'   start = c(1000L, 5000L),
#'   end = c(2000L, 6000L),
#'   build = "b38"
#' )
#' @export
region_set <- function(
  chr = character(),
  start = integer(),
  end = integer(),
  build = NULL,
  chr_prefix = "chr",
  complement = FALSE,
  canonicalize = TRUE
) {
  if (data.table::is.data.table(chr)) {
    stop(
      "`region_set()` builds regions from `chr`/`start`/`end` vectors. ",
      "To coerce a `data.table` of regions, use `as_region_set()`."
    )
  }
  new_region_set(
    chr = chr,
    start = start,
    end = end,
    build = build,
    chr_prefix = chr_prefix,
    complement = complement,
    canonicalize = canonicalize
  )
}

#' Create a new region_set object
#'
#' Constructor for region_set. Validates inputs, formats chromosome
#' names, and optionally reduces the regions to their canonical form.
#'
#' @inheritParams region_set
#'
#' @return A `region_set` object.
#' @section NA semantics:
#' A row's `chr` and `end` may be NA, meaning "unconstrained on this axis":
#' - `chr = NA` matches every chromosome.
#' - `end = NA` extends to the end of the chromosome.
#'
#' A start is always a position.
#'
#' Set operations propagate this:
#' - Intersection (`&`) of two regions where exactly one has `chr = NA`
#'   expands the NA side to the explicit chromosomes from the other
#'   (see [expand_na_chr()]); when both have `chr = NA` the NA is kept and
#'   the position constraints are intersected directly.
#' - Union (`+`/`|`) combines the rows and reduces them to the canonical
#'   form when `canonicalize`: contiguous or overlapping ranges merge,
#'   and a region with `chr = NA` absorbs the coverage of the regions it
#'   spans.
#' @keywords internal
new_region_set <- function(
  chr = character(),
  start = integer(),
  end = integer(),
  build = NULL,
  chr_prefix = "chr",
  complement = FALSE,
  canonicalize = TRUE
) {
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
    if (length(start) == 0) start <- 1L
    if (length(end) == 0) end <- NA_integer_
  }

  data.table::data.table(
    chr = format_chr(chr, prefix = chr_prefix),
    start = start,
    end = end
  ) |>
    as_region_set(
      build = build,
      complement = complement,
      canonicalize = canonicalize
    )
}

#' Validate a region_set
#'
#' Checks the invariants every region_set holds: the `chr`, `start` and
#' `end` columns, each of its own type; a start on every row that is a
#' position; and an end either unconstrained or at or after its start.
#'
#' @param gregions Object to validate
#' @return The input invisibly; stops with error if invalid
#' @keywords internal
validate_region_set <- function(
  gregions
) {
  if (!data.table::is.data.table(gregions)) {
    stop(
      "Object does not inherit the data.table class.\n",
      "Actual class: ", class(gregions)
    )
  }
  missing_cols <- setdiff(c("chr", "start", "end"), names(gregions))
  if (length(missing_cols) != 0L) {
    stop(
      "Genomic regions require columns `chr`, `start`, and `end`; ",
      "missing: ", paste(missing_cols, collapse = ", "), ".\n",
      "To build regions from vectors, use `region_set()`."
    )
  }
  wrong_type <- c(
    chr = !is.character(gregions$chr),
    start = !is.integer(gregions$start),
    end = !is.integer(gregions$end)
  )
  if (any(wrong_type)) {
    offending <- names(wrong_type)[wrong_type]
    stop(
      "A region's `chr` must be character and its `start` and `end` ",
      "integer.\nGot: ",
      paste0(
        offending, " ",
        sapply(offending, \(col) class(gregions[[col]])[1]),
        collapse = ", "
      )
    )
  }
  not_a_position <- is.na(gregions$start) | gregions$start < 1L
  if (any(not_a_position)) {
    stop(
      "A region's start must be a position of at least 1.\n",
      "Got: ", paste(gregions$start[not_a_position], collapse = ", ")
    )
  }
  if (any(gregions$end < gregions$start, na.rm = TRUE)) {
    stop("start must be less than end.")
  }
  invisible(gregions)
}

#' Create empty region_set (no regions)
#'
#' @param build Genome build identifier
#' @return Empty region_set (0 rows)
#' @keywords internal
empty_region_set <- function(
  build = NULL
) {
  new_region_set(build = build)
}

#' Create full-genome region_set (all positions)
#'
#' Returns a region_set with NA for chr and end, and start = 1.
#'
#' @param build Genome build identifier
#' @return region_set representing the entire genome
#' @keywords internal
full_region_set <- function(
  build = NULL
) {
  new_region_set(
    chr = NA_character_,
    build = build
  )
}

#' Build the general rule covering every position from a lower bound
#'
#' A bound outside the position axis resolves against it: a bound below the
#' first position is clamped to it, and a bound above every position —
#' necessarily an infinite one — covers none.
#'
#' @param start Lower bound on the positions covered
#' @param build Genome build identifier
#' @return region_set holding the one general rule from the resolved
#'   bound on, with no rows when the bound lies above every position
#' @keywords internal
general_rule_from <- function(
  start,
  build = NULL
) {
  if (is.infinite(start) && 0 < start) {
    return(empty_region_set(build = build))
  }
  new_region_set(
    start = max(start, 1L),
    build = build
  )
}

#' Build the general rule covering every position up to an upper bound
#'
#' A bound outside the position axis resolves against it: a bound below the
#' first position covers no position, and a bound above every position —
#' necessarily an infinite one — extends to the end of every chromosome.
#'
#' @param end Upper bound on the positions covered
#' @param build Genome build identifier
#' @return region_set holding the one general rule up to the resolved
#'   bound, with no rows when the bound lies below every position
#' @keywords internal
general_rule_to <- function(
  end,
  build = NULL
) {
  if (end < 1L) {
    return(empty_region_set(build = build))
  }
  new_region_set(
    end = if (is.infinite(end)) NA_integer_ else end,
    build = build
  )
}

# Utility Functions

#' Check if object is region_set
#'
#' @param x Object to test
#' @return TRUE if x inherits from region_set
#' @keywords internal
is_region_set <- function(
  x
) {
  inherits(x, "region_set")
}

#' Format chromosome names with prefix
#'
#' Ensures all chromosome names have the specified prefix (e.g., "chr").
#' A missing name is returned as NA.
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
  distinct_chr <- unique(chr)
  formatted <- data.table::fifelse(
    distinct_chr == "" | grepl(paste0("^", prefix), distinct_chr),
    distinct_chr,
    paste0(prefix, distinct_chr)
  )
  formatted[data.table::chmatch(chr, distinct_chr)]
}

#' Deep copy region_set
#'
#' @param gregions A region_set object
#' @return A copy with class preserved
#' @keywords internal
copy_region_set <- function(gregions) {
  data.table::copy(gregions) |>
    as_region_set(canonicalize = FALSE)
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

#' Check if region_set covers the whole genome
#'
#' @param gregions A region_set object
#' @return TRUE if gregions constrains no position
#' @keywords internal
is_full_genome <- function(gregions) {
  if (is_complement(gregions)) {
    return(nrow(gregions) == 0L)
  }
  nrow(gregions) == 1L &&
    is.na(gregions$chr) &&
    gregions$start == 1L &&
    is.na(gregions$end)
}

#' Check if region_set covers no position at all
#'
#' @param gregions A region_set object
#' @return TRUE if no row can satisfy gregions
#' @keywords internal
is_empty_genome <- function(gregions) {
  is_full_genome(!gregions)
}


#' @export
print.region_set <- function(
  x,
  ...
) {
  NextMethod()
  if (!is.null(build(x))) {
    cat("Build:", build(x), "\n")
  }
  if (is_complement(x)) {
    cat("Complement (excludes these regions)\n")
  } else {
    cat("Direct (includes these regions)\n")
  }
}

#' @export
str.region_set <- function(
  object,
  ...
) {
  sprintf(
    "%s{%s}",
    ifelse(is_complement(object), "!", ""),
    object[
      ,
      .(region_str = sprintf(
        "%s:%i-%s",
        ifelse(is.na(chr), "*", chr),
        start,
        ifelse(is.na(end), "", end)
      )),
      by = .I
    ][
      ,
      paste(region_str, collapse = ", ")
    ]
  )
}
