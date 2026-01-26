# =============================================================================
# Genomic Regions
# =============================================================================
# This module provides the genomic_regions class for representing and
# manipulating genomic coordinate ranges. Key features:
#
# - Represents regions as data.table with chr, start, end columns
# - Supports set operations: union (|/+), intersection (&)
# - Handles full-genome queries (NA values mean "any")
# - Liftover between genome builds (e.g., b37 -> b38)
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

#' Validate a genomic_regions data.table
#'
#' Checks that the object is a data.table.
#'
#' @param genomic_regions_dt Object to validate
#' @return The input invisibly; stops with error if invalid
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
  if (!is.null(build(x))) {
    cat("Build:", build(x))
  }
}

#' @export
str.genomic_regions <- function(
  x,
  ...
) {
  sprintf(
    "{%s}",
    x[
      ,
      .(region_str = sprintf(
        "%s:%i-%s",
        ifelse(is.na(chr), "*", chr),
        ifelse(is.na(start), 1, start),
        ifelse(is.na(end), "", end)
      )),
      by = .I
    ][
      ,
      paste(region_str, collapse = ", ")
    ]
  )
}
