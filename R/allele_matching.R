# =============================================================================
# Allele Matching Functions
# =============================================================================
# Functions for use when the file does not include the actual effect/non-effect
# alleles, instead listing allele1, allele2, and effect allele separately,
# where non-effect allele must then be deduced from which of allele1/2 does not
# match the effect allele.

#' Add information to column_info for allele matching
#'
#' Modify the columns of column_info to include the relevant information for
#' awk-based matching of allele1/allele2 as effect allele and non-effect
#' allele. This is done by reference and column_info is returned invisibly.
#'
#' @param column_info data.table with column metadata
#'
#' @param return data.table with modified data (invisible).
#' @keywords internal
add_allele_matching_to_column_info <- function(
  column_info
) {
  if (!needs_a1_a2_to_ref_matching(column_info)) {
    return(column_info)
  }

  # Set up ref/alt derivation from allele1/allele2
  column_info[
    standard_name == "alt",
    encoded_names := .(c("ref", "alt"))
  ][
    standard_name == "alt",
    split_encoding_column := match_a1_a2_to_ref(
      a1_bash_index = column_info[standard_name == "allele1", bash_index],
      a2_bash_index = column_info[standard_name == "allele2", bash_index],
      alt_bash_index = column_info[standard_name == "alt", bash_index]
    )
  ][
    standard_name == "alt",
    recode_columns := sprintf(
      "%s = nea OFS %s",
      column_info[standard_name == "alt", bash_index],
      column_info[standard_name == "alt", bash_index]
    )
  ][] |>
    invisible()
}

#' Check if allele matching is needed
#'
#' @param column_info data.table with column metadata
#'
#' @return TRUE if file has allele1, allele2, alt but no ref
#' @keywords internal
needs_a1_a2_to_ref_matching <- function(
  column_info
) {
  all(c("allele1", "allele2", "alt") %in% column_info$standard_name) &
    !"ref" %in% column_info$standard_name
}

#' Generate awk code for allele matching
#'
#' Creates awk if/else block that determines the non-effect allele (nea)
#' by comparing effect allele (alt) to allele1/allele2.
#'
#' @param a1_bash_index Awk column ref for allele1
#' @param a2_bash_index Awk column ref for allele2
#' @param alt_bash_index Awk column ref for alt (effect allele)
#'
#' @return Awk code string for allele matching
#' @keywords internal
match_a1_a2_to_ref <- function(
  a1_bash_index,
  a2_bash_index,
  alt_bash_index
) {
  sprintf(
    "# Deduce NEA based on EA vs Allele1/Allele2
if (%s == %s) {
  nea = %s
} else {
  nea = %s
}",
    a1_bash_index,
    alt_bash_index,
    a2_bash_index,
    a1_bash_index
  )
}
