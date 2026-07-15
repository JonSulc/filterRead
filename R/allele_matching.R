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
#' @return data.table with modified data (invisible).
#' @keywords internal
add_allele_matching_to_column_info <- function(
  column_info
) {
  if (!needs_a1_a2_to_ref_matching(column_info)) {
    return(column_info)
  }

  # Set up ref derivation from allele1/allele2. The alt column is kept as the
  # alt output column; a single ref child (the deduced non-effect allele) is
  # appended after it, referencing the nea variable computed by the split.
  column_info[
    standard_name == "alt",
    encoded_names := .(c("ref"))
  ][
    standard_name == "alt",
    encoded_refs := .(c("nea"))
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
      "%s = %s OFS nea",
      column_info[standard_name == "alt", bash_index],
      column_info[standard_name == "alt", bash_index]
    )
  ][] |>
    invisible()
}

#' Resolve ref/alt when both an encoded allele pair and effect allele exist
#'
#' MetaBrain-style files carry the unordered allele pair in one encoded
#' column (`SNPAlleles` -> ref/alt) and the effect allele in a separate
#' column (`SNPEffectAllele` -> alt). Both targeting `alt` would emit a
#' duplicate column. The effect allele is authoritative and the order
#' within the pair is not guaranteed, so `alt` is taken from the effect
#' allele and `ref` from the other member. This appends an effect-allele
#' normalization to the encoded column's split so the decoded array holds
#' `(ref, alt) = (non-effect, effect)`, and clears the standalone
#' effect-allele column's `alt` mapping so the value is not emitted twice.
#'
#' Applies only when there is exactly one encoded ref/alt column and exactly
#' one standalone `alt` column; otherwise column_info is returned unchanged.
#'
#' @param column_info data.table with column metadata, before encoded-column
#'   expansion.
#' @return column_info (modified by reference), invisibly.
#' @keywords internal
add_effect_allele_to_encoded_pair <- function(
  column_info
) {
  is_encoded_ref_alt <- sapply(
    column_info$encoded_names,
    \(names) setequal(names, c("ref", "alt"))
  )
  is_effect_alt <- column_info$standard_name == "alt" &
    sapply(column_info$encoded_names, is.null)
  is_effect_alt[is.na(is_effect_alt)] <- FALSE

  if (sum(is_encoded_ref_alt) != 1 || sum(is_effect_alt) != 1) {
    return(invisible(column_info))
  }

  effect_index <- column_info$bash_index[is_effect_alt]
  array_name <- paste0(
    "encoded",
    column_info$encoded_column_index[is_encoded_ref_alt]
  )

  column_info[
    is_encoded_ref_alt,
    split_encoding_column := paste(
      split_encoding_column,
      order_alleles_by_effect(array_name, effect_index),
      sep = "\n"
    )
  ]
  column_info[is_effect_alt, standard_name := NA_character_]
  invisible(column_info)
}

#' Generate awk to order a decoded allele pair as (ref, alt)
#'
#' Swaps the two elements of the decoded array when the first equals the
#' effect allele, so element 1 is the non-effect (ref) allele and element 2
#' the effect (alt) allele. Both sides are upper-cased so the comparison is
#' case-insensitive.
#'
#' @param array_name Name of the awk array holding the split allele pair.
#' @param effect_index Awk column reference for the effect allele.
#' @return Awk code string.
#' @keywords internal
order_alleles_by_effect <- function(
  array_name,
  effect_index
) {
  sprintf(
    "# Order decoded alleles as (ref, alt) using the effect allele
if (toupper(%s[1]) == toupper(%s)) {
  effect_tmp = %s[1]
  %s[1] = %s[2]
  %s[2] = effect_tmp
}",
    array_name, effect_index,
    array_name, array_name, array_name, array_name
  )
}

#' Check if allele matching is needed
#'
#' Allele matching is needed when the file carries `allele1`, `allele2`,
#' and `alt` but no source of `ref` — neither as a native column nor
#' produced by an encoded column (e.g. SNPAlleles → ref + alt).
#'
#' @param column_info data.table with column metadata
#'
#' @return TRUE if file has allele1, allele2, alt and no source of ref.
#' @keywords internal
needs_a1_a2_to_ref_matching <- function(
  column_info
) {
  ref_sources <- c(
    column_info$standard_name,
    unlist(column_info$encoded_names)
  )
  all(c("allele1", "allele2", "alt") %in% column_info$standard_name) &&
    !"ref" %in% ref_sources
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
