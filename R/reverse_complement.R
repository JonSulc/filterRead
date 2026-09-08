#' Reverse-complement allele strings
#'
#' Reverses each allele and complements its bases (A<->T, C<->G), so a
#' value expressed on one strand becomes its representation on the other.
#' Used when lifting variants across regions that are inverted between
#' genome builds. Values are upper-cased first; bases outside `ACGT`
#' (e.g. `N`, indel placeholders) are reversed in place but left
#' uncomplemented, and `NA` is preserved.
#'
#' @param alleles Character vector of alleles (single- or multi-base).
#' @return Character vector of reverse-complemented alleles.
#' @keywords internal
reverse_complement <- function(alleles) {
  complemented <- chartr("ACGT", "TGCA", toupper(alleles))
  # Reversal is a no-op on single bases, so only multi-base values are split.
  # nchar(NA_character_) is NA, so without the explicit NA guard the mask
  # carries NA and the subscripted assignment below errors.
  multi_base <- !is.na(complemented) & 1L < nchar(complemented)
  complemented[multi_base] <- complemented[multi_base] |>
    strsplit("", fixed = TRUE) |>
    vapply(\(bases) paste(rev(bases), collapse = ""), character(1))
  complemented
}
