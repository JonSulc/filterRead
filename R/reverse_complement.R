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
  rc_one <- function(allele) {
    if (is.na(allele)) {
      return(NA_character_)
    }
    bases <- strsplit(toupper(allele), "", fixed = TRUE)[[1]]
    paste(chartr("ACGT", "TGCA", rev(bases)), collapse = "")
  }
  vapply(alleles, rc_one, character(1), USE.NAMES = FALSE)
}
