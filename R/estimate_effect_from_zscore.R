#' Estimate effect and SE from z-score, allele frequency, and sample size
#'
#' Implements the conversion from Vukcevic et al. 2011 ("Disease model
#' distortion in association studies") used by z-score-only summary
#' statistics like eQTLGen:
#'
#'   effect_se = 1 / sqrt((sample_size + zscore^2) * 2 * af * (1 - af))
#'   effect    = zscore * effect_se
#'
#' All inputs must be the same length, or length 1 to apply that single
#' value to every row. NA in any input propagates to NA on both outputs
#' for the corresponding row.
#'
#' @param zscore Numeric vector of z-scores.
#' @param af Numeric vector of allele frequencies in `[0, 1]`.
#' @param sample_size Numeric vector of per-variant sample sizes.
#'
#' @return A data.table with `effect` and `effect_se` columns.
#' @export
#' @examples
#' \dontrun{
#' # Attach results to a data.table that already carries zscore, af,
#' # and a sample-size column:
#' dt[, c("effect", "effect_se") := estimate_effect_from_zscore(
#'   zscore, af, sample_size
#' )]
#' }
estimate_effect_from_zscore <- function(zscore, af, sample_size) {
  lengths <- c(length(zscore), length(af), length(sample_size))
  unique_lengths <- setdiff(unique(lengths), 1L)
  if (length(unique_lengths) > 1) {
    stop(
      "`zscore`, `af`, and `sample_size` must be the same length or ",
      "length 1; got ", paste(lengths, collapse = ", "), "."
    )
  }

  effect_se <- 1 / sqrt(
    (sample_size + zscore^2) * 2 * af * (1 - af)
  )
  data.table::data.table(
    effect    = zscore * effect_se,
    effect_se = effect_se
  )
}
