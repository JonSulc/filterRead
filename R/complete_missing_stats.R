#' Derive missing summary-statistics columns
#'
#' Adds the missing column among `effect`, `effect_se`, and `pval` when
#' it can be inferred from the others:
#'
#' - If `effect` is missing but `odds_ratio` and `pval` are present,
#'   `effect = log(odds_ratio) * sqrt(3) / pi` (PMID 11113947), then
#'   `effect_se = -effect / qnorm(pval / 2)`.
#' - If `effect_se` is missing but `effect` and `pval` are present,
#'   `effect_se = -abs(effect) / qnorm(pval / 2)`.
#' - If `pval` is missing but `effect` and `effect_se` are present,
#'   `pval = 2 * pnorm(-abs(effect / effect_se))`.
#'
#' When no derivation is possible or none is needed, `dt` is returned
#' unchanged and no message is emitted.
#'
#' Modifies `dt` by reference and returns it.
#'
#' @param dt A data.table with standardized column names.
#'
#' @return `dt`, with derived columns added.
#' @export
complete_missing_stats <- function(dt) {
  if (!data.table::is.data.table(dt)) {
    stop("`dt` must be a data.table.")
  }
  cols <- names(dt)
  if (all(c("effect", "effect_se", "pval") %in% cols)) {
    return(dt)
  }
  if (!"effect" %in% cols) {
    if (!all(c("odds_ratio", "pval") %in% cols)) {
      return(dt)
    }
    message(
      "Approximating effect from odds_ratio (PMID: 11113947) and ",
      "deriving effect_se from p-value."
    )
    dt[, effect := log(odds_ratio) * sqrt(3) / pi][
      ,
      effect_se := -effect / stats::qnorm(pval / 2)
    ][]
    return(dt)
  }
  if (!"effect_se" %in% cols) {
    if (!"pval" %in% cols) {
      return(dt)
    }
    dt[, effect_se := -abs(effect) / stats::qnorm(pval / 2)][]
    return(dt)
  }
  dt[, pval := 2 * stats::pnorm(-abs(effect / effect_se))][]
}
