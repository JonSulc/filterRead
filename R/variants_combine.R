#' Bind a list of variants into one, reconciling builds
#'
#' Coerces each input with [as_variants()], lifts each input not already in
#' `target` (the first input's build unless given), stacks them, and
#' re-establishes the `variants` via [new_variants()]. `source`/`multi_match`
#' are forwarded to [liftover.variants()]; `dots` are forwarded to
#' [data.table::rbindlist()]. `target` is canonicalized leniently so a
#' non-native target (e.g. `dahu42`) is allowed; an input already in `target`
#' is used as-is (so binding non-native variants needs no liftover).
#'
#' @keywords internal
bind_variants_impl <- function(parts, target, source, multi_match, fill,
                               dots) {
  if (length(parts) == 0) {
    if (is.null(target)) {
      stop(
        "Cannot bind an empty list without `target` ",
        "(no build to assign)."
      )
    }
    return(new_variants(data.table::data.table(), build = target))
  }
  parts <- lapply(parts, as_variants)
  target <- if (is.null(target)) {
    build(parts[[1]])
  } else {
    normalize_build(target, allow_null = FALSE, allow_unsupported = TRUE)
  }
  lifted <- lapply(parts, function(v) {
    if (identical(build(v), target)) {
      v
    } else {
      liftover.variants(v, target = target, source = source,
                        multi_match = multi_match)
    }
  })
  bound <- do.call(
    data.table::rbindlist,
    c(list(lifted, fill = fill), dots)
  )
  new_variants(bound, build = target)
}

#' Combine variants row-wise into a single variants
#'
#' Row-binds a list of `variants` (or data.tables coercible with
#' [as_variants()]). Inputs in a build other than `target` are lifted to it;
#' `target` defaults to the first input's build. Per-row `defining_build` is
#' carried through unchanged. An empty `x` returns a 0-row `variants` in
#' `target`, which must then be supplied (there is no input build to inherit).
#'
#' @param x A list of `variants` / data.tables.
#' @param target Build to express the result in; defaults to the first input's.
#' @param source,multi_match Forwarded to [liftover.variants()].
#' @param fill Forwarded to [data.table::rbindlist()]; defaults to `TRUE` so
#'   ragged provenance/domain columns reconcile (the lone divergence from
#'   `rbindlist`, whose default is `FALSE`).
#' @param ... Other [data.table::rbindlist()] options (`idcol`, `use.names`).
#' @return A `variants` in `target`.
#' @export
rbindlist_variants <- function(x, target = NULL, source = "best",
                               multi_match = "first", fill = TRUE, ...) {
  bind_variants_impl(x, target, source, multi_match, fill, list(...))
}

#' Row-bind variants (pairwise/variadic convenience)
#'
#' @param ... `variants` to bind.
#' @param target Build to express the result in; defaults to the first input's.
#' @param source,multi_match Forwarded to [liftover.variants()].
#' @return A `variants` in `target`.
#' @export
rbind.variants <- function(..., target = NULL, source = "best",
                           multi_match = "first") {
  bind_variants_impl(list(...), target, source, multi_match,
                     fill = TRUE, dots = list())
}
