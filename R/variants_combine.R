#' Bind a list of variant sets into one, reconciling builds
#'
#' Coerces each input with [as_variant_set()], lifts each input not already in
#' `target` (the first input's build unless given), stacks them, and
#' re-establishes the `variant_set` via [new_variant_set()]. `source`/
#' `multi_match` are forwarded to [liftover.variant_set()]; `dots` are
#' forwarded to [data.table::rbindlist()]. `target` is canonicalized
#' leniently so a non-native target (e.g. `dahu42`) is allowed; an input
#' already in `target` is used as-is (so binding non-native variants needs
#' no liftover).
#'
#' @keywords internal
bind_variant_sets_impl <- function(parts, target, source, multi_match, fill,
                                   dots) {
  if (length(parts) == 0) {
    if (is.null(target)) {
      stop(
        "Cannot bind an empty list without `target` ",
        "(no build to assign)."
      )
    }
    return(new_variant_set(data.table::data.table(), build = target))
  }
  parts <- lapply(parts, as_variant_set)
  target <- normalize_build(
    target %||% build(parts[[1]]),
    allow_null = FALSE, allow_unsupported = TRUE
  )
  lifted <- lapply(parts, function(v) {
    if (identical(build(v), target)) {
      v
    } else {
      liftover.variant_set(v, target = target, source = source,
                           multi_match = multi_match)
    }
  })
  bound <- do.call(
    data.table::rbindlist,
    c(list(lifted, fill = fill), dots)
  )
  new_variant_set(bound, build = target)
}

#' Combine variants row-wise into a single variant_set
#'
#' Row-binds a list of variant sets (or data.tables coercible with
#' [as_variant_set()]). Inputs in a build other than `target` are lifted to it;
#' `target` defaults to the first input's build. Per-row `defining_build` is
#' carried through unchanged. An empty `x` returns a 0-row `variant_set` in
#' `target`, which must then be supplied (there is no input build to inherit).
#'
#' @param x A list of variant sets / data.tables.
#' @param target Build to express the result in; defaults to the first input's.
#' @param source,multi_match Forwarded to [liftover.variant_set()].
#' @param fill Forwarded to [data.table::rbindlist()]; defaults to `TRUE` so
#'   ragged provenance/domain columns reconcile (the lone divergence from
#'   `rbindlist`, whose default is `FALSE`).
#' @param ... Other [data.table::rbindlist()] options (`idcol`, `use.names`).
#' @return A `variant_set` in `target`.
#' @export
rbindlist_variant_sets <- function(x, target = NULL, source = "best",
                                   multi_match = "first", fill = TRUE, ...) {
  bind_variant_sets_impl(x, target, source, multi_match, fill, list(...))
}

#' Row-bind variants (pairwise/variadic convenience)
#'
#' Binds with `fill = TRUE`; use [rbindlist_variant_sets()] for control over
#' `fill` and other [data.table::rbindlist()] options.
#'
#' @param ... Variant sets to bind.
#' @param target Build to express the result in; defaults to the first input's.
#' @param source,multi_match Forwarded to [liftover.variant_set()].
#' @return A `variant_set` in `target`.
#' @export
rbind.variant_set <- function(..., target = NULL, source = "best",
                              multi_match = "first") {
  bind_variant_sets_impl(list(...), target, source, multi_match,
                         fill = TRUE, dots = list())
}
