#' Canonical identity columns of a variant
#'
#' The columns that define a variant's identity, independent of build
#' provenance. Keying, deduplication, and equality use these and ignore
#' the build-suffixed provenance columns.
#'
#' @return Character vector `c("chr", "pos", "ref", "alt")`.
#' @keywords internal
coordinate_columns <- function() {
  c("chr", "pos", "ref", "alt")
}

#' (Re)apply the `variants` class to a data.table in place
#'
#' @param x A data.table.
#' @return `x` with class `c("variants", "data.table", "data.frame")`.
#' @keywords internal
set_variants_class <- function(x) {
  data.table::setattr(
    x, "class",
    c("variants", class(data.table::data.table())) |> unique()
  )
  x
}

#' Construct a build-aware variants table
#'
#' Wraps a data.table of variants (`chr`, `pos`, `ref`, `alt`) as a
#' `variants`: an S3-classed data.table carrying a `build` attribute, a
#' `chr_pos_ref_alt_<build>` `variant_id`, per-build provenance columns, and
#' a `defining_build` column recording the build each variant was declared in.
#' Liftover always sources from that defining build, so every target is
#' derived directly from it, independent of any intermediate lifts. An
#' existing `defining_build` column is preserved (e.g. when re-wrapping
#' variants assembled from mixed builds).
#'
#' @param x A data.table with `chr`, `pos`, `ref`, `alt` columns.
#' @param build Genome build (`"b36"`, `"b37"`, `"b38"`).
#' @return A `variants` object (S3-classed data.table).
#' @export
new_variants <- function(x, build = build(x)) {
  stopifnot(data.table::is.data.table(x))
  missing_cols <- setdiff(coordinate_columns(), names(x))
  if (length(missing_cols) > 0) {
    stop(
      "`x` is missing column(s) required for a variants table: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  if (is.null(build)) {
    stop("Cannot construct variants without a build.")
  }
  build <- normalize_build(build, allow_null = FALSE)
  v <- data.table::copy(x)
  build(v) <- build
  if (!"defining_build" %in% names(v)) {
    v[, defining_build := build]
  }
  v <- add_variant_id(v, build = build)
  v <- record_build(v, build = build)
  set_variants_class(v)
}

#' Coerce an object to variants
#'
#' @param x Object to coerce (currently a data.table).
#' @param build Genome build; defaults to `build(x)`.
#' @param ... Unused.
#' @return A `variants` object.
#' @export
as_variants <- function(x, build = build(x), ...) {
  UseMethod("as_variants")
}

#' @rdname as_variants
#' @export
as_variants.variants <- function(x, build = build(x), ...) {
  x
}

#' @rdname as_variants
#' @export
as_variants.data.table <- function(x, build = build(x), ...) {
  new_variants(x, build = build)
}

#' @export
build.variants <- function(x) {
  attr(x, "build")
}

#' @export
`build<-.variants` <- function(x, value) {
  data.table::setattr(x, "build", normalize_build(value, allow_null = FALSE))
  x
}

#' Lift variants to another genome build
#'
#' Dispatches to the allele-aware data.table liftover and records provenance
#' (`retain_builds = TRUE`); an already-recorded build is restored from its
#' provenance columns. Lifts from the current build.
#'
#' @param x A `variants`.
#' @param target Target build.
#' @param ... Passed to [liftover.data.table()] (e.g. `from`, `multi_match`).
#' @return A `variants` in the target build.
#' @export
liftover.variants <- function(x, target, ...) {
  lifted <- liftover.data.table(
    x,
    target = target,
    retain_builds = TRUE,
    ...
  )
  set_variants_class(lifted)
}

#' @export
unique.variants <- function(x, ...) {
  result <- data.table::as.data.table(x)[
    !duplicated(x, by = coordinate_columns())
  ]
  data.table::setattr(result, "build", attr(x, "build"))
  set_variants_class(result)
}

#' @export
print.variants <- function(x, ...) {
  cat(sprintf(
    "<variants: %d row(s), build %s>\n",
    nrow(x), attr(x, "build") %||% "unset"
  ))
  NextMethod()
}
