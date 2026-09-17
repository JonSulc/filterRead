#' Check if a table holds a variant set's coordinate columns
#'
#' Each of `chr`, `pos`, `ref` and `alt` present exactly once, `pos` integer
#' and the others character.
#'
#' @param dt A data.table.
#' @return `TRUE` where `dt` is one, `FALSE` otherwise.
#' @keywords internal
is_variant_coordinate_table <- function(dt) {
  named <- names(dt)[names(dt) %in% coordinate_columns()]
  # `named` carries every occurrence, so `identical()` rejects a table naming
  # a coordinate column twice, as cbind of two variant sets does.
  identical(sort(named), sort(coordinate_columns())) &&
    is.character(dt[["chr"]]) &&
    is.integer(dt[["pos"]]) &&
    is.character(dt[["ref"]]) &&
    is.character(dt[["alt"]])
}

#' Subset a variant set
#'
#' A result holding the coordinate columns keeps the `variant_set` class and
#' `variants`' build; one that does not comes back a plain data.table with no
#' build. A `j` returning an atomic vector returns that vector as it stands.
#'
#' @param variants A `variant_set`.
#' @param ... Forwarded to `[.data.table`.
#' @return The subset, a `variant_set` or a data.table.
#' @export
`[.variant_set` <- function(variants, ...) {
  result <- NextMethod()
  if (data.table::is.data.table(result)) {
    if (is_variant_coordinate_table(result)) {
      reclass_as_variant_set(result)
      data.table::setattr(result, "build", build(variants))
    } else {
      data.table::setattr(result, "class", class(data.table::data.table()))
      data.table::setattr(result, "build", NULL)
    }
  }
  result
}

#' Bind columns onto a variant set
#'
#' A result holding the coordinate columns keeps the `variant_set` class and
#' the first bound set's build; one that does not comes back a plain
#' data.table.
#'
#' @param ... Tables and vectors to bind, at least one a `variant_set`.
#' @param keep.rownames,check.names,key,stringsAsFactors Forwarded to
#'   `cbind.data.table`.
#' @return The bound table, a `variant_set` or a data.table.
#' @export
cbind.variant_set <- function(..., keep.rownames = FALSE, check.names = FALSE,
                              key = NULL, stringsAsFactors = FALSE) {
  result <- getS3method("cbind", "data.table")(
    ...,
    keep.rownames = keep.rownames,
    check.names = check.names,
    key = key,
    stringsAsFactors = stringsAsFactors
  )
  if (is_variant_coordinate_table(result)) {
    bound_variants <- Filter(
      \(part) inherits(part, "variant_set"), list(...)
    )
    reclass_as_variant_set(result)
    data.table::setattr(result, "build", build(bound_variants[[1]]))
  }
  result
}
