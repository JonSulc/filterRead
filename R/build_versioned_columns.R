#' Record the current coordinates under the current build
#'
#' Duplicates each column listed in `columns` into `<col>_<build>`,
#' so downstream merges between data.tables in different builds can
#' key on the same coordinate system (e.g. `chr_b38`, `pos_b38`,
#' `variant_id_b38`). Columns absent from `dt` are skipped silently;
#' suffixed columns that already exist are left untouched. Modifies
#' `dt` by reference.
#'
#' @param dt A data.table.
#' @param build Build name used as the suffix. Defaults to `build(dt)`.
#' @param columns Character vector of column names to duplicate.
#'   Default `c("chr", "pos", "variant_id", "ref", "alt")`.
#' @return `dt` (invisibly).
#' @export
record_build <- function(
  dt,
  build   = NULL,
  columns = c("chr", "pos", "variant_id", "ref", "alt")
) {
  stopifnot(data.table::is.data.table(dt))
  if (is.null(build)) {
    build <- build(dt)
  }
  if (is.null(build)) {
    stop(
      "Cannot add build-versioned columns without a build. Set ",
      "`build(dt) <- \"b37\"` first or pass `build` explicitly."
    )
  }
  build <- normalize_build(build, allow_null = FALSE)
  # Restore over-allocation so the column adds below land in place;
  # upstream `build(dt) <- ...` calls can zero the truelength even
  # when selfref stays valid, which otherwise forces a shallow copy
  # and breaks the by-reference contract for callers that don't
  # capture the return.
  data.table::setalloccol(dt)
  for (col in columns) {
    if (!col %in% names(dt)) {
      next
    }
    suffix_name <- paste0(col, "_", build)
    if (suffix_name %in% names(dt)) {
      next
    }
    dt[, (suffix_name) := get(col)]
  }
  invisible(dt)
}

#' Read a coordinate or allele column as recorded for a build
#'
#' Returns column `base` holding `build`'s values: the `<base>_<build>`
#' suffixed column when present, the canonical `base` column when `build` is
#' `dt`'s current build, else `NULL`. Read-side counterpart to [record_build()].
#'
#' @param dt A data.table.
#' @param base Unsuffixed column name (e.g. "chr", "pos", "ref", "alt").
#' @param build Build whose values to read.
#' @return The column vector, or `NULL` if not recorded.
#' @keywords internal
build_versioned_column <- function(dt, base, build) {
  suffixed <- paste0(base, "_", build)
  if (suffixed %in% names(dt)) {
    return(dt[[suffixed]])
  }
  if (identical(build, build(dt))) {
    return(dt[[base]])
  }
  NULL
}

#' Construct variant_id from chr / pos / ref / alt
#'
#' Adds a `variant_id` column shaped as `chr_pos_ref_alt_<build>`
#' (e.g. `chr1_12345_A_G_b38`). Rows where `chr` or `pos` is NA get
#' `NA` for `variant_id`. Modifies `dt` by reference.
#'
#' @param dt A data.table with `chr`, `pos`, `ref`, `alt` columns.
#' @param build Build name used in the suffix. Defaults to `build(dt)`.
#' @param overwrite If FALSE (default) and `variant_id` already exists,
#'   leave it alone. If TRUE, recompute.
#' @return `dt` (invisibly).
#' @export
add_variant_id <- function(
  dt,
  build     = NULL,
  overwrite = FALSE
) {
  stopifnot(data.table::is.data.table(dt))
  if (is.null(build)) {
    build <- build(dt)
  }
  if (is.null(build)) {
    stop("Cannot construct variant_id without a build.")
  }
  build <- normalize_build(build, allow_null = FALSE)
  required <- c("chr", "pos", "ref", "alt")
  missing_cols <- setdiff(required, names(dt))
  if (length(missing_cols) > 0) {
    stop(
      "`dt` is missing column(s) required for variant_id: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  if ("variant_id" %in% names(dt) && !overwrite) {
    return(invisible(dt))
  }
  dt[
    ,
    variant_id := data.table::fifelse(
      is.na(chr) | is.na(pos),
      NA_character_,
      sprintf("%s_%s_%s_%s_%s", chr, pos, ref, alt, build)
    )
  ]
  invisible(dt)
}
