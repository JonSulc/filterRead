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
  build <- normalize_build(build, allow_null = FALSE, allow_unsupported = TRUE)
  # Add provenance columns by reference with set(). Do not setalloccol() first:
  # on a table with spare slots it forces a reallocation, so the columns land on
  # a copy a non-capturing caller never sees. set() adds in place when slots are
  # free; at truelength 0 it reallocates (without the := shallow-copy warning),
  # which is safe here because those callers capture the return value.
  for (col in columns) {
    if (!col %in% names(dt)) {
      next
    }
    suffix_name <- paste0(col, "_", build)
    if (suffix_name %in% names(dt)) {
      next
    }
    data.table::set(dt, j = suffix_name, value = dt[[col]])
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

#' Decode variant_id strings into coordinate columns
#'
#' Inverse of [add_variant_id()]: decodes `chr_pos_ref_alt_<build>` strings
#' into a table of `chr`, `pos`, `ref`, `alt`, and `build`. The build is any
#' non-`_` tag (validity is enforced only at liftover). An element that does
#' not match the structure yields an all-NA row.
#'
#' @param x Character vector of variant ids.
#' @return A `data.table` with columns `chr`, `pos` (integer), `ref`, `alt`,
#'   and `build`, one row per element of `x`.
#' @export
parse_variant_id <- function(x) {
  x <- as.character(x)
  pattern <- "^chr[0-9XYMTxymt]+_[0-9]+_[^_]+_[^_]+_[^_]+$"
  out <- data.table::data.table(
    chr   = rep(NA_character_, length(x)),
    pos   = rep(NA_integer_,   length(x)),
    ref   = rep(NA_character_, length(x)),
    alt   = rep(NA_character_, length(x)),
    build = rep(NA_character_, length(x))
  )
  valid <- !is.na(x) & grepl(pattern, x)
  if (any(valid)) {
    parts <- data.table::tstrsplit(x[valid], "_", fixed = TRUE)
    out[valid, c("chr", "pos", "ref", "alt", "build") := .(
      parts[[1]], as.integer(parts[[2]]), parts[[3]], parts[[4]], parts[[5]]
    )]
  }
  out[]
}

#' Build variant_id values from coordinate vectors
#'
#' Shared formatter for the `chr_pos_ref_alt_<build>` id. Rows where any
#' component (`chr`, `pos`, `ref`, `alt`, `build`) is NA get `NA`.
#'
#' @param chr,pos,ref,alt Coordinate vectors of equal length.
#' @param build Build suffix.
#' @return Character vector of ids.
#' @keywords internal
variant_id_values <- function(chr, pos, ref, alt, build) {
  data.table::fifelse(
    is.na(chr) | is.na(pos) | is.na(ref) | is.na(alt) | is.na(build),
    NA_character_,
    sprintf("%s_%s_%s_%s_%s", chr, pos, ref, alt, build)
  )
}

#' Construct variant_id from chr / pos / ref / alt
#'
#' Adds a `variant_id` column shaped as `chr_pos_ref_alt_<build>`
#' (e.g. `chr1_12345_A_G_b38`). Rows where any of `chr`, `pos`, `ref`, or `alt`
#' is NA get `NA` for `variant_id`. Modifies `dt` by reference.
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
  build <- normalize_build(build, allow_null = FALSE, allow_unsupported = TRUE)
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
    variant_id := variant_id_values(chr, pos, ref, alt, build)
  ]
  invisible(dt)
}
