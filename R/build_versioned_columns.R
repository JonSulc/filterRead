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
  build <- build %||% build(dt)
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
#' Inverse of [add_variant_id()]: `chr_pos_ref_alt_<build>` decodes into its
#' five components. Only the chromosome may hold `_`, so a name such as
#' `1_KI270706v1_random` decodes whole. The chromosome comes back bare, the
#' build tag as the id writes it, and an element that does not match the
#' structure decodes to an all-NA row.
#'
#' @param variant_ids Character vector of variant ids.
#' @return A `data.table` with columns `chr`, `pos` (integer), `ref`, `alt`,
#'   and `build`, one row per element of `variant_ids`.
#' @export
parse_variant_id <- function(variant_ids) {
  variant_ids <- as.character(variant_ids)
  chr_is_underscore_free <- grepl(
    "^[^_]+_[0-9]+_[^_]+_[^_]+_[^_]+$", variant_ids, perl = TRUE
  )
  decoded <- if (all(chr_is_underscore_free) && length(variant_ids) != 0L) {
    parse_underscore_free_chr_variant_id(variant_ids)
  } else if (!any(chr_is_underscore_free)) {
    parse_underscored_chr_variant_id(variant_ids)
  } else {
    data.table::rbindlist(list(
      parse_underscore_free_chr_variant_id(
        variant_ids[chr_is_underscore_free]
      ),
      parse_underscored_chr_variant_id(variant_ids[!chr_is_underscore_free])
    ))[order(c(
      which(chr_is_underscore_free),
      which(!chr_is_underscore_free)
    ))]
  }
  decoded[, chr := drop_prefix(chr)][]
}

#' Decode variant ids whose chromosome holds no `_`
#'
#' Every `_` in an id separates two components.
#'
#' @param variant_ids Character vector of variant ids.
#' @return A `data.table` with columns `chr`, `pos` (integer), `ref`, `alt`,
#'   and `build`, one row per element of `variant_ids`.
#' @keywords internal
parse_underscore_free_chr_variant_id <- function(variant_ids) {
  data.table::tstrsplit(
    variant_ids, "_",
    fixed = TRUE,
    type.convert = list(as.integer = 2L),
    names = c("chr", "pos", "ref", "alt", "build")
  ) |>
    data.table::setDT()
}

#' Decode variant ids whose chromosome may hold `_`
#'
#' The last four `_`-separated components are the position, the reference
#' allele, the alternate allele and the build; everything before them is the
#' chromosome. An id that does not fit decodes to an all-NA row.
#'
#' @param variant_ids Character vector of variant ids.
#' @return A `data.table` with columns `chr`, `pos` (integer), `ref`, `alt`,
#'   and `build`, one row per element of `variant_ids`.
#' @keywords internal
parse_underscored_chr_variant_id <- function(variant_ids) {
  decoded <- data.table::data.table(
    chr   = rep(NA_character_, length(variant_ids)),
    pos   = rep(NA_integer_,   length(variant_ids)),
    ref   = rep(NA_character_, length(variant_ids)),
    alt   = rep(NA_character_, length(variant_ids)),
    build = rep(NA_character_, length(variant_ids))
  )
  trailing_components_start <- regexpr(
    "_[0-9]+_[^_]+_[^_]+_[^_]+$", variant_ids, perl = TRUE
  )
  decodable <- !is.na(trailing_components_start) &
    1L < trailing_components_start
  if (any(decodable)) {
    decodable_ids <- variant_ids[decodable]
    chr_separator_position <- trailing_components_start[decodable]
    trailing_components <- data.table::tstrsplit(
      substring(decodable_ids, chr_separator_position + 1L), "_", fixed = TRUE
    )
    decoded[decodable, c("chr", "pos", "ref", "alt", "build") := .(
      substr(decodable_ids, 1L, chr_separator_position - 1L),
      as.integer(trailing_components[[1]]),
      trailing_components[[2]],
      trailing_components[[3]],
      trailing_components[[4]]
    )]
  }
  decoded[]
}

#' Build variant_id values from coordinate vectors
#'
#' Shared formatter for the `chr_pos_ref_alt_<build>` id. Variants where any
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

#' Validate a variant_id column against coordinates and build
#'
#' For every variant whose `variant_id` parses (see [parse_variant_id()]),
#' each non-NA `chr`/`pos`/`ref`/`alt` value must equal the decoded component
#' and the embedded build must equal `build` (compared after canonicalizing
#' known synonyms). NA coordinate cells and variants with an unparseable
#' `variant_id` impose no constraint. Errors listing the offending variants;
#' otherwise returns `x` invisibly.
#'
#' @param x A data.table with a `variant_id` column.
#' @param build Build the ids are expected to embed. Defaults to `build(x)`.
#' @param parsed Optional pre-computed [parse_variant_id()] result for
#'   `x$variant_id`, to avoid re-parsing on a hot path.
#' @return `x`, invisibly.
#' @export
validate_variant_id <- function(x, build = NULL, parsed = NULL) {
  stopifnot(data.table::is.data.table(x), "variant_id" %in% names(x))
  build <- build %||% build(x)
  parsed <- parsed %||% parse_variant_id(x$variant_id)
  parseable <- !is.na(parsed$build)
  problems <- character(0)
  if (!is.null(build)) {
    build <- normalize_build(
      build, allow_null = FALSE, allow_unsupported = TRUE
    )
    embedded <- normalize_build(
      parsed$build[parseable],
      allow_null = FALSE, allow_unsupported = TRUE
    )
    bad <- embedded != build
    if (any(bad)) {
      problems <- c(problems, sprintf(
        "build: variant(s) %s embed %s, expected %s",
        paste(which(parseable)[bad], collapse = ", "),
        paste(unique(embedded[bad]), collapse = "/"), build
      ))
    }
  }
  for (col in coordinate_columns()) {
    if (!col %in% names(x)) {
      next
    }
    present <- !is.na(x[[col]])
    bad <- parseable & present & x[[col]] != parsed[[col]]
    if (any(bad)) {
      problems <- c(problems, sprintf(
        "%s: variant(s) %s disagree with variant_id",
        col, paste(which(bad), collapse = ", ")
      ))
    }
  }
  if (length(problems) > 0) {
    stop(
      "variant_id inconsistent with coordinates:\n  ",
      paste(problems, collapse = "\n  ")
    )
  }
  invisible(x)
}

#' Construct variant_id from chr / pos / ref / alt
#'
#' Adds a `variant_id` column shaped as `chr_pos_ref_alt_<build>`
#' (e.g. `1_12345_A_G_b38`). Variants where any of `chr`, `pos`, `ref`, or
#' `alt` is NA get `NA` for `variant_id`. Modifies `dt` by reference.
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
  build <- build %||% build(dt)
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
  ][]
  invisible(dt)
}
