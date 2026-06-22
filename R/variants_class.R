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
  x[]
}

#' Construct a build-aware variants table
#'
#' Wraps a data.frame or data.table of variants (`chr`, `pos`, `ref`, `alt`)
#' as a `variants`: an S3-classed data.table carrying a `build` attribute, a
#' `chr_pos_ref_alt_<build>` `variant_id`, per-build provenance columns, and
#' a `defining_build` column recording the build each variant was declared in.
#' Liftover always sources from that defining build, so every target is
#' derived directly from it, independent of any intermediate lifts. An
#' existing `defining_build` column is preserved (e.g. when re-wrapping
#' variants assembled from mixed builds).
#'
#' A 0-row input that omits some or all coordinate columns has those columns
#' scaffolded as empty typed vectors (`integer` for `pos`, `character`
#' otherwise). A non-empty input with missing coordinate columns still errors.
#'
#' @param x A data.frame (or data.table) with `chr`, `pos`, `ref`, `alt`
#'   columns, or a 0-row frame that may omit them.
#' @param build Genome build string; native builds are `"b36"`, `"b37"`,
#'   `"b38"` (and their synonyms). Non-native builds are accepted and
#'   carried through, but cannot be lifted over.
#' @return A `variants` object (S3-classed data.table).
#' @export
new_variants <- function(x, build = NULL) {
  stopifnot(is.data.frame(x))
  if (is.null(build)) {
    build <- build(x)
  }
  x <- if (data.table::is.data.table(x)) {
    data.table::copy(x)
  } else {
    data.table::as.data.table(x)
  }
  if ("variant_id" %in% names(x)) {
    return(new_variants_from_variant_id(x, build))
  }
  missing_cols <- setdiff(coordinate_columns(), names(x))
  if (length(missing_cols) > 0) {
    if (nrow(x) > 0) {
      stop(
        "`x` is missing column(s) required for a variants table: ",
        paste(missing_cols, collapse = ", ")
      )
    }
    x <- scaffold_coordinate_columns(x, missing_cols)
  }
  finalize_variants(x, build)
}

#' Add empty type-correct coordinate columns to a 0-row table
#'
#' @keywords internal
scaffold_coordinate_columns <- function(x, cols) {
  for (col in cols) {
    x[, (col) := if (col == "pos") integer() else character()]
  }
  x
}

#' Stamp build, defining_build, variant_id, and provenance, then class
#'
#' Shared construction tail. `v` must already be a private copy with the
#' coordinate columns present. The build need not be native (only liftover
#' requires that), so it is canonicalized leniently.
#'
#' @keywords internal
finalize_variants <- function(v, build) {
  if (is.null(build)) {
    stop("Cannot construct variants without a build.")
  }
  build <- normalize_build(build, allow_null = FALSE, allow_unsupported = TRUE)
  set_build(v, build)
  if (!"defining_build" %in% names(v)) {
    v[, defining_build := build]
  }
  v <- add_variant_id(v, build = build)
  v <- record_build(v, build = build)
  set_variants_class(v)
}

#' Construct variants from a table carrying a variant_id column
#'
#' Parses coordinates from `variant_id`, validates any present coordinates and
#' the embedded build against it, backfills the rest, and fills NA ids. A
#' supplied id is never clobbered. The build need not be native. `new_variants`
#' has already coerced and copied `x`, so this mutates it in place.
#'
#' @keywords internal
new_variants_from_variant_id <- function(x, build) {
  parsed <- parse_variant_id(x$variant_id)
  embedded <- unique(parsed$build[!is.na(parsed$build)])
  if (is.null(build) && 1 < length(embedded)) {
    return(combine_by_embedded_build(x, parsed))
  }
  resolved <- if (!is.null(build)) {
    normalize_build(build, allow_null = FALSE, allow_unsupported = TRUE)
  } else if (length(embedded) == 1) {
    normalize_build(embedded, allow_null = FALSE, allow_unsupported = TRUE)
  } else {
    stop("Cannot construct variants without a build.")
  }
  check_variant_id(x, resolved, parsed = parsed)
  for (col in coordinate_columns()) {
    decoded <- parsed[[col]]
    if (col %in% names(x)) {
      # Coerce to the parsed column's canonical type (integer pos, else
      # character) so fcoalesce -- which requires matching types -- accepts a
      # double pos or a factor coordinate.
      existing <- if (col == "pos") {
        as.integer(x[[col]])
      } else {
        as.character(x[[col]])
      }
      x[, (col) := data.table::fcoalesce(decoded, existing)]
    } else {
      x[, (col) := decoded]
    }
  }
  computed <- variant_id_values(x$chr, x$pos, x$ref, x$alt, resolved)
  x[, variant_id := data.table::fcoalesce(variant_id, computed)]
  finalize_variants(x, resolved)
}

#' Split a mixed-build variant_id table by build and recombine
#'
#' Each row is constructed in its embedded build (rows with an unparseable id
#' default to the first parseable row's build), then the per-build groups are
#' recombined through [rbindlist_variants()] targeting the first parseable
#' row's build, preserving input row order. Each row keeps its embedded build
#' as `defining_build`.
#'
#' @keywords internal
combine_by_embedded_build <- function(x, parsed) {
  target <- parsed$build[!is.na(parsed$build)][1]
  row_build <- parsed$build
  row_build[is.na(row_build)] <- target
  idx <- split(seq_len(nrow(x)), row_build)
  parts <- lapply(names(idx), function(b) new_variants(x[idx[[b]]], build = b))
  combined <- rbindlist_variants(parts, target = target)
  # Restore input row order: split() sorts groups alphabetically, so combined
  # has rows in the order of unlist(idx). Compute the inverse permutation.
  original_order <- order(unlist(idx, use.names = FALSE))
  combined[original_order]
}

#' Coerce an object to variants
#'
#' @param x A data.frame, data.table, or tibble to coerce.
#' @param build Genome build; defaults to `build(x)`.
#' @param ... Unused.
#' @return A `variants` object.
#' @export
as_variants <- function(x, build = NULL, ...) {
  UseMethod("as_variants")
}

#' @rdname as_variants
#' @export
as_variants.variants <- function(x, build = NULL, ...) {
  x
}

#' @rdname as_variants
#' @export
as_variants.data.frame <- function(x, build = NULL, ...) {
  new_variants(x, build = build)
}

#' Set a variants' genome build by reference
#'
#' Records the (normalized) genome build in the `build` attribute in place.
#' Unlike `build(x) <- value`, this is a plain side-effecting setter, so it does
#' not bump the object's reference count and is safe to call immediately before
#' a `:=`. Use it to relabel an existing `variants` without lifting coordinates.
#' It is the canonical in-place build setter for the variants core.
#'
#' @param x A variants (or data.table).
#' @param value Build name or synonym (non-native tags pass through).
#' @return `x`, invisibly.
#' @export
set_build <- function(x, value) {
  data.table::setattr(
    x, "build",
    normalize_build(value, allow_null = FALSE, allow_unsupported = TRUE)
  )
  invisible(x)
}

#' @export
build.variants <- function(x) {
  attr(x, "build")
}

#' @export
`build<-.variants` <- function(x, value) {
  set_build(x, value)
  x
}

#' Builds for which a variants table carries coordinates
#'
#' The current build (canonical columns) plus any build with a
#' `chr_<build>` provenance column present.
#'
#' @param x A `variants`.
#' @return Character vector of build names.
#' @keywords internal
recorded_builds <- function(x) {
  suffixed <- .valid_builds[paste0("chr_", .valid_builds) %in% names(x)]
  unique(c(build(x), suffixed))
}

#' A build's values for a coordinate or allele column
#'
#' Build `b`'s values from [build_versioned_column()], or a type-correct NA
#' vector (integer for `pos`, character otherwise) when `b` is not recorded.
#' Always returns a vector of length `nrow(x)`.
#'
#' @param x A `variants`.
#' @param base One of "chr", "pos", "ref", "alt".
#' @param b A build name.
#' @return A vector of length `nrow(x)`.
#' @keywords internal
build_column <- function(x, base, b) {
  recorded <- build_versioned_column(x, base, b)
  if (!is.null(recorded)) {
    return(recorded)
  }
  if (base == "pos") {
    rep(NA_integer_, nrow(x))
  } else {
    rep(NA_character_, nrow(x))
  }
}

#' Lift a group of variant rows from one source build
#'
#' Sets the rows' canonical coordinates to source build `from`'s recorded
#' columns and lifts them to `target`, carrying every other column through and
#' fanning out under `multi_match = "all"`. Chain-gap rows are kept with NA
#' coordinates. `from = NA` returns the rows with NA coordinates.
#'
#' @param rows A data.table of variant rows that share a source build.
#' @param from Source build, or NA for rows with no usable source.
#' @param target Target build.
#' @param multi_match Forwarded to [liftover.data.table()].
#' @return The rows lifted to `target`, all columns preserved.
#' @keywords internal
lift_one_source <- function(rows, from, target, multi_match) {
  rows <- data.table::copy(rows)
  if (is.na(from)) {
    for (col in coordinate_columns()) {
      rows[, (col) := if (col == "pos") NA_integer_ else NA_character_]
    }
    return(rows)
  }
  for (col in coordinate_columns()) {
    rows[, (col) := build_column(rows, col, from)]
  }
  set_build(rows, from)
  liftover.data.table(
    rows,
    target        = target,
    drop_unlifted = FALSE,
    multi_match   = multi_match,
    retain_builds = FALSE,
    use_cache     = FALSE
  )
}

#' Choose each row's source build for liftover
#'
#' For `source = "best"` the source is the highest-precedence build with
#' non-NA coordinates: an already-recorded target, the
#' `defining_build`, the current build, then the newest other recorded build.
#' For `"current"` or a build name it is that single build for every row (NA
#' where the row has no coordinates in it). Reads recorded coordinates only;
#' performs no liftover.
#'
#' @param x A `variants`.
#' @param target Target build.
#' @param source `"best"`, `"current"`, or a build name.
#' @return Character vector of length `nrow(x)`; each element a build name
#'   or NA.
#' @keywords internal
select_source_build <- function(x, target, source) {
  if (!identical(source, "best")) {
    s <- if (identical(source, "current")) {
      build(x)
    } else {
      normalize_build(source, allow_null = FALSE)
    }
    defined <- !is.na(build_column(x, "chr", s)) &
      !is.na(build_column(x, "pos", s))
    return(ifelse(defined, s, NA_character_))
  }

  source_of <- rep(NA_character_, nrow(x))

  target_cols <- paste0(c("chr", "pos"), "_", target)
  if (all(target_cols %in% names(x))) {
    have <- !is.na(x[[target_cols[1]]]) & !is.na(x[[target_cols[2]]])
    source_of[have] <- target
  }

  if ("defining_build" %in% names(x)) {
    for (s in unique(x$defining_build[!is.na(x$defining_build)])) {
      defined <- !is.na(x$defining_build) & x$defining_build == s &
        !is.na(build_column(x, "chr", s)) & !is.na(build_column(x, "pos", s))
      source_of[is.na(source_of) & defined] <- s
    }
  }

  others <- setdiff(recorded_builds(x), c(target, build(x)))
  others <- others[order(match(others, .valid_builds), decreasing = TRUE)]
  for (s in c(build(x), others)) {
    defined <- !is.na(build_column(x, "chr", s)) &
      !is.na(build_column(x, "pos", s))
    source_of[is.na(source_of) & defined] <- s
  }
  source_of
}

#' Lift variants to another genome build, selecting the source per row
#'
#' With `source = "best"` (default) each row is lifted from its defining
#' build -- the `defining_build` it was declared in -- so a b37-declared variant
#' lifts from b37 even when currently expressed in another build. Lifting from
#' the defining build makes the result independent of intermediate lifts
#' (`b38 -> b37 -> b38 -> b36` equals `b38 -> b36`) and round-trip stable.
#' The source is the highest-precedence build with non-NA coordinates: an
#' already-recorded target, else the
#' `defining_build`, else the current build, else the newest other
#' recorded build. A row whose source has no mapping at the target is left NA.
#' `source = "current"` lifts every row from the current build.
#'
#' `multi_match = "first"` (default) gives one output row per input variant;
#' `"all"` keeps every target position the chosen source maps a variant to,
#' fanning those out in place. `liftover.variants` manages the lift parameters
#' itself; for raw liftover options use [liftover.data.table()] directly.
#'
#' @param x A `variants`.
#' @param target Target build.
#' @param source Which build to lift each row from: `"best"` (default; the
#'   `defining_build`, chosen per row with fallback), `"current"`
#'   (the current build), or a build name (`"b36"`/`"b37"`/`"b38"` or a
#'   synonym) to force that build for every row.
#' @param multi_match How a position that maps to several target positions is
#'   resolved: `"first"` (default, one row per input variant), `"last"`, or
#'   `"all"` (one output row per target position the source maps to).
#' @param ... Must be empty; `liftover.variants` manages the lift parameters.
#'   Use [liftover.data.table()] directly for raw liftover options.
#' @return A `variants` in the target build. With `multi_match = "all"` a
#'   variant that maps to several positions contributes several rows,
#'   inserted in input order immediately after one another.
#' @export
liftover.variants <- function(x, target, source = "best",
                             multi_match = c("first", "last", "all"), ...) {
  multi_match <- match.arg(multi_match)
  target <- normalize_build(target, allow_null = FALSE)
  if (...length() > 0L) {
    stop(
      "liftover.variants forwards no extra arguments; `source` selects the ",
      "build to lift from. Use liftover.data.table() directly for raw ",
      "liftover options."
    )
  }
  if (source %in% c("best", "current") && identical(build(x), target)) {
    return(x)
  }

  # Tag each row with its input position and chosen source, then lift each
  # source's rows together with `by = .src`; full rows carry all columns and
  # fan-out through `liftover.data.table`. `.anchor` restores input order.
  work <- data.table::copy(x)
  work[, c(".anchor", ".src") := .(
    seq_len(.N), select_source_build(x, target, source)
  )]

  result <- work[
    ,
    lift_one_source(.SD, .BY$.src, target, multi_match),
    by = .src
  ]
  data.table::setorder(result, .anchor)
  result[, c(".anchor", ".src") := NULL]

  set_build(result, target)
  result <- add_variant_id(result, build = target, overwrite = TRUE)
  result <- record_build(result, build = target)
  set_variants_class(result)
}

#' @export
unique.variants <- function(x, ...) {
  result <- data.table::as.data.table(x)[
    !duplicated(x, by = coordinate_columns())
  ]
  set_build(result, build(x))
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
