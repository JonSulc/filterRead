#' Require the columns needed for variant-identity filtering
#'
#' `finterface[v]` matches on `chr`, `pos`, `ref`, and `alt`, and errors when
#' any is absent. Each must map to a real input column: a coordinate available
#' only via an encoded parent (e.g. a `chr:pos` field split into virtual
#' `chr`/`pos` with no real column of their own) is rejected, because the
#' codegen does not split such a column, so a match would return no rows; a
#' coordinate that also has its own real column is accepted. rsid-indexed files,
#' which lack `chr`/`pos`, are rejected too.
#'
#' @param finterface A file_interface object.
#' @return `finterface` invisibly; stops with a clear error if unsupported.
#' @keywords internal
require_variant_columns <- function(finterface) {
  if (needs_rsid_matching(finterface)) {
    stop(
      "`finterface[v]` is not supported for rsid-indexed files ",
      "(no chr/pos columns).",
      call. = FALSE
    )
  }
  required <- c("chr", "pos", "ref", "alt")
  missing_cols <- setdiff(required, column_names(finterface))
  if (length(missing_cols) != 0L) {
    stop(
      "`finterface[v]` requires the file to expose chr, pos, ref, alt; ",
      "missing: ", paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  # Virtual columns synthesized from an encoded parent have no input column of
  # their own, so their `input_name` is NA while a real column's is set.
  encoded_cols <- finterface$column_info[
    standard_name %in% required & is.na(input_name),
    standard_name
  ]
  if (length(encoded_cols) != 0L) {
    stop(
      "`finterface[v]` does not yet support variant filtering over encoded ",
      "coordinate columns; these arrive via an encoded column: ",
      paste(encoded_cols, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(finterface)
}

#' Build the position set and both-orderings key set for a variants object
#'
#' Lifts `variants` to `build`, then formats each `(chr, pos, ref, alt)`
#' component to match the file's on-disk representation (chr prefix, allele
#' upper-casing, quoting) via [check_post_processing()]. Rows missing any
#' component (an unmapped `pos` after the lift, or a missing allele) cannot form
#' a key and are dropped, with a `warning`. Both allele orderings
#' (`chr_pos_ref_alt` and `chr_pos_alt_ref`) are emitted so the awk side stays
#' one key, one lookup for efficiency.
#'
#' @param variants A `variants` object.
#' @param finterface A file_interface object.
#' @param build Target build; `variants` is lifted to it (a no-op when it is
#'   already in that build).
#' @return A list with `positions` (unique `pos` values) and `keys`
#'   (unique formatted keys, both allele orderings).
#' @keywords internal
variant_keyset <- function(variants, finterface, build) {
  components <- liftover(variants, build)[
    , .(chr, pos, ref, alt)
  ][
    , Map(
      \(values, column) check_post_processing(
        values, column, finterface, to_write = TRUE
      ),
      .SD, names(.SD)
    )
  ]

  n_before <- nrow(components)
  components <- na.omit(components)
  if (nrow(components) < n_before) {
    warning(
      n_before - nrow(components), " variant(s) dropped from matching due ",
      "to missing chr/pos/ref/alt (e.g. unmapped position).",
      call. = FALSE
    )
  }

  list(
    positions = unique(components$pos),
    keys = unique(c(
      components[, paste(chr, pos, ref, alt, sep = "_")],
      components[, paste(chr, pos, alt, ref, sep = "_")]
    ))
  )
}

#' Awk expression that builds a row's composite variant key
#'
#' @param finterface A file_interface object.
#' @return A string like `$1 "_" $2 "_" $3 "_" $4`.
#' @keywords internal
variant_key_expr <- function(finterface) {
  finterface$column_info[
    c("chr", "pos", "ref", "alt"),
    paste(bash_index, collapse = ' "_" '),
    on = "standard_name"
  ]
}

#' `new_filter_condition_impl` method for variant-identity filtering
#'
#' Lowers `v` to a position-membership pre-filter AND a composite-key
#' membership test (both allele orderings), matching file rows on
#' `(chr, pos, {ref, alt})` after lifting `v` to the file's build. `context` is
#' the normalized context env built by `as_fc_context()` in the public
#' `new_filter_condition()` wrapper, so the interface is read from
#' `context$finterface`, mirroring `new_filter_condition_impl.genomic_regions`.
#'
#' @keywords internal
#' @export
new_filter_condition_impl.variants <- function(x, context, build = NULL, ...) {
  finterface <- context$finterface
  build <- build %||% build(finterface) %||% build(x)

  require_variant_columns(finterface)
  keyset <- variant_keyset(x, finterface, build)

  if (length(keyset$keys) == 0) {
    return(empty_filter_condition(
      build = build,
      genomic_regions = empty_genomic_regions(build = build),
      context = context
    ))
  }

  pos_expr <- finterface$column_info[standard_name == "pos", bash_index]
  key_expr <- variant_key_expr(finterface)

  # The position pre-filter is an optimization: awk `&&` short-circuits, so the
  # per-row `$1 "_" $2 "_" $3 "_" $4` key string is built only for rows whose
  # position is in `posset`, skipping the string concatenation for rows at no
  # candidate position.
  in_membership_atom(pos_expr, keyset$positions, context, build) &
    in_membership_atom(key_expr, keyset$keys, context, build)
}
