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
