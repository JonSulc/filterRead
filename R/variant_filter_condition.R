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
