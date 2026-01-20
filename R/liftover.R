#' @importFrom rtracklayer import.chain

get_chain_dt <- function(
  from,
  to
) {
  build_names <- c(
    b36 = "hg18",
    b37 = "hg19",
    b38 = "hg38"
  )
  sprintf(
    "~/rcp_storage/common/Users/sulc/data/ucsc/%sTo%s.over.chain",
    build_names[from],
    build_names[to] |>
      stringr::str_to_title()
  ) |>
    make_chain_from_file() |>
    get_full_chain_dt() |>
    data.table::setattr("from", from) |>
    data.table::setattr("build", to) |>
    data.table::setattr("to", to)
}


make_chain_from_file <- function(
  path
) {
  rtracklayer::import.chain(path)
}

get_chr_chain_dt <- function(chain, chr) {
  data.table::as.data.table(chain[[chr]]@ranges) |>
    cbind(data.table::data.table(
      chr = chr,
      offset = chain[[chr]]@offset,
      new_chr = rep(
        chain[[chr]]@space,
        chain[[chr]]@length
      ),
      rev = rep(
        chain[[chr]]@reversed,
        chain[[chr]]@length
      )
    )) |>
    data.table::setkey(chr, start, end) |>
    data.table::setcolorder()
}

get_full_chain_dt <- function(chain) {
  chain_dt <- lapply(names(chain), get_chr_chain_dt, chain = chain) |>
    data.table::rbindlist() |>
    data.table::setkey(chr, start, end)
  chain_dt
}

liftover <- function(
  x,
  ...
) {
  UseMethod("liftover")
}

# =============================================================================
# Build Attribute Accessors
# =============================================================================

#' Get genome build from an object
#'
#' S3 generic for extracting the genome build (e.g., "b37", "b38") from
#' various object types used in filterRead.
#'
#' @param x Object to get build from
#' @return Build string or NULL if not set
#' @export
build <- function(x) {
  UseMethod("build")
}

#' @export
build.default <- function(x) {
  attr(x, "build")
}

#' @export
build.character <- function(x) {
  x
}

#' @export
build.list <- function(x) {
  x$build
}

#' @export
build.file_interface <- build.list

#' Set genome build on an object
#'
#' Replacement function for setting the genome build attribute.
#' Uses data.table::setattr for data.tables (modifies by reference).
#'
#' @param x Object to modify
#' @param value Build string to set (e.g., "b37", "b38")
#' @return Modified object
#' @export
`build<-` <- function(x, value) {
  UseMethod("build<-")
}

#' @export
`build<-.default` <- function(x, value) {
  attr(x, "build") <- value
  x
}

#' @export
`build<-.data.table` <- function(x, value) {
  data.table::setattr(x, "build", value)
  x
}

#' @export
`build<-.list` <- function(x, value) {
  x$build <- value
  x
}

#' @export
`build<-.file_interface` <- `build<-.list`
