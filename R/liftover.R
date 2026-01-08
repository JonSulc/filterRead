#' @import data.table
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
    get_full_chain_dt()
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
