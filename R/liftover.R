#' @import data.table
#' @importFrom rtracklayer import.chain

convert_to_build <- function(
  genomic_ranges,
  from,
  to,
  chain_dt = get_chain_dt(from, to)
) {
  if (to == from) {
    return(genomic_ranges)
  }

  liftover_genomic_ranges(
    genomic_ranges,
    chain_dt
  )
}

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


liftover_genomic_ranges <- function(
  genomic_ranges,
  chain_dt
) {
  if (nrow(genomic_ranges) == 0 || all(genomic_ranges[, is.na(chr)])) {
    return(genomic_ranges)
  }

  if (!is.character(genomic_ranges$chr)) {
    genomic_ranges[
      ,
      chr := as.character(chr)
    ][]
  }
  genomic_ranges[
    !grepl("^chr", chr),
    chr := paste0("chr", chr)
  ][]

  genomic_ranges[
    ,
    data.table::foverlaps(
      .SD,
      chain_dt
    )[
      ,
      .(
        chr = new_chr,
        start = data.table::fifelse(
          rev,
          end - offset - (i.start - start),
          i.start - offset
        ),
        end = data.table::fifelse(
          rev,
          end - offset - (i.end - start),
          i.end - offset
        )
      )
    ]
  ]
  genomic_ranges[
    ,
    data.table::foverlaps(
      .SD,
      chain_dt
    )
  ][
    ,
    .(
      chr = new_chr,
      start = start,
      end = end,
      offset = offset,
      rev = rev,
      i.start = pmax(start, i.start),
      i.end = pmin(end, i.end)
    )
  ][
    ,
    .(
      chr = chr,
      start = data.table::fifelse(
        rev,
        end - offset - (i.end - start),
        i.start - offset
      ),
      end = data.table::fifelse(
        rev,
        end - offset - (i.start - start),
        i.end - offset
      )
    )
  ]
}
