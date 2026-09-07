#' Read a chain from the session cache
#'
#' Returns the parsed chain for a build pair, loading it through
#' [load_chain_dt()]'s file and RDS handling the first time the pair is asked
#' for under the current chain directory. The returned data.table is the
#' cached object itself, shared by every caller: read it, never modify it.
#' Callers that need their own copy call [get_chain_dt()].
#'
#' Entries are keyed on `(from, to, chain path)` and are dropped only by an
#' explicit invalidation, so a chain file replaced on disk out of band is not
#' picked up until then.
#'
#' @param from Source build (b36, b37, b38, or synonyms like hg38)
#' @param to Target build (b36, b37, b38, or synonyms like hg38)
#' @param auto_download Logical, whether to automatically download chain
#'   files if not found. Default TRUE.
#' @return The cached chain data.table, keyed by `(chr, start, end)`.
#' @keywords internal
get_shared_chain_dt <- function(
  from,
  to,
  auto_download = TRUE
) {
  key <- paste(from, to, get_chain_path(warn = FALSE), sep = "\r")
  cached <- .filterread_env$chain_cache[[key]]
  if (!is.null(cached)) {
    return(cached)
  }
  chain_dt <- load_chain_dt(from, to, auto_download = auto_download) |>
    prepare_chain_dt()
  assign(key, chain_dt, envir = .filterread_env$chain_cache)
  chain_dt
}

#' Ready a chain for the overlap join
#'
#' Repairs the internal attributes a data.table read back from RDS carries,
#' and puts a secondary index on the block coordinates: the overlap join reads
#' an index rather than the key, and without one it re-sorts every block on
#' each call. Applied where a chain is written to RDS and again where one
#' enters the session cache, so a chain file written before the index existed
#' is readied on load.
#'
#' @param chain_dt A chain data.table.
#' @return `chain_dt`, invisibly.
#' @keywords internal
prepare_chain_dt <- function(chain_dt) {
  data.table::setDT(chain_dt)
  data.table::setindexv(chain_dt, c("chr", "start", "end"))
  invisible(chain_dt)
}

#' Drop every chain held in the session cache
#'
#' The next read of a build pair loads it from disk again.
#'
#' @return NULL, invisibly.
#' @keywords internal
invalidate_chain_cache <- function() {
  .filterread_env$chain_cache <- new.env(parent = emptyenv())
  invisible(NULL)
}
