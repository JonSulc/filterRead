#' Read a chain from the session cache
#'
#' Returns the parsed chain for a build pair, from the first of these that
#' has it:
#' - the session cache, keyed on `(from, to, chain path)`;
#' - the RDS beside the chain file, when one is there and valid;
#' - the chain file, downloaded first when it is absent and `auto_download`
#'   is TRUE, then read through [read_chain_file()] and written to the RDS.
#'
#' The returned data.table is the cached object itself, shared by every
#' caller: read it, never modify it. Callers that need their own copy call
#' [get_chain()].
#'
#' Session entries are dropped only by an explicit invalidation, so a chain
#' file replaced on disk out of band is not picked up until then.
#'
#' @param from Source build (b36, b37, b38, or synonyms like hg38)
#' @param to Target build (b36, b37, b38, or synonyms like hg38)
#' @param auto_download Logical, whether to automatically download chain
#'   files if not found. Default TRUE.
#' @return The cached chain data.table, keyed by `(chr, start, end)`.
#' @keywords internal
get_shared_chain <- function(
  from,
  to,
  auto_download = TRUE
) {
  chain_path <- get_chain_path(warn = FALSE)
  key <- paste(from, to, chain_path, sep = "\r")
  cached <- .filterread_env$chain_cache[[key]]
  if (!is.null(cached)) {
    return(cached)
  }

  rds_file <- file.path(chain_path, get_chain_rds_filename(from, to))
  chain_dt <- NULL
  if (file.exists(rds_file)) {
    from_rds <- readRDS(rds_file)
    if (data.table::is.data.table(from_rds) &&
      all(c("chr", "start", "end") %in% names(from_rds))) {
      chain_dt <- from_rds
    } else {
      warning("Invalid chain cache, regenerating: ", rds_file)
      file.remove(rds_file)
    }
  }
  if (is.null(chain_dt)) {
    ucsc_file <- file.path(chain_path, get_chain_filename(from, to))
    if (!file.exists(ucsc_file)) {
      if (!auto_download) {
        stop(
          sprintf("Chain file not found: %s\n", ucsc_file),
          "Use setup_chain_files() to download, or ",
          "set_chain_path() to configure the directory."
        )
      }
      message(sprintf(
        "Chain file not found. Downloading %s to %s chain...",
        from, to
      ))
      setup_chain_files(
        builds = sprintf("%s_to_%s", from, to),
        path = chain_path
      )
      # setup_chain_files parses what it downloads and writes the RDS, so
      # the chain is already on disk in its parsed form.
      if (file.exists(rds_file)) {
        chain_dt <- readRDS(rds_file)
      }
    }
    if (is.null(chain_dt)) {
      chain_dt <- read_chain_file(from, to) |>
        set_chain_index()
      tryCatch(
        saveRDS(chain_dt, rds_file),
        error = function(e) {
          warning("Failed to cache chain file as RDS: ", e$message)
        }
      )
    }
  }

  chain_dt <- set_chain_index(chain_dt)
  assign(key, chain_dt, envir = .filterread_env$chain_cache)
  chain_dt
}

#' Set the index the overlap join reads
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
set_chain_index <- function(chain_dt) {
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
