#' Get chain filename for a build conversion
#'
#' @param from Source build (b36, b37, b38, or synonyms like hg38)
#' @param to Target build (b36, b37, b38, or synonyms like hg38)
#' @return Chain filename (uncompressed)
#' @keywords internal
get_chain_filename <- function(from, to) {
  from <- normalize_build(from, allow_null = FALSE)
  to <- normalize_build(to, allow_null = FALSE)
  sprintf(
    "%sTo%s.over.chain",
    .build_to_ucsc[from],
    .build_to_ucsc[to] |>
      stringr::str_to_title()
  )
}

#' Get RDS cache filename for a chain file
#'
#' @param from Source build (b36, b37, b38, or synonyms like hg38)
#' @param to Target build (b36, b37, b38, or synonyms like hg38)
#' @return RDS cache filename using internal build names
#' @keywords internal
get_chain_rds_filename <- function(from, to) {
  from <- normalize_build(from, allow_null = FALSE)
  to <- normalize_build(to, allow_null = FALSE)
  sprintf("%s_to_%s.chain_dt.rds", from, to)
}

#' Get chain file download URLs and filenames
#'
#' @param from Source build (b36, b37, b38, or synonyms like hg38)
#' @param to Target build (b36, b37, b38, or synonyms like hg38)
#' @return List with url, filename, and uncompressed filename
#' @keywords internal
get_chain_downloads <- function(from, to) {
  from <- normalize_build(from, allow_null = FALSE)
  to <- normalize_build(to, allow_null = FALSE)
  uncompressed <- get_chain_filename(from, to)
  filename <- paste0(uncompressed, ".gz")

  url <- sprintf(
    "https://hgdownload.soe.ucsc.edu/goldenPath/%s/liftOver/%s",
    .build_to_ucsc[from],
    filename
  )

  list(
    url = url,
    filename = filename,
    uncompressed = uncompressed
  )
}

#' Download UCSC chain files for liftover
#'
#' Downloads chain files from UCSC for genome build conversion.
#' Files are stored in the directory returned by get_chain_path().
#'
#' @param builds Character vector of build pairs to download, e.g.,
#'   c("b37_to_b38", "b38_to_b37"). Default downloads all common pairs.
#' @param path Directory to store chain files. Defaults to get_chain_path().
#' @param force Logical, re-download even if files exist.
#' @return Invisibly returns path to chain file directory.
#' @export
#' @examples
#' \dontrun{
#' # Download all common chain files
#' setup_chain_files()
#'
#' # Download specific pair
#' setup_chain_files(builds = "b37_to_b38")
#' }
setup_chain_files <- function(
  builds = c(
    "b37_to_b38",
    "b38_to_b37",
    "b36_to_b37",
    "b37_to_b36",
    "b36_to_b38",
    "b38_to_b36"
  ),
  path = get_chain_path(warn = FALSE),
  force = FALSE
) {
  # Create directory if needed
  if (!dir.exists(path)) {
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
  }

  # Parse build pairs
  build_pairs <- lapply(builds, function(b) {
    parts <- strsplit(b, "_to_")[[1]]
    list(from = parts[1], to = parts[2])
  })

  failed <- character(0)

  for (pair in build_pairs) {
    downloads <- get_chain_downloads(pair$from, pair$to)
    dest_file <- file.path(path, downloads$uncompressed)
    gz_file <- file.path(path, downloads$filename)

    # Skip if exists and not forcing
    if (file.exists(dest_file) && !force) {
      message(sprintf("Chain file exists: %s", downloads$uncompressed))
      next
    }

    message(sprintf("Downloading: %s", downloads$filename))

    success <- tryCatch(
      {
        download_with_retry(
          url = downloads$url,
          dest_file = gz_file,
          max_tries = 3
        )
        # Decompress
        message("  Decompressing...")
        R.utils::gunzip(gz_file, destname = dest_file, remove = TRUE)

        # Parse and cache as RDS
        message("  Caching parsed chain file...")
        from_build <- normalize_build(pair$from)
        to_build <- normalize_build(pair$to)
        chain_dt <- parse_chain_file(dest_file) |>
          data.table::setattr("from", from_build) |>
          data.table::setattr("build", to_build) |>
          data.table::setattr("to", to_build)
        rds_file <- file.path(
          path,
          get_chain_rds_filename(from_build, to_build)
        )
        saveRDS(chain_dt, rds_file)

        message("  Complete")
        TRUE
      },
      error = function(e) {
        warning(sprintf(
          "Failed to download %s: %s",
          downloads$filename,
          e$message
        ))
        # Clean up partial files
        if (file.exists(gz_file)) file.remove(gz_file)
        if (file.exists(dest_file)) file.remove(dest_file)
        FALSE
      }
    )

    if (!success) {
      failed <- c(failed, sprintf("%s_to_%s", pair$from, pair$to))
    }
  }

  # Report results
  if (length(failed) > 0) {
    warning(
      "Failed to download chain files:\n",
      paste("  -", failed, collapse = "\n"),
      "\n\nYou can manually download from:\n",
      "  https://hgdownload.soe.ucsc.edu/goldenPath/{build}/liftOver/"
    )
  }

  invisible(path)
}

#' Clear cached chain file RDS files
#'
#' Removes all cached RDS files for chain data, forcing re-parsing
#' on next use. Useful after package updates that change the chain
#' file parsing format.
#'
#' @param path Directory containing chain files.
#'   Defaults to get_chain_path().
#' @return Invisibly returns paths of removed files.
#' @export
clear_chain_cache <- function(path = get_chain_path(warn = FALSE)) {
  rds_files <- list.files(path, "\\.chain_dt\\.rds$", full.names = TRUE)
  if (length(rds_files) > 0) {
    file.remove(rds_files)
    message("Removed ", length(rds_files), " cached chain files")
  } else {
    message("No cached chain files found")
  }
  invisible(rds_files)
}

#' Get chain data.table for liftover
#'
#' Loads or creates a chain data.table for coordinate conversion between
#' genome builds. Checks for cached RDS first, falls back to parsing the
#' raw chain file, and auto-downloads if needed.
#'
#' @param from Source build (b36, b37, b38, or synonyms like hg38)
#' @param to Target build (b36, b37, b38, or synonyms like hg38)
#' @param auto_download Logical, whether to automatically download chain
#'   files if not found. Default TRUE.
#' @return data.table with chain alignment data, keyed by (chr, start, end).
#'   Has attributes: from, to, build (same as to).
#' @keywords internal
get_chain_dt <- function(
  from,
  to,
  auto_download = TRUE
) {
  chain_path <- get_chain_path(warn = FALSE)
  chain_file <- file.path(chain_path, get_chain_filename(from, to))
  rds_file <- file.path(chain_path, get_chain_rds_filename(from, to))

  # Fast path: load from RDS cache
  if (file.exists(rds_file)) {
    chain_dt <- readRDS(rds_file)
    if (data.table::is.data.table(chain_dt) &&
      all(c("chr", "start", "end") %in% names(chain_dt))) {
      return(chain_dt)
    }
    warning("Invalid chain cache, regenerating: ", rds_file)
    file.remove(rds_file)
  }

  # Check for raw chain file
  if (!file.exists(chain_file)) {
    if (auto_download) {
      message(sprintf(
        "Chain file not found. Downloading %s to %s chain...",
        from, to
      ))
      setup_chain_files(
        builds = sprintf("%s_to_%s", from, to),
        path = chain_path
      )
      # setup_chain_files now creates RDS, so reload from cache
      if (file.exists(rds_file)) {
        return(readRDS(rds_file))
      }
    } else {
      stop(
        sprintf("Chain file not found: %s\n", chain_file),
        "Use setup_chain_files() to download, or ",
        "set_chain_path() to configure the directory."
      )
    }
  }

  # Parse chain file and cache as RDS
  chain_dt <- chain_file |>
    parse_chain_file() |>
    data.table::setattr("from", from) |>
    data.table::setattr("build", to) |>
    data.table::setattr("to", to)

  tryCatch(
    saveRDS(chain_dt, rds_file),
    error = function(e) {
      warning("Failed to cache chain file as RDS: ", e$message)
    }
  )

  chain_dt
}


#' Convert a single chain block to data.table format
#'
#' Parses alignment blocks from a UCSC chain file and computes coordinate
#' mappings. Handles both forward and reverse strand alignments.
#'
#' @param chain_header Named list with chain header fields (tName, tStart,
#'   tEnd, qName, qStart, qEnd, qStrand, qSize, etc.)
#' @param blocks List of character vectors, each containing alignment block
#'   data (width, dt, dq triplets)
#' @return data.table with columns: start, end, width, chr, offset, new_chr,
#'   rev
#' @keywords internal
make_single_chain_dt <- function(
  chain_header,
  blocks
) {
  if (length(blocks) == 1) {
    chain_dt <- data.table::data.table(
      width = as.integer(blocks[[1]]),
      dt = NA_integer_,
      dq = NA_integer_
    )
  } else {
    chain_dt <- rbindlist(
      blocks |>
        sapply(as.integer) |>
        lapply(as.list),
      fill = TRUE,
      use.names = FALSE
    ) |>
      setNames(c("width", "dt", "dq"))
  }

  chain_dt[
    ,
    start := as.integer(chain_header$tStart) +
      cumsum(shift(dt, n = 1L, type = "lag", fill = 0)) +
      cumsum(shift(width, n = 1L, type = "lag", fill = 0)) +
      1
  ][
    ,
    end := as.integer(chain_header$tStart) +
      cumsum(width) +
      cumsum(shift(dt, n = 1L, type = "lag", fill = 0))
  ][
    ,
    chr := chain_header$tName
  ][
    ,
    new_chr := chain_header$qName
  ][
    ,
    rev := chain_header$qStrand == "-"
  ][
    ,
    offset := if (!rev[[1]]) {
      as.integer(chain_header$tStart) -
        as.integer(chain_header$qStart) +
        cumsum(shift(dt, n = 1L, type = "lag", fill = 0)) -
        cumsum(shift(dq, n = 1L, type = "lag", fill = 0))
    } else {
      as.integer(chain_header$tStart) -
        (as.integer(chain_header$qSize) - as.integer(chain_header$qStart)) +
        cumsum(width) +
        cumsum(shift(width, n = 1L, type = "lag", fill = 0)) +
        cumsum(shift(dt, n = 1L, type = "lag", fill = 0)) +
        cumsum(shift(dq, n = 1L, type = "lag", fill = 0))
    }
  ][
    ,
    .(
      start, end, width,
      chr, offset, new_chr, rev
    )
  ]
}


#' Parse a UCSC chain file into a data.table
#'
#' Parses chain files following the UCSC format specification.
#' See https://genome.ucsc.edu/goldenPath/help/chain.html
#'
#' Chain files map FROM query TO target. For hg19ToHg38.over.chain:
#' - query = hg19 (source, what we lift FROM)
#' - target = hg38 (destination, what we lift TO)
#'
#' Output structure matches what the liftover function expects:
#' - chr: source chromosome (qName)
#' - start/end: ranges in source coordinates
#' - new_chr: target chromosome (tName)
#' - offset: source_pos - target_pos (so target = source - offset)
#' - rev: TRUE if query strand is reversed
#'
#' @param path Path to the chain file
#' @return data.table with columns: chr, start, end, width, new_chr, offset, rev
#' @keywords internal
parse_chain_file <- function(path) {
  lines <- readLines(path)

  # Find chain header lines
  header_idx <- grep("^chain\\s", lines)
  if (length(header_idx) == 0) {
    stop("No chain headers found in file: ", path)
  }

  # Determine end index for each chain (next header - 1, or end of file)
  end_idx <- c(header_idx[-1] - 2L, length(lines))

  header_names <- c(
    "chain", "score",
    "tName", "tSize", "tStrand", "tStart", "tEnd",
    "qName", "qSize", "qStrand", "qStart", "qEnd",
    "id"
  )

  chain_dt <- lapply(
    seq_along(header_idx),
    \(idx) {
      chain_header <- strsplit(lines[header_idx[idx]], "\\s+")[[1]] |>
        as.list() |>
        setNames(header_names)
      blocks <- lines[(header_idx[idx] + 1L):end_idx[idx]] |>
        strsplit("\\s+")
      make_single_chain_dt(
        chain_header,
        blocks
      )
    }
  ) |>
    data.table::rbindlist()
  data.table::setkey(chain_dt, chr, start, end)
  data.table::setcolorder(chain_dt)
  chain_dt
}

#' Convert genomic coordinates between genome builds
#'
#' S3 generic for lifting over genomic coordinates from one genome build
#' to another using UCSC chain files.
#'
#' @param x Object containing genomic coordinates (e.g., genomic_regions)
#' @param ... Additional arguments passed to methods
#' @return Object with coordinates converted to the target build
#' @export
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

#' @rdname build
#' @export
build.default <- function(x) {
  attr(x, "build")
}

#' @rdname build
#' @export
build.filter_condition <- build.default

#' @rdname build
#' @export
build.character <- function(x) {
  x
}

#' @rdname build
#' @export
build.list <- function(x) {
  x$build
}

#' @rdname build
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
#' @rdname build-set
#' @export
`build<-` <- function(x, value) {
  UseMethod("build<-")
}

#' @rdname build-set
#' @export
`build<-.default` <- function(x, value) {
  attr(x, "build") <- value
  x
}

#' @rdname build-set
#' @export
`build<-.filter_condition` <- `build<-.default`

#' @rdname build-set
#' @export
`build<-.data.table` <- function(x, value) {
  data.table::setattr(x, "build", value)
  x
}

#' @rdname build-set
#' @export
`build<-.list` <- function(x, value) {
  x$build <- value
  x
}

#' @rdname build-set
#' @export
`build<-.file_interface` <- `build<-.list`
