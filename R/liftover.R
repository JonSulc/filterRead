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
#'   Has attributes `from` and `to` recording the source and target builds.
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
#' Chain files map FROM target TO query. For hg19ToHg38.over.chain:
#' - target = hg19 (source, what we lift FROM)
#' - query = hg38 (destination, what we lift TO)
#'
#' Output structure matches what the liftover function expects:
#' - chr: source chromosome (tName)
#' - start/end: ranges in source coordinates
#' - new_chr: destination chromosome (qName)
#' - offset: source_pos - destination_pos (so destination = source - offset)
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

#' Translate genomic intervals through a UCSC chain
#'
#' Maps `(chr, start, end)` intervals from one reference genome to
#' another by overlapping them with a UCSC chain data.table and
#' applying each chain block's offset and strand. The shared core for
#' [liftover.genomic_regions()] and [liftover.data.table()]; not called
#' directly by users. Columns beyond `chr`/`start`/`end` in
#' `positions_dt` are carried through unchanged.
#'
#' @param positions_dt A keyed data.table with at least `chr`, `start`,
#'   `end` columns in the source build's coordinate system; extra
#'   columns are carried through.
#' @param chain_dt Chain data.table from [get_chain_dt()] for the
#'   desired source-to-target build pair, keyed on `(chr, start, end)`.
#' @param mult Forwarded to [data.table::foverlaps].
#' @param nomatch Forwarded to [data.table::foverlaps].
#' @param allele_cols Character vector of `positions_dt` column names to
#'   reverse-complement on reverse-strand (`rev = TRUE`) blocks. Default
#'   `character()` (no reverse-complement).
#'
#' @return data.table with lifted `chr`, `start`, `end` (in the target
#'   build) plus any extra columns from `positions_dt`.
#' @keywords internal
lift_chain_overlap <- function(
  positions_dt,
  chain_dt,
  mult = "all",
  nomatch = 0L,
  allele_cols = character()
) {
  extra_cols <- setdiff(names(positions_dt), c("chr", "start", "end"))
  lifted <- data.table::foverlaps(
    positions_dt,
    chain_dt,
    mult = mult,
    nomatch = nomatch
  )[
    ,
    c(
      list(
        chr = new_chr,
        start = data.table::fifelse(
          rev,
          end - offset - (pmin(end, i.end) - start),
          pmax(start, i.start) - offset
        ),
        end = data.table::fifelse(
          rev,
          end - offset - (pmax(start, i.start) - start),
          pmin(end, i.end) - offset
        ),
        .lift_rev = rev
      ),
      .SD
    ),
    .SDcols = extra_cols
  ]
  # On reverse-strand blocks the lifted region is the reverse complement of
  # the source, so allele columns are complemented there. The chain's `rev`
  # flag drives this, which handles palindromic A/T and C/G SNVs. Unmatched
  # rows (NA `.lift_rev`) keep their alleles.
  for (col in intersect(allele_cols, names(lifted))) {
    lifted[(.lift_rev), (col) := reverse_complement(get(col))]
  }
  lifted[, .lift_rev := NULL]
  lifted[]
}

#' Lift variant coordinates between genome builds
#'
#' Translates a data.table's genomic coordinates from one reference
#' genome to another (`from` defaults to `build(x)`, lifted to
#' `target`) using UCSC chain files. Accepts the conventional shapes
#' for variant tables: `pos` for point variants, `start` and/or `end`
#' for ranges. Whichever of those columns is present is updated in
#' place; everything else (alleles, effects, p-values, ...) carries
#' through unchanged. Source positions that overlap more than one
#' chain block are resolved according to `multi_match`. If
#' `retain_builds = TRUE`, both source-build and target-build
#' suffixed copies of the standard coordinate columns are kept on the
#' result so downstream merges across builds key on the same
#' coordinate system; on a subsequent lift, those suffixed columns
#' are reused instead of running a fresh chain conversion (avoiding
#' round-trip drift).
#'
#' @param x A data.table with `chr` plus one or more of `pos`, `start`,
#'   `end` in the source build's coordinate system. The input is not
#'   modified; the lifted result is returned. `pos` and `start`/`end`
#'   are mutually exclusive.
#' @param target Target build (`"b36"`, `"b37"`, `"b38"`, or any synonym
#'   accepted by [normalize_build()]).
#' @param from Source build. Defaults to `build(x)`. Pass explicitly to
#'   override or when `x` carries no `build` attribute.
#' @param drop_unlifted If TRUE (default), rows whose coordinates do
#'   not lift (e.g. fall in a chain gap) are dropped. If FALSE, those
#'   rows appear with `NA` for `chr` and the coordinate columns.
#' @param multi_match How to resolve source positions that overlap more
#'   than one chain block: `"first"` (default) keeps the first match
#'   per source row, `"last"` keeps the last, and `"all"` emits one
#'   row per match. Forwarded to [data.table::foverlaps]'s `mult`
#'   argument.
#' @param retain_builds If TRUE, call [record_build()]
#'   for both `from` and `target` so the result carries
#'   `chr_<from>`/`pos_<from>` (source-build snapshot) and
#'   `chr_<target>`/`pos_<target>` (target-build mirror). The same
#'   suffixed columns are inspected on entry, and if all are present
#'   their values are copied into the canonical `chr`/`pos` columns
#'   instead of re-running the chain lift.
#' @param ... Reserved for future arguments.
#'
#' @return A data.table with `chr` and the coordinate columns present
#'   in `x` updated to the target build, with `build(result) == target`.
#'   Row count matches the input when `multi_match` is `"first"` or
#'   `"last"`; with `"all"` rows are duplicated for each chain match.
#' @export
liftover.data.table <- function(
  x,
  target,
  from = build(x),
  drop_unlifted = TRUE,
  multi_match = c("first", "last", "all"),
  retain_builds = FALSE,
  ...
) {
  if (is.null(from)) {
    stop(
      "Cannot lift a data.table without a source build. Either set ",
      "`build(dt) <- \"b37\"` (or similar) or pass `from = \"b37\"`."
    )
  }
  from <- normalize_build(from, allow_null = FALSE)
  target <- normalize_build(target, allow_null = FALSE)
  multi_match <- match.arg(multi_match)

  coord_cols <- detect_coordinate_columns(x)
  allele_cols <- intersect(c("ref", "alt"), names(x))

  x_work <- data.table::copy(x)

  # Source-build suffix columns must be added before the coordinate
  # update, while the original values are still in chr/pos.
  if (retain_builds) {
    record_build(x_work, build = from)
  }

  # Same-build short-circuit: the source-build suffix columns added
  # above already double as the target-build mirror.
  if (identical(from, target) || nrow(x_work) == 0) {
    build(x_work) <- target
    return(x_work)
  }

  # When the target build's coordinates (and alleles, when present) are
  # already recorded, copy them back. Liftover is not coordinate-invertible,
  # so a build's recorded columns are its stable representation.
  cache_value_cols <- c("chr", coord_cols$output, allele_cols)
  cache_cols <- paste0(cache_value_cols, "_", target)
  if (all(cache_cols %in% names(x_work))) {
    for (col in cache_value_cols) {
      x_work[, (col) := build_versioned_column(x_work, col, target)]
    }
    build(x_work) <- target
    return(x_work)
  }

  chain_dt <- get_chain_dt(from, target)

  positions <- data.table::data.table(
    I     = seq_len(nrow(x_work)),
    chr   = format_chr(x_work$chr, prefix = "chr"),
    start = x_work[[coord_cols$start]],
    end   = x_work[[coord_cols$end]]
  )
  for (col in allele_cols) {
    positions[, (col) := x_work[[col]]]
  }
  positions <- positions[
    !is.na(chr) & !is.na(start) & !is.na(end)
  ]
  data.table::setkey(positions, chr, start, end)

  if (nrow(positions) == 0) {
    if (drop_unlifted) {
      result <- x_work[0]
    } else {
      result <- x_work
      na_assignment <- list(NA_character_)
      na_assignment <- c(
        na_assignment,
        rep(list(NA_integer_), length(coord_cols$output))
      )
      result[
        ,
        c("chr", coord_cols$output) := na_assignment
      ]
    }
    build(result) <- target
    if (!retain_builds) return(result)
    return(record_build(result, build = target))
  }

  lifted <- lift_chain_overlap(
    positions,
    chain_dt,
    mult        = multi_match,
    nomatch     = if (drop_unlifted) 0L else NA,
    allele_cols = allele_cols
  )
  data.table::setkey(lifted, I)

  result <- data.table::copy(x_work[lifted$I])
  result[, chr := lifted$chr]
  for (col in coord_cols$output) {
    lifted_col <- if (col == coord_cols$end) "end" else "start"
    result[, (col) := lifted[[lifted_col]]]
  }
  for (col in allele_cols) {
    result[, (col) := lifted[[col]]]
  }
  build(result) <- target

  if (!retain_builds) return(result)
  record_build(result, build = target)
}

#' Determine which coordinate columns a data.table carries
#'
#' Resolves the input column to use for `start` and `end` of the
#' foverlaps interval, plus the names of the columns whose values
#' should be overwritten with the lifted coordinates. Errors if `pos`
#' is mixed with `start`/`end` or none of the three is present.
#'
#' @param x A data.table.
#' @return List with `start`, `end` (column names in `x` to feed into
#'   foverlaps as the source interval) and `output` (column names in
#'   `x` to overwrite with lifted coordinates).
#' @keywords internal
detect_coordinate_columns <- function(x) {
  has_pos   <- "pos"   %in% names(x)
  has_start <- "start" %in% names(x)
  has_end   <- "end"   %in% names(x)
  if (has_pos && (has_start || has_end)) {
    stop("data.table has both `pos` and `start`/`end`; pick one.")
  }
  if (has_pos) {
    return(list(start = "pos", end = "pos", output = "pos"))
  }
  if (has_start && has_end) {
    return(list(start = "start", end = "end", output = c("start", "end")))
  }
  if (has_start) {
    return(list(start = "start", end = "start", output = "start"))
  }
  if (has_end) {
    return(list(start = "end", end = "end", output = "end"))
  }
  stop(
    "data.table needs at least one of `pos`, `start`, or `end` to lift."
  )
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
