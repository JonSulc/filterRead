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
  sprintf("%s_to_%s.chain.rds", from, to)
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
        chain <- parse_chain_file(dest_file) |>
          data.table::setattr("from", from_build) |>
          data.table::setattr("to", to_build) |>
          set_chain_index()
        rds_file <- file.path(
          path,
          get_chain_rds_filename(from_build, to_build)
        )
        saveRDS(chain, rds_file)

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

  invalidate_chain_cache()

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
  invalidate_chain_cache()
  # Matches get_chain_rds_filename()'s extension.
  rds_files <- list.files(path, "\\.chain\\.rds$", full.names = TRUE)
  if (length(rds_files) > 0) {
    file.remove(rds_files)
    message("Removed ", length(rds_files), " cached chain files")
  } else {
    message("No cached chain files found")
  }
  invisible(rds_files)
}

#' Get a caller's own copy of a chain
#'
#' Returns a copy of the shared chain for a build pair, which the caller may
#' modify.
#'
#' @seealso [get_shared_chain()], which holds the chain this copies.
#' @param from Source build (b36, b37, b38, or synonyms like hg38)
#' @param to Target build (b36, b37, b38, or synonyms like hg38)
#' @param auto_download Logical, whether to automatically download chain
#'   files if not found. Default TRUE.
#' @return data.table with chain alignment data, keyed by (chr, start, end).
#'   Has attributes `from` and `to` recording the source and target builds.
#' @keywords internal
get_chain <- function(
  from,
  to,
  auto_download = TRUE
) {
  get_shared_chain(from, to, auto_download = auto_download) |>
    data.table::copy()
}

#' Read a chain from the UCSC chain file
#'
#' Parses the UCSC chain file for a build pair, which must be present in the
#' chain directory.
#'
#' @seealso [get_shared_chain()], which holds the chain for the session.
#' @param from Source build (b36, b37, b38, or synonyms like hg38)
#' @param to Target build (b36, b37, b38, or synonyms like hg38)
#' @return data.table with chain alignment data. Has attributes `from` and
#'   `to` recording the source and target builds.
#' @keywords internal
read_chain_file <- function(
  from,
  to
) {
  file.path(
    get_chain_path(warn = FALSE),
    get_chain_filename(from, to)
  ) |>
    parse_chain_file() |>
    data.table::setattr("from", from) |>
    data.table::setattr("to", to)
}


#' Parse one chain-file entry into its chain blocks
#'
#' Reads the alignment block lines under one chain header and computes each
#' block's coordinates. Handles both forward and reverse strand alignments.
#'
#' @param chain_header Named list with chain header fields (tName, tStart,
#'   tEnd, qName, qStart, qEnd, qStrand, qSize, etc.)
#' @param blocks List of character vectors, each containing alignment block
#'   data (width, dt, dq triplets)
#' @return data.table of chain blocks with columns: start, end, width, chr,
#'   offset, new_chr, rev
#' @keywords internal
parse_chain_blocks <- function(
  chain_header,
  blocks
) {
  if (length(blocks) == 1) {
    chain <- data.table::data.table(
      width = as.integer(blocks[[1]]),
      dt = NA_integer_,
      dq = NA_integer_
    )
  } else {
    chain <- rbindlist(
      blocks |>
        sapply(as.integer) |>
        lapply(as.list),
      fill = TRUE,
      use.names = FALSE
    ) |>
      setNames(c("width", "dt", "dq"))
  }

  chain[
    ,
    start := as.integer(chain_header$tStart) +
      cumsum(shift(dt, n = 1L, type = "lag", fill = 0L)) +
      cumsum(shift(width, n = 1L, type = "lag", fill = 0L)) +
      1L
  ][
    ,
    end := as.integer(chain_header$tStart) +
      cumsum(width) +
      cumsum(shift(dt, n = 1L, type = "lag", fill = 0L))
  ][
    ,
    chr := drop_prefix(chain_header$tName)
  ][
    ,
    new_chr := drop_prefix(chain_header$qName)
  ][
    ,
    rev := chain_header$qStrand == "-"
  ][
    ,
    offset := if (!rev[[1]]) {
      as.integer(chain_header$tStart) -
        as.integer(chain_header$qStart) +
        cumsum(shift(dt, n = 1L, type = "lag", fill = 0L)) -
        cumsum(shift(dq, n = 1L, type = "lag", fill = 0L))
    } else {
      as.integer(chain_header$tStart) -
        (as.integer(chain_header$qSize) - as.integer(chain_header$qStart)) +
        cumsum(width) +
        cumsum(shift(width, n = 1L, type = "lag", fill = 0L)) +
        cumsum(shift(dt, n = 1L, type = "lag", fill = 0L)) +
        cumsum(shift(dq, n = 1L, type = "lag", fill = 0L))
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

  chain <- lapply(
    seq_along(header_idx),
    \(idx) {
      chain_header <- strsplit(lines[header_idx[idx]], "\\s+")[[1]] |>
        as.list() |>
        setNames(header_names)
      blocks <- lines[(header_idx[idx] + 1L):end_idx[idx]] |>
        strsplit("\\s+")
      parse_chain_blocks(
        chain_header,
        blocks
      )
    }
  ) |>
    data.table::rbindlist()
  data.table::setkey(chain, chr, start, end)
  data.table::setcolorder(chain)
  chain
}

#' Convert genomic coordinates between genome builds
#'
#' S3 generic for lifting over genomic coordinates from one genome build
#' to another using UCSC chain files.
#'
#' @param x Object containing genomic coordinates (e.g., region_set)
#' @param ... Additional arguments passed to methods
#' @return Object with coordinates converted to the target build
#' @export
liftover <- function(
  x,
  ...
) {
  UseMethod("liftover")
}

#' Lift regions through a UCSC chain
#'
#' Maps `(chr, start, end)` regions from one reference build to another by
#' overlapping them with a UCSC chain and applying each chain block's offset
#' and strand.
#'
#' @param positions_dt A data.table with at least `chr`, `start` and `end`
#'   columns in the source build's coordinate system; every other column is
#'   carried through. None of the three may hold `NA`: the join accepts them
#'   and returns `NA` coordinates, or drops them under `nomatch = 0L`.
#' @param chain The chain for the source-to-target build pair, ideally keyed
#'   to avoid re-sorting.
#' @param mult How a position overlapping more than one chain block is
#'   resolved. Forwarded to the overlap join in [data.table::data.table];
#'   `"all"` fans the position out across every block covering it.
#' @param nomatch Whether a position no chain block covers is kept, with `NA`
#'   coordinates, or dropped. Forwarded to the overlap join in
#'   [data.table::data.table]; `0L` drops it.
#' @param allele_cols Column names of `positions_dt` to reverse-complement on
#'   reverse-strand chain blocks.
#' @return A data.table with `chr`, `start` and `end` in the target build,
#'   plus every other column of `positions_dt`; those named in `allele_cols`
#'   are reverse-complemented wherever the covering chain block is on the
#'   reverse strand.
#' @keywords internal
lift_chain_overlap <- function(
  positions_dt,
  chain,
  mult = "all",
  nomatch = 0L,
  allele_cols = character()
) {
  extra_cols <- setdiff(names(positions_dt), c("chr", "start", "end"))
  lifted <- chain[
    positions_dt,
    c(
      list(
        chr = new_chr,
        start = data.table::fifelse(
          rev,
          x.end - offset - (pmin(x.end, i.end) - x.start),
          pmax(x.start, i.start) - offset
        ),
        end = data.table::fifelse(
          rev,
          x.end - offset - (pmax(x.start, i.start) - x.start),
          pmin(x.end, i.end) - offset
        ),
        .lift_rev = rev
      ),
      carried
    ),
    on = .(chr, start <= end, end >= start),
    mult = mult,
    nomatch = nomatch,
    env = list(carried = as.list(extra_cols) |> setNames(extra_cols))
  ]
  # On reverse-strand blocks the lifted region is the reverse complement of
  # the source, so allele columns are complemented there. The chain's `rev`
  # flag drives this, which handles palindromic A/T and C/G SNVs. Unmatched
  # `lifted` rows (NA `.lift_rev`) keep their alleles.
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
#'   `end` in the coordinates of build `from`. `chr` takes the bare
#'   spelling (`"1"`); a prefixed name (`"chr1"`) matches no chain block.
#'   The input is not modified; the lifted result is returned. `pos` and
#'   `start`/`end` are mutually exclusive.
#' @param target Target build (`"b36"`, `"b37"`, `"b38"`, or any synonym
#'   accepted by [normalize_build()]).
#' @param from Source build. Defaults to `build(x)`. Pass explicitly to
#'   override or when `x` carries no `build` attribute.
#' @param drop_unlifted If TRUE (default), input rows whose coordinates
#'   do not lift (e.g. fall in a chain gap) are dropped. If FALSE, those
#'   rows appear with `NA` for `chr` and the coordinate columns.
#' @param multi_match How to resolve source positions that overlap more
#'   than one chain block: `"first"` (default) keeps the first match
#'   per source row, `"last"` keeps the last, and `"all"` emits one
#'   row per match. Forwarded to the overlap join in
#'   [data.table::data.table] as its `mult` argument.
#' @param retain_builds If TRUE, call [record_build()]
#'   for both `from` and `target` so the result carries
#'   `chr_<from>`/`pos_<from>` (source-build snapshot) and
#'   `chr_<target>`/`pos_<target>` (target-build mirror). The same
#'   suffixed columns are inspected on entry, and if all are present
#'   their values are copied into the canonical `chr`/`pos` columns
#'   instead of re-running the chain lift.
#' @param use_cache If TRUE (default), the round-trip short-circuit
#'   described under `retain_builds` applies: when the target build's
#'   suffixed columns are already present, their values are copied into
#'   the canonical columns instead of running a chain lift. FALSE forces
#'   a fresh chain lift even when those columns are present.
#' @param ... Reserved for future arguments.
#'
#' @return A data.table with `chr` and the coordinate columns present
#'   in `x` updated to the target build, with `build(result) == target`.
#'   With `multi_match = "all"` each source position contributes one
#'   output row per chain match. With `drop_unlifted = FALSE` every
#'   input row is kept (unlifted rows carry NA coordinates); the default
#'   `drop_unlifted = TRUE` removes them, so the row count equals the
#'   input only when `multi_match` is not `"all"` and every position
#'   lifts.
#' @export
liftover.data.table <- function(
  x,
  target,
  from = build(x),
  drop_unlifted = TRUE,
  multi_match = c("first", "last", "all"),
  retain_builds = FALSE,
  use_cache = TRUE,
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
    set_build(x_work, target)
    return(x_work)
  }

  # When the target build's coordinates (and alleles, when present) are
  # already recorded, copy them back. Liftover is not coordinate-invertible,
  # so a build's recorded columns are its stable representation.
  cache_value_cols <- c("chr", coord_cols$output, allele_cols)
  cache_cols <- paste0(cache_value_cols, "_", target)
  if (use_cache && all(cache_cols %in% names(x_work))) {
    for (col in cache_value_cols) {
      x_work[, (col) := build_versioned_column(x_work, col, target)]
    }
    set_build(x_work, target)
    return(x_work)
  }

  chain <- get_shared_chain(from, target)

  positions <- data.table::data.table(
    I     = seq_len(nrow(x_work)),
    chr   = x_work$chr,
    start = x_work[[coord_cols$start]],
    end   = x_work[[coord_cols$end]]
  )
  for (col in allele_cols) {
    positions[, (col) := x_work[[col]]]
  }
  positions <- positions[
    !is.na(chr) & !is.na(start) & !is.na(end)
  ]

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
    set_build(result, target)
    if (!retain_builds) return(result)
    return(record_build(result, build = target))
  }

  lifted <- lift_chain_overlap(
    positions,
    chain,
    mult        = multi_match,
    nomatch     = NA,
    allele_cols = allele_cols
  )
  # Each position contributes its mapping(s): matched positions duplicate
  # under multi_match = "all"; chain-gap positions come back with NA
  # coordinates. `.ord`/`.sub` restore input order with fan-out rows grouped
  # after their source row.
  result <- x_work[lifted$I]
  result[, chr := lifted$chr]
  for (col in coord_cols$output) {
    lifted_col <- if (col == coord_cols$end) "end" else "start"
    result[, (col) := lifted[[lifted_col]]]
  }
  for (col in allele_cols) {
    result[, (col) := lifted[[col]]]
  }
  result[, c(".ord", ".sub") := .(lifted$I, data.table::rowid(lifted$I))]

  # Rows with NA input coordinates were excluded from `positions`; re-add them
  # with NA coordinates so every input row is represented.
  unmatched <- rep(TRUE, nrow(x_work))
  unmatched[positions$I] <- FALSE
  missing <- which(unmatched)
  if (length(missing) > 0L) {
    na_rows <- data.table::copy(x_work[missing])
    na_rows[, chr := NA_character_]
    for (col in coord_cols$output) {
      na_rows[, (col) := NA_integer_]
    }
    na_rows[, c(".ord", ".sub") := .(missing, 0L)]
    result <- data.table::rbindlist(
      list(result, na_rows), use.names = TRUE
    )
  }
  data.table::setorder(result, .ord, .sub)
  result[, c(".ord", ".sub") := NULL]

  if (drop_unlifted) {
    result <- result[!is.na(chr)]
  }
  set_build(result, target)

  if (!retain_builds) return(result)
  record_build(result, build = target)
}

#' Determine which coordinate columns a data.table carries
#'
#' Resolves the input column to use for `start` and `end` of the
#' source interval, plus the names of the columns whose values
#' should be overwritten with the lifted coordinates. Errors if `pos`
#' is mixed with `start`/`end` or none of the three is present.
#'
#' @param x A data.table.
#' @return List with `start`, `end` (column names in `x` bounding the
#'   source interval to overlap with the chain) and `output` (column
#'   names in `x` to overwrite with lifted coordinates).
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
#' Records `value` in the object's `build` attribute as it stands, so
#' `build(x) <- "hg19"` stores `"hg19"`. It dispatches on the class of `x`
#' and falls back to a plain attribute assignment, which makes it the form
#' to reach for when that class is open.
#'
#' A replacement call assigns its result back into `x` and so raises the
#' table's reference count, at which point data.table copies it on the next
#' modification by reference. Use [set_build()] on a data.table that a `:=`
#' is about to reach.
#'
#' @param x Object to modify
#' @param value Build string to set (e.g., "b37", "b38")
#' @return Modified object
#' @seealso [set_build()], which normalizes the build name and records it on
#'   a data.table by reference.
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
