#' Get or infer genome build from a file interface
#'
#' Returns the build if already set, otherwise infers it from the file data.
#'
#' @param finterface A file_interface object
#' @param nsnps Number of SNPs to use for build inference
#' @return The genome build string (b36, b37, b38) or NA_character_
#' @keywords internal
get_build_from_file_interface <- function(
  finterface,
  nsnps = 1e4
) {
  if (!is.null(build(finterface))) {
    return(build(finterface))
  }
  if (needs_rsid_matching(finterface)) {
    warning(
      "Build inferrence for files that require RSID matching",
      "is not yet implemented. Please specify a build using, e.g.,",
      " 'build = \"b38\"'."
    )
    # TODO: Check handling of NA builds in other functions
    return(NA_character_)
  }
  infer_build_from_file(finterface, nsnps = nsnps)
}

#' Infer genome build from a file
#'
#' Reads the first nsnps rows from a file and infers the genome build.
#'
#' @param finterface A file_interface object
#' @param nsnps Number of SNPs to read for inference
#' @param ... Additional arguments passed to infer_build
#' @return The inferred genome build
#' @keywords internal
infer_build_from_file <- function(
  finterface,
  nsnps = 1e4,
  ...
) {
  cat("Inferring build from file ", finterface$filename, "...\n", sep = "")
  head(finterface, nsnps) |>
    infer_build(...)
}

#' Build a tabix query command for summary statistics
#'
#' @param summary_stats data.table with chr and pos columns
#' @param ref_filename Path to the tabix-indexed reference file
#' @return Character string containing the tabix command
#' @keywords internal
get_tabix_query <- function(
  summary_stats,
  ref_filename
) {
  sprintf(
    "tabix %s %s",
    ref_filename,
    unique(summary_stats[, .(chr, pos)])[
      ,
      sprintf("%s:%s-%s", substring(chr, 4), pos, pos) |>
        paste(collapse = " ")
    ]
  )
}

#' Infer genome build from summary statistics
#'
#' Compares variant positions against dbSNP reference files to determine
#' the most likely genome build.
#'
#' @param summary_stats data.table with chr, pos, ref, alt columns
#' @param return_build_match Logical, whether to return match percentages
#' @return The inferred build, or a data.table with match info if requested
#' @keywords internal
infer_build <- function(
  summary_stats,
  return_build_match = FALSE
) {
  if (!build_can_be_inferred(summary_stats)) {
    return(NA_character_)
  }

  # Duplicate SNPs pose problems when determining the percentage of matches
  summary_stats <- summary_stats[
    !duplicated(summary_stats[, .(chr, pos, ref, alt)])
  ]

  results <- data.table::data.table(
    build = c("b37", "b38"),
    ref_filename = get_dbsnp_filename(c("b37", "b38"), full_path = TRUE)
  )[
    ,
    n := get_tabix_matches(summary_stats, ref_filename),
    by = build
  ][]
  results <- rbind(
    data.table::data.table(
      build = "b36",
      ref_filename = NA_character_,
      n = nrow(summary_stats) - sum(results$n)
    ),
    results
  )[
    ,
    # Simplified estimate that doesn't account for b37/b38 overlap
    build_match := n / nrow(unique(summary_stats[, .(chr, pos, ref, alt)]))
  ][]

  results[
    which.max(build_match),
    message(
      "Build inferred to be ", build,
      sprintf(
        ifelse(
          build == "b36",
          " as %.1f%% of positions match no known b37 or b38 variants",
          " with a %.1f%% match rate."
        ),
        build_match * 100
      )
    )
  ]

  if (return_build_match) {
    return(
      results[
        ,
        .(build, build_match)
      ]
    )
  }

  results[
    which.max(n),
    build
  ]
}
#' Check if build can be inferred from summary statistics
#'
#' @param summary_stats data.table to check
#' @return Logical indicating whether required columns exist
#' @keywords internal
build_can_be_inferred <- function(summary_stats) {
  all(
    c("chr", "pos", "ref", "alt") %in% colnames(summary_stats)
  )
}

#' Count matching positions in a reference file using tabix
#'
#' @param summary_stats data.table with chr, pos, ref, alt columns
#' @param ref_filename Path to tabix-indexed reference file
#' @return Integer count of matching variants
#' @keywords internal
get_tabix_matches <- function(
  summary_stats,
  ref_filename
) {
  temp_file <- tempfile()

  get_tabix_query(
    summary_stats,
    ref_filename
  ) |>
    writeLines(temp_file)

  hits <- data.table::fread(
    cmd = paste("bash", temp_file),
    col.names = c("chr", "pos", "rsid", "ref", "alt"),
    select = 1:5,
    colClasses = list(
      character = c(1, 3, 4, 5),
      numeric = 2
    ),
    sep = "\t"
  ) |>
    suppressWarnings() |>
    # tabix sometimes has multiple positions for SNPs so may return the same one
    # multiple times
    unique()

  if (nrow(hits) == 0) {
    return(0)
  }

  hits[
    ,
    chr := paste0("chr", chr)
  ][]

  filtered_hits <- data.table::rbindlist(
    list(
      hits[summary_stats, on = c("chr", "pos", "ref"), nomatch = NULL],
      hits[summary_stats, on = c(chr = "chr", pos = "pos", ref = "alt"), nomatch = NULL]
    ),
    use.names = TRUE,
    fill = TRUE
  )

  if (nrow(filtered_hits) == 0) {
    return(0)
  }

  filtered_hits[
    mapply(
      function(ialt, iref, a) {
        query_allele <- if (is.na(ialt)) iref else ialt
        query_allele %in% strsplit(a, ",", fixed = TRUE)[[1]] |
          query_allele == a |
          a %in% strsplit(query_allele, ",", fixed = TRUE)[[1]]
      },
      i.alt, i.ref, alt
    ),
    .(
      chr = chr,
      pos = pos,
      ref = ref,
      alt = data.table::fcoalesce(i.alt, i.ref)
    )
  ] |>
    unique() |>
    nrow()
}
