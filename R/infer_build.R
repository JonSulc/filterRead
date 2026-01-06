get_build_from_file_interface <- function(
  finterface,
  nsnps = 1e4
) {
  if (!is.null(finterface$build)) {
    return(finterface$build)
  }
  if (needs_rsid_matching(finterface)) {
    warning(
      "Build inferrence for files that require RSID matching",
      "is not yet implemented. Please specify a build using, e.g.,",
      " 'build = \"b38\"'."
    )
    return(NA_character_)
  }
  infer_build_from_file(finterface, nsnps = nsnps)
}

infer_build_from_file <- function(
  finterface,
  nsnps = 1e4,
  ...
) {
  cat("Inferring build from file ", finterface$filename, "...\n", sep = "")
  head(finterface, nsnps) |>
    infer_build(...)
}

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

infer_build_rsid <- function(
  summary_stats
) {
  results <- data.table::data.table(
    build = c("b37", "b38"),
    ref_filename = file.path(
      get_dbsnp_path(),
      get_dbsnp_filename(c("b37", "b38"))
    )
  )[
    ,
    n := get_rsid_matches(summary_stats, ref_filename, build = build),
    by = build
  ]
}

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
build_can_be_inferred <- function(summary_stats) {
  all(
    c("chr", "pos", "ref", "alt") %in% colnames(summary_stats)
  )
}
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

get_rsid_matches <- function(
  summary_stats,
  ref_filename,
  build = "auto"
) {
  stop("This only makes sense if there is more information than just RSID")
  test <- new_file_interface(ref_filename, build = build)[
    rsid %in% summary_stats$rsid
  ]
}
