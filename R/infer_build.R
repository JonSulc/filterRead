#' @import data.table

get_build_from_file_interface <- function(
  finterface,
  nsnps = 1e4,
  refsnp_db = refsnp_db22()
) {
  if (!is.null(finterface$build)) {
    return(finterface$build)
  }
  return("b38")
}

infer_build_from_file <- function(
  finterface,
  nsnps = 1e4,
  ...
) {
  cat("Inferring build from file ", finterface$filename, "...\n")
  # Need to read only chr 22
  summary_stats <- finterface[chr == 22]
  if (!is.null(nsnps)) {
    summary_stats <- summary_stats[seq_len(nsnps)]
  }

  infer_build(summary_stats, ...)
}

infer_build <- function(
  summary_stats,
  refsnp_db = refsnp_db22(),
  return_build_match = FALSE
) {
  # Reference and alternate alleles are arbitrary
  # Include both possibilities
  summary_stats <- rbind(
    summary_stats[, .(chr = chr, pos = pos, ref = ref, alt = alt)],
    summary_stats[, .(chr = chr, pos = pos, ref = alt, alt = ref)]
  ) |>
    unique()

  summary_stats_builds <- refsnp_db[
    summary_stats,
    on = c("chr", "pos", "ref", "alt")
  ][
    is.na(build),
    build := "b36"
  ][
    ,
    .(
      # Because we include both ref/alt and alt/ref, half of the rows are
      # expected to be mismatches and should not be flagged as likely b36
      n = ifelse(
        build == "b36",
        .N - nrow(summary_stats) / 2,
        as.double(.N)
      )
    ),
    by = build
  ][
    ,
    .(
      build = build,
      n = n,
      build_match = n / (nrow(summary_stats) / 2)
    )
  ]

  summary_stats_builds[
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
      summary_stats_builds[
        ,
        .(build, build_match)
      ]
    )
  }

  summary_stats_builds[
    which.max(n),
    build
  ]
}
