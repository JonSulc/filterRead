tabix_colnames <- c(
  "chr",
  "pos",
  "rsid",
  "ref",
  "alt",
  "qual",
  "filter",
  "info"
)

is_single_genomic_block <- function(
  fcondition
) {
  # Determine whether all non-genomic conditions apply to the same genomic range
  if (!is.call(fcondition)) {
    return(TRUE)
  }
  if (is_and_block(fcondition)) {
    return(TRUE)
  }
  if (!has_non_genomic_condition(fcondition)) {
    return(TRUE)
  }
  if (!has_chromosome_condition(fcondition) &&
    !has_position_condition(fcondition)) {
    return(TRUE)
  }
  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(
      is_single_genomic_block(fcondition[[2]]) &&
        is_single_genomic_block(fcondition[[3]]) &&
        identical(
          genomic_regions(fcondition[[2]]),
          genomic_regions(fcondition[[3]])
        )
    )
  }
  FALSE
}

get_tabix_process_substitution <- function(
  chr,
  start,
  end,
  dbsnp_filename
) {
  stopifnot(length(start) == length(end))
  stopifnot(length(chr) == 1 | length(chr) == length(start))
  regions <- sprintf(
    sprintf(
      "%s%s",
      drop_chr_prefix(chr),
      ifelse(is.na(start) & is.na(end),
        "",
        sprintf(
          ":%s-%s",
          ifelse(is.na(start) | is.infinite(start), "", start),
          ifelse(is.na(end) | is.infinite(end), "", end)
        )
      )
    )
  )
  sprintf(
    "<(tabix %s %s)",
    dbsnp_filename,
    paste(regions, collapse = " ")
  )
}
