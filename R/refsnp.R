#' @import data.table
#' @importFrom jsonlite fromJSON
#' @importFrom stringr str_match

# RefSNP data is only loaded if necessary and saved in an internal environment
internal_data <- new.env(parent = emptyenv())
# RefSNP data for the inference of genome build
data_path_refsnp_db22 <- "~/rcp_storage/common/Users/sulc/data/dbsnp/chr22.csv"

#### Create the (chr22) database from the RefSNP JSON file ####
read_json_file <- function(filename, nlines = -1L, ...) {
  readLines(filename, n = nlines, ...) |>
    lapply(parse_single_line_to_snp) |>
    data.table::rbindlist()
}
parse_single_line_to_snp <- function(single_line, ...) {
  jsonlite::fromJSON(single_line) |>
    parse_single_snp_json(...)
}
parse_single_snp_json <- function(single_snp_json, output_db_file = NULL) {
  is_genomic <- is_contig_genomic(
    single_snp_json$primary_snapshot_data$placements_with_allele$
      placement_annot$seq_id_traits_by_assembly
  )
  db <- data.table::data.table(
    build = single_snp_json$primary_snapshot_data$placements_with_allele$
      placement_annot$seq_id_traits_by_assembly[is_genomic] |>
      sapply(\(seq_id_traits) {
        stringr::str_match(seq_id_traits$assembly_name, "^[^.]+") |>
          stringr::str_replace("^GRCh", "b")
      }),
    chr = single_snp_json$primary_snapshot_data$placements_with_allele$alleles[
      is_genomic
    ] |>
      lapply(\(alleles) alleles$allele$spdi$seq_id |>
        get_chr_from_seq_id()),
    # SPDI notation acts as a 0-based interbase coordinate for the variant start
    # PMID: 31738401
    pos = single_snp_json$primary_snapshot_data$placements_with_allele$alleles[
      is_genomic
    ] |>
      lapply(\(alleles) alleles$allele$spdi$position + 1),
    ref = single_snp_json$primary_snapshot_data$placements_with_allele$alleles[
      is_genomic
    ] |>
      lapply(\(alleles) alleles$allele$spdi$deleted_sequence),
    alt = single_snp_json$primary_snapshot_data$placements_with_allele$alleles[
      is_genomic
    ] |>
      lapply(\(alleles) alleles$allele$spdi$inserted_sequence)
  )
  if (nrow(db) != 0) {
    db <- db[
      ,
      c(build = build, chr = chr, pos = pos, ref = ref, alt = alt),
      by = seq_len(nrow(db))
    ][
      ,
      .(build, chr, pos, ref, alt)
    ][
      ref != alt
    ] |>
      unique()
  }

  if (is.null(output_db_file)) {
    return(db)
  }
  data.table::fwrite(db, output_db_file, append = TRUE)
  NULL
}
is_contig_genomic <- function(contig_id) {
  sapply(contig_id, \(traits) {
    length(traits) != 0 &
      all(nrow(traits) != 0) &
      all(traits$is_chromosome)
  })
}
get_chr_from_seq_id <- function(seq_id) {
  chr <- stringr::str_match(seq_id, "^NC_([0-9]+)[.]")[, 2] |>
    as.numeric()
  paste0("chr", chr)
}

# Used for testing and debugging
extract_line_number <- function(filename, skip, n = 3) {
  con <- file(filename, "r")
  on.exit(close(con))
  for (i in seq_len(skip)) readLines(con, n = 1)
  readLines(con, n = n)
}

#### Infer the genome build from the chr22 database ####
refsnp_db22 <- function() {
  if (!exists("refsnp_db22", envir = internal_data)) {
    cat("Loading SNP reference for genome inference...\n")
    internal_data[["refsnp_db22"]] <- data.table::fread(data_path_refsnp_db22)
  }
  internal_data[["refsnp_db22"]]
}
