# A GWAS-VCF file has the nine standard VCF columns:
# "CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT"
# followed by a single sample column holding one variant's summary
# statistics, packed as FORMAT describes.
vcf_cols_1_9 <- c(
  "CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT"
)
gwas_vcf_formats <- c(
  ES = "effect",
  SE = "effect_se",
  LP = "log10p",
  AF = "af",
  SS = "sample_size",
  EZ = "zscore",
  SI = "imputation_accuracy",
  NC = "n_cases",
  ID = "rsid"
)
gwas_vcf_regexes <- c(
  ES = "([-0-9.]+)",
  SE = "([0-9.]+)",
  LP = "([0-9.]+)",
  AF = "([0-9.]+)",
  SS = "([0-9]+)",
  EZ = "([-0-9.]+)",
  SI = "([0-9.]+)",
  NC = "([0-9]+)",
  ID = "(rs[0-9]+)"
)

#' Parsing specification for a GWAS-VCF file
#'
#' Returns the encoded-column parsing specification when the file's
#' header and first row describe a GWAS-VCF file.
#'
#' @param finterface A file_interface object whose `column_info` is not
#'   yet set
#' @return Column parsing specification, or NULL when the file is not a
#'   GWAS-VCF
#' @keywords internal
get_gwas_vcf_parsing <- function(finterface) {
  # head() gives the file's own header only until column_info exists;
  # afterwards it names columns from column_names(), which standardizes
  # them and includes the decoded ones.
  stopifnot(is.null(finterface$column_info))
  first_row <- head(finterface)
  raw_names <- names(first_row)
  if (!matches_gwas_vcf_structure(raw_names, first_row$FORMAT)) {
    return()
  }
  gwas_vcf_encoded_column_parsing(raw_names[10], first_row$FORMAT)
}

#' Check whether column names and FORMAT describe a GWAS-VCF file
#'
#' A GWAS-VCF file carries the nine standard VCF columns followed by a
#' single sample column, whose name is the study identifier. The last
#' of the nine, FORMAT, lists the values packed into the sample
#' column.
#'
#' @param raw_names Character vector of the file's input column names
#' @param format_string The file's FORMAT field value
#' @return Logical indicating whether the file is a GWAS-VCF
#' @keywords internal
matches_gwas_vcf_structure <- function(raw_names, format_string) {
  length(raw_names) == 10L &&
    identical(raw_names[1:9], vcf_cols_1_9) &&
    lists_gwas_vcf_keys(format_string)
}

#' Check whether a FORMAT string lists only GWAS-VCF keys
#'
#' The FORMAT field of a GWAS-VCF file is a colon-separated list of the
#' known GWAS-VCF keys. Anything else — a genotype VCF's `GT:AD:DP:GQ:PL`,
#' an empty or missing field, a file with no FORMAT column at all — is
#' not one.
#'
#' @param format_string The file's FORMAT field value
#' @return Logical indicating whether the value is a GWAS-VCF FORMAT
#'   string
#' @keywords internal
lists_gwas_vcf_keys <- function(format_string) {
  keys <- paste(names(gwas_vcf_formats), collapse = "|")
  isTRUE(grepl(sprintf("^(%s)(:(%s))*$", keys, keys), format_string))
}

#' Get column parsing specification for GWAS-VCF files
#'
#' Parses the FORMAT field to determine how to extract values from the
#' encoded sample column.
#'
#' @param sample_name Name of the encoded sample column
#' @param format_string Colon-separated FORMAT keys naming the values
#'   packed into it
#' @return Column parsing specification
#' @keywords internal
gwas_vcf_encoded_column_parsing <- function(sample_name, format_string) {
  secondary_columns <- strsplit(format_string, ":") |>
    unlist()
  data.table::data.table(
    input_name = sample_name,
    regex = sprintf(
      "^%s$",
      paste(
        gwas_vcf_regexes[secondary_columns],
        collapse = ":"
      )
    ),
    encoded_names = list(gwas_vcf_formats[secondary_columns] |> unname()),
    delimiter = ":"
  )
}
