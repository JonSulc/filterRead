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
#' Reads the file's raw column names and its FORMAT field once, and
#' returns the encoded-column parsing specification when they describe a
#' GWAS-VCF file.
#'
#' @param finterface A file_interface object
#' @return Column parsing specification, or NULL when the file is not
#'   GWAS-VCF
#' @keywords internal
get_gwas_vcf_parsing <- function(finterface) {
  raw_names <- column_names(finterface, raw = TRUE)
  format_string <- head(finterface)$FORMAT
  if (!matches_gwas_vcf_structure(raw_names, format_string)) {
    return()
  }
  gwas_vcf_encoded_column_parsing(raw_names, format_string)
}

#' Check whether column names and FORMAT describe a GWAS-VCF file
#'
#' A GWAS-VCF file carries the nine standard VCF columns followed by a
#' single sample column, and a FORMAT string listing GWAS-VCF keys. The
#' sample column's name is the study identifier, which is arbitrary and
#' plays no part in the test.
#'
#' @param raw_names Character vector of the file's input column
#'   names
#' @param format_string The file's FORMAT field value
#' @return Logical indicating whether the file is GWAS-VCF format
#' @keywords internal
matches_gwas_vcf_structure <- function(raw_names, format_string) {
  length(raw_names) == 10L &&
    all(raw_names[1:9] == vcf_cols_1_9) &&
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
#' encoded GWAS column, the tenth of the file.
#'
#' @param raw_names Character vector of the file's input column
#'   names
#' @param format_string The file's FORMAT field value
#' @return Column parsing specification
#' @keywords internal
gwas_vcf_encoded_column_parsing <- function(raw_names, format_string) {
  secondary_columns <- strsplit(format_string, ":") |>
    unlist()
  data.table::data.table(
    input_name = raw_names[10],
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
