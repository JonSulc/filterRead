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

# Keys marking the sample column as GWAS summary statistics.
gwas_vcf_indicator_keys <- c("ES", "SE", "LP", "EZ", "SI", "NC")

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
#' of the nine, FORMAT, lists the values packed into the sample column
#' and carries at least one recognized indicator key, identifying it as
#' GWAS-VCF.
#'
#' @param raw_names Character vector of the file's input column names
#' @param format_string The file's FORMAT field value
#' @return Logical indicating whether the file is a GWAS-VCF
#' @keywords internal
matches_gwas_vcf_structure <- function(raw_names, format_string) {
  length(raw_names) == 10L &&
    identical(raw_names[1:9], vcf_cols_1_9) &&
    carries_indicator_key(format_string)
}

#' Check whether a FORMAT string carries an indicator key
#'
#' An indicator key marks the sample column as holding GWAS summary
#' statistics, and any one of them admits the file as a GWAS-VCF.
#' A FORMAT naming GT is rejected whatever else it carries: GT records
#' genotypes, which a GWAS-VCF has none of.
#'
#' @param format_string The file's FORMAT field value
#' @return Logical indicating whether the value admits the file as a
#'   GWAS-VCF
#' @keywords internal
carries_indicator_key <- function(format_string) {
  # VCF's grammar for a FORMAT key, which rejects a malformed field: an
  # empty key, a leading or trailing colon, an empty or missing value.
  shape <- "^[A-Za-z_][0-9A-Za-z_.]*(:[A-Za-z_][0-9A-Za-z_.]*)*$"
  if (!isTRUE(grepl(shape, format_string))) {
    return(FALSE)
  }
  keys <- unlist(strsplit(format_string, ":"))
  # GT records genotypes, which a GWAS-VCF has none of. A genotype
  # caller may name a field after an indicator key, so a FORMAT naming
  # GT is a genotype VCF whatever else it carries.
  !"GT" %in% keys && any(keys %in% gwas_vcf_indicator_keys)
}

#' Get column parsing specification for GWAS-VCF files
#'
#' Builds the specification for splitting an encoded sample column into
#' one output column per FORMAT key. A recognized key names its column
#' through `gwas_vcf_formats`; any other key names its column itself.
#'
#' @param sample_name Name of the encoded sample column
#' @param format_string Colon-separated FORMAT keys naming the values
#'   packed into it
#' @return Column parsing specification
#' @keywords internal
gwas_vcf_encoded_column_parsing <- function(sample_name, format_string) {
  # One group per key, each matching anything but the delimiter.
  keys <- strsplit(format_string, ":") |> unlist()
  known <- keys %in% names(gwas_vcf_formats)
  data.table::data.table(
    input_name = sample_name,
    regex = sprintf(
      "^%s$",
      paste(rep("([^:]*)", length(keys)), collapse = ":")
    ),
    encoded_names = list(
      data.table::fifelse(known, gwas_vcf_formats[keys], keys) |> unname()
    ),
    delimiter = ":"
  )
}
