# IEUGWAS summary statistics files have the following columns:
# "CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT"
# Followed by a single column with the ID of the GWAS
# (whose format is specified in FORMAT)
ieugwas_cols_1_9 <- c(
  "CHROM", "POS", "ID", "REF", "ALT", "QUAL", "FILTER", "INFO", "FORMAT"
)
ieugwas_formats <- c(
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
ieugwas_regexes <- c(
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

#' Parsing specification for an IEUGWAS file
#'
#' Reads the file's raw column names and its FORMAT field once, and
#' returns the encoded-column parsing specification when they describe an
#' IEUGWAS file.
#'
#' @param finterface A file_interface object
#' @return Column parsing specification, or NULL when the file is not
#'   IEUGWAS
#' @keywords internal
get_ieugwas_parsing <- function(finterface) {
  raw_names <- column_names(finterface, raw = TRUE)
  format <- head(finterface)$FORMAT
  if (!matches_ieugwas_structure(raw_names, format)) {
    return()
  }
  ieugwas_encoded_column_parsing(raw_names, format)
}

#' Check whether column names and FORMAT describe an IEUGWAS file
#'
#' An IEUGWAS file carries the nine standard VCF columns followed by a
#' single sample column, and a FORMAT string listing IEUGWAS keys. The
#' sample column's name is the GWAS identifier, which is arbitrary and
#' plays no part in the test.
#'
#' @param raw_names Character vector of the file's input column
#'   names
#' @param format The file's FORMAT field value
#' @return Logical indicating whether the file is IEUGWAS format
#' @keywords internal
matches_ieugwas_structure <- function(raw_names, format) {
  length(raw_names) == 10L &&
    all(raw_names[1:9] == ieugwas_cols_1_9) &&
    lists_ieugwas_keys(format)
}

#' Check whether a FORMAT string lists only IEUGWAS keys
#'
#' The FORMAT field of an IEUGWAS VCF is a colon-separated list of the
#' known IEUGWAS keys. Anything else — a genotype VCF's `GT:AD:DP:GQ:PL`,
#' an empty or missing field, a file with no FORMAT column at all — is
#' not one.
#'
#' @param format The file's FORMAT field value
#' @return Logical indicating whether the value is an IEUGWAS FORMAT
#'   string
#' @keywords internal
lists_ieugwas_keys <- function(format) {
  keys <- paste(names(ieugwas_formats), collapse = "|")
  isTRUE(grepl(sprintf("^(%s)(:(%s))*$", keys, keys), format))
}

#' Get column parsing specification for IEUGWAS files
#'
#' Parses the FORMAT field to determine how to extract values from the
#' encoded GWAS column, the tenth of the file.
#'
#' @param raw_names Character vector of the file's input column
#'   names
#' @param format The file's FORMAT field value
#' @return Column parsing specification
#' @keywords internal
ieugwas_encoded_column_parsing <- function(raw_names, format) {
  secondary_columns <- strsplit(format, ":") |>
    unlist()
  data.table::data.table(
    input_name = raw_names[10],
    regex = sprintf(
      "^%s$",
      paste(
        ieugwas_regexes[secondary_columns],
        collapse = ":"
      )
    ),
    encoded_names = list(ieugwas_formats[secondary_columns] |> unname()),
    delimiter = ":"
  )
}
