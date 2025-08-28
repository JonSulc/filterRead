#' @import data.table

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

is_ieugwas_file <- function(finterface) {
  fcolumn_names <- column_names(
    finterface,
    original = TRUE
  )
  if (length(fcolumn_names) != 10) {
    return(FALSE)
  }
  all(fcolumn_names[1:9] == ieugwas_cols_1_9) &&
    grepl("^[a-z0-9-]+$", fcolumn_names[10])
}

get_ieugwas_column_parsing <- function(finterface) {
  secondary_columns <- head(finterface)$FORMAT |>
    strsplit(":") |>
    unlist()
  data.table::data.table(
    input_name = column_names(finterface, original = TRUE)[10],
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
