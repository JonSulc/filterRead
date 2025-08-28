# Helper function to create IEUGWAS test data
create_ieugwas_data <- function(
    format_string = "ES:SE:LP",
    trait_name = "trait-1",
    trait_data = c("0.1:0.05:2.3", "0.2:0.06:1.8", "-0.1:0.04:3.2"),
    nrows = 3) {
  base_data <- list(
    CHROM = seq_len(nrows),
    POS = seq(100, by = 100, length.out = nrows),
    ID = rep(".", nrows),
    REF = rep(c("A", "C", "G", "T"), length.out = nrows),
    ALT = rep(c("T", "G", "A", "C"), length.out = nrows),
    QUAL = rep(".", nrows),
    FILTER = rep("PASS", nrows),
    INFO = rep(".", nrows),
    FORMAT = rep(format_string, nrows)
  )

  # Add trait column
  base_data[[trait_name]] <- rep(trait_data, length.out = nrows)

  data.table::data.table(base_data)
}

test_that("is_ieugwas_file correctly identifies IEUGWAS format", {
  # Test valid IEUGWAS file
  ieugwas_data <- create_ieugwas_data()
  local_csv_file("ieugwas_valid.csv", ieugwas_data, sep = "\t")
  finterface <- new_file_interface("ieugwas_valid.csv", ieugwas_parsing = FALSE)

  expect_true(is_ieugwas_file(finterface))
})

test_that("is_ieugwas_file rejects non-IEUGWAS format", {
  # Test file with wrong number of columns
  wrong_cols_data <- data.table::data.table(
    CHROM = c(1, 1),
    POS = c(100, 200),
    ID = c(".", "."),
    REF = c("A", "C")
  )
  local_csv_file("not_ieugwas_cols.csv", wrong_cols_data, sep = "\t")
  finterface <- new_file_interface("not_ieugwas_cols.csv",
    ieugwas_parsing = FALSE
  )

  expect_false(is_ieugwas_file(finterface))

  # Test file with wrong column names
  wrong_names_data <- data.table::data.table(
    CHR = c(1, 1),
    POS = c(100, 200),
    ID = c(".", "."),
    REF = c("A", "C"),
    ALT = c("T", "G"),
    QUAL = c(".", "."),
    FILTER = c("PASS", "PASS"),
    INFO = c(".", "."),
    FORMAT = c("ES:SE", "ES:SE"),
    `trait-1` = c("0.1:0.05", "0.2:0.06")
  )
  local_csv_file("not_ieugwas_names.csv", wrong_names_data, sep = "\t")
  finterface <- new_file_interface("not_ieugwas_names.csv",
    ieugwas_parsing = FALSE
  )

  expect_false(is_ieugwas_file(finterface))

  # Test file with invalid trait column name
  invalid_trait_data <- data.table::data.table(
    CHROM = c(1, 1),
    POS = c(100, 200),
    ID = c(".", "."),
    REF = c("A", "C"),
    ALT = c("T", "G"),
    QUAL = c(".", "."),
    FILTER = c("PASS", "PASS"),
    INFO = c(".", "."),
    FORMAT = c("ES:SE", "ES:SE"),
    `INVALID_TRAIT!` = c("0.1:0.05", "0.2:0.06")
  )
  local_csv_file("invalid_trait.csv", invalid_trait_data, sep = "\t")
  finterface <- new_file_interface("invalid_trait.csv",
    ieugwas_parsing = FALSE
  )

  expect_false(is_ieugwas_file(finterface))
})

test_that("get_ieugwas_column_parsing handles various FORMAT combinations", {
  # Test ES:SE:LP format
  ieugwas_data_esl <- create_ieugwas_data(
    format_string = "ES:SE:LP",
    trait_name = "ukb-123",
    trait_data = c("0.1:0.05:2.3", "0.2:0.06:1.8")
  )
  local_csv_file("ieugwas_esl.csv", ieugwas_data_esl, sep = "\t")
  finterface <- new_file_interface("ieugwas_esl.csv", ieugwas_parsing = FALSE)

  parsing_info <- get_ieugwas_column_parsing(finterface)

  expect_equal(parsing_info$input_name, "ukb-123")
  expect_equal(parsing_info$regex, "^([-0-9.]+):([0-9.]+):([0-9.]+)$")
  expect_equal(
    parsing_info$encoded_names[[1]],
    c("effect", "effect_se", "log10p")
  )
  expect_equal(parsing_info$delimiter, ":")

  # Test ES:SE:AF format
  ieugwas_data_esa <- create_ieugwas_data(
    format_string = "ES:SE:AF",
    trait_name = "trait-456",
    trait_data = c("0.1:0.05:0.3", "0.2:0.06:0.4")
  )
  local_csv_file("ieugwas_esa.csv", ieugwas_data_esa, sep = "\t")
  finterface <- new_file_interface("ieugwas_esa.csv", ieugwas_parsing = FALSE)

  parsing_info <- get_ieugwas_column_parsing(finterface)

  expect_equal(parsing_info$input_name, "trait-456")
  expect_equal(parsing_info$regex, "^([-0-9.]+):([0-9.]+):([0-9.]+)$")
  expect_equal(parsing_info$encoded_names[[1]], c("effect", "effect_se", "af"))
  expect_equal(parsing_info$delimiter, ":")
})

test_that("get_ieugwas_column_parsing handles complex FORMAT strings", {
  # Test all available formats
  ieugwas_data_all <- create_ieugwas_data(
    format_string = "ES:SE:LP:AF:SS:EZ:SI:NC:ID",
    trait_name = "full-trait",
    trait_data = "0.1:0.05:2.3:0.3:1000:-1.5:0.9:500:rs123456",
    nrows = 1
  )
  local_csv_file("ieugwas_all.csv", ieugwas_data_all, sep = "\t")
  finterface <- new_file_interface("ieugwas_all.csv", ieugwas_parsing = FALSE)

  parsing_info <- get_ieugwas_column_parsing(finterface)

  expected_regex <- paste(
    "([-0-9.]+)", "([0-9.]+)", "([0-9.]+)", "([0-9.]+)", "([0-9]+)",
    "([-0-9.]+)", "([0-9.]+)", "([0-9]+)", "(rs[0-9]+)",
    sep = ":"
  )

  expect_equal(parsing_info$input_name, "full-trait")
  expect_equal(parsing_info$regex, paste0("^", expected_regex, "$"))
  expect_equal(
    parsing_info$encoded_names[[1]],
    c(
      "effect", "effect_se", "log10p", "af", "sample_size", "zscore",
      "imputation_accuracy", "n_cases", "rsid"
    )
  )
  expect_equal(parsing_info$delimiter, ":")

  # Test single format
  ieugwas_data_single <- create_ieugwas_data(
    format_string = "ES",
    trait_name = "simple-trait",
    trait_data = "0.1",
    nrows = 1
  )
  local_csv_file("ieugwas_single.csv", ieugwas_data_single, sep = "\t")
  finterface <- new_file_interface("ieugwas_single.csv",
    ieugwas_parsing = FALSE
  )

  parsing_info <- get_ieugwas_column_parsing(finterface)

  expect_equal(parsing_info$input_name, "simple-trait")
  expect_equal(parsing_info$regex, "^([-0-9.]+)$")
  expect_equal(parsing_info$encoded_names[[1]], "effect")
  expect_equal(parsing_info$delimiter, ":")
})

test_that("IEUGWAS integration with file_interface works correctly", {
  # Test automatic parsing is enabled by default
  ieugwas_data <- create_ieugwas_data(
    trait_name = "ukbb-neale",
    trait_data = c("0.1:0.05:2.3", "0.2:0.06:1.8", "-0.1:0.04:3.2")
  )
  local_csv_file("ieugwas_integration.csv", ieugwas_data, sep = "\t")

  # Test with ieugwas_parsing = TRUE (default)
  finterface_with_parsing <- new_file_interface(
    "ieugwas_integration.csv",
    ieugwas_parsing = TRUE
  )

  column_info <- finterface_with_parsing$column_info
  encoded_columns <- column_info[!sapply(encoded_names, is.null)]

  expect_equal(nrow(encoded_columns), 1)
  expect_equal(encoded_columns$input_name, "ukbb-neale")
  expect_equal(encoded_columns$regex, "^([-0-9.]+):([0-9.]+):([0-9.]+)$")
  expect_equal(
    encoded_columns$encoded_names[[1]],
    c("effect", "effect_se", "log10p")
  )
  expect_equal(encoded_columns$delimiter, ":")

  # Test with ieugwas_parsing = FALSE
  finterface_no_parsing <- new_file_interface(
    "ieugwas_integration.csv",
    ieugwas_parsing = FALSE
  )

  column_info_no_parsing <- finterface_no_parsing$column_info
  encoded_columns_no_parsing <- column_info_no_parsing[
    !sapply(encoded_names, is.null)
  ]

  expect_equal(nrow(encoded_columns_no_parsing), 0)
})

test_that("IEUGWAS parsing handles edge cases", {
  # Test with missing values in FORMAT
  ieugwas_data_na <- create_ieugwas_data(
    format_string = NA,
    trait_name = "trait-na",
    trait_data = "0.1:0.05",
    nrows = 1
  )
  local_csv_file("ieugwas_na.csv", ieugwas_data_na, sep = "\t")
  finterface <- new_file_interface("ieugwas_na.csv", ieugwas_parsing = FALSE)

  expect_error(get_ieugwas_column_parsing(finterface))

  # Test with empty FORMAT
  ieugwas_data_empty <- create_ieugwas_data(
    format_string = "",
    trait_name = "trait-empty",
    trait_data = "",
    nrows = 1
  )
  local_csv_file("ieugwas_empty.csv", ieugwas_data_empty, sep = "\t")
  finterface <- new_file_interface("ieugwas_empty.csv",
    ieugwas_parsing = FALSE
  )

  parsing_info <- get_ieugwas_column_parsing(finterface)
  expect_equal(parsing_info$regex, "^$")
  expect_equal(length(parsing_info$encoded_names[[1]]), 0)
})
