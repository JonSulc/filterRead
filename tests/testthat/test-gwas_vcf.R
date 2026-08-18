# Helper function to create GWAS-VCF test data
create_gwas_vcf_data <- function(
  format_string = "ES:SE:LP",
  trait_name = "trait-1",
  trait_data = c("0.1:0.05:2.3", "0.2:0.06:1.8", "-0.1:0.04:3.2"),
  nrows = 3
) {
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

  data.table::as.data.table(base_data)
}

test_that("matches_gwas_vcf_structure identifies a GWAS-VCF file", {
  expect_true(
    matches_gwas_vcf_structure(
      c(vcf_cols_1_9, "ieuGWAS-42"),
      "ES:SE:LP:AF:ID"
    )
  )
  expect_true(
    matches_gwas_vcf_structure(c(vcf_cols_1_9, "trait-1"), "ES:SE:LP")
  )
  expect_true(
    matches_gwas_vcf_structure(
      c(vcf_cols_1_9, "column,name_10 is-Ignored!"),
      "ES:SE"
    )
  )
})

test_that("matches_gwas_vcf_structure rejects a single-sample genotype VCF", {
  expect_false(
    matches_gwas_vcf_structure(c(vcf_cols_1_9, "NA12878"), "GT:AD:DP:GQ:PL")
  )
})

test_that("matches_gwas_vcf_structure rejects other column layouts", {
  expect_false(
    matches_gwas_vcf_structure(c("CHROM", "POS", "ID", "REF"), "ES:SE")
  )
  expect_false(
    matches_gwas_vcf_structure(
      c("CHR", vcf_cols_1_9[2:9], "trait-1"),
      "ES:SE"
    )
  )
  expect_false(
    matches_gwas_vcf_structure(
      c(vcf_cols_1_9, "trait-1", "trait-2"),
      "ES:SE"
    )
  )
})

test_that("matches_gwas_vcf_structure yields FALSE for an NA name", {
  raw_names <- c(vcf_cols_1_9, "trait-1")
  raw_names[3] <- NA_character_

  expect_false(matches_gwas_vcf_structure(raw_names, "ES:SE:LP"))
})

test_that("lists_gwas_vcf_keys accepts the GWAS-VCF keys and combinations", {
  for (key in names(gwas_vcf_formats)) {
    expect_true(lists_gwas_vcf_keys(key))
  }
  expect_true(lists_gwas_vcf_keys("ES:SE"))
  expect_true(lists_gwas_vcf_keys("ES:SE:LP:AF"))
  expect_true(lists_gwas_vcf_keys("ID:AF:SS"))
  expect_true(lists_gwas_vcf_keys("EZ:SI:NC"))
  expect_true(lists_gwas_vcf_keys("ES:SE:LP:AF:SS:EZ:SI:NC:ID"))
})

test_that("lists_gwas_vcf_keys rejects keys it does not know", {
  expect_false(lists_gwas_vcf_keys("GT:AD:DP:GQ:PL"))
  expect_false(lists_gwas_vcf_keys("INVALID"))
  expect_false(lists_gwas_vcf_keys("ES:INVALID:LP"))
  expect_false(lists_gwas_vcf_keys("es:se:lp"))
})

test_that("lists_gwas_vcf_keys rejects malformed and missing FORMAT", {
  expect_false(lists_gwas_vcf_keys("ES::LP"))
  expect_false(lists_gwas_vcf_keys("ES:LP:"))
  expect_false(lists_gwas_vcf_keys(":ES:LP"))
  expect_false(lists_gwas_vcf_keys(""))
  expect_false(lists_gwas_vcf_keys(NA_character_))
  expect_false(lists_gwas_vcf_keys(NULL))
})

test_that("gwas_vcf_encoded_column_parsing describes the sample column", {
  parsing_info <- gwas_vcf_encoded_column_parsing("ukb-123", "ES:SE:LP")

  expect_equal(parsing_info$input_name, "ukb-123")
  expect_equal(parsing_info$regex, "^([^:]*):([^:]*):([^:]*)$")
  expect_equal(
    parsing_info$encoded_names[[1]],
    c("effect", "effect_se", "log10p")
  )
  expect_equal(parsing_info$delimiter, ":")

  parsing_info <- gwas_vcf_encoded_column_parsing("trait-456", "ES:SE:AF")

  expect_equal(parsing_info$input_name, "trait-456")
  expect_equal(parsing_info$regex, "^([^:]*):([^:]*):([^:]*)$")
  expect_equal(parsing_info$encoded_names[[1]], c("effect", "effect_se", "af"))
  expect_equal(parsing_info$delimiter, ":")
})

test_that("gwas_vcf_encoded_column_parsing describes every key at once", {
  parsing_info <- gwas_vcf_encoded_column_parsing(
    "full-trait",
    "ES:SE:LP:AF:SS:EZ:SI:NC:ID"
  )

  expect_equal(parsing_info$input_name, "full-trait")
  expect_equal(
    parsing_info$regex,
    paste0("^", paste(rep("([^:]*)", 9), collapse = ":"), "$")
  )
  expect_equal(
    parsing_info$encoded_names[[1]],
    c(
      "effect", "effect_se", "log10p", "af", "sample_size", "zscore",
      "imputation_accuracy", "n_cases", "rsid"
    )
  )
  expect_equal(parsing_info$delimiter, ":")
})

test_that("gwas_vcf_encoded_column_parsing covers every FORMAT key", {
  parsing_info <- gwas_vcf_encoded_column_parsing("simple-trait", "ES")

  expect_equal(parsing_info$input_name, "simple-trait")
  expect_equal(parsing_info$regex, "^([^:]*)$")
  expect_equal(parsing_info$encoded_names[[1]], "effect")
  expect_equal(parsing_info$delimiter, ":")

  for (key in names(gwas_vcf_formats)) {
    parsing_info <- gwas_vcf_encoded_column_parsing("trait-1", key)
    expect_equal(parsing_info$encoded_names[[1]], unname(gwas_vcf_formats[key]))
  }

  for (combo in c("ES:SE", "ES:SE:LP", "ES:SE:LP:AF", "ID:AF:SS", "EZ:SI:NC")) {
    combo_parts <- strsplit(combo, ":")[[1]]
    parsing_info <- gwas_vcf_encoded_column_parsing("combo-trait", combo)
    expect_equal(
      parsing_info$encoded_names[[1]],
      unname(gwas_vcf_formats[combo_parts])
    )
  }
})

test_that("an unrecognized FORMAT key is named by the key itself", {
  parsing_info <- gwas_vcf_encoded_column_parsing("ukb-123", "ES:SE:ADF:LP")

  expect_equal(parsing_info$input_name, "ukb-123")
  expect_equal(parsing_info$regex, "^([^:]*):([^:]*):([^:]*):([^:]*)$")
  expect_equal(
    parsing_info$encoded_names[[1]],
    c("effect", "effect_se", "ADF", "log10p")
  )
  expect_equal(parsing_info$delimiter, ":")
})

test_that("get_gwas_vcf_parsing rejects a constructed interface", {
  # head() gives the file's own header only while column_info is
  # absent, which is what the precondition guards.
  non_gwas_vcf_data <- data.table::data.table(
    chr = c(1, 2),
    pos = c(100, 200),
    pval = c(0.01, 0.05)
  )
  local_csv_file("non_gwas_vcf.csv", non_gwas_vcf_data, sep = "\t")
  finterface <- new_file_interface(
    "non_gwas_vcf.csv",
    gwas_vcf_parsing = FALSE
  ) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_error(get_gwas_vcf_parsing(finterface), "column_info")
})

test_that("gwas_vcf_parsing = TRUE records the encoded column", {
  gwas_vcf_data <- create_gwas_vcf_data(
    trait_name = "ukbb-neale",
    trait_data = c("0.1:0.05:2.3", "0.2:0.06:1.8", "-0.1:0.04:3.2")
  )
  local_csv_file("gwas_vcf_integration.csv", gwas_vcf_data, sep = "\t")
  finterface <- new_file_interface(
    "gwas_vcf_integration.csv",
    gwas_vcf_parsing = TRUE
  ) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  encoded_columns <- finterface$column_info[!sapply(encoded_names, is.null)]

  expect_equal(nrow(encoded_columns), 1)
  expect_equal(encoded_columns$input_name, "ukbb-neale")
  expect_equal(encoded_columns$regex, "^([^:]*):([^:]*):([^:]*)$")
  expect_equal(
    encoded_columns$encoded_names[[1]],
    c("effect", "effect_se", "log10p")
  )
  expect_equal(encoded_columns$delimiter, ":")
})

test_that("gwas_vcf_parsing = FALSE leaves the sample column packed", {
  gwas_vcf_data <- create_gwas_vcf_data(
    trait_name = "ukbb-neale",
    trait_data = c("0.1:0.05:2.3", "0.2:0.06:1.8", "-0.1:0.04:3.2")
  )
  local_csv_file("gwas_vcf_integration.csv", gwas_vcf_data, sep = "\t")
  finterface <- new_file_interface(
    "gwas_vcf_integration.csv",
    gwas_vcf_parsing = FALSE
  ) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  encoded_columns <- finterface$column_info[!sapply(encoded_names, is.null)]

  expect_equal(nrow(encoded_columns), 0)
})

test_that("an uppercase sample identifier is still a GWAS-VCF file", {
  gwas_vcf_data <- create_gwas_vcf_data(
    format_string = "ES:SE:LP:AF",
    trait_name = "ieuGWAS-42",
    trait_data = c("0.1:0.05:2.3:0.3", "0.2:0.06:1.8:0.4", "-0.1:0.04:3.2:0.2")
  )
  local_csv_file("gwas_vcf_uppercase.csv", gwas_vcf_data, sep = "\t")
  finterface <- new_file_interface("gwas_vcf_uppercase.csv", build = "b37") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_equal(
    finterface[][, .(effect, effect_se, log10p, af)],
    data.table::data.table(
      effect = c(0.1, 0.2, -0.1),
      effect_se = c(0.05, 0.06, 0.04),
      log10p = c(2.3, 1.8, 3.2),
      af = c(0.3, 0.4, 0.2)
    )
  )
})

test_that("a row omitting a trailing sub-field decodes it as NA", {
  # VCF permits a sample value to drop trailing sub-fields, so a row
  # naming fewer values than FORMAT does is well formed.
  dropped_field_data <- create_gwas_vcf_data(
    trait_name = "ukbb-neale",
    trait_data = c("0.1:0.05:2.3", "0.2:0.06", "-0.1:0.04:3.2")
  )
  local_csv_file("gwas_vcf_dropped_field.csv", dropped_field_data, sep = "\t")
  finterface <- new_file_interface(
    "gwas_vcf_dropped_field.csv",
    build = "b37"
  ) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_equal(
    finterface[][, .(effect, effect_se, log10p)],
    data.table::data.table(
      effect = c(0.1, 0.2, -0.1),
      effect_se = c(0.05, 0.06, 0.04),
      log10p = c(2.3, NA, 3.2)
    )
  )
})

test_that("a single-sample genotype VCF opens as a plain file", {
  genotype_data <- create_gwas_vcf_data(
    format_string = "GT:AD:DP",
    trait_name = "na12878",
    trait_data = "0/1:10,5:15",
    nrows = 1
  )
  local_csv_file("genotype_vcf.csv", genotype_data, sep = "\t")
  expect_no_warning(
    finterface <- new_file_interface("genotype_vcf.csv", build = "b37") |>
      suppressMessages() |>
      withr::with_output_sink(new = "/dev/null")
  )

  expect_false("effect" %in% column_names(finterface))
  expect_true("na12878" %in% column_names(finterface))
})

test_that("a GWAS-VCF with meta lines and a #CHROM header is detected", {
  local_file_with_comments(
    "gwas_vcf_commented.vcf.gz",
    comment_lines = c(
      "##fileformat=VCFv4.2",
      "##FORMAT=<ID=ES,Number=A,Type=Float,Description=\"Effect size\">"
    ),
    header_prefix = "#",
    content_dt = data.table::data.table(
      CHROM = "1",
      POS = 10177,
      ID = ".",
      REF = "AC",
      ALT = "A",
      QUAL = ".",
      FILTER = "PASS",
      INFO = ".",
      FORMAT = "ES:SE:LP",
      `UKB-b-19953` = "-0.0048:0.0031:0.9208"
    )
  )
  finterface <- new_file_interface(
    "gwas_vcf_commented.vcf.gz",
    build = "b37"
  ) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  encoded_columns <- finterface$column_info[!sapply(encoded_names, is.null)]

  expect_equal(nrow(encoded_columns), 1)
  expect_equal(encoded_columns$input_name, "UKB-b-19953")
  expect_equal(
    encoded_columns$encoded_names[[1]],
    c("effect", "effect_se", "log10p")
  )
})
