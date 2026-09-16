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

test_that(
  "matches_gwas_vcf_structure holds for no single-sample genotype VCF",
  {
    expect_false(
      matches_gwas_vcf_structure(c(vcf_cols_1_9, "NA12878"), "GT:AD:DP:GQ:PL")
    )
  }
)

test_that(
  "matches_gwas_vcf_structure holds only for the GWAS-VCF column layout",
  {
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
  }
)

test_that("matches_gwas_vcf_structure yields FALSE for an NA name", {
  raw_names <- c(vcf_cols_1_9, "trait-1")
  raw_names[3] <- NA_character_

  expect_false(matches_gwas_vcf_structure(raw_names, "ES:SE:LP"))
})

test_that("carries_indicator_key admits one indicator key", {
  for (key in gwas_vcf_indicator_keys) {
    expect_true(carries_indicator_key(key))
  }
  expect_true(carries_indicator_key("ES:SE"))
  expect_true(carries_indicator_key("ES:SE:LP:AF"))
  expect_true(carries_indicator_key("ES:SE:LP:AF:SS:EZ:SI:NC:ID"))
})

test_that("carries_indicator_key admits a FORMAT with unknown keys", {
  expect_true(carries_indicator_key("ES:SE:XX:LP"))
  expect_true(carries_indicator_key("ES:SE:ADF"))
  expect_true(carries_indicator_key("ES:GLE:AHAP"))
})

test_that("carries_indicator_key holds for no FORMAT of only shared keys", {
  expect_false(carries_indicator_key("AF"))
  expect_false(carries_indicator_key("SS"))
  expect_false(carries_indicator_key("ID"))
  expect_false(carries_indicator_key("AF:SS"))
  expect_false(carries_indicator_key("ID:AF:SS"))
})

test_that("carries_indicator_key holds for no genotype VCF FORMAT", {
  expect_false(carries_indicator_key("GT:AD:DP:GQ:PL"))
  expect_false(carries_indicator_key("GT:AD:DP:GQ:PL:AF"))
  expect_false(carries_indicator_key("es:se:lp"))
})

test_that("carries_indicator_key holds for no FORMAT naming GT", {
  expect_false(carries_indicator_key("GT:GQ:AD:DP:VF:NL:SB:NC"))
  expect_false(carries_indicator_key("GT:PS:PQ:SE:NE:PR"))
  expect_false(carries_indicator_key("GT:AD:DP:GQ:PL:ES"))
})

test_that("carries_indicator_key holds for no malformed or missing FORMAT", {
  expect_false(carries_indicator_key("ES::LP"))
  expect_false(carries_indicator_key("ES:LP:"))
  expect_false(carries_indicator_key(":ES:LP"))
  expect_false(carries_indicator_key(""))
  expect_false(carries_indicator_key(NA_character_))
  expect_false(carries_indicator_key(NULL))
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
    paste0(
      "^([^:]*):([^:]*):([^:]*):([^:]*):([^:]*)",
      ":([^:]*):([^:]*):([^:]*):([^:]*)$"
    )
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

test_that("gwas_vcf_encoded_column_parsing describes a single-key FORMAT", {
  parsing_info <- gwas_vcf_encoded_column_parsing("simple-trait", "ES")

  expect_equal(parsing_info$input_name, "simple-trait")
  expect_equal(parsing_info$regex, "^([^:]*)$")
  expect_equal(parsing_info$encoded_names[[1]], "effect")
  expect_equal(parsing_info$delimiter, ":")
})

test_that("gwas_vcf_encoded_column_parsing names each key on its own", {
  name_of <- function(key) {
    gwas_vcf_encoded_column_parsing("trait-1", key)$encoded_names[[1]]
  }

  expect_equal(name_of("ES"), "effect")
  expect_equal(name_of("SE"), "effect_se")
  expect_equal(name_of("LP"), "log10p")
  expect_equal(name_of("AF"), "af")
  expect_equal(name_of("SS"), "sample_size")
  expect_equal(name_of("EZ"), "zscore")
  expect_equal(name_of("SI"), "imputation_accuracy")
  expect_equal(name_of("NC"), "n_cases")
  expect_equal(name_of("ID"), "rsid")
})

test_that("gwas_vcf_encoded_column_parsing names a combination of keys", {
  names_of <- function(format_string) {
    gwas_vcf_encoded_column_parsing(
      "combo-trait",
      format_string
    )$encoded_names[[1]]
  }

  expect_equal(names_of("ES:SE"), c("effect", "effect_se"))
  expect_equal(names_of("ID:AF:SS"), c("rsid", "af", "sample_size"))
  expect_equal(
    names_of("EZ:SI:NC"),
    c("zscore", "imputation_accuracy", "n_cases")
  )
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

test_that("get_gwas_vcf_parsing errors on a constructed interface", {
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

test_that("a FORMAT naming ID decodes four columns", {
  # The sample column's ID sub-field decodes to rsid, which the file's
  # own ID column already supplies, so only four decoded columns
  # survive. The two carry different values here to pin which wins.
  gwas_vcf_data <- create_gwas_vcf_data(
    format_string = "ES:SE:LP:AF:ID",
    trait_name = "UKB-b-19953",
    trait_data = c(
      "0.1:0.05:2.3:0.3:rs99",
      "0.2:0.06:1.8:0.4:rs88",
      "-0.1:0.04:3.2:0.2:rs77"
    )
  )
  gwas_vcf_data[, ID := c("rs11", "rs22", "rs33")][]
  local_csv_file("gwas_vcf_with_id.csv", gwas_vcf_data, sep = "\t")
  finterface <- new_file_interface("gwas_vcf_with_id.csv", build = "b37") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  result <- finterface[]

  expect_equal(
    result[, .(effect, effect_se, log10p, af)],
    data.table::data.table(
      effect = c(0.1, 0.2, -0.1),
      effect_se = c(0.05, 0.06, 0.04),
      log10p = c(2.3, 1.8, 3.2),
      af = c(0.3, 0.4, 0.2)
    )
  )
  expect_equal(result$rsid, c("rs11", "rs22", "rs33"))
  expect_equal(
    names(result),
    c("chr", "pos", "rsid", "ref", "alt", "QUAL", "FILTER", "INFO",
      "FORMAT", "UKB-b-19953", "effect", "effect_se", "log10p", "af")
  )
})

test_that("a record omitting a trailing sub-field decodes it as NA", {
  # VCF permits a sample value to drop trailing sub-fields, so a
  # record naming fewer values than FORMAT does is well formed.
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

test_that("a genotype VCF naming an indicator key opens as a plain VCF", {
  no_call_data <- create_gwas_vcf_data(
    format_string = "GT:GQ:AD:DP:VF:NL:SB:NC",
    trait_name = "SampleA",
    trait_data = "0/1:60:450,50:500:0.10:20:-15.5:0.0021",
    nrows = 1
  )
  local_csv_file("genotype_no_call.csv", no_call_data, sep = "\t")
  finterface <- new_file_interface("genotype_no_call.csv", build = "b37") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_false("n_cases" %in% column_names(finterface))
  expect_equal(
    finterface[]$SampleA,
    "0/1:60:450,50:500:0.10:20:-15.5:0.0021"
  )
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

test_that("an unrecognized FORMAT key reads back as its own column", {
  passthrough_data <- create_gwas_vcf_data(
    format_string = "ES:SE:XX:LP",
    trait_name = "UKB-b-19953",
    trait_data = c(
      "0.1:0.05:500.5:2.3", "0.2:0.06:1500.5:1.8", "-0.1:0.04:2000.5:3.2"
    )
  )
  local_csv_file("gwas_vcf_passthrough.csv", passthrough_data, sep = "\t")
  finterface <- new_file_interface(
    "gwas_vcf_passthrough.csv",
    build = "b37"
  ) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_equal(
    finterface[][, .(effect, effect_se, XX, log10p)],
    data.table::data.table(
      effect = c(0.1, 0.2, -0.1),
      effect_se = c(0.05, 0.06, 0.04),
      XX = c(500.5, 1500.5, 2000.5),
      log10p = c(2.3, 1.8, 3.2)
    )
  )
  expect_equal(finterface[XX < 1000]$XX, 500.5)
})

test_that("a FORMAT of only shared keys opens as a plain VCF", {
  shared_key_data <- create_gwas_vcf_data(
    format_string = "AF:SS",
    trait_name = "trait-1",
    trait_data = "0.3:1000",
    nrows = 1
  )
  local_csv_file("shared_keys.csv", shared_key_data, sep = "\t")
  expect_no_warning(
    finterface <- new_file_interface("shared_keys.csv", build = "b37") |>
      suppressMessages() |>
      withr::with_output_sink(new = "/dev/null")
  )

  encoded_columns <- finterface$column_info[!sapply(encoded_names, is.null)]

  expect_equal(nrow(encoded_columns), 0)
  expect_equal(finterface[]$`trait-1`, "0.3:1000")
})
