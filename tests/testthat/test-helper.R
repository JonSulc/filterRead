test_that("Dummy csv initialization works", {
  expect_false(file.exists(filename = "data.csv"))
  local_csv_file(filename = "data.csv")
  expect_true(file.exists(filename = "data.csv"))
})

test_that("Dummy csv cleanup works", {
  expect_false(file.exists(filename = "data.csv"))
})

test_that("Non-quoted values work properly", {
  local_csv_file(filename = "data.csv")
  test <- data.table::fread("data.csv", quote = "")
  expect_false(any(stringr::str_detect(names(test), "\"")))
  expect_equal(
    test,
    dummy_dt()
  )
})

test_that("Quoted values work", {
  local_csv_file(filename = "data.csv", quote = TRUE)
  test <- data.table::fread("data.csv", quote = "")
  expect_true(all(stringr::str_detect(names(test), "\"")))
  expect_equal(
    test,
    dummy_dt(values_are_quoted = TRUE)
  )
})

test_that("Prefixes work", {
  local_csv_file(filename = "data.csv", prefix = c(char = "chr"))
  test <- data.table::fread("data.csv", quote = "")
  expect_equal(
    test,
    dummy_dt(prefix = c(char = "chr"))
  )
})

test_that("Simulated stats initialize properly", {
  dt <- dummy_summary_stats(
    nrows = 100,
    chr = 3,
    start = 42,
    end = 421,
    random_names = FALSE,
    prefix = list(chr = "chr")
  )
  expect_equal(nrow(dt), 100)
  expect_true(all(dt[
    ,
    chr == "chr3" &
      42 <= pos & pos <= 421 &
      ref %chin% c("A", "C", "G", "T") & alt %chin% c("A", "C", "G", "T") &
      0 <= pval & pval <= 1 &
      is.numeric(effect)
  ]))

  expect_false(file.exists(filename = "data.csv"))
  local_summary_stats(filename = "data.csv")
  expect_true(file.exists(filename = "data.csv"))
})

test_that("Dummy summary stats cleanup works", {
  expect_false(file.exists(filename = "data.csv"))
})

test_that("Quoted summary stats work", {
  local_summary_stats(filename = "data.csv", values_are_quoted = TRUE)
  test <- data.table::fread("data.csv", quote = "")
  expect_true(all(stringr::str_detect(names(test), "\"[^\"]")))
})

test_that("Column encdoding works", {
  finterface_enc <- local_summary_stats_interface(
    "encoded.csv",
    encode_columns = summary_stats_standard_names_dt[
      list(input_name = "MarkerName", delimiter = "-c|:|-"),
      on = c("input_name", "delimiter")
    ],
    random_names = FALSE
  )
  raw_data <- data.table::fread("encoded.csv")
  expect_equal(
    names(raw_data),
    c("MarkerName", "ref", "alt", "effect", "pval")
  )
  expect_true(
    grepl("^(b3[6-8])-c([^:]{1,2}):([0-9]+)-[0-9]+$", raw_data$MarkerName) |>
      all()
  )
})

test_that("Creating RSID-coded summary stats works", {
  summary_stats <- dummy_rsid_summary_stats(columns_to_drop = c("chr", "pos"))
  expect_equal(
    colnames(summary_stats),
    c("rsid", "ref", "alt", "effect", "pval")
  )
  expect_equal(
    colnames(dummy_rsid_summary_stats(columns_to_drop = "chr")),
    c("rsid", "pos", "ref", "alt", "effect", "pval")
  )
  expect_equal(
    colnames(dummy_rsid_summary_stats(columns_to_drop = "pos")),
    c("rsid", "chr", "ref", "alt", "effect", "pval")
  )
  expect_equal(
    colnames(dummy_rsid_summary_stats(columns_to_drop = NULL)),
    c("rsid", "chr", "pos", "ref", "alt", "effect", "pval")
  )

  expect_equal(
    nrow(summary_stats),
    500
  )
  expect_false(
    any(duplicated(summary_stats$rsid))
  )

  finterface <- local_rsid_summary_stats_interface() |>
    suppressWarnings()
  expect_equal(
    column_names(finterface),
    c("chr", "pos", "rsid", "ref", "alt", "effect", "pval")
  )
  expect_equal(
    column_names(finterface, original = TRUE),
    c("rsid", "ref", "alt", "effect", "pval")
  )
})

test_that("switch_alt_ref_to_a1_a2 produces correct columns and values", {
  # Create a dummy summary stats table
  dt <- dummy_summary_stats(nrows = 100, random_names = FALSE)
  dt_copy <- data.table::copy(dt)

  # Apply the function
  result <- switch_alt_ref_to_a1_a2(dt_copy)

  # Check columns
  expect_true(all(c("allele1", "allele2", "alt") %in% names(result)))
  expect_false("ref" %in% names(result))

  # For each row, alt should match one of the alleles
  expect_true(all(result$alt == result$allele1 | result$alt == result$allele2))

  # Check randomness: about half the time allele1 == alt
  prop_allele1_is_alt <- mean(result$allele1 == result$alt)
  expect_true(0.3 < prop_allele1_is_alt && prop_allele1_is_alt < 0.7)
})
