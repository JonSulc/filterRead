test_that("toupper emitted for native allele columns", {
  column_info <- data.table::data.table(
    name            = c("chr", "pos", "ref", "alt", "effect", "pval"),
    standard_name   = c("chr", "pos", "ref", "alt", "effect", "pval"),
    bash_index      = paste0("$", 1:6),
    encoding_column = NA_character_,
    encoded_names   = list(NULL, NULL, NULL, NULL, NULL, NULL)
  )
  expect_equal(
    build_allele_uppercase_code(column_info),
    list(
      before_if = c("$3 = toupper($3)", "$4 = toupper($4)"),
      after_if = character(0)
    )
  )
})

test_that("build_allele_uppercase_code emits toupper for allele1 / allele2", {
  column_info <- data.table::data.table(
    name            = c("chr", "pos", "allele1", "allele2", "alt", "pval"),
    standard_name   = c("chr", "pos", "allele1", "allele2", "alt", "pval"),
    bash_index      = paste0("$", 1:6),
    encoding_column = NA_character_,
    encoded_names   = list(NULL, NULL, NULL, NULL, NULL, NULL)
  )
  expect_equal(
    build_allele_uppercase_code(column_info)$before_if,
    c("$3 = toupper($3)", "$4 = toupper($4)", "$5 = toupper($5)")
  )
})

test_that("toupper helper is empty when no allele columns are present", {
  column_info <- data.table::data.table(
    name            = c("chr", "pos", "effect", "pval"),
    standard_name   = c("chr", "pos", "effect", "pval"),
    bash_index      = paste0("$", 1:4),
    encoding_column = NA_character_,
    encoded_names   = list(NULL, NULL, NULL, NULL)
  )
  expect_equal(
    build_allele_uppercase_code(column_info),
    list(before_if = character(0), after_if = character(0))
  )
})

test_that("encoded virtual alleles route by required_for_if", {
  # Simulates a SNPAlleles-style encoded column producing ref + alt virtuals.
  column_info <- data.table::data.table(
    name            = c("SNPAlleles", "ref", "alt"),
    standard_name   = c(NA, "ref", "alt"),
    bash_index      = c("$1", "encoded1[1]", "encoded1[2]"),
    encoding_column = c(NA, "SNPAlleles", "SNPAlleles"),
    encoded_names   = list(c("ref", "alt"), NULL, NULL)
  )
  # Encoded column not used in any condition: toupper goes in after_if.
  expect_equal(
    build_allele_uppercase_code(column_info, required_for_if = character(0)),
    list(
      before_if = character(0),
      after_if = c(
        "encoded1[1] = toupper(encoded1[1])",
        "encoded1[2] = toupper(encoded1[2])"
      )
    )
  )
  # Encoded column used in a condition: toupper goes in before_if.
  expect_equal(
    build_allele_uppercase_code(column_info, required_for_if = "SNPAlleles"),
    list(
      before_if = c(
        "encoded1[1] = toupper(encoded1[1])",
        "encoded1[2] = toupper(encoded1[2])"
      ),
      after_if = character(0)
    )
  )
})

test_that("Mixed-case ref/alt are upper-cased on read", {
  local_csv_file(
    "data.csv",
    dt = data.table::data.table(
      chr    = paste0("chr", 1:4),
      pos    = c(100L, 200L, 300L, 400L),
      ref    = c("a", "G", "c", "T"),
      alt    = c("G", "a", "T", "c"),
      effect = c(0.1, 0.2, 0.3, 0.4),
      pval   = c(0.01, 0.02, 0.03, 0.04)
    )
  )
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  result <- finterface[]
  expect_equal(result$ref, c("A", "G", "C", "T"))
  expect_equal(result$alt, c("G", "A", "T", "C"))
})

test_that("Already-upper-case alleles round-trip identically", {
  dt <- data.table::data.table(
    chr    = paste0("chr", 1:4),
    pos    = c(100L, 200L, 300L, 400L),
    ref    = c("A", "G", "C", "T"),
    alt    = c("G", "A", "T", "C"),
    effect = c(0.1, 0.2, 0.3, 0.4),
    pval   = c(0.01, 0.02, 0.03, 0.04)
  )
  local_csv_file("data.csv", dt = dt)
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_equal(finterface[]$ref, dt$ref)
  expect_equal(finterface[]$alt, dt$alt)
})

test_that("allele1 / allele2 columns are upper-cased on read", {
  dt <- data.table::data.table(
    chr     = paste0("chr", 1:4),
    pos     = c(100L, 200L, 300L, 400L),
    allele1 = c("a", "G", "c", "T"),
    allele2 = c("G", "a", "T", "c"),
    alt     = c("G", "A", "T", "C"),
    effect  = c(0.1, 0.2, 0.3, 0.4),
    pval    = c(0.01, 0.02, 0.03, 0.04)
  )
  local_csv_file("data.csv", dt = dt)
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  result <- finterface[]
  expect_equal(result$allele1, c("A", "G", "C", "T"))
  expect_equal(result$allele2, c("G", "A", "T", "C"))
})

test_that("Filter literals on allele columns are upper-cased at parse time", {
  dt <- data.table::data.table(
    chr    = paste0("chr", 1:4),
    pos    = c(100L, 200L, 300L, 400L),
    ref    = c("a", "G", "c", "T"),
    alt    = c("G", "a", "T", "c"),
    effect = c(0.1, 0.2, 0.3, 0.4),
    pval   = c(0.01, 0.02, 0.03, 0.04)
  )
  local_csv_file("data.csv", dt = dt)
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  # Lowercase RHS should still match the (uppercased) file values.
  result_lower <- finterface[ref == "a"]
  result_upper <- finterface[ref == "A"]
  expect_equal(result_lower$pos, 100L)
  expect_equal(result_upper$pos, 100L)
  # %in% RHS literals also normalize.
  result_in <- finterface[ref %in% c("a", "c")]
  expect_equal(sort(result_in$pos), c(100L, 300L))
})
