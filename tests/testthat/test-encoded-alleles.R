test_that("SNPAlleles encodes (ref, alt) virtual columns end-to-end", {
  dt <- data.table::data.table(
    chr        = paste0("chr", 1:4),
    pos        = c(100L, 200L, 300L, 400L),
    SNPAlleles = c("A/G", "C/T", "G/A", "T/C"),
    effect     = c(0.1, 0.2, 0.3, 0.4),
    pval       = c(0.01, 0.02, 0.03, 0.04)
  )
  local_csv_file("data.csv", dt = dt)

  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_true(all(c("ref", "alt") %in% column_names(finterface)))
  result <- finterface[]
  expect_equal(result$ref, c("A", "C", "G", "T"))
  expect_equal(result$alt, c("G", "T", "A", "C"))
})

test_that("filter on encoded ref/alt picks the right rows", {
  dt <- data.table::data.table(
    chr        = paste0("chr", 1:4),
    pos        = c(100L, 200L, 300L, 400L),
    SNPAlleles = c("A/G", "C/T", "G/A", "T/C"),
    effect     = c(0.1, 0.2, 0.3, 0.4),
    pval       = c(0.01, 0.02, 0.03, 0.04)
  )
  local_csv_file("data.csv", dt = dt)

  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_equal(finterface[ref == "A"]$pos, 100L)
  expect_equal(finterface[alt == "A"]$pos, 300L)
})

test_that("lower-case SNPAlleles inputs are upper-cased on output", {
  dt <- data.table::data.table(
    chr        = paste0("chr", 1:4),
    pos        = c(100L, 200L, 300L, 400L),
    SNPAlleles = c("a/g", "C/t", "g/A", "t/c"),
    effect     = c(0.1, 0.2, 0.3, 0.4),
    pval       = c(0.01, 0.02, 0.03, 0.04)
  )
  local_csv_file("data.csv", dt = dt)

  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  result <- finterface[]
  expect_equal(result$ref, c("A", "C", "G", "T"))
  expect_equal(result$alt, c("G", "T", "A", "C"))
})

test_that("allele matching does not trigger when SNPAlleles provides ref", {
  column_info <- data.table::data.table(
    standard_name = c(
      "chr", "pos", "SNPAlleles", "allele1", "allele2", "alt"
    ),
    encoded_names = list(
      NULL, NULL, c("ref", "alt"), NULL, NULL, NULL
    )
  )
  expect_false(needs_a1_a2_to_ref_matching(column_info))
})

test_that("allele matching still triggers without an encoded ref source", {
  column_info <- data.table::data.table(
    standard_name = c("chr", "pos", "allele1", "allele2", "alt"),
    encoded_names = list(NULL, NULL, NULL, NULL, NULL)
  )
  expect_true(needs_a1_a2_to_ref_matching(column_info))
})
