test_that("Commented lines are correctly ignored", {
  local_file_with_comments(
    "commented.vcf.gz",
    comment_lines = c(
      "##fileformat=VCFv4.2",
      "##INFO=<ID=AF,Number=A,Type=Float,Description=\"Allele Frequency\">"
    ),
    header_prefix = "#",
    content_dt = data.table::data.table(
      CHROM = "chr1",
      POS = 10177,
      ID = "rs123",
      REF = "AC",
      ALT = "A",
      QUAL = ".",
      FILTER = "PASS",
      INFO = "AF=0.601001",
      FORMAT = "ES:SE:LP:AF:ID",
      sample = "-0.00481799:0.00310296:0.920819:0.601001:rs123"
    )
  )

  result <- new_file_interface("commented.vcf.gz", ieugwas_parsing = FALSE) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null") |>
    head()

  # Verify comments were skipped and # prefix was trimmed
  expect_equal(names(result)[1], "chr")
  expect_equal(nrow(result), 1)
  expect_equal(result$chr, "chr1")
  expect_equal(result$pos, 10177)
})
