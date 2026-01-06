commented_file <- "~/rcp_storage/common//Users/sulc//data/gwas_summary_statistics//raw/ieu-b-110.vcf.gz"

test_that("Commented lines are correctly ignored", {
  expect_equal(
    new_file_interface(commented_file, ieugwas_parsing = FALSE) |>
      suppressMessages() |>
      withr::with_output_sink(new = "/dev/null") |>
      head(),
    data.table::data.table(
      chr = "chr1",
      pos = 10177,
      rsid = "rs1264289758",
      ref = "AC",
      alt = "A",
      QUAL = ".",
      FILTER = "PASS",
      INFO = "AF=0.601001",
      FORMAT = "ES:SE:LP:AF:ID",
      "ieu-b-110" = "-0.00481799:0.00310296:0.920819:0.601001:rs1264289758"
    )
  )
})
