test_that("check_post_processing normalizes chr prefix to the file's form", {
  prefixed <- local_file_interface(
    filename = "prefixed.csv",
    dt = data.table::data.table(
      chr = c("chr1", "chr2"), pos = c(100L, 200L),
      ref = c("A", "C"), alt = c("G", "T"), pval = c(0.1, 0.2)
    ),
    build = "b38"
  )
  bare <- local_file_interface(
    filename = "bare.csv",
    dt = data.table::data.table(
      chr = c("1", "2"), pos = c(100L, 200L),
      ref = c("A", "C"), alt = c("G", "T"), pval = c(0.1, 0.2)
    ),
    build = "b38"
  )
  expect_equal(
    check_post_processing(c("chr1", "1"), "chr", prefixed, to_write = TRUE),
    c("chr1", "chr1")
  )
  expect_equal(
    check_post_processing(c("chr1", "1"), "chr", bare, to_write = TRUE),
    c("1", "1")
  )
})
