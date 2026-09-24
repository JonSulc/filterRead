test_that("resolve_post_processing spells a chr literal alike for any file", {
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
    resolve_post_processing(c("chr1", "1"), "chr", prefixed, to_write = TRUE),
    c("1", "1", "chr1", "chr1")
  )
  expect_equal(
    resolve_post_processing(c("chr1", "1"), "chr", bare, to_write = TRUE),
    c("1", "1", "chr1", "chr1")
  )
})

test_that("drop_prefix takes the prefix as a literal string", {
  expect_equal(drop_prefix(c("c.r1", "chr2"), "c.r"), c("1", "chr2"))
})
