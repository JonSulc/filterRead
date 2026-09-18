test_that("resolve_post_processing normalizes chr prefix to the file's form", {
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
    c("chr1", "chr1")
  )
  expect_equal(
    resolve_post_processing(c("chr1", "1"), "chr", bare, to_write = TRUE),
    c("1", "1")
  )
})

test_that("resolve_prefix does not prefix a blank or missing value", {
  expect_equal(
    resolve_prefix(c("1", "", NA, "chr2"), "chr"),
    c("chr1", "", NA, "chr2")
  )
})

test_that("resolve_prefix passes values through where there is no prefix", {
  expect_equal(resolve_prefix(c("1", "chr2"), NA_character_), c("1", "chr2"))
  expect_identical(resolve_prefix(c(1L, 2L), ""), c(1L, 2L))
  expect_equal(resolve_prefix(c("1", "chr2"), character(0)), c("1", "chr2"))
})

test_that("a chr column disagreeing on the prefix errors on a literal", {
  expect_warning(
    finterface <- local_file_interface(
      dt = data.table::data.table(
        chr = c("chr1", "2"), pos = c(1000L, 2000L),
        ref = c("A", "C"), alt = c("G", "T")
      ),
      build = "b38"
    ),
    "agree on a prefix"
  )
  regions <- region_set(chr = "chr1", build = "b38")
  positions_only <- region_set(start = 1L, end = 1500L, build = "b38")
  variants <- new_variant_set(
    data.table::data.table(chr = "chr1", pos = 1000L, ref = "A", alt = "G"),
    build = "b38"
  )

  expect_error(finterface[regions], "agree on a prefix")
  expect_error(finterface[chr == "chr1"], "agree on a prefix")
  expect_error(finterface[chr %in% "chr1"], "agree on a prefix")
  expect_error(finterface[variants], "agree on a prefix")

  expect_equal(finterface[positions_only]$chr, "chr1")
})
