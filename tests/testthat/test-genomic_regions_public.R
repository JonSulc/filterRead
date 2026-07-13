test_that("genomic_regions() refuses a data.table and points at as_genomic_regions", {
  expect_error(
    genomic_regions(
      data.table::data.table(chr = "chr1", start = 1L, end = 2L)
    ),
    "as_genomic_regions"
  )
})

test_that("finterface[genomic_regions(...)] returns in-range rows", {
  fi <- local_file_interface(
    dt = data.table::data.table(
      chr  = c("chr1", "chr1", "chr2"),
      pos  = c(100L, 200L, 300L),
      ref  = c("A", "C", "G"),
      alt  = c("G", "T", "A"),
      pval = c(0.1, 0.2, 0.3)
    ),
    build = "b38"
  )
  gregions <- genomic_regions(
    chr = "1", start = 150L, end = 350L, build = "b38"
  )
  result <- fi[gregions]
  expect_equal(result$pos, 200L)
  expect_equal(result$chr, "chr1")
})

test_that("empty genomic_regions matches nothing without awk", {
  testthat::local_mocked_bindings(
    fcondition_to_awk = function(...) {
      stop("fcondition_to_awk should not be called for empty genomic_regions")
    }
  )
  fi <- local_summary_stats_interface(build = "b38")
  empty <- as_genomic_regions(
    data.table::data.table(
      chr = character(), start = integer(), end = integer()
    ),
    build = "b38"
  )
  expect_equal(nrow(fi[empty]), 0)
})

test_that("buildless empty genomic_regions matches nothing without awk", {
  testthat::local_mocked_bindings(
    fcondition_to_awk = function(...) {
      stop("fcondition_to_awk should not be called for empty genomic_regions")
    }
  )
  fi <- local_summary_stats_interface(build = "b38")
  empty <- genomic_regions(
    chr = character(), start = integer(), end = integer()
  )
  expect_equal(nrow(fi[empty]), 0)
})

test_that("finterface[gregions] lifts the regions to the file build", {
  testthat::local_mocked_bindings(
    get_chain_dt = function(from, to) make_fwd_chain(100L, 200L, 50L, "b37", "b38")
  )
  fi <- local_file_interface(
    dt = data.table::data.table(
      chr  = c("chr1", "chr1"),
      pos  = c(100L, 500L),
      ref  = c("A", "C"),
      alt  = c("G", "T"),
      pval = c(0.1, 0.2)
    ),
    build = "b38"
  )
  # b37 chr1:140-160 lifts (offset 50) to b38 chr1:90-110, matching pos 100.
  gregions <- genomic_regions(
    chr = "1", start = 140L, end = 160L, build = "b37"
  )
  result <- fi[gregions]
  expect_equal(result$pos, 100L)
})

test_that("finterface[gregions] errors when the region has no build but the file does", {
  fi <- local_file_interface(
    dt = data.table::data.table(
      chr = "chr1", pos = 100L, ref = "A", alt = "G", pval = 0.1
    ),
    build = "b38"
  )
  gregions <- genomic_regions(chr = "1", start = 90L, end = 110L)
  expect_error(fi[gregions], "no build")
})

test_that("finterface[gregions] errors when the file has no build but the region does", {
  fi <- local_file_interface(
    dt = data.table::data.table(
      chr = "chr1", pos = 100L, ref = "A", alt = "G", pval = 0.1
    ),
    build = NULL
  )
  gregions <- genomic_regions(
    chr = "1", start = 90L, end = 110L, build = "b37"
  )
  expect_error(fi[gregions], "no declared build")
})
