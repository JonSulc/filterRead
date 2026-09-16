test_that("region_set() errors on a data.table and points at as_region_set", {
  expect_error(
    region_set(
      data.table::data.table(chr = "chr1", start = 1L, end = 2L)
    ),
    "as_region_set"
  )
})

test_that("finterface[region_set(...)] returns in-range rows", {
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
  regions <- region_set(
    chr = "1", start = 150L, end = 350L, build = "b38"
  )
  result <- fi[regions]
  expect_equal(result$pos, 200L)
  expect_equal(result$chr, "chr1")
})

test_that("empty region_set matches nothing without awk", {
  testthat::local_mocked_bindings(
    fcondition_to_awk = function(...) {
      stop("fcondition_to_awk should not be called for empty region_set")
    }
  )
  fi <- local_summary_stats_interface(build = "b38")
  empty <- as_region_set(
    data.table::data.table(
      chr = character(), start = integer(), end = integer()
    ),
    build = "b38"
  )
  expect_equal(nrow(fi[empty]), 0)
})

test_that("buildless empty region_set matches nothing without awk", {
  testthat::local_mocked_bindings(
    fcondition_to_awk = function(...) {
      stop("fcondition_to_awk should not be called for empty region_set")
    }
  )
  fi <- local_summary_stats_interface(build = "b38")
  empty <- region_set(
    chr = character(), start = integer(), end = integer()
  )
  expect_equal(nrow(fi[empty]), 0)
})

test_that("finterface[regions] lifts the regions to the file build", {
  testthat::local_mocked_bindings(
    get_shared_chain = function(from, to) make_fwd_chain(100L, 200L, 50L, "b37", "b38")
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
  regions <- region_set(
    chr = "1", start = 140L, end = 160L, build = "b37"
  )
  result <- fi[regions]
  expect_equal(result$pos, 100L)
})

test_that("finterface[regions] errors when the region has no build but the file does", {
  fi <- local_file_interface(
    dt = data.table::data.table(
      chr = "chr1", pos = 100L, ref = "A", alt = "G", pval = 0.1
    ),
    build = "b38"
  )
  regions <- region_set(chr = "1", start = 90L, end = 110L)
  expect_error(fi[regions], "no build")
})

test_that("finterface[regions] errors when the file has no build but the region does", {
  fi <- local_file_interface(
    dt = data.table::data.table(
      chr = "chr1", pos = 100L, ref = "A", alt = "G", pval = 0.1
    ),
    build = NULL
  )
  regions <- region_set(
    chr = "1", start = 90L, end = 110L, build = "b37"
  )
  expect_error(fi[regions], "no declared build")
})

test_that("as_region_set() reports each missing coordinate column", {
  expect_error(
    as_region_set(
      data.table::data.table(start = 1L, end = 2L), build = "b38"
    ),
    "missing: chr"
  )
  expect_error(
    as_region_set(
      data.table::data.table(chr = "chr1", end = 2L), build = "b38"
    ),
    "missing: start"
  )
  expect_error(
    as_region_set(
      data.table::data.table(chr = "chr1", start = 1L), build = "b38"
    ),
    "missing: end"
  )
  expect_error(
    as_region_set(
      data.table::data.table(foo = 1L), build = "b38"
    ),
    "missing: chr, start, end"
  )
})

test_that("a blank chromosome name selects rows whose chromosome is blank", {
  fi <- local_file_interface(
    dt = data.table::data.table(
      chr  = c("chr1", "", "chr2"),
      pos  = c(100L, 200L, 300L),
      ref  = c("A", "C", "G"),
      alt  = c("G", "T", "A"),
      pval = c(0.1, 0.2, 0.3)
    ),
    quote = FALSE,
    build = "b38"
  )
  blank_only <- region_set(chr = "", start = 150L, end = 350L,
                           build = "b38")
  expect_equal(fi[blank_only]$pos, 200L)

  with_named <- region_set(chr = c("", "1"), start = 1L, end = 350L,
                           build = "b38")
  result <- fi[with_named]
  expect_equal(result$chr, c("chr1", ""))
  expect_equal(result$pos, c(100L, 200L))

  every_chr <- region_set(chr = NA_character_, start = 1L, end = 350L,
                          build = "b38")
  expect_equal(nrow(fi[every_chr]), 3L)
})

test_that("chr %in% a vector holding a blank chromosome name reads", {
  fi <- local_file_interface(
    dt = data.table::data.table(
      chr  = c("chr1", "", "chr2"),
      pos  = c(100L, 200L, 300L),
      ref  = c("A", "C", "G"),
      alt  = c("G", "T", "A"),
      pval = c(0.1, 0.2, 0.3)
    ),
    quote = FALSE,
    build = "b38"
  )
  chromosomes <- c("1", "")
  result <- fi[chr %in% chromosomes]
  expect_equal(result$chr, c("chr1", ""))
})
