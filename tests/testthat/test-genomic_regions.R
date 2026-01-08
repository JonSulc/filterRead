test_that("empty initialization works", {
  genomic_regions <- new_genomic_regions()
  expect_equal(
    nrow(genomic_regions),
    0
  )
  expect_true(
    is.genomic_regions(genomic_regions)
  )
  expect_equal(
    colnames(genomic_regions),
    c("chr", "start", "end")
  )
  expect_null(
    attr(genomic_regions, "build")
  )
  expect_false(
    attr(genomic_regions, "pos_only")
  )

  genomic_regions_b38 <- new_genomic_regions(build = "b38")
  expect_equal(
    nrow(genomic_regions_b38),
    0
  )
  expect_true(
    is.genomic_regions(genomic_regions_b38)
  )
  expect_equal(
    colnames(genomic_regions_b38),
    c("chr", "start", "end")
  )
  expect_equal(
    attr(genomic_regions_b38, "build"),
    "b38"
  )
  expect_false(
    attr(genomic_regions_b38, "pos_only")
  )
})

test_that("incomplete initialization works", {
  gr_pos <- new_genomic_regions(pos = 42)
  expect_equal(
    gr_pos,
    new_genomic_regions(chr = NA_character_, pos = 42)
  )
  expect_true(
    attr(gr_pos, "pos_only")
  )
  expect_null(attr(gr_pos, "build"))

  expect_equal(
    new_genomic_regions(
      chr = "chr1"
    ),
    new_genomic_regions(
      chr = "chr1",
      start = NA_integer_,
      end = NA_integer_
    )
  )

  expect_equal(
    new_genomic_regions(
      chr = "chr1",
      start = 42
    ),
    new_genomic_regions(
      chr = "chr1",
      start = 42,
      end = NA_integer_
    )
  )
})

test_that("initialization works", {
  genomic_regions <- new_genomic_regions(
    chr = "chr1",
    start = 21,
    end = 42
  )
  expect_equal(
    genomic_regions,
    data.table::data.table(
      chr = "chr1",
      start = 21,
      end = 42
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    attributes(genomic_regions)[
      setdiff(names(attributes(genomic_regions)), ".internal.selfref")
    ],
    list(
      names = c("chr", "start", "end"),
      row.names = 1,
      class = c("genomic_regions", "data.table", "data.frame"),
      pos_only = FALSE
    )
  )

  genomic_regions <- new_genomic_regions(
    chr = "chr1",
    pos = 42
  )
  expect_equal(
    genomic_regions,
    data.table::data.table(
      chr = "chr1",
      start = 42,
      end = 42
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    attributes(genomic_regions)[
      setdiff(names(attributes(genomic_regions)), ".internal.selfref")
    ],
    list(
      names = c("chr", "start", "end"),
      row.names = 1,
      class = c("genomic_regions", "data.table", "data.frame"),
      pos_only = TRUE
    )
  )
})
