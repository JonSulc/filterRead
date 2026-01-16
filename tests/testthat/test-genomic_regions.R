test_that("empty initialization works", {
  gregions <- new_genomic_regions()
  expect_equal(
    nrow(gregions),
    0
  )
  expect_true(
    is_genomic_regions(gregions)
  )
  expect_equal(
    colnames(gregions),
    c("chr", "start", "end")
  )
  expect_null(
    attr(gregions, "build")
  )

  genomic_regions_b38 <- new_genomic_regions(build = "b38")
  expect_equal(
    nrow(genomic_regions_b38),
    0
  )
  expect_true(
    is_genomic_regions(genomic_regions_b38)
  )
  expect_equal(
    colnames(genomic_regions_b38),
    c("chr", "start", "end")
  )
  expect_equal(
    attr(genomic_regions_b38, "build"),
    "b38"
  )
})

test_that("incomplete initialization works", {
  gregions <- new_genomic_regions(start = 42)
  expect_equal(
    gregions,
    new_genomic_regions(chr = NA_character_, start = 42, end = NA_integer_)
  )
  expect_null(attr(gregions, "build"))

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
      sorted = c("chr", "start", "end")
    ),
    list_as_map = TRUE # Ignore order
  )

  genomic_regions <- new_genomic_regions(
    chr = "chr1",
    start = 42
  )
  expect_equal(
    genomic_regions,
    data.table::data.table(
      chr = "chr1",
      start = 42,
      end = NA_integer_
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
      sorted = c("chr", "start", "end")
    ),
    list_as_map = TRUE # Ignore order
  )
})

test_that("merging overlapping regions works", {
  gregions <- new_genomic_regions(
    chr = "chr1",
    start = c(123, 123),
    end = c(234, 456)
  )
  expect_equal(
    new_genomic_regions(
      chr = "chr1",
      start = c(123, 345),
      end = c(234, 456)
    ) |>
      merge_contiguous_regions(),
    new_genomic_regions(
      chr = "chr1",
      start = c(123, 345),
      end = c(234, 456)
    )
  )
  expect_equal(
    new_genomic_regions(
      chr = c("chr1", "chr2"),
      start = c(123, 123),
      end = c(234, 456)
    ) |>
      merge_contiguous_regions(),
    new_genomic_regions(
      chr = c("chr1", "chr2"),
      start = c(123, 123),
      end = c(234, 456)
    )
  )

  expect_equal(
    new_genomic_regions(
      chr = "chr1",
      start = c(123, 123),
      end = c(234, 456)
    ),
    new_genomic_regions(
      chr = "chr1",
      start = 123,
      end = 456
    )
  )
  expect_equal(
    new_genomic_regions(
      chr = "chr1",
      start = c(123, 123),
      end = c(234, 456),
      merge_contiguous = FALSE
    ) |>
      nrow(),
    2
  )
})

test_that("addition works", {
  gregions1 <- new_genomic_regions(
    chr = "chr1",
    start = 123,
    end = 234
  )
  gregions2 <- new_genomic_regions(
    chr = "chr1",
    start = 345,
    end = 456
  )
  expect_equal(
    gregions1 + gregions2,
    new_genomic_regions(
      chr = "chr1",
      start = c(123, 345),
      end = c(234, 456)
    )
  )
})

test_that("is_genomic_regions returns FALSE for non-genomic_regions", {
  expect_false(is_genomic_regions(data.table::data.table()))
  expect_false(is_genomic_regions(data.frame()))
  expect_false(is_genomic_regions(list()))
  expect_false(is_genomic_regions(NULL))
})

test_that("new_genomic_regions accepts data.table input", {
  dt <- data.table::data.table(
    chr = "chr1",
    start = 100L,
    end = 200L
  )
  gregions <- new_genomic_regions(dt, build = "b37")
  expect_true(is_genomic_regions(gregions))
  expect_equal(attr(gregions, "build"), "b37")
  expect_equal(gregions$chr, "chr1")
  expect_equal(gregions$start, 100L)
  expect_equal(gregions$end, 200L)
})

test_that("as_genomic_regions converts data.table", {
  dt <- data.table::data.table(
    chr = "chr1",
    start = 100L,
    end = 200L
  )
  gregions <- as_genomic_regions(dt, build = "b38")
  expect_true(is_genomic_regions(gregions))
  expect_equal(attr(gregions, "build"), "b38")
})

test_that("as_genomic_regions errors on invalid input", {
  expect_error(
    as_genomic_regions(list(chr = "chr1", start = 100, end = 200)),
    "data.frame"
  )
})

test_that("subsetting preserves build attribute", {
  gregions <- new_genomic_regions(
    chr = c("chr1", "chr2"),
    start = c(100, 200),
    end = c(150, 250),
    build = "b38"
  )
  subset <- gregions[1]
  expect_true(is_genomic_regions(subset))
  expect_equal(attr(subset, "build"), "b38")
})

test_that("union operator is equivalent to addition", {
  gregions1 <- new_genomic_regions(
    chr = "chr1",
    start = 100,
    end = 200
  )
  gregions2 <- new_genomic_regions(
    chr = "chr1",
    start = 300,
    end = 400
  )
  expect_equal(gregions1 | gregions2, gregions1 + gregions2)
})

test_that("union merges overlapping regions", {
  gregions1 <- new_genomic_regions(
    chr = "chr1",
    start = 100,
    end = 250
  )
  gregions2 <- new_genomic_regions(
    chr = "chr1",
    start = 200,
    end = 400
  )
  result <- gregions1 | gregions2
  expect_equal(nrow(result), 1)
  expect_equal(result$start, 100)
  expect_equal(result$end, 400)
})

test_that("intersection returns overlapping portion", {
  gregions1 <- new_genomic_regions(
    chr = "chr1",
    start = 100,
    end = 300
  )
  gregions2 <- new_genomic_regions(
    chr = "chr1",
    start = 200,
    end = 400
  )
  result <- gregions1 & gregions2
  expect_equal(result$start, 200)
  expect_equal(result$end, 300)
})

test_that("intersection returns empty for non-overlapping same chr", {
  gregions1 <- new_genomic_regions(
    chr = "chr1",
    start = 100,
    end = 200
  )
  gregions2 <- new_genomic_regions(
    chr = "chr1",
    start = 300,
    end = 400
  )
  result <- gregions1 & gregions2
  expect_equal(nrow(result), 0)
})

test_that("intersection returns empty for different chromosomes", {
  gregions1 <- new_genomic_regions(
    chr = "chr1",
    start = 100,
    end = 200
  )
  gregions2 <- new_genomic_regions(
    chr = "chr2",
    start = 100,
    end = 200
  )
  result <- gregions1 & gregions2
  expect_equal(nrow(result), 0)
})

test_that("intersection handles NA values", {
  gregions1 <- new_genomic_regions(
    chr = "chr1",
    start = NA_integer_,
    end = 300
  )
  gregions2 <- new_genomic_regions(
    chr = "chr1",
    start = 200,
    end = NA_integer_
  )
  result <- gregions1 & gregions2
  expect_equal(result$start, 200)
  expect_equal(result$end, 300)
})

test_that("intersection handles entire chromosomes", {
  gregions1 <- new_genomic_regions(
    chr = "chr1"
  )
  gregions2 <- new_genomic_regions(
    chr = "chr2"
  )
  expect_equal(
    gregions1 & gregions1,
    gregions1
  )
  expect_equal(
    gregions1 & gregions2,
    new_genomic_regions()
  )
})

test_that("intersection handles partial chromosomes", {
  gregions1 <- new_genomic_regions(
    chr = "chr1",
    end = 234
  )
  gregions2 <- new_genomic_regions(
    chr = "chr1",
    start = 123
  )
  expect_equal(
    gregions1 & gregions2,
    new_genomic_regions(
      chr = "chr1",
      start = 123,
      end = 234
    )
  )
})

test_that("addition handles partial chromosomes", {
  gregions1 <- new_genomic_regions(
    chr = "chr1",
    end = 234
  )
  gregions2 <- new_genomic_regions(
    chr = "chr1",
    start = 123
  )
  expect_equal(
    gregions1 + gregions2,
    new_genomic_regions(
      new_genomic_regions(
        chr = "chr1"
      )
    )
  )
})

test_that("rbind combines genomic_regions", {
  gregions1 <- new_genomic_regions(
    chr = "chr1",
    start = 100,
    end = 200,
    build = "b37"
  )
  gregions2 <- new_genomic_regions(
    chr = "chr2",
    start = 300,
    end = 400
  )
  result <- rbind(gregions1, gregions2) |>
    suppressWarnings()
  expect_equal(nrow(result), 2)
  expect_true(is_genomic_regions(result))
  expect_equal(attr(result, "build"), "b37")
})

test_that("constructor errors on incompatible lengths", {
  expect_error(
    new_genomic_regions(
      chr = c("chr1", "chr2"),
      start = c(100, 200, 300),
      end = c(150, 250)
    ),
    "Invalid argument lengths"
  )
})

test_that("constructor allows length-1 recycling", {
  gregions <- new_genomic_regions(
    chr = "chr1",
    start = c(100, 200),
    end = c(150, 250)
  )
  expect_equal(nrow(gregions), 2)
  expect_equal(unique(gregions$chr), "chr1")
})

test_that("addition with empty regions works", {
  gregions1 <- new_genomic_regions(
    chr = "chr1",
    start = 100,
    end = 200
  )
  g_empty <- new_genomic_regions()
  expect_equal(gregions1 + g_empty, gregions1)
})

test_that("intersection with empty regions returns empty", {
  gregions1 <- new_genomic_regions(
    chr = "chr1",
    start = 100,
    end = 200
  )
  g_empty <- new_genomic_regions()
  result <- gregions1 & g_empty
  expect_equal(
    gregions1 & g_empty,
    g_empty
  )
})

test_that("addition merges overlapping regions", {
  gregions1 <- new_genomic_regions(
    chr = "chr1",
    start = 100,
    end = 250
  )
  gregions2 <- new_genomic_regions(
    chr = "chr1",
    start = 200,
    end = 400
  )
  result <- gregions1 + gregions2
  expect_equal(nrow(result), 1)
  expect_equal(result$start, 100)
  expect_equal(result$end, 400)
})

test_that("intersection handles multi-row genomic_regions", {
  gregions1 <- new_genomic_regions(
    chr = c("chr1", "chr2"),
    start = c(123, 345),
    end = c(234, 456)
  )
  expect_equal(
    gregions1 & gregions1,
    gregions1
  )
  expect_equal(
    gregions1 & new_genomic_regions(
      chr = "chr1",
      start = 42,
      end = 421
    ),
    new_genomic_regions(
      chr = "chr1",
      start = 123,
      end = 234
    )
  )
  expect_equal(
    gregions1 & new_genomic_regions(
      chr = c("chr1", "chr2"),
      start = c(142, 342),
      end = c(242, 442)
    ),
    new_genomic_regions(
      chr = c("chr1", "chr2"),
      start = c(142, 345),
      end = c(234, 442)
    )
  )
})

test_that("chr formatting works", {
  expect_equal(
    new_genomic_regions(chr = 1),
    new_genomic_regions(chr = "chr1")
  )
})

test_that("start cannot be after end", {
  expect_error(
    new_genomic_regions(
      start = 42,
      end = 21
    )
  )
  expect_no_error(
    new_genomic_regions(
      start = 1:10,
      end = 2:11
    )
  )
  expect_no_error(
    new_genomic_regions(
      start = 1:10,
      end = 1:10
    )
  )
  expect_error(
    new_genomic_regions(
      start = 1:10,
      end = 18:9
    )
  )
  expect_error(
    new_genomic_regions(
      start = 1:10,
      end = 9
    )
  )
  expect_no_error(
    new_genomic_regions(
      start = 1:10,
      end = 10
    )
  )
  expect_no_error(
    new_genomic_regions(
      start = 1:10,
      end = NA
    )
  )
})

test_that("adjacent regions merge", {
  # Regions with a gap should remain separate
  expect_equal(
    new_genomic_regions(
      chr = "chr1",
      start = c(100, 202),
      end = c(200, 300)
    ) |>
      nrow(),
    2
  )
  # Regions that are adjacent should be merged
  expect_equal(
    new_genomic_regions(
      chr = "chr1",
      start = c(100, 201),
      end = c(200, 300)
    ),
    new_genomic_regions(
      chr = "chr1",
      start = 100,
      end = 300
    )
  )
})

test_that("intersection with different builds warns", {
  # TODO This now does a liftover of the second, need to test this
  gregions_b37 <- new_genomic_regions(
    chr = "chr1",
    start = 100,
    end = 200,
    build = "b37"
  )
  gregions_b38 <- new_genomic_regions(
    chr = "chr1",
    start = 100,
    end = 200,
    build = "b38"
  )
  expect_warning(
    gregions_b37 & gregions_b38,
    "same build"
  )
})

test_that("intersection with NA chr matches any chromosome", {
  gregions_any <- new_genomic_regions(
    chr = NA_character_,
    start = 100,
    end = 200
  )
  gregions_chr1 <- new_genomic_regions(
    chr = "chr1",
    start = 100,
    end = 200
  )
  expect_equal(
    gregions_any & gregions_chr1,
    gregions_chr1
  )

  gregions1 <- new_genomic_regions(
    chr = c(NA_character_, "chr1"),
    start = c(1, 1001),
    end = c(100, 1100)
  )
  gregions2 <- new_genomic_regions(
    chr = c(NA_character_, "chr2", NA_character_),
    start = c(1042, 42, 21),
    end = c(1142, 142, 42)
  )
  expect_equal(
    gregions1 & gregions2,
    new_genomic_regions(
      chr = c(NA_character_, "chr1", "chr2"),
      start = c(21, 1042, 42),
      end = c(42, 1100, 100)
    )
  )
})

test_that("empty subsetting preserves class and build", {
  gregions <- new_genomic_regions(
    chr = "chr1",
    start = 100,
    end = 200,
    build = "b38"
  )

  # Subset with 0 should return empty but preserve attributes

  empty_subset <- gregions[0]
  expect_equal(nrow(empty_subset), 0)
  expect_true(is_genomic_regions(empty_subset))
  expect_equal(attr(empty_subset, "build"), "b38")

  # Subset with FALSE should also preserve attributes

  empty_logical <- gregions[FALSE]
  expect_equal(nrow(empty_logical), 0)
  expect_true(is_genomic_regions(empty_logical))
  expect_equal(attr(empty_logical, "build"), "b38")
})
