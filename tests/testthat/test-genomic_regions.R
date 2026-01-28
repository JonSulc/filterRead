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
      sorted = c("chr", "start", "end"),
      include = TRUE
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
      sorted = c("chr", "start", "end"),
      include = TRUE
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

test_that("merging preserves attributes", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = c(123, 1234),
    end = c(1234, 2345),
    build = "b37",
    merge_contiguous = FALSE,
    include = FALSE
  )
  merged <- merge_contiguous_regions(gregions)
  expect_equal(
    build(merged),
    "b37"
  )
  expect_false(
    is_included(merged)
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
    "no applicable"
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

# Tests for as_genomic_regions S3 methods
test_that("as_genomic_regions.data.frame preserves build", {
  df <- data.frame(chr = "1", start = 100, end = 200)
  attr(df, "build") <- "b38"

  result <- as_genomic_regions(df)
  expect_true(is_genomic_regions(result))
  expect_equal(build(result), "b38")
  expect_equal(nrow(result), 1)
})

test_that("as_genomic_regions with explicit build parameter", {
  df <- data.frame(chr = "1", start = 100, end = 200)

  result <- as_genomic_regions(df, build = "b37")
  expect_equal(build(result), "b37")

  # Explicit build should override object's build
  attr(df, "build") <- "b38"
  result2 <- as_genomic_regions(df, build = "b36")
  expect_equal(build(result2), "b36")
})

test_that("as_genomic_regions.eq_filter_condition handles chr equality", {
  finterface <- local_summary_stats_interface()
  fcondition <- new_filter_condition(
    rlang::quo(chr == 1),
    finterface,
    build = "b38"
  )

  result <- as_genomic_regions(fcondition)
  expect_equal(
    as_genomic_regions(fcondition),
    new_genomic_regions(chr = 1, build = "b38")
  )
})

test_that("as_genomic_regions.eq_filter_condition handles pos equality", {
  finterface <- local_summary_stats_interface()
  fcondition <- new_filter_condition(
    rlang::quo(pos == 12345),
    finterface,
    build = "b38"
  )

  expect_equal(
    as_genomic_regions(fcondition),
    new_genomic_regions(start = 12345, end = 12345, build = "b38")
  )
})

test_that("as_genomic_regions.lt_filter_condition handles pos < value", {
  finterface <- local_summary_stats_interface()
  fcondition <- new_filter_condition(
    rlang::quo(pos < 1000),
    finterface,
    build = "b38"
  )

  expect_equal(
    as_genomic_regions(fcondition),
    new_genomic_regions(end = 999, build = "b38")
  )
})

test_that("as_genomic_regions.gt_filter_condition handles pos > value", {
  finterface <- local_summary_stats_interface()
  fcondition <- new_filter_condition(
    rlang::quo(pos > 1000),
    finterface,
    build = "b38"
  )

  expect_equal(
    as_genomic_regions(fcondition),
    new_genomic_regions(start = 1001, build = "b38")
  )
})

# Tests for internal helper functions
test_that("empty_genomic_regions creates valid empty regions", {
  empty <- empty_genomic_regions(build = "b38")
  expect_true(is_genomic_regions(empty))
  expect_equal(nrow(empty), 0)
  expect_equal(build(empty), "b38")
  expect_equal(names(empty), c("chr", "start", "end"))
})

test_that("full_genomic_regions creates valid full genome regions", {
  full <- full_genomic_regions(build = "b38")
  expect_true(is_genomic_regions(full))
  expect_equal(nrow(full), 1)
  expect_equal(build(full), "b38")
  expect_true(is.na(full$chr))
})

test_that("copy_genomic_regions preserves class and attributes", {
  gr <- new_genomic_regions(chr = "1", start = 100, end = 200, build = "b38")
  copied <- copy_genomic_regions(gr)

  expect_true(is_genomic_regions(copied))
  expect_equal(build(copied), "b38")
  expect_equal(copied$chr, "chr1")

  # Verify it's a copy (modifying original doesn't affect copy)
  gr[, chr := "2"]
  expect_equal(copied$chr, "chr1")
})

# Edge case tests for NULL/NA handling
test_that("genomic_regions handles NULL build correctly", {
  gr <- new_genomic_regions(chr = "1", start = 100, end = 200, build = NULL)
  expect_null(build(gr))
  expect_true(is_genomic_regions(gr))
})

test_that("genomic_regions operations work with NA chromosomes", {
  gr1 <- new_genomic_regions(
    chr = c("1", NA),
    start = c(100, 200),
    end = c(150, 250),
    build = "b38"
  )
  expect_equal(nrow(gr1), 2)
  # data.table sorts NAs first
  expect_true(is.na(gr1$chr[1]))
})

# Edge case tests for empty inputs
test_that("empty genomic_regions union produces empty result", {
  empty1 <- empty_genomic_regions(build = "b38")
  empty2 <- empty_genomic_regions(build = "b38")

  result <- empty1 | empty2
  expect_equal(nrow(result), 0)
  expect_true(is_genomic_regions(result))
  expect_equal(build(result), "b38")
})

test_that("empty genomic_regions intersection produces empty result", {
  empty1 <- empty_genomic_regions(build = "b38")
  empty2 <- empty_genomic_regions(build = "b38")

  result <- empty1 & empty2
  expect_equal(nrow(result), 0)
  expect_true(is_genomic_regions(result))
})

test_that("intersection with empty returns empty", {
  gr <- new_genomic_regions(chr = "1", start = 100, end = 200, build = "b38")
  empty <- empty_genomic_regions(build = "b38")

  result <- gr & empty
  expect_equal(nrow(result), 0)

  result2 <- empty & gr
  expect_equal(nrow(result2), 0)
})

# Boundary condition tests
test_that("single-position regions work in intersection", {
  gr1 <- new_genomic_regions(
    chr = "1",
    start = 100,
    end = 100,
    build = "b38"
  )
  gr2 <- new_genomic_regions(
    chr = "1",
    start = 50,
    end = 150,
    build = "b38"
  )

  result <- gr1 & gr2
  expect_equal(nrow(result), 1)
  expect_equal(result$start, 100)
  expect_equal(result$end, 100)
})

test_that("adjacent non-overlapping regions have empty intersection", {
  gr1 <- new_genomic_regions(
    chr = "1",
    start = 100,
    end = 200,
    build = "b38"
  )
  gr2 <- new_genomic_regions(
    chr = "1",
    start = 201,
    end = 300,
    build = "b38"
  )

  result <- gr1 & gr2
  expect_equal(nrow(result), 0)
})

test_that("exact boundary overlap produces single-position result", {
  gr1 <- new_genomic_regions(
    chr = "1",
    start = 100,
    end = 200,
    build = "b38"
  )
  gr2 <- new_genomic_regions(
    chr = "1",
    start = 200,
    end = 300,
    build = "b38"
  )

  result <- gr1 & gr2
  expect_equal(nrow(result), 1)
  expect_equal(result$start, 200)
  expect_equal(result$end, 200)
})

test_that("complete containment intersection returns inner region", {
  outer <- new_genomic_regions(
    chr = "1",
    start = 100,
    end = 500,
    build = "b38"
  )
  inner <- new_genomic_regions(
    chr = "1",
    start = 200,
    end = 300,
    build = "b38"
  )

  result <- outer & inner
  expect_equal(nrow(result), 1)
  expect_equal(result$start, 200)
  expect_equal(result$end, 300)
})

test_that("`+` handles negative genomic_regions", {
  gregions1 <- new_genomic_regions(
    chr = 1,
    start = 123,
    include = FALSE
  )
  gregions2 <- new_genomic_regions(
    chr = 1,
    end = 234,
    include = FALSE
  )

  expect_equal(
    gregions1 + gregions2,
    new_genomic_regions(
      chr = 1,
      start = 123,
      end = 234,
      include = FALSE
    )
  )
})

# =============================================================================
# Negation Tests
# =============================================================================

# Basic `!` operator tests ---------------------------------------------------

test_that("`!` flips is_included from TRUE to FALSE", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  expect_true(is_included(gregions))

  negated <- !gregions
  expect_false(is_included(negated))
  expect_equal(nrow(negated), 1)
  expect_equal(negated$chr, "chr1")
  expect_equal(negated$start, 100)
  expect_equal(negated$end, 200)
})

test_that("`!` flips is_included from FALSE to TRUE", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    include = FALSE
  )
  expect_false(is_included(gregions))

  negated <- !gregions
  expect_true(is_included(negated))
})

test_that("double negation returns equivalent to original", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  double_negated <- !!gregions

  expect_equal(is_included(double_negated), is_included(gregions))
  expect_equal(double_negated$chr, gregions$chr)
  expect_equal(double_negated$start, gregions$start)
  expect_equal(double_negated$end, gregions$end)
  expect_equal(build(double_negated), build(gregions))
})

test_that("`!` preserves build attribute", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b37"
  )
  negated <- !gregions
  expect_equal(build(negated), "b37")
})

# is_included accessor tests -------------------------------------------------

test_that("is_included returns TRUE for default positive regions", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200
  )
  expect_true(is_included(gregions))
})

test_that("is_included returns FALSE for negative regions", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    include = FALSE
  )
  expect_false(is_included(gregions))
})

test_that("is_included<- can set the attribute", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200
  )
  expect_true(is_included(gregions))

  is_included(gregions) <- FALSE
  expect_false(is_included(gregions))

  is_included(gregions) <- TRUE
  expect_true(is_included(gregions))
})

# Unary `-` operator tests ---------------------------------------------------

test_that("unary `-` is equivalent to `!`", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )

  expect_equal(is_included(-gregions), is_included(!gregions))
  expect_equal((-gregions)$chr, (!gregions)$chr)
  expect_equal((-gregions)$start, (!gregions)$start)
  expect_equal((-gregions)$end, (!gregions)$end)
})

test_that("unary `-` preserves build attribute", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b37"
  )
  negated <- -gregions
  expect_equal(build(negated), "b37")
})

# Binary `-` (set difference) tests ------------------------------------------

test_that("binary `-` performs set difference (A - B = A & !B)", {
  region_a <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 300,
    build = "b38"
  )
  region_b <- new_genomic_regions(
    chr = 1,
    start = 200,
    end = 250,
    build = "b38"
  )

  # A - B should remove the overlapping portion
  result <- region_a - region_b
  # Result should be chr1:100-199 and chr1:251-300
  expect_equal(nrow(result), 2)
  expect_equal(result$start, c(100, 251))
  expect_equal(result$end, c(199, 300))
})

test_that("set difference with non-overlapping regions returns A unchanged", {
  region_a <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  region_b <- new_genomic_regions(
    chr = 1,
    start = 300,
    end = 400,
    build = "b38"
  )

  result <- region_a - region_b
  expect_equal(nrow(result), 1)
  expect_equal(result$start, 100)
  expect_equal(result$end, 200)
})

test_that("A - A returns empty region", {
  region_a <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )

  result <- region_a - region_a
  expect_equal(nrow(result), 0)
})

test_that("A - empty returns A unchanged", {
  region_a <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  empty <- empty_genomic_regions(build = "b38")

  result <- region_a - empty
  expect_equal(nrow(result), 1)
  expect_equal(result$start, 100)
  expect_equal(result$end, 200)
})

test_that("empty - A returns empty", {
  region_a <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  empty <- empty_genomic_regions(build = "b38")

  result <- empty - region_a
  expect_equal(nrow(result), 0)
})

test_that("set difference with different chromosomes returns A unchanged", {
  region_a <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  region_b <- new_genomic_regions(
    chr = 2,
    start = 100,
    end = 200,
    build = "b38"
  )

  result <- region_a - region_b
  expect_equal(nrow(result), 1)
  expect_equal(result$chr, "chr1")
})

# Union with negative regions tests ------------------------------------------

test_that("positive + negative expands negative and unions", {
  # positive region: chr1:100-200
  # negative region: !chr1:150-250 (everything except chr1:150-250)
  # Result should be everything except chr1:201-250
  positive <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  negative <- new_genomic_regions(
    chr = 1,
    start = 150,
    end = 250,
    build = "b38",
    include = FALSE
  )

  result <- positive + negative
  # The union of chr1:100-200 with !chr1:150-250
  # is everything except chr1:201-250
  expect_true(is_included(result))
  expect_equal(
    result,
    full_genomic_regions(build = "b38") - new_genomic_regions(
      chr = 1,
      start = 201,
      end = 250,
      build = "b38"
    )
  )
})

test_that("negative + positive is commutative with positive + negative", {
  positive <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  negative <- new_genomic_regions(
    chr = 1,
    start = 150,
    end = 250,
    include = FALSE,
    build = "b38"
  )

  result1 <- positive + negative
  result2 <- negative + positive

  expect_equal(is_included(result1), is_included(result2))
  expect_equal(result1$chr, result2$chr)
  expect_equal(result1$start, result2$start)
  expect_equal(result1$end, result2$end)
})

# Intersection with negative regions tests -----------------------------------

test_that("negative & negative combines underlying regions (still negative)", {
  # !chr1:100-200 & !chr1:150-250 should be !(chr1:100-200 | chr1:150-250)
  # which is !(chr1:100-250) = !chr1:100-250
  neg1 <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38",
    include = FALSE
  )
  neg2 <- new_genomic_regions(
    chr = 1,
    start = 150,
    end = 250,
    build = "b38",
    include = FALSE
  )

  expect_equal(
    neg1 & neg2,
    new_genomic_regions(
      chr = 1,
      start = 100,
      end = 250,
      build = "b38",
      include = FALSE
    )
  )
})

test_that("positive & negative works correctly", {
  # chr1:100-300 & !chr1:150-200
  # Should give chr1:100-149 and chr1:201-300
  positive <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 300,
    build = "b38"
  )
  negative <- new_genomic_regions(
    chr = 1,
    start = 150,
    end = 200,
    build = "b38",
    include = FALSE
  )

  result <- positive & negative
  expect_true(is_included(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$start, c(100, 201))
  expect_equal(result$end, c(149, 300))
})

test_that("negative & positive is same as positive & negative", {
  positive <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 300,
    build = "b38"
  )
  negative <- new_genomic_regions(
    chr = 1,
    start = 150,
    end = 200,
    build = "b38",
    include = FALSE
  )

  result1 <- positive & negative
  result2 <- negative & positive

  expect_equal(is_included(result1), is_included(result2))
  expect_equal(nrow(result1), nrow(result2))
  expect_equal(result1$start, result2$start)
  expect_equal(result1$end, result2$end)
})

# expand_genomic_regions behavior tests --------------------------------------

test_that("empty negative region expands to full genome", {
  empty_neg <- empty_genomic_regions(build = "b38")
  is_included(empty_neg) <- FALSE

  expanded <- expand_genomic_regions(empty_neg)
  expect_true(is_included(expanded))
  expect_equal(nrow(expanded), 1)
  expect_true(is.na(expanded$chr))
  expect_true(is.na(expanded$start))
  expect_true(is.na(expanded$end))
})

test_that("single-row negative region expands to complement", {
  # !chr1:100-200 should expand to:
  # - All other chromosomes (chr2-chr22)
  # - chr1:1-99
  # - chr1:201-end
  negative <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38",
    include = FALSE
  )

  expanded <- expand_genomic_regions(negative)
  expect_true(is_included(expanded))
  # Should have rows for chr2-chr22 (21 rows) plus chr1:1-99 and chr1:201-end
  expect_true(nrow(expanded) >= 23)
  # Check chr1 portions exist
  chr1_rows <- expanded[chr == "chr1"]
  expect_equal(nrow(chr1_rows), 2)
  expect_equal(chr1_rows$start, c(1, 201))
  expect_equal(chr1_rows$end[1], 99)
  expect_true(is.na(chr1_rows$end[2]))  # to end of chromosome
})

test_that("positive region returns unchanged from expand_genomic_regions", {
  positive <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )

  result <- expand_genomic_regions(positive)
  expect_equal(result$chr, positive$chr)
  expect_equal(result$start, positive$start)
  expect_equal(result$end, positive$end)
  expect_true(is_included(result))
})

# String and print representation tests --------------------------------------

test_that("str() shows `!` prefix for negative regions", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    include = FALSE
  )

  str_output <- str(gregions)
  expect_true(grepl("^!", str_output))
})

test_that("str() shows no prefix for positive regions", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200
  )

  str_output <- str(gregions)
  expect_false(grepl("^!", str_output))
})

test_that("print() shows Excluded for negative regions", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    include = FALSE
  )

  output <- capture.output(print(gregions))
  expect_true(any(grepl("Excluded", output)))
})

test_that("print() shows Included for positive regions", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200
  )

  output <- capture.output(print(gregions))
  expect_true(any(grepl("Included", output)))
})

# Edge cases -----------------------------------------------------------------

test_that("negation of empty region", {
  empty <- empty_genomic_regions(build = "b38")
  negated <- !empty

  expect_false(is_included(negated))
  expect_equal(nrow(negated), 0)
  expect_equal(build(negated), "b38")
})

test_that("negation of full genome region", {
  full <- full_genomic_regions(build = "b38")
  negated <- !full

  expect_false(is_included(negated))
  expect_equal(nrow(negated), 1)
  expect_true(is.na(negated$chr))
})

test_that("negation with NA chromosome", {
  # NA chromosome means "any chromosome"
  gregions <- new_genomic_regions(
    chr = NA_character_,
    start = 100,
    end = 200,
    build = "b38"
  )
  negated <- !gregions

  expect_false(is_included(negated))
  expect_true(is.na(negated$chr))
  expect_equal(negated$start, 100)
  expect_equal(negated$end, 200)
})

test_that("single-position negative region works", {
  # !chr1:100-100
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 100,
    build = "b38",
    include = FALSE
  )

  expanded <- expand_genomic_regions(gregions)
  expect_true(is_included(expanded))
  chr1_rows <- expanded[chr == "chr1"]
  expect_equal(nrow(chr1_rows), 2)
  expect_equal(chr1_rows$start, c(1, 101))
  expect_equal(chr1_rows$end[1], 99)
})

test_that("multi-row negative region expands correctly", {
  # !{chr1:100-200, chr2:300-400}
  # This is the intersection of !chr1:100-200 and !chr2:300-400
  negative <- new_genomic_regions(
    chr = c(1, 2),
    start = c(100, 300),
    end = c(200, 400),
    build = "b38",
    include = FALSE
  )

  expanded <- expand_genomic_regions(negative)
  expect_true(is_included(expanded))

  # Should exclude both chr1:100-200 and chr2:300-400
  # Check chr1 doesn't include 100-200
  chr1_rows <- expanded[chr == "chr1"]
  expect_true(all(chr1_rows$end < 100 | chr1_rows$start > 200))

  # Check chr2 doesn't include 300-400
  chr2_rows <- expanded[chr == "chr2"]
  expect_true(all(chr2_rows$end < 300 | chr2_rows$start > 400))
})

test_that("negative region at start of chromosome expands correctly", {
  # !chr1:1-100 (start of chromosome)
  negative <- new_genomic_regions(
    chr = 1,
    start = 1,
    end = 100,
    build = "b38",
    include = FALSE
  )

  expanded <- expand_genomic_regions(negative)
  expect_true(is_included(expanded))
  chr1_rows <- expanded[chr == "chr1"]
  # Should only have one row for chr1 (101 to end), no row for before
  expect_equal(nrow(chr1_rows), 1)
  expect_equal(chr1_rows$start, 101)
  expect_true(is.na(chr1_rows$end))
})

test_that("negative region at end of chromosome expands correctly", {
  # !chr1:1000-NA (end of chromosome)
  negative <- new_genomic_regions(
    chr = 1,
    start = 1000,
    end = NA_integer_,
    build = "b38",
    include = FALSE
  )

  expanded <- expand_genomic_regions(negative)
  expect_true(is_included(expanded))
  chr1_rows <- expanded[chr == "chr1"]
  # Should only have one row for chr1 (1 to 999), no row for after
  expect_equal(nrow(chr1_rows), 1)
  expect_equal(chr1_rows$start, 1)
  expect_equal(chr1_rows$end, 999)
})
