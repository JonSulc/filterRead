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
      complement = FALSE
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
      complement = FALSE
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
      normalize_regions = FALSE
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
    normalize_regions = FALSE,
    complement = TRUE
  )
  merged <- merge_contiguous_regions(gregions)
  expect_equal(
    build(merged),
    "b37"
  )
  expect_true(
    is_complement(merged)
  )
})

test_that("merging keeps rows separated by an unconstrained position", {
  gregions <- new_genomic_regions(
    chr = c(1, 1),
    start = c(NA, 200),
    end = c(100, NA),
    build = "b38"
  )
  expect_equal(nrow(gregions), 2)
  expect_equal(gregions$start, c(NA_integer_, 200L))
  expect_equal(gregions$end, c(100L, NA_integer_))
})

test_that("merging joins rows that meet across an unconstrained position", {
  gregions <- new_genomic_regions(
    chr = c(1, 1),
    start = c(NA, 101),
    end = c(100, NA),
    build = "b38"
  )
  expect_equal(nrow(gregions), 1)
  expect_true(is.na(gregions$start))
  expect_true(is.na(gregions$end))
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
      chr = "chr1"
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
      start = c(21, 1042, 43),
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

expect_source_regions <- function(fcall, expected, finterface) {
  testthat::expect_equal(
    new_filter_condition(fcall, finterface, build = "b38") |>
      source_regions(),
    expected,
    info = paste(deparse(rlang::get_expr(fcall)), collapse = "")
  )
}

test_that("source_regions reports the positions a comparison denotes", {
  finterface <- local_summary_stats_interface()
  cases <- list(
    list(
      rlang::quo(pos == 12345),
      new_genomic_regions(start = 12345, end = 12345, build = "b38")
    ),
    list(
      rlang::quo(12345 == pos),
      new_genomic_regions(start = 12345, end = 12345, build = "b38")
    ),
    list(
      rlang::quo(pos < 1000),
      new_genomic_regions(end = 999, build = "b38")
    ),
    list(
      rlang::quo(1000 > pos),
      new_genomic_regions(end = 999, build = "b38")
    ),
    list(
      rlang::quo(pos > 1000),
      new_genomic_regions(start = 1001, build = "b38")
    ),
    list(
      rlang::quo(1000 < pos),
      new_genomic_regions(start = 1001, build = "b38")
    ),
    list(
      rlang::quo(pos <= 1000),
      new_genomic_regions(end = 1000, build = "b38")
    ),
    list(
      rlang::quo(1000 >= pos),
      new_genomic_regions(end = 1000, build = "b38")
    ),
    list(
      rlang::quo(pos >= 1000),
      new_genomic_regions(start = 1000, build = "b38")
    ),
    list(
      rlang::quo(1000 <= pos),
      new_genomic_regions(start = 1000, build = "b38")
    )
  )
  for (case in cases) expect_source_regions(case[[1]], case[[2]], finterface)
})

test_that("source_regions reports the chromosomes a comparison denotes", {
  finterface <- local_summary_stats_interface()
  cases <- list(
    list(
      rlang::quo(chr == 1),
      new_genomic_regions(chr = 1, build = "b38")
    ),
    list(
      rlang::quo(1 == chr),
      new_genomic_regions(chr = 1, build = "b38")
    ),
    list(
      rlang::quo(chr < 4),
      new_genomic_regions(chr = 1:3, build = "b38")
    ),
    list(
      rlang::quo(4 > chr),
      new_genomic_regions(chr = 1:3, build = "b38")
    ),
    list(
      rlang::quo(chr <= 3),
      new_genomic_regions(chr = 1:3, build = "b38")
    ),
    list(
      rlang::quo(3 >= chr),
      new_genomic_regions(chr = 1:3, build = "b38")
    ),
    list(
      rlang::quo(chr > 19),
      new_genomic_regions(chr = 20:22, build = "b38")
    ),
    list(
      rlang::quo(chr >= 20),
      new_genomic_regions(chr = 20:22, build = "b38")
    ),
    list(
      rlang::quo(20 <= chr),
      new_genomic_regions(chr = 20:22, build = "b38")
    ),
    list(
      rlang::quo(20 < chr),
      new_genomic_regions(chr = 21:22, build = "b38")
    )
  )
  for (case in cases) expect_source_regions(case[[1]], case[[2]], finterface)
})

test_that(
  "source_regions falls back to the full genome off the chromosome range",
  {
    finterface <- local_summary_stats_interface()
    for (fcall in list(rlang::quo(chr < 100), rlang::quo(100 > chr))) {
      expect_source_regions(
        fcall,
        full_genomic_regions(build = "b38"),
        finterface
      )
    }
  }
)

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

test_that("`+` handles a complement-stored genomic_regions", {
  gregions1 <- new_genomic_regions(
    chr = 1,
    start = 123,
    complement = TRUE
  )
  gregions2 <- new_genomic_regions(
    chr = 1,
    end = 234,
    complement = TRUE
  )

  expect_equal(
    gregions1 + gregions2,
    new_genomic_regions(
      chr = 1,
      start = 123,
      end = 234,
      complement = TRUE
    )
  )
})

# Negation Tests

# Basic `!` operator tests ---------------------------------------------------

test_that("`!` marks a directly stored set as its complement", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  expect_false(is_complement(gregions))

  negated <- !gregions
  expect_true(is_complement(negated))
  expect_equal(nrow(negated), 1)
  expect_equal(negated$chr, "chr1")
  expect_equal(negated$start, 100)
  expect_equal(negated$end, 200)
})

test_that("`!` marks a complement-stored set as directly stored", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    complement = TRUE
  )
  expect_true(is_complement(gregions))

  negated <- !gregions
  expect_false(is_complement(negated))
})

test_that("double negation returns equivalent to original", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  double_negated <- !!gregions

  expect_equal(is_complement(double_negated), is_complement(gregions))
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

# is_complement accessor tests -----------------------------------------------

test_that("is_complement returns FALSE for a directly stored set", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200
  )
  expect_false(is_complement(gregions))
})

test_that("is_complement returns TRUE for a complement-stored set", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    complement = TRUE
  )
  expect_true(is_complement(gregions))
})

test_that("set_complement can set the attribute", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200
  )
  expect_false(is_complement(gregions))

  set_complement(gregions, TRUE)
  expect_true(is_complement(gregions))

  set_complement(gregions, FALSE)
  expect_false(is_complement(gregions))
})

# Unary `-` operator tests ---------------------------------------------------

test_that("unary `-` is equivalent to `!`", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )

  expect_equal(is_complement(-gregions), is_complement(!gregions))
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

# Union with complement-stored regions tests ---------------------------------

test_that("direct + complemented expands the complement and unions", {
  # directly stored: chr1:100-200
  # complement-stored: !chr1:150-250 (everything except chr1:150-250)
  # Result should be everything except chr1:201-250
  direct <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  complemented <- new_genomic_regions(
    chr = 1,
    start = 150,
    end = 250,
    build = "b38",
    complement = TRUE
  )

  result <- direct + complemented
  # The union of chr1:100-200 with !chr1:150-250
  # is everything except chr1:201-250
  expect_false(is_complement(result))
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

test_that("union of a direct and a complemented set is commutative", {
  direct <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  complemented <- new_genomic_regions(
    chr = 1,
    start = 150,
    end = 250,
    complement = TRUE,
    build = "b38"
  )

  result1 <- direct + complemented
  result2 <- complemented + direct

  expect_equal(is_complement(result1), is_complement(result2))
  expect_equal(result1$chr, result2$chr)
  expect_equal(result1$start, result2$start)
  expect_equal(result1$end, result2$end)
})

# Intersection with complement-stored regions tests --------------------------

test_that("complemented & complemented combines rows and stays complemented", {
  # !chr1:100-200 & !chr1:150-250 should be !(chr1:100-200 | chr1:150-250)
  # which is !(chr1:100-250) = !chr1:100-250
  neg1 <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38",
    complement = TRUE
  )
  neg2 <- new_genomic_regions(
    chr = 1,
    start = 150,
    end = 250,
    build = "b38",
    complement = TRUE
  )

  expect_equal(
    neg1 & neg2,
    new_genomic_regions(
      chr = 1,
      start = 100,
      end = 250,
      build = "b38",
      complement = TRUE
    )
  )
})

test_that("direct & complemented subtracts the complemented rows", {
  # chr1:100-300 & !chr1:150-200
  # Should give chr1:100-149 and chr1:201-300
  direct <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 300,
    build = "b38"
  )
  complemented <- new_genomic_regions(
    chr = 1,
    start = 150,
    end = 200,
    build = "b38",
    complement = TRUE
  )

  result <- direct & complemented
  expect_false(is_complement(result))
  expect_equal(nrow(result), 2)
  expect_equal(result$start, c(100, 201))
  expect_equal(result$end, c(149, 300))
})

test_that("intersection of a direct and a complemented set is commutative", {
  direct <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 300,
    build = "b38"
  )
  complemented <- new_genomic_regions(
    chr = 1,
    start = 150,
    end = 200,
    build = "b38",
    complement = TRUE
  )

  result1 <- direct & complemented
  result2 <- complemented & direct

  expect_equal(is_complement(result1), is_complement(result2))
  expect_equal(nrow(result1), nrow(result2))
  expect_equal(result1$start, result2$start)
  expect_equal(result1$end, result2$end)
})

# expand_genomic_regions behavior tests --------------------------------------

test_that("a complement-stored set with no rows expands to the whole genome", {
  empty_neg <- empty_genomic_regions(build = "b38")
  set_complement(empty_neg, TRUE)

  expanded <- expand_genomic_regions(empty_neg)
  expect_false(is_complement(expanded))
  expect_equal(nrow(expanded), 1)
  expect_true(is.na(expanded$chr))
  expect_true(is.na(expanded$start))
  expect_true(is.na(expanded$end))
})

test_that("a single complement-stored row expands to its complement", {
  # !chr1:100-200 should expand to:
  # - All other chromosomes (chr2-chr22)
  # - chr1:1-99
  # - chr1:201-end
  complemented <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38",
    complement = TRUE
  )

  expanded <- expand_genomic_regions(complemented)
  expect_false(is_complement(expanded))
  # Should have rows for chr2-chr22 (21 rows) plus chr1:1-99 and chr1:201-end
  expect_true(nrow(expanded) >= 23)
  # Check chr1 portions exist
  chr1_rows <- expanded[chr == "chr1"]
  expect_equal(nrow(chr1_rows), 2)
  expect_equal(chr1_rows$start, c(NA, 201))
  expect_equal(chr1_rows$end[1], 99)
  expect_true(is.na(chr1_rows$end[2])) # to end of chromosome
})

test_that("a directly stored set is returned unchanged by expansion", {
  direct <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )

  result <- expand_genomic_regions(direct)
  expect_equal(result$chr, direct$chr)
  expect_equal(result$start, direct$start)
  expect_equal(result$end, direct$end)
  expect_false(is_complement(result))
})

# String and print representation tests --------------------------------------

test_that("str() shows a `!` prefix for a complement-stored set", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    complement = TRUE
  )

  str_output <- str(gregions)
  expect_true(grepl("^!", str_output))
})

test_that("str() shows no prefix for a directly stored set", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200
  )

  str_output <- str(gregions)
  expect_false(grepl("^!", str_output))
})

# Edge cases -----------------------------------------------------------------

test_that("negation of empty region", {
  empty <- empty_genomic_regions(build = "b38")
  negated <- !empty

  expect_true(is_complement(negated))
  expect_equal(nrow(negated), 0)
  expect_equal(build(negated), "b38")
})

test_that("negation of full genome region", {
  full <- full_genomic_regions(build = "b38")
  negated <- !full

  expect_true(is_complement(negated))
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

  expect_true(is_complement(negated))
  expect_true(is.na(negated$chr))
  expect_equal(negated$start, 100)
  expect_equal(negated$end, 200)
})

test_that("a single-position complement-stored region works", {
  # !chr1:100-100
  gregions <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 100,
    build = "b38",
    complement = TRUE
  )

  expanded <- expand_genomic_regions(gregions)
  expect_false(is_complement(expanded))
  chr1_rows <- expanded[chr == "chr1"]
  expect_equal(nrow(chr1_rows), 2)
  expect_equal(chr1_rows$start, c(NA, 101))
  expect_equal(chr1_rows$end[1], 99)
})

test_that("a multi-row complement-stored set expands correctly", {
  # !{chr1:100-200, chr2:300-400}
  # This is the intersection of !chr1:100-200 and !chr2:300-400
  complemented <- new_genomic_regions(
    chr = c(1, 2),
    start = c(100, 300),
    end = c(200, 400),
    build = "b38",
    complement = TRUE
  )

  expanded <- expand_genomic_regions(complemented)
  expect_false(is_complement(expanded))

  # Should exclude both chr1:100-200 and chr2:300-400
  # Check chr1 doesn't include 100-200
  chr1_rows <- expanded[chr == "chr1"]
  expect_true(all(chr1_rows$end < 100 | chr1_rows$start > 200))

  # Check chr2 doesn't include 300-400
  chr2_rows <- expanded[chr == "chr2"]
  expect_true(all(chr2_rows$end < 300 | chr2_rows$start > 400))
})

test_that("a complement-stored region at a chromosome start expands", {
  # !chr1:1-100 (start of chromosome)
  complemented <- new_genomic_regions(
    chr = 1,
    start = 1,
    end = 100,
    build = "b38",
    complement = TRUE
  )

  expanded <- expand_genomic_regions(complemented)
  expect_false(is_complement(expanded))
  chr1_rows <- expanded[chr == "chr1"]
  # Should only have one row for chr1 (101 to end), no row for before
  expect_equal(nrow(chr1_rows), 1)
  expect_equal(chr1_rows$start, 101)
  expect_true(is.na(chr1_rows$end))
})

test_that("a complement-stored region at a chromosome end expands", {
  # !chr1:1000-NA (end of chromosome)
  complemented <- new_genomic_regions(
    chr = 1,
    start = 1000,
    end = NA_integer_,
    build = "b38",
    complement = TRUE
  )

  expanded <- expand_genomic_regions(complemented)
  expect_false(is_complement(expanded))
  chr1_rows <- expanded[chr == "chr1"]
  # Should only have one row for chr1 (1 to 999), no row for after
  expect_equal(nrow(chr1_rows), 1)
  expect_equal(chr1_rows$start, NA_integer_)
  expect_equal(chr1_rows$end, 999)
})

# Equality (==) Tests ---------------------------------------------------------

test_that("`==` returns TRUE for identical simple regions", {
  region1 <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  region2 <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )

  expect_true(region1 == region2)
})

test_that("`==` returns TRUE for self-comparison", {
  region <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )

  expect_true(region == region)
})

test_that("`==` returns FALSE for different regions", {
  region1 <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  region2 <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 300,
    build = "b38"
  )

  expect_false(region1 == region2)
})

test_that("`==` returns FALSE when row counts differ", {
  region1 <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  region2 <- rbind(
    new_genomic_regions(chr = 1, start = 100, end = 150, build = "b38"),
    new_genomic_regions(chr = 1, start = 251, end = 300, build = "b38")
  )

  expect_false(region1 == region2)
})

test_that("`==` returns TRUE across the two stored forms", {
  # A single complement-stored region equals its expansion
  complemented <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38",
    complement = TRUE
  )
  expanded <- expand_genomic_regions(complemented)

  expect_true(complemented == expanded)
})

test_that("`==` returns TRUE for empty regions", {
  empty1 <- new_genomic_regions(build = "b38")
  empty2 <- new_genomic_regions(build = "b38")

  expect_true(empty1 == empty2)
})

test_that("`==` returns TRUE for full genome regions", {
  full1 <- full_genomic_regions(build = "b38")
  full2 <- full_genomic_regions(build = "b38")

  expect_true(full1 == full2)
})

test_that("`==` returns TRUE for multi-row identical regions", {
  region1 <- rbind(
    new_genomic_regions(chr = 1, start = 100, end = 200, build = "b38"),
    new_genomic_regions(chr = 2, start = 300, end = 400, build = "b38")
  )
  region2 <- rbind(
    new_genomic_regions(chr = 1, start = 100, end = 200, build = "b38"),
    new_genomic_regions(chr = 2, start = 300, end = 400, build = "b38")
  )

  expect_true(region1 == region2)
})

test_that("`==` returns FALSE for different chromosomes", {
  region1 <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )
  region2 <- new_genomic_regions(
    chr = 2,
    start = 100,
    end = 200,
    build = "b38"
  )

  expect_false(region1 == region2)
})

# Compact Function Tests ------------------------------------------------------

test_that("compact returns original when already compact", {
  region <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b38"
  )

  compacted <- compact(region)
  expect_equal(nrow(compacted), 1)
  expect_equal(compacted$chr, "chr1")
  expect_equal(compacted$start, 100)
  expect_equal(compacted$end, 200)
})

test_that("compact reduces adjacent regions via negation", {
  # Two adjacent regions on same chromosome
  adjacent <- rbind(
    new_genomic_regions(chr = 1, start = 100, end = 200, build = "b38"),
    new_genomic_regions(chr = 1, start = 201, end = 300, build = "b38"),
    normalize_regions = FALSE
  )

  compacted <- compact(adjacent)
  # Should find a more compact representation
  expect_true(nrow(compacted) <= nrow(adjacent))
})

test_that("compact prefers the complement-stored form when it has fewer rows", {
  # A region covering most chromosomes has fewer rows as a complement
  # E.g., "all of chr1-chr21" can be represented as "!chr22"
  many_chrs <- new_genomic_regions(chr = 1:21)

  compacted <- compact(many_chrs)
  # Should be stored as the complement of chr22 = 1 row
  expect_equal(
    nrow(compacted),
    1
  )
  expect_true(is_complement(compacted))
  expect_equal(
    compacted,
    new_genomic_regions(chr = 22, complement = TRUE)
  )
})

test_that("compact preserves build attribute", {
  region <- new_genomic_regions(
    chr = 1,
    start = 100,
    end = 200,
    build = "b37"
  )

  compacted <- compact(region)
  expect_equal(build(compacted), "b37")
})

test_that("compact handles empty regions", {
  empty <- new_genomic_regions(build = "b38")

  compacted <- compact(empty)
  expect_equal(nrow(compacted), 0)
})

test_that("compact handles full genome region", {
  full <- full_genomic_regions(build = "b38")

  compacted <- compact(full)
  expect_equal(nrow(compacted), 0)
  expect_true(is_complement(compacted))
  expect_true(is_full_genome(compacted))
})

test_that("compact returns semantically equivalent region", {
  # The compacted region should equal the original
  overlapping <- rbind(
    new_genomic_regions(chr = 1, start = 100, end = 200, build = "b38"),
    new_genomic_regions(chr = 1, start = 150, end = 250, build = "b38"),
    new_genomic_regions(chr = 2, start = 300, end = 400, build = "b38")
  )

  compacted <- compact(overlapping)
  expect_true(overlapping == compacted)
})

test_that("genomic_regions construction coerces coordinates to integer", {
  gr <- new_genomic_regions(chr = "1", start = 1000, end = 2000, build = "b38")
  expect_type(gr$start, "integer")
  expect_type(gr$end, "integer")
})

# subtract_regions_dt tests -------------------------------------------------

test_that("subtract_regions_dt keeps a row outside everything it removes", {
  expect_equal(
    subtract_regions_dt(
      data.table::data.table(chr = "chr1", start = 100L, end = 200L),
      data.table::data.table(chr = "chr1", start = 300L, end = 400L)
    ),
    data.table::data.table(chr = "chr1", start = 100L, end = 200L)
  )
})

test_that("subtract_regions_dt ignores a region to remove on another chr", {
  expect_equal(
    subtract_regions_dt(
      data.table::data.table(chr = "chr1", start = 100L, end = 200L),
      data.table::data.table(chr = "chr2", start = 100L, end = 200L)
    ),
    data.table::data.table(chr = "chr1", start = 100L, end = 200L)
  )
})

test_that("subtract_regions_dt drops a row it removes entirely", {
  expect_equal(
    subtract_regions_dt(
      data.table::data.table(chr = "chr1", start = 100L, end = 200L),
      data.table::data.table(chr = "chr1", start = 50L, end = 250L)
    ),
    data.table::data.table(
      chr = character(),
      start = integer(),
      end = integer()
    )
  )
})

test_that("subtract_regions_dt trims a row it removes part of", {
  expect_equal(
    subtract_regions_dt(
      data.table::data.table(chr = "chr1", start = 100L, end = 300L),
      data.table::data.table(chr = "chr1", start = 50L, end = 150L)
    ),
    data.table::data.table(chr = "chr1", start = 151L, end = 300L)
  )
  expect_equal(
    subtract_regions_dt(
      data.table::data.table(chr = "chr1", start = 100L, end = 300L),
      data.table::data.table(chr = "chr1", start = 250L, end = 400L)
    ),
    data.table::data.table(chr = "chr1", start = 100L, end = 249L)
  )
})

test_that("subtract_regions_dt splits a row it removes the middle of", {
  expect_equal(
    data.table::setorder(
      subtract_regions_dt(
        data.table::data.table(chr = "chr1", start = 100L, end = 300L),
        data.table::data.table(chr = "chr1", start = 150L, end = 200L)
      ),
      chr, start, end
    ),
    data.table::data.table(
      chr = c("chr1", "chr1"),
      start = c(100L, 201L),
      end = c(149L, 300L)
    )
  )
})

test_that("subtract_regions_dt applies an NA-chromosome row to every row", {
  expect_equal(
    data.table::setorder(
      subtract_regions_dt(
        data.table::data.table(
          chr = c("chr1", "chr2"),
          start = c(100L, 100L),
          end = c(300L, 300L)
        ),
        data.table::data.table(
          chr = NA_character_,
          start = 150L,
          end = 200L
        )
      ),
      chr, start, end
    ),
    data.table::data.table(
      chr = c("chr1", "chr1", "chr2", "chr2"),
      start = c(100L, 201L, 100L, 201L),
      end = c(149L, 300L, 149L, 300L)
    )
  )
})

test_that("subtract_regions_dt reads NA start and end as unconstrained", {
  expect_equal(
    subtract_regions_dt(
      data.table::data.table(
        chr = "chr1",
        start = NA_integer_,
        end = NA_integer_
      ),
      data.table::data.table(
        chr = NA_character_,
        start = NA_integer_,
        end = 200L
      )
    ),
    data.table::data.table(chr = "chr1", start = 201L, end = NA_integer_)
  )
})

test_that("subtract_regions_dt removes each region in turn", {
  expect_equal(
    data.table::setorder(
      subtract_regions_dt(
        data.table::data.table(chr = "chr1", start = 100L, end = 500L),
        data.table::data.table(
          chr = "chr1",
          start = c(150L, 300L),
          end = c(200L, 350L)
        )
      ),
      chr, start, end
    ),
    data.table::data.table(
      chr = c("chr1", "chr1", "chr1"),
      start = c(100L, 201L, 351L),
      end = c(149L, 299L, 500L)
    )
  )
})

test_that("subtract_regions_dt keeps every row when nothing is removed", {
  expect_equal(
    subtract_regions_dt(
      data.table::data.table(chr = "chr1", start = 100L, end = 200L),
      data.table::data.table(
        chr = character(),
        start = integer(),
        end = integer()
      )
    ),
    data.table::data.table(chr = "chr1", start = 100L, end = 200L)
  )
})

test_that("subtract_regions_dt returns no rows when given none", {
  expect_equal(
    subtract_regions_dt(
      data.table::data.table(
        chr = character(),
        start = integer(),
        end = integer()
      ),
      data.table::data.table(chr = "chr1", start = 100L, end = 200L)
    ),
    data.table::data.table(
      chr = character(),
      start = integer(),
      end = integer()
    )
  )
})

test_that("subtract_regions_dt removes every unconstrained-chromosome region", {
  expect_equal(
    data.table::setorder(
      subtract_regions_dt(
        data.table::data.table(
          chr = c("chr1", "chr2"),
          start = c(1L, 1L),
          end = c(1000L, 1000L)
        ),
        data.table::data.table(
          chr = NA_character_,
          start = c(NA_integer_, 200L),
          end = c(100L, NA_integer_)
        )
      ),
      chr, start, end
    ),
    data.table::data.table(
      chr = c("chr1", "chr2"),
      start = c(101L, 101L),
      end = c(199L, 199L)
    )
  )
})

test_that("subtract_regions_dt rejects an unrepresentable difference", {
  expect_error(
    subtract_regions_dt(
      data.table::data.table(
        chr = NA_character_,
        start = 100L,
        end = 200L
      ),
      data.table::data.table(chr = "chr1", start = 150L, end = 160L)
    ),
    "enumerating the chromosomes"
  )
})

# Absorption by an unconstrained chromosome ----------------------------------

test_that("a region on every chromosome absorbs one it covers", {
  expect_equal(
    full_genomic_regions(build = "b38") |
      new_genomic_regions(chr = 1, build = "b38"),
    full_genomic_regions(build = "b38")
  )
  expect_equal(
    new_genomic_regions(end = 200, build = "b38") |
      new_genomic_regions(chr = 1, start = 1, end = 100, build = "b38"),
    new_genomic_regions(end = 200, build = "b38")
  )
})

test_that("a region on every chromosome trims one it overlaps", {
  expect_equal(
    new_genomic_regions(end = 200, build = "b38") |
      new_genomic_regions(chr = 1, build = "b38"),
    new_genomic_regions(
      chr = c(NA_character_, "chr1"),
      start = c(NA, 201),
      end = c(200, NA),
      build = "b38"
    )
  )
  expect_equal(
    new_genomic_regions(end = 200, build = "b38") |
      new_genomic_regions(chr = 1, start = 150, end = 300, build = "b38"),
    new_genomic_regions(
      chr = c(NA_character_, "chr1"),
      start = c(NA, 201),
      end = c(200, 300),
      build = "b38"
    )
  )
})

test_that("compact keeps the absorbed coverage", {
  expect_true(
    is_full_genome(
      compact(
        full_genomic_regions(build = "b38") |
          new_genomic_regions(chr = 1, build = "b38")
      )
    )
  )
})

test_that("construction leaves the rows canonical", {
  expect_canonical(new_genomic_regions(
    chr = c(1, 1),
    start = c(NA, 200),
    end = c(100, NA),
    build = "b38"
  ))
  expect_canonical(new_genomic_regions(
    chr = c(NA_character_, "chr1"),
    start = c(NA, 150),
    end = c(200, 300),
    build = "b38"
  ))
  expect_canonical(new_genomic_regions(
    chr = c(NA_character_, "chr1", "chr1"),
    start = c(100, 1, 250),
    end = c(200, 400, NA),
    build = "b38"
  ))
  expect_canonical(
    full_genomic_regions(build = "b38") |
      new_genomic_regions(chr = 1, build = "b38")
  )
  expect_canonical(
    new_genomic_regions(end = 200, build = "b38") |
      new_genomic_regions(chr = 1, start = 150, end = 300, build = "b38")
  )
})

# Complement of an unconstrained chromosome ----------------------------------

test_that("the complement of the whole genome matches nothing", {
  expect_equal(
    nrow(expand_genomic_regions(!full_genomic_regions(build = "b38"))),
    0L
  )
  expect_equal(
    nrow(
      new_genomic_regions(chr = 1, build = "b38") &
        !full_genomic_regions(build = "b38")
    ),
    0L
  )
  expect_equal(
    nrow(
      full_genomic_regions(build = "b38") &
        !full_genomic_regions(build = "b38")
    ),
    0L
  )
})

test_that("complementing an open-chromosome range enumerates nothing", {
  expect_equal(
    expand_genomic_regions(!new_genomic_regions(end = 200, build = "b38")),
    new_genomic_regions(start = 201, build = "b38")
  )
  expect_equal(
    new_genomic_regions(chr = 1, build = "b38") &
      !new_genomic_regions(end = 200, build = "b38"),
    new_genomic_regions(chr = 1, start = 201, build = "b38")
  )
})

test_that("the empty set equals the complement of the whole genome", {
  expect_true(
    empty_genomic_regions(build = "b38") ==
      !full_genomic_regions(build = "b38")
  )
})

test_that("the coverage predicates read the complement bit", {
  full <- full_genomic_regions(build = "b38")
  empty <- empty_genomic_regions(build = "b38")
  chr1 <- new_genomic_regions(chr = 1, build = "b38")

  expect_true(is_full_genome(full))
  expect_false(is_empty_genome(full))

  expect_false(is_full_genome(empty))
  expect_true(is_empty_genome(empty))

  expect_false(is_full_genome(!full))
  expect_true(is_empty_genome(!full))

  expect_true(is_full_genome(!empty))
  expect_false(is_empty_genome(!empty))

  expect_false(is_full_genome(chr1))
  expect_false(is_empty_genome(chr1))

  expect_false(is_full_genome(!chr1))
  expect_false(is_empty_genome(!chr1))
})

test_that("uniting complementary halves of the genome covers everything", {
  result <- new_genomic_regions(end = 200, build = "b38") |
    new_genomic_regions(start = 201, build = "b38")
  expect_true(is_full_genome(result))
})

test_that("intersecting complementary halves of the genome matches nothing", {
  result <- !new_genomic_regions(end = 200, build = "b38") &
    !new_genomic_regions(start = 201, build = "b38")
  expect_true(is_empty_genome(result))
})

test_that("absorption keeps a complement-stored set complemented", {
  gregions <- new_genomic_regions(
    chr = c(NA_character_, "chr1"),
    start = c(100, 1),
    end = c(200, 1000),
    complement = TRUE,
    build = "b38"
  )

  expect_true(is_complement(gregions))
  expect_equal(build(gregions), "b38")
  expect_equal(gregions$chr, c(NA, "chr1", "chr1"))
  expect_equal(gregions$start, c(100L, NA, 201L))
  expect_equal(gregions$end, c(200L, 99L, 1000L))
})

test_that("complementing a lower bound leaves the start unconstrained", {
  expanded <- expand_genomic_regions(
    !new_genomic_regions(chr = 1, start = 100, build = "b38")
  )
  chr1_rows <- expanded[chr == "chr1"]

  expect_equal(nrow(chr1_rows), 1)
  expect_true(is.na(chr1_rows$start))
  expect_equal(chr1_rows$end, 99)
})

test_that("a region starting at the first position is left open", {
  gregions <- new_genomic_regions(chr = 1, start = 1, end = 100, build = "b38")

  expect_true(is.na(gregions$start))
  expect_true(
    gregions == new_genomic_regions(chr = 1, end = 100, build = "b38")
  )
})

test_that("source_regions returns the whole condition's regions", {
  finterface <- local_summary_stats_interface()
  fcondition <- new_filter_condition(
    rlang::quo((chr == 1 & ref == "A") | (chr == 2 & ref == "C")),
    finterface,
    build = "b38"
  )
  expect_true(
    source_regions(fcondition) ==
      new_genomic_regions(chr = c(1, 2), build = "b38")
  )
})

test_that("source_regions reports the full genome when nothing is recorded", {
  fcondition <- as_filter_condition(rlang::quo(chr == 1))
  expect_null(genomic_constraint(fcondition))
  expect_true(is_full_genome(source_regions(fcondition)))
})

test_that("source_regions rejects an object that is not a condition", {
  expect_error(
    source_regions(new_genomic_regions(chr = 1, build = "b38")),
    "is.filter_condition(fcondition) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    source_regions(NULL),
    "is.filter_condition(fcondition) is not TRUE",
    fixed = TRUE
  )
  expect_error(
    source_regions(42),
    "is.filter_condition(fcondition) is not TRUE",
    fixed = TRUE
  )
})

test_that("genomic_regions constructors and operators keep over-allocation", {
  # A shallow copy comes back with truelength 0, and data.table takes another
  # shallow copy — warning in the caller — on its next `:=`.
  gregions <- genomic_regions(chr = 1, start = 1L, end = 10L, build = "b37")
  other <- genomic_regions(chr = 1, start = 20L, end = 30L, build = "b37")
  converted <- as_genomic_regions(
    data.frame(chr = "chr1", start = 1L, end = 10L),
    build = "b37"
  )
  empty <- genomic_regions(
    chr = character(), start = integer(), end = integer(), build = "b37"
  )

  expect_true(data.table::truelength(gregions) != 0L)
  expect_true(data.table::truelength(converted) != 0L)
  expect_true(data.table::truelength(!gregions) != 0L)
  expect_true(data.table::truelength(gregions | other) != 0L)
  expect_true(data.table::truelength(gregions & other) != 0L)
  expect_true(data.table::truelength(liftover(gregions, "b38")) != 0L)
  expect_true(data.table::truelength(liftover(empty, "b38")) != 0L)
})

test_that("negation leaves its operand's complement bit alone", {
  gregions <- genomic_regions(chr = 1, start = 1L, end = 10L, build = "b37")
  negated <- !gregions

  expect_true(is_complement(negated))
  expect_false(is_complement(gregions))
})

test_that("lifting an empty genomic_regions to no target keeps its build", {
  gregions <- genomic_regions(
    chr = character(), start = integer(), end = integer(), build = "b37"
  )

  expect_equal(build(liftover(gregions, NULL)), "b37")
})

test_that("lifting an empty genomic_regions leaves its operand's build alone", {
  gregions <- genomic_regions(
    chr = character(), start = integer(), end = integer(), build = "b37"
  )
  lifted <- liftover(gregions, "b38")

  expect_equal(build(lifted), "b38")
  expect_equal(build(gregions), "b37")
})
