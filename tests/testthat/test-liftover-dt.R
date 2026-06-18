test_that("liftover.data.table maps APOE rs429358 b37 -> b38", {
  dt <- data.table::data.table(
    chr    = "chr19",
    pos    = 45411941L,
    rsid   = "rs429358",
    effect = 0.4
  )
  build(dt) <- "b37"

  lifted <- liftover(dt, "b38")
  expect_equal(lifted$chr, "chr19")
  expect_equal(lifted$pos, 44908684L)
  expect_equal(lifted$rsid, "rs429358")
  expect_equal(lifted$effect, 0.4)
  expect_equal(build(lifted), "b38")
})

test_that("liftover.data.table b37 -> b38 -> b37 round-trip", {
  dt <- data.table::data.table(
    chr = "chr19",
    pos = c(45411941L, 45412079L)
  )
  build(dt) <- "b37"

  lifted_back <- liftover(liftover(dt, "b38"), "b37")
  expect_equal(lifted_back$chr, dt$chr)
  expect_equal(lifted_back$pos, dt$pos)
  expect_equal(build(lifted_back), "b37")
})

test_that("liftover.data.table on the same build is a build-only no-op", {
  dt <- data.table::data.table(chr = "chr1", pos = 100L)
  build(dt) <- "b37"

  result <- liftover(dt, "b37")
  expect_equal(result$chr, dt$chr)
  expect_equal(result$pos, dt$pos)
  expect_equal(build(result), "b37")
})

test_that("liftover.data.table errors when no build is set or supplied", {
  dt <- data.table::data.table(chr = "chr19", pos = 45411941L)
  expect_error(liftover(dt, "b38"), "without a source build")
})

test_that("liftover.data.table accepts an explicit `from` build", {
  dt <- data.table::data.table(chr = "chr19", pos = 45411941L)
  result <- liftover(dt, target = "b38", from = "b37")
  expect_equal(result$pos, 44908684L)
  expect_equal(build(result), "b38")
})

test_that("liftover.data.table on an empty data.table sets build only", {
  dt <- data.table::data.table(
    chr    = character(0),
    pos    = integer(0),
    effect = numeric(0)
  )
  build(dt) <- "b37"

  result <- liftover(dt, "b38")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("chr", "pos", "effect"))
  expect_equal(build(result), "b38")
})

test_that(
  "liftover.data.table drops or NA-fills rows in chain gaps per drop_unlifted",
  {
    dt <- data.table::data.table(
      chr  = c("chr19", "chr1"),
      pos  = c(45411941L, 1L),
      rsid = c("rs429358", "gap")
    )
    build(dt) <- "b37"

    dropped <- liftover(dt, "b38", drop_unlifted = TRUE)
    expect_equal(nrow(dropped), 1)
    expect_equal(dropped$rsid, "rs429358")
    expect_equal(dropped$pos, 44908684L)

    kept <- liftover(dt, "b38", drop_unlifted = FALSE)
    expect_equal(nrow(kept), 2)
    expect_true(any(is.na(kept$pos)))
    expect_equal(kept[rsid == "rs429358", pos], 44908684L)
  }
)

test_that("liftover.data.table accepts start/end ranges", {
  dt <- data.table::data.table(
    chr   = "chr19",
    start = 45411941L,
    end   = 45412079L
  )
  build(dt) <- "b37"

  result <- liftover(dt, "b38")
  expect_equal(result$start, 44908684L)
  expect_equal(result$end, 44908822L)
  expect_equal(build(result), "b38")
})

test_that("liftover.data.table accepts start-only or end-only", {
  start_only <- data.table::data.table(chr = "chr19", start = 45411941L)
  build(start_only) <- "b37"
  end_only <- data.table::data.table(chr = "chr19", end = 45411941L)
  build(end_only) <- "b37"

  result_start <- liftover(start_only, "b38")
  result_end <- liftover(end_only, "b38")
  expect_equal(result_start$start, 44908684L)
  expect_false("end" %in% names(result_start))
  expect_equal(result_end$end, 44908684L)
  expect_false("start" %in% names(result_end))
})

test_that(
  "liftover.data.table errors when both pos and start/end are supplied",
  {
    dt <- data.table::data.table(
      chr   = "chr19",
      pos   = 45411941L,
      start = 45411941L,
      end   = 45411941L
    )
    build(dt) <- "b37"
    expect_error(liftover(dt, "b38"), "both `pos` and `start`/`end`")
  }
)

test_that("liftover.data.table errors when no coordinate column is present", {
  dt <- data.table::data.table(chr = "chr19", rsid = "rs1")
  build(dt) <- "b37"
  expect_error(
    liftover(dt, "b38"),
    "`pos`, `start`, or `end`"
  )
})

test_that("liftover.data.table preserves non-coordinate columns", {
  dt <- data.table::data.table(
    chr       = c("chr19", "chr19"),
    pos       = c(45411941L, 45412079L),
    ref       = c("T", "C"),
    alt       = c("C", "T"),
    effect    = c(0.5, -0.3),
    effect_se = c(0.05, 0.04),
    pval      = c(1e-8, 1e-5)
  )
  build(dt) <- "b37"

  result <- liftover(dt, "b38")
  expect_equal(result$ref, dt$ref)
  expect_equal(result$alt, dt$alt)
  expect_equal(result$effect, dt$effect)
  expect_equal(result$effect_se, dt$effect_se)
  expect_equal(result$pval, dt$pval)
})

test_that("lift_chain_overlap reverse-complements named allele columns on minus-strand blocks", {
  # Two source blocks on chr1: [100,199] forward, [200,299] reverse.
  chain_dt <- data.table::data.table(
    start   = c(100L, 200L),
    end     = c(199L, 299L),
    width   = c(100L, 100L),
    chr     = c("chr1", "chr1"),
    offset  = c(-1000L, 5000L),
    new_chr = c("chr1", "chr1"),
    rev     = c(FALSE, TRUE)
  )
  data.table::setkey(chain_dt, chr, start, end)

  positions <- data.table::data.table(
    I     = 1:2,
    chr   = c("chr1", "chr1"),
    start = c(150L, 250L),
    end   = c(150L, 250L),
    ref   = c("A", "A"),
    alt   = c("C", "T")
  )
  data.table::setkey(positions, chr, start, end)

  lifted <- lift_chain_overlap(
    positions, chain_dt, mult = "first", nomatch = 0L,
    allele_cols = c("ref", "alt")
  )
  lifted <- lifted[order(I)]

  # Forward block: alleles unchanged.
  expect_equal(lifted[I == 1L, ref], "A")
  expect_equal(lifted[I == 1L, alt], "C")
  # Reverse block: alleles reverse-complemented (palindrome-safe; A/T swap).
  expect_equal(lifted[I == 2L, ref], "T")
  expect_equal(lifted[I == 2L, alt], "A")
  # The internal rev marker must not leak into the result.
  expect_false(".lift_rev" %in% names(lifted))
})

test_that("liftover.data.table reverse-complements alleles across a minus-strand chain", {
  # Synthetic chain mapping chr1[100,300] (b37) onto chr1 reverse strand (b38).
  rev_chain <- data.table::data.table(
    start = 100L, end = 300L, width = 201L,
    chr = "chr1", offset = 5000L, new_chr = "chr1", rev = TRUE
  )
  data.table::setkey(rev_chain, chr, start, end)
  data.table::setattr(rev_chain, "from", "b37")
  data.table::setattr(rev_chain, "to", "b38")
  testthat::local_mocked_bindings(get_chain_dt = function(from, to) rev_chain)

  dt <- data.table::data.table(chr = "chr1", pos = 200L, ref = "A", alt = "C")
  build(dt) <- "b37"

  lifted <- liftover(dt, "b38")
  expect_equal(lifted$ref, "T")  # reverse complement of A
  expect_equal(lifted$alt, "G")  # reverse complement of C
  expect_equal(build(lifted), "b38")
})
