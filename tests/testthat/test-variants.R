test_that("new_variants/as_variants resolve default build without recursing", {
  dt <- data.table::data.table(chr = "chr1", pos = 100L, ref = "A", alt = "G")
  data.table::setattr(dt, "build", "b38")
  v <- as_variants(dt)
  expect_s3_class(v, "variants")
  expect_equal(build(v), "b38")
  expect_equal(build(new_variants(dt)), "b38")
})

test_that("new_variants validates required columns and sets class + build", {
  dt <- data.table::data.table(
    chr = "chr1", pos = 100L, ref = "A", alt = "G"
  )
  v <- new_variants(dt, build = "b38")
  expect_s3_class(v, "variants")
  expect_true(data.table::is.data.table(v))
  expect_equal(build(v), "b38")
  # Provenance + variant_id + defining_build stamped on construction.
  expect_true(all(c("chr_b38", "pos_b38", "ref_b38", "alt_b38") %in% names(v)))
  expect_equal(v$variant_id, "chr1_100_A_G_b38")
  expect_equal(v$defining_build, "b38")
})

test_that("new_variants errors when an identity column is missing", {
  dt <- data.table::data.table(chr = "chr1", pos = 100L, ref = "A")
  expect_error(new_variants(dt, build = "b38"), "alt")
})

test_that("coordinate_columns returns only the canonical identity columns", {
  expect_equal(coordinate_columns(), c("chr", "pos", "ref", "alt"))
})

test_that("liftover.variants preserves class and round-trips alleles via provenance", {
  v <- new_variants(
    data.table::data.table(chr = "chr19", pos = 45411941L, ref = "A", alt = "G"),
    build = "b37"
  )
  lifted <- liftover(v, "b38")
  expect_s3_class(lifted, "variants")
  expect_equal(build(lifted), "b38")
  expect_equal(lifted$pos, 44908684L)
  # Original b37 coordinates retained as provenance.
  expect_equal(lifted$pos_b37, 45411941L)

  back <- liftover(lifted, "b37")
  expect_equal(back$pos, 45411941L)
  expect_equal(back$ref, "A")
  expect_equal(back$alt, "G")
})

test_that("unique.variants dedups on identity, ignoring provenance columns", {
  v <- new_variants(
    data.table::data.table(
      chr = c("chr1", "chr1"),
      pos = c(100L, 100L),
      ref = c("A", "A"),
      alt = c("G", "G")
    ),
    build = "b38"
  )
  # Add a divergent provenance column to prove it is excluded from identity.
  v[, pos_b37 := c(50L, 60L)]
  expect_equal(nrow(unique(v)), 1L)
})

test_that("best-source liftover recovers rows NA in the current build via a newer build", {
  fwd_chain <- function(lo, hi, offset, from, to) {
    ch <- data.table::data.table(
      start = lo, end = hi, width = hi - lo + 1L,
      chr = "chr1", offset = offset, new_chr = "chr1", rev = FALSE
    )
    data.table::setkey(ch, chr, start, end)
    data.table::setattr(ch, "from", from)
    data.table::setattr(ch, "to", to)
    ch
  }
  chains <- list(
    b37_b36 = fwd_chain(800L, 1000L, 100L, "b37", "b36"),  # covers row A (b37 900)
    b38_b36 = fwd_chain(500L, 2500L, 200L, "b38", "b36")   # covers row B (b38 2000)
  )
  testthat::local_mocked_bindings(
    get_chain_dt = function(from, to) chains[[paste(from, to, sep = "_")]]
  )

  # Currently in b37; row B failed b38->b37 (b37 NA) but keeps b38 provenance.
  v <- new_variants(
    data.table::data.table(
      chr = c("chr1", "chr1"),
      pos = c(900L, NA_integer_),
      ref = c("A", "C"),
      alt = c("G", "T")
    ),
    build = "b37"
  )
  record_extra_build(
    v, "b38", c("chr1", "chr1"), c(1000L, 2000L), c("A", "C"), c("G", "T")
  )

  lifted <- liftover(v, "b36")  # source = "best" by default
  # Row A lifts from the current build (b37); row B, NA in b37, is recovered
  # directly from its recorded b38 coordinates.
  expect_equal(lifted$pos, c(800L, 1800L))
  expect_equal(build(lifted), "b36")
})

test_that("select_source_build picks the defining build then falls back", {
  v <- new_variants(
    data.table::data.table(
      chr = c("chr1", "chr1"), pos = c(100L, NA_integer_),
      ref = c("A", "C"), alt = c("G", "T")
    ),
    build = "b37"
  )
  record_extra_build(
    v, "b38", c("chr1", "chr1"), c(500L, 600L), c("A", "C"), c("G", "T")
  )

  # Row 1 is defined in its defining_build b37; row 2 is NA in b37 and falls
  # back to b38.
  expect_equal(select_source_build(v, "b36", "best"), c("b37", "b38"))
  # A fixed source applies to every row that has coordinates there.
  expect_equal(select_source_build(v, "b36", "b38"), c("b38", "b38"))
  # Rows with no coordinates in the requested build are NA.
  expect_equal(
    select_source_build(v, "b36", "current"),
    c("b37", NA_character_)
  )
})

test_that("source = 'current' does not recover NA-at-current rows", {
  fwd_chain <- function(lo, hi, offset, from, to) {
    ch <- data.table::data.table(
      start = lo, end = hi, width = hi - lo + 1L,
      chr = "chr1", offset = offset, new_chr = "chr1", rev = FALSE
    )
    data.table::setkey(ch, chr, start, end)
    data.table::setattr(ch, "from", from)
    data.table::setattr(ch, "to", to)
    ch
  }
  testthat::local_mocked_bindings(
    get_chain_dt = function(from, to) fwd_chain(800L, 1000L, 100L, "b37", "b36")
  )
  v <- new_variants(
    data.table::data.table(
      chr = c("chr1", "chr1"),
      pos = c(900L, NA_integer_),
      ref = c("A", "C"),
      alt = c("G", "T")
    ),
    build = "b37"
  )
  record_extra_build(
    v, "b38", c("chr1", "chr1"), c(1000L, 2000L), c("A", "C"), c("G", "T")
  )

  lifted <- liftover(v, "b36", source = "current")
  # Source fixed to the current build (b37): row A lifts; row B is NA in b37
  # and is kept with NA coordinates.
  expect_equal(nrow(lifted), 2L)
  expect_equal(lifted$pos, c(800L, NA))
})

test_that("best-source liftover restores a recorded build from the cache (no chain lift)", {
  # Target b38 is already recorded, so it is used verbatim with no chain.
  v <- new_variants(
    data.table::data.table(
      chr = c("chr1", "chr1"),
      pos = c(800L, 1800L),
      ref = c("A", "C"),
      alt = c("G", "T")
    ),
    build = "b36"
  )
  record_extra_build(
    v, "b38", c("chr1", "chr1"), c(1000L, 2000L), c("A", "C"), c("G", "T")
  )

  back <- liftover(v, "b38")
  expect_equal(back$pos, c(1000L, 2000L))
  expect_equal(build(back), "b38")
})

test_that("source selects the build to lift from (best / current / named)", {
  # A b37-declared variant currently expressed in b38. The b37 and b38 chains
  # map to different b36 positions, so which build was used as the source is
  # observable in the result.
  chains <- list(
    b37_b38 = data.table::data.table(            # 100 -> 5000
      start = 1L, end = 1000L, width = 1000L, chr = "chr1",
      offset = -4900L, new_chr = "chr1", rev = FALSE
    ),
    b37_b36 = data.table::data.table(            # 100 -> 90
      start = 1L, end = 1000L, width = 1000L, chr = "chr1",
      offset = 10L, new_chr = "chr1", rev = FALSE
    ),
    b38_b36 = data.table::data.table(            # 5000 -> 4000
      start = 1L, end = 10000L, width = 10000L, chr = "chr1",
      offset = 1000L, new_chr = "chr1", rev = FALSE
    )
  )
  for (nm in names(chains)) {
    data.table::setkey(chains[[nm]], chr, start, end)
    parts <- strsplit(nm, "_")[[1]]
    data.table::setattr(chains[[nm]], "from", parts[1])
    data.table::setattr(chains[[nm]], "to", parts[2])
  }
  testthat::local_mocked_bindings(
    get_chain_dt = function(from, to) chains[[paste(from, to, sep = "_")]]
  )

  # Declared in b37, then lifted to b38 (the lift keeps b37 as defining_build).
  v <- liftover(
    new_variants(
      data.table::data.table(chr = "chr1", pos = 100L, ref = "A", alt = "G"),
      build = "b37"
    ),
    "b38"
  )

  expect_equal(liftover(v, "b36")$pos, 90L)                        # best: declared b37
  expect_equal(liftover(v, "b36", source = "b37")$pos, 90L)        # named b37
  expect_equal(liftover(v, "b36", source = "b38")$pos, 4000L)      # named b38
  expect_equal(liftover(v, "b36", source = "current")$pos, 4000L)  # current is b38
})

test_that("a chain-gap row is not retried from another build (source fixed by coords)", {
  # The variant's source build is b37, whose b36 chain gaps at its position.
  # b38 (also recorded) maps, so the NA result shows the source stays b37.
  chains <- list(
    b37_b36 = data.table::data.table(           # gaps at pos 999
      start = 1L, end = 100L, width = 100L, chr = "chr1",
      offset = 0L, new_chr = "chr1", rev = FALSE
    ),
    b38_b36 = data.table::data.table(           # covers pos 5000
      start = 4000L, end = 6000L, width = 2001L, chr = "chr1",
      offset = 200L, new_chr = "chr1", rev = FALSE
    )
  )
  for (nm in names(chains)) {
    data.table::setkey(chains[[nm]], chr, start, end)
    parts <- strsplit(nm, "_")[[1]]
    data.table::setattr(chains[[nm]], "from", parts[1])
    data.table::setattr(chains[[nm]], "to", parts[2])
  }
  testthat::local_mocked_bindings(
    get_chain_dt = function(from, to) chains[[paste(from, to, sep = "_")]]
  )

  v <- new_variants(
    data.table::data.table(chr = "chr1", pos = 999L, ref = "A", alt = "G"),
    build = "b37"
  )
  record_extra_build(v, "b38", "chr1", 5000L, "A", "G")

  lifted <- liftover(v, "b36")
  # b37 is the source and gaps here, so the row is kept with NA coordinates.
  expect_equal(nrow(lifted), 1L)
  expect_true(is.na(lifted$pos))
})

test_that("source = 'best', multi_match = 'all' fans out multi-mapped variants in order", {
  # Two overlapping chain blocks (a duplicated region) both cover pos 150, so
  # variant x maps twice; variant y (pos 190) maps once.
  overlapping <- function() {
    ch <- data.table::data.table(
      start   = c(100L, 120L),
      end     = c(200L, 180L),
      width   = c(101L, 61L),
      chr     = c("chr1", "chr1"),
      offset  = c(50L, 30L),
      new_chr = c("chr1", "chr1"),
      rev     = c(FALSE, FALSE)
    )
    data.table::setkey(ch, chr, start, end)
    data.table::setattr(ch, "from", "b37")
    data.table::setattr(ch, "to", "b38")
    ch
  }
  testthat::local_mocked_bindings(get_chain_dt = function(from, to) overlapping())

  v <- new_variants(
    data.table::data.table(
      chr = c("chr1", "chr1"),
      pos = c(150L, 190L),
      ref = c("A", "C"),
      alt = c("G", "T"),
      id  = c("x", "y")
    ),
    build = "b37"
  )

  result <- liftover(v, "b38", multi_match = "all")
  expect_equal(result$id, c("x", "x", "y"))        # x fanned out in place, before y
  expect_setequal(result[id == "x", pos], c(100L, 120L))
  expect_equal(result[id == "y", pos], 140L)

  one <- liftover(v, "b38")                          # multi_match = "first"
  expect_equal(nrow(one), 2L)
})

test_that("liftover.variants rejects forwarded liftover.data.table arguments", {
  v <- new_variants(
    data.table::data.table(chr = "chr1", pos = 100L, ref = "A", alt = "G"),
    build = "b37"
  )
  expect_error(
    liftover(v, "b38", drop_unlifted = FALSE),
    "forwards no extra arguments"
  )
})

test_that("liftover.variants records the target build's provenance columns", {
  fwd_chain <- function(from, to) {
    ch <- data.table::data.table(
      start = 1L, end = 1000L, width = 1000L, chr = "chr1",
      offset = -4900L, new_chr = "chr1", rev = FALSE   # 100 -> 5000
    )
    data.table::setkey(ch, chr, start, end)
    data.table::setattr(ch, "from", from)
    data.table::setattr(ch, "to", to)
    ch
  }
  testthat::local_mocked_bindings(
    get_chain_dt = function(from, to) fwd_chain(from, to)
  )

  v <- new_variants(
    data.table::data.table(chr = "chr1", pos = 100L, ref = "A", alt = "G"),
    build = "b37"
  )
  lifted <- liftover(v, "b38")

  # The target build's provenance must be stamped, not just the source's, so a
  # later lift can source from b38 directly.
  expect_true(all(
    c("chr_b38", "pos_b38", "ref_b38", "alt_b38", "variant_id_b38") %in%
      names(lifted)
  ))
  expect_equal(lifted$pos_b38, 5000L)
  expect_equal(lifted$variant_id, "chr1_5000_A_G_b38")
})

test_that("a file-interface read coerces to variants carrying its inferred build", {
  dt <- data.table::data.table(
    chr  = paste0("chr", c(1L, 1L)),
    pos  = c(100L, 200L),
    ref  = c("A", "C"),
    alt  = c("G", "T"),
    pval = c(1e-9, 1e-8)
  )
  local_csv_file("data.csv", dt = dt)
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  result <- finterface[]
  v <- as_variants(result, build = "b38")
  expect_s3_class(v, "variants")
  expect_equal(build(v), "b38")
  expect_equal(v$variant_id, c("chr1_100_A_G_b38", "chr1_200_C_T_b38"))
})

test_that("liftover.variants reverse-complements alleles on strand flip", {
  # A minus-strand (rev = TRUE) block: the b37 forward alleles are expressed
  # as their reverse complement in b38. Strand-driven, so palindrome-safe.
  rev_chain <- function() {
    ch <- data.table::data.table(
      start = 100L, end = 300L, width = 201L,
      chr = "chr1", offset = 5000L, new_chr = "chr1", rev = TRUE
    )
    data.table::setkey(ch, chr, start, end)
    data.table::setattr(ch, "from", "b37")
    data.table::setattr(ch, "to", "b38")
    ch
  }
  testthat::local_mocked_bindings(get_chain_dt = function(from, to) rev_chain())

  v <- new_variants(
    data.table::data.table(chr = "chr1", pos = 200L, ref = "A", alt = "C"),
    build = "b37"
  )

  lifted <- liftover(v, "b38")
  expect_equal(build(lifted), "b38")
  expect_equal(lifted$ref, "T")          # reverse complement of A
  expect_equal(lifted$alt, "G")          # reverse complement of C
  expect_false(is.na(lifted$pos))

  # Round trip: lifting back to b37 restores the original alleles from the
  # recorded b37 provenance.
  back <- liftover(lifted, "b37")
  expect_equal(back$ref, "A")
  expect_equal(back$alt, "C")
  expect_equal(back$pos, 200L)
})

test_that("new_variants scaffolds an empty table into a 0-row variants", {
  v <- as_variants(data.table::data.table(), build = "b38")
  expect_s3_class(v, "variants")
  expect_equal(nrow(v), 0L)
  expect_equal(build(v), "b38")
  expect_true(all(c("chr", "pos", "ref", "alt", "variant_id",
                    "defining_build") %in% names(v)))
  expect_type(v$chr, "character")
  expect_type(v$pos, "integer")
})

test_that("new_variants scaffolds coordinates while keeping other columns", {
  v <- as_variants(
    data.table::data.table(gene_id = character(0)), build = "b38"
  )
  expect_equal(nrow(v), 0L)
  expect_true("gene_id" %in% names(v))
  expect_true(all(c("chr", "pos", "ref", "alt") %in% names(v)))
})

test_that("new_variants still errors on a non-empty input missing coordinates", {
  expect_error(
    new_variants(data.table::data.table(gene_id = "X"), build = "b38"),
    "chr"
  )
})

test_that("new_variants constructs with a non-native build", {
  v <- as_variants(
    data.table::data.table(chr = "chr1", pos = 100L, ref = "A", alt = "G"),
    build = "dahu42"
  )
  expect_equal(build(v), "dahu42")
  expect_equal(v$variant_id, "chr1_100_A_G_dahu42")
  expect_true("chr_dahu42" %in% names(v))
})

test_that("liftover of a non-native build errors", {
  v <- as_variants(
    data.table::data.table(chr = "chr1", pos = 100L, ref = "A", alt = "G"),
    build = "dahu42"
  )
  expect_error(liftover(v, "b38"), "[Uu]nknown build")
})

test_that("new_variants accepts a base data.frame", {
  v <- new_variants(
    data.frame(chr = "chr1", pos = 100L, ref = "A", alt = "G"),
    build = "b38"
  )
  expect_s3_class(v, "variants")
  expect_true(data.table::is.data.table(v))
  expect_equal(v$variant_id, "chr1_100_A_G_b38")
})

test_that("as_variants accepts a data.frame", {
  v <- as_variants(
    data.frame(chr = "chr2", pos = 200L, ref = "C", alt = "T"),
    build = "b38"
  )
  expect_s3_class(v, "variants")
  expect_equal(build(v), "b38")
})

test_that("new_variants builds from a variant_id-only table", {
  v <- as_variants(
    data.table::data.table(variant_id = "chr5_49861645_A_G_b38")
  )
  expect_s3_class(v, "variants")
  expect_equal(build(v), "b38")
  expect_equal(v$chr, "chr5")
  expect_equal(v$pos, 49861645L)
  expect_equal(v$ref, "A")
  expect_equal(v$alt, "G")
  expect_equal(v$defining_build, "b38")
})

test_that("variant_id backfill coerces a double pos and factor chr", {
  dt <- data.table::data.table(
    chr = factor("chr1"), pos = 100, ref = "A", alt = "G",
    variant_id = "chr1_100_A_G_b38"
  )
  v <- new_variants(dt, build = "b38")
  expect_type(v$pos, "integer")
  expect_equal(v$pos, 100L)
  expect_equal(v$chr, "chr1")
})

test_that("new_variants builds from a non-native variant_id", {
  v <- as_variants(
    data.table::data.table(variant_id = "chr2_3000_C_T_dahu42")
  )
  expect_equal(build(v), "dahu42")
  expect_equal(v$pos, 3000L)
})

test_that("new_variants backfills a mixed table and validates present values", {
  dt <- data.table::data.table(
    chr = c("chr1", NA, "chr3"),
    pos = c(100L, NA, 300L),
    ref = c("A", NA, "G"),
    alt = c("G", NA, "A"),
    variant_id = c(NA, "chr2_200_C_T_b38", "chr3_300_G_A_b38")
  )
  v <- new_variants(dt, build = "b38")
  expect_equal(v$chr, c("chr1", "chr2", "chr3"))
  expect_equal(v$pos, c(100L, 200L, 300L))
  expect_equal(
    v$variant_id,
    c("chr1_100_A_G_b38", "chr2_200_C_T_b38", "chr3_300_G_A_b38")
  )
})

test_that("new_variants preserves a non-canonical id and fills NA ids", {
  dt <- data.table::data.table(
    chr = c("chr1", "chr2"), pos = c(100L, 200L),
    ref = c("A", "C"), alt = c("G", "T"),
    variant_id = c("rs1", NA)
  )
  v <- new_variants(dt, build = "b38")
  expect_equal(v$variant_id, c("rs1", "chr2_200_C_T_b38"))
})

test_that("new_variants derives build from the variant_id suffix", {
  v <- new_variants(
    data.table::data.table(variant_id = "chr1_10_A_T_b37")
  )
  expect_equal(build(v), "b37")
})

test_that("new_variants errors when build conflicts with the id suffix", {
  expect_error(
    new_variants(
      data.table::data.table(variant_id = "chr5_49861645_A_G_b38"),
      build = "b37"
    ),
    "build"
  )
})
