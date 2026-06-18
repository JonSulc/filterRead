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
