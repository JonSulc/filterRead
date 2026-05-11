test_that("add_build_versioned_columns duplicates standard coord columns", {
  dt <- data.table::data.table(
    chr = c("chr1", "chr2"),
    pos = c(100L, 200L)
  )
  build(dt) <- "b38"

  add_build_versioned_columns(dt)
  expect_true(all(c("chr_b38", "pos_b38") %in% names(dt)))
  expect_equal(dt$chr_b38, dt$chr)
  expect_equal(dt$pos_b38, dt$pos)
})

test_that("add_build_versioned_columns is idempotent", {
  dt <- data.table::data.table(
    chr = c("chr1", "chr2"),
    pos = c(100L, 200L)
  )
  build(dt) <- "b38"

  add_build_versioned_columns(dt)
  pre <- data.table::copy(dt)
  add_build_versioned_columns(dt)
  expect_equal(dt, pre)
})

test_that("add_build_versioned_columns skips columns absent from dt", {
  dt <- data.table::data.table(
    chr = "chr1",
    pos = 100L
  )
  build(dt) <- "b38"

  add_build_versioned_columns(dt)
  expect_setequal(
    names(dt),
    c("chr", "pos", "chr_b38", "pos_b38")
  )
})

test_that("add_variant_id constructs the canonical id format", {
  dt <- data.table::data.table(
    chr = c("chr19", "chr1"),
    pos = c(45411941L, 12345L),
    ref = c("T", "A"),
    alt = c("C", "G")
  )
  build(dt) <- "b37"

  add_variant_id(dt)
  expect_equal(
    dt$variant_id,
    c("chr19_45411941_T_C_b37", "chr1_12345_A_G_b37")
  )
})

test_that("add_variant_id yields NA where chr or pos is NA", {
  dt <- data.table::data.table(
    chr = c("chr19", NA_character_),
    pos = c(45411941L, NA_integer_),
    ref = c("T", "A"),
    alt = c("C", "G")
  )
  build(dt) <- "b38"

  add_variant_id(dt)
  expect_equal(dt$variant_id[1], "chr19_45411941_T_C_b38")
  expect_true(is.na(dt$variant_id[2]))
})

test_that("add_variant_id leaves existing variant_id alone unless overwrite", {
  dt <- data.table::data.table(
    chr        = "chr19",
    pos        = 45411941L,
    ref        = "T",
    alt        = "C",
    variant_id = "stale"
  )
  build(dt) <- "b37"

  add_variant_id(dt)
  expect_equal(dt$variant_id, "stale")

  add_variant_id(dt, overwrite = TRUE)
  expect_equal(dt$variant_id, "chr19_45411941_T_C_b37")
})

test_that("liftover retain_builds = TRUE keeps source- and target-build snapshots", {
  dt <- data.table::data.table(
    chr = "chr19",
    pos = 45411941L
  )
  build(dt) <- "b37"

  lifted <- liftover(dt, "b38", retain_builds = TRUE)
  expect_equal(lifted$chr, "chr19")
  expect_equal(lifted$pos, 44908684L)
  expect_equal(lifted$chr_b37, "chr19")
  expect_equal(lifted$pos_b37, 45411941L)
  expect_equal(lifted$chr_b38, "chr19")
  expect_equal(lifted$pos_b38, 44908684L)
})

test_that("liftover reuses cached target-build columns verbatim", {
  # Cache-poisoning: chr_b37 / pos_b37 deliberately diverge from what
  # the chain would produce. If liftover honors the cache, the
  # returned chr/pos must take those exact bogus values.
  dt <- data.table::data.table(
    chr     = "chr19",
    pos     = 44908684L,
    chr_b37 = "chrXX",
    pos_b37 = 999999L
  )
  build(dt) <- "b38"

  back <- liftover(dt, "b37")
  expect_equal(back$chr, "chrXX")
  expect_equal(back$pos, 999999L)
  expect_equal(build(back), "b37")
})

test_that("cache hit avoids the chain lookup entirely", {
  dt <- data.table::data.table(
    chr     = "chr19",
    pos     = 44908684L,
    chr_b37 = "chr19",
    pos_b37 = 45411941L
  )
  build(dt) <- "b38"

  testthat::local_mocked_bindings(
    lift_chain_overlap = function(...) {
      stop("lift_chain_overlap should not run when a cache hit applies")
    }
  )
  back <- liftover(dt, "b37")
  expect_equal(back$pos, 45411941L)
})

test_that("liftover same-build no-op honors retain_builds", {
  dt <- data.table::data.table(
    chr = "chr1",
    pos = 100L
  )
  build(dt) <- "b37"

  result <- liftover(dt, "b37", retain_builds = TRUE)
  expect_equal(result$chr, "chr1")
  expect_equal(result$pos, 100L)
  expect_equal(result$chr_b37, "chr1")
  expect_equal(result$pos_b37, 100L)
})
