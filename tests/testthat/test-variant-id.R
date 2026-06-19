test_that("parse_variant_id decodes ids, mitochondria, non-native, NAs rest", {
  out <- parse_variant_id(c(
    "chr5_49861645_A_G_b38", "chrMT_73_A_G_b37", "chr2_3000_C_T_dahu42",
    "rs123", "not_a_variant", NA
  ))
  expect_equal(out$chr, c("chr5", "chrMT", "chr2", NA, NA, NA))
  expect_equal(out$pos, c(49861645L, 73L, 3000L, NA, NA, NA))
  expect_equal(out$ref, c("A", "A", "C", NA, NA, NA))
  expect_equal(out$alt, c("G", "G", "T", NA, NA, NA))
  expect_equal(out$build, c("b38", "b37", "dahu42", NA, NA, NA))
})

test_that("parse_variant_id round-trips add_variant_id output", {
  dt <- data.table::data.table(
    chr = c("chr1", "chrX"), pos = c(100L, 200L),
    ref = c("A", "C"), alt = c("G", "T")
  )
  add_variant_id(dt, build = "b37")
  out <- parse_variant_id(dt$variant_id)
  expect_equal(out$chr, dt$chr)
  expect_equal(out$pos, dt$pos)
  expect_equal(out$ref, dt$ref)
  expect_equal(out$alt, dt$alt)
  expect_equal(out$build, c("b37", "b37"))
})
