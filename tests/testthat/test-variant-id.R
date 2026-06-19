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

test_that("variant_id_values builds ids and NAs any missing component", {
  expect_equal(
    variant_id_values(
      c("chr1", NA, "chr2", "chr3"), c(10L, 20L, NA, 40L),
      c("A", "C", "G", NA), c("G", "T", "A", "C"), "b38"
    ),
    c("chr1_10_A_G_b38", NA, NA, NA)
  )
})

test_that("add_variant_id and record_build accept a non-native build", {
  dt <- data.table::data.table(
    chr = "chr1", pos = 100L, ref = "A", alt = "G"
  )
  add_variant_id(dt, build = "dahu42")
  expect_equal(dt$variant_id, "chr1_100_A_G_dahu42")
  record_build(dt, build = "dahu42")
  expect_true("chr_dahu42" %in% names(dt))
})

test_that("check_variant_id passes consistent rows and skips NA cells", {
  dt <- data.table::data.table(
    chr = c("chr5", NA), pos = c(49861645L, NA),
    ref = c("A", NA), alt = c("G", NA),
    variant_id = c("chr5_49861645_A_G_b38", "chr1_10_A_T_b38")
  )
  expect_invisible(check_variant_id(dt, "b38"))
})

test_that("check_variant_id treats synonyms and native builds as equal", {
  dt <- data.table::data.table(
    chr = "chr5", pos = 49861645L, ref = "A", alt = "G",
    variant_id = "chr5_49861645_A_G_b38"
  )
  expect_invisible(check_variant_id(dt, "hg38"))
})

test_that("check_variant_id accepts a non-native build", {
  dt <- data.table::data.table(
    chr = "chr2", pos = 3000L, ref = "C", alt = "T",
    variant_id = "chr2_3000_C_T_dahu42"
  )
  expect_invisible(check_variant_id(dt, "dahu42"))
})

test_that("check_variant_id errors on a coordinate mismatch", {
  dt <- data.table::data.table(
    chr = "chr6", pos = 49861645L, ref = "A", alt = "G",
    variant_id = "chr5_49861645_A_G_b38"
  )
  expect_error(check_variant_id(dt, "b38"), "disagree")
})

test_that("check_variant_id reads the default build from the object", {
  dt <- data.table::data.table(
    chr = "chr5", pos = 49861645L, ref = "A", alt = "G",
    variant_id = "chr5_49861645_A_G_b38"
  )
  data.table::setattr(dt, "build", "b38")
  expect_invisible(check_variant_id(dt))
})

test_that("check_variant_id errors on a build mismatch", {
  dt <- data.table::data.table(
    chr = "chr5", pos = 49861645L, ref = "A", alt = "G",
    variant_id = "chr5_49861645_A_G_b38"
  )
  expect_error(check_variant_id(dt, "b37"), "build")
})

test_that("check_variant_id imposes no constraint for unparseable ids", {
  dt <- data.table::data.table(
    chr = "chr5", pos = 49861645L, ref = "A", alt = "G",
    variant_id = "rs999"
  )
  expect_invisible(check_variant_id(dt, "b38"))
})
