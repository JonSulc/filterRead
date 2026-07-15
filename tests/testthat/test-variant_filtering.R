ss_dt <- function() {
  data.table::data.table(
    chr  = c("chr1", "chr1", "chr2"),
    pos  = c(100L, 200L, 300L),
    ref  = c("A", "C", "G"),
    alt  = c("G", "T", "A"),
    pval = c(0.1, 0.2, 0.3)
  )
}

test_that("require_variant_columns errors when any required column is absent", {
  full <- data.table::data.table(
    chr = "chr1", pos = 100L, ref = "A", alt = "G", pval = 0.1
  )
  for (column in c("chr", "pos", "ref", "alt")) {
    fi <- local_file_interface(
      filename = paste0("data_", column, ".csv"),
      dt = full[, -column, with = FALSE],
      build = "b38"
    )
    expect_error(require_variant_columns(fi), column)
  }
})

test_that("require_variant_columns passes for a full coordinate file", {
  fi <- local_file_interface(dt = ss_dt(), build = "b38")
  expect_silent(require_variant_columns(fi))
})

test_that("require_variant_columns rejects encoded coordinate columns", {
  fi <- local_summary_stats_interface(
    encode_columns = summary_stats_standard_names_dt[
      list(input_name = "chr_colon_pos"), on = "input_name"
    ],
    build = "b38"
  )
  expect_error(require_variant_columns(fi), "encoded")
})

test_that("passes when chr/pos coexist with an encoded parent", {
  # chr/pos are present as their own columns alongside an encoded chr:pos
  # parent; the real columns satisfy the requirement.
  fi <- local_file_interface(
    dt = data.table::data.table(
      chr = c("chr1", "chr2"), pos = c(100L, 300L),
      chr_colon_pos = c("1:100", "2:300"),
      ref = c("A", "G"), alt = c("G", "A"), pval = c(0.1, 0.3)
    ),
    build = "b38"
  )
  expect_silent(require_variant_columns(fi))
})

test_that("rejects a coordinate present only via an encoded parent", {
  # chr comes only from the encoded chr:pos parent (no real chr column), so it
  # does not satisfy the requirement; the real pos is fine.
  fi <- local_file_interface(
    dt = data.table::data.table(
      pos = c(100L, 300L),
      chr_colon_pos = c("1:100", "2:300"),
      ref = c("A", "G"), alt = c("G", "A"), pval = c(0.1, 0.3)
    ),
    build = "b38"
  )
  expect_error(require_variant_columns(fi), "chr")
})

test_that("require_variant_columns rejects rsid-indexed files", {
  fi <- local_rsid_summary_stats_interface(build = "b38")
  expect_error(require_variant_columns(fi), "rsid-indexed")
})

test_that("variant_keyset builds both allele orderings, upper-cased", {
  fi <- local_file_interface(dt = ss_dt(), build = "b38")
  v <- new_variants(
    data.table::data.table(
      chr = "chr1", pos = 100L, ref = "a", alt = "g"
    ),
    build = "b38"
  )
  ks <- variant_keyset(v, fi, "b38")
  expect_setequal(ks$keys, c("chr1_100_A_G", "chr1_100_G_A"))
  expect_equal(ks$positions, 100L)
})

test_that("variant_keyset matches a bare-chr file's representation", {
  fi <- local_file_interface(
    dt = data.table::data.table(
      chr = c("1", "2"), pos = c(100L, 300L),
      ref = c("A", "G"), alt = c("G", "A"), pval = c(0.1, 0.3)
    ),
    build = "b38"
  )
  v <- new_variants(
    data.table::data.table(
      chr = "chr1", pos = 100L, ref = "A", alt = "G"
    ),
    build = "b38"
  )
  ks <- variant_keyset(v, fi, "b38")
  expect_setequal(ks$keys, c("1_100_A_G", "1_100_G_A"))
})

test_that("variant_keyset drops incomplete rows with a warning", {
  fi <- local_file_interface(dt = ss_dt(), build = "b38")
  v <- new_variants(
    data.table::data.table(
      chr = c("chr1", "chr2"),
      pos = c(100L, NA_integer_),
      ref = c("A", "G"),
      alt = c("G", "A")
    ),
    build = "b38"
  )
  expect_warning(ks <- variant_keyset(v, fi, "b38"), "dropped")
  expect_setequal(ks$keys, c("chr1_100_A_G", "chr1_100_G_A"))
  expect_equal(ks$positions, 100L)
})
