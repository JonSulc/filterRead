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

test_that("finterface[v] matches on chr/pos/ref/alt", {
  fi <- local_file_interface(dt = ss_dt(), build = "b38")
  v <- new_variants(
    data.table::data.table(chr = "chr1", pos = 100L, ref = "A", alt = "G"),
    build = "b38"
  )
  result <- fi[v]
  expect_equal(nrow(result), 1)
  expect_equal(result$pos, 100L)
  expect_equal(result$alt, "G")
})

test_that("finterface[v] excludes a different allele at the same position", {
  fi <- local_file_interface(dt = ss_dt(), build = "b38")
  # file row chr1:200 is C/T; ask for chr1:200 A/G -> no match
  v <- new_variants(
    data.table::data.table(chr = "chr1", pos = 200L, ref = "A", alt = "G"),
    build = "b38"
  )
  expect_equal(nrow(fi[v]), 0)
})

test_that("finterface[v] includes a swapped allele order", {
  fi <- local_file_interface(dt = ss_dt(), build = "b38")
  # file row chr2:300 is G/A; ask for chr2:300 A/G (swapped) -> match
  v <- new_variants(
    data.table::data.table(chr = "chr2", pos = 300L, ref = "A", alt = "G"),
    build = "b38"
  )
  result <- fi[v]
  expect_equal(nrow(result), 1)
  expect_equal(result$pos, 300L)
})

test_that("finterface[v] with empty v matches nothing without awk", {
  testthat::local_mocked_bindings(
    fcondition_to_awk = function(...) {
      stop("fcondition_to_awk should not be called for an empty variants set")
    }
  )
  fi <- local_file_interface(dt = ss_dt(), build = "b38")
  v <- new_variants(
    data.table::data.table(
      chr = character(), pos = integer(),
      ref = character(), alt = character()
    ),
    build = "b38"
  )
  expect_equal(nrow(fi[v]), 0)
})

test_that("finterface[v] with all rows dropped matches nothing, no awk", {
  # A non-empty v whose rows all carry an unmappable (NA) pos: variant_keyset
  # drops them all (with a warning), leaving an empty keyset that takes the
  # same empty-genomic_regions short-circuit as the 0-row case.
  testthat::local_mocked_bindings(
    fcondition_to_awk = function(...) {
      stop("fcondition_to_awk should not be called when every variant drops")
    }
  )
  fi <- local_file_interface(dt = ss_dt(), build = "b38")
  v <- new_variants(
    data.table::data.table(
      chr = c("chr1", "chr2"),
      pos = c(NA_integer_, NA_integer_),
      ref = c("A", "G"),
      alt = c("G", "A")
    ),
    build = "b38"
  )
  expect_warning(result <- fi[v], "dropped")
  expect_equal(nrow(result), 0)
})

test_that("finterface[v] errors on a ref-absent file", {
  fi <- local_file_interface(
    dt = data.table::data.table(chr = "chr1", pos = 100L, pval = 0.1),
    build = "b38"
  )
  v <- new_variants(
    data.table::data.table(chr = "chr1", pos = 100L, ref = "A", alt = "G"),
    build = "b38"
  )
  expect_error(fi[v], "ref")
})

test_that("finterface[v] composes with a column condition", {
  # `v & pval < X` is a composite: the pval arm references a file column, the v
  # arm dispatches to new_filter_condition_impl.variants, and the two are ANDed.
  fi <- local_file_interface(dt = ss_dt(), build = "b38")
  v <- new_variants(
    data.table::data.table(
      chr = c("chr1", "chr2"), pos = c(100L, 300L),
      ref = c("A", "A"), alt = c("G", "G")
    ),
    build = "b38"
  )
  # v matches chr1:100 (pval .1) and chr2:300 (swap order; pval .3).
  # Adding pval < 0.15 keeps only chr1:100.
  result <- fi[v & pval < 0.15]
  expect_equal(nrow(result), 1)
  expect_equal(result$pos, 100L)
})

test_that("finterface[v] lifts v across builds and matches swapped alleles", {
  # Synthetic b37->b38 chain so the test is deterministic and needs no real
  # chain files, mirroring tests/testthat/test-liftover-dt.R. The block maps
  # b37 chr1[100,200] onto b38 by subtracting offset 50, so b37 chr1:150 lifts
  # to b38 chr1:100. The lift is forward-strand, so alleles carry through
  # unchanged: v arrives as G/A while the ss_dt() row chr1:100 is A/G,
  # exercising the cross-build lift and swapped-allele-order match together.
  testthat::local_mocked_bindings(
    get_chain_dt = function(from, to) {
      make_fwd_chain(100L, 200L, 50L, "b37", "b38")
    }
  )

  fi <- local_file_interface(dt = ss_dt(), build = "b38")
  v <- new_variants(
    data.table::data.table(chr = "chr1", pos = 150L, ref = "G", alt = "A"),
    build = "b37"
  )
  result <- fi[v]
  expect_equal(nrow(result), 1)
  expect_equal(result$pos, 100L)
  # The returned row is the file's, in the file's allele order (A/G), not v's.
  expect_equal(result$ref, "A")
  expect_equal(result$alt, "G")
})

test_that("finterface[v] equals variant_id %in% both orderings", {
  fi <- local_file_interface(dt = ss_dt(), build = "b38")
  v <- new_variants(
    data.table::data.table(
      chr = c("chr1", "chr2"),
      pos = c(100L, 300L),
      ref = c("A", "A"),
      alt = c("G", "G")
    ),
    build = "b38"
  )
  by_variant <- fi[v][order(pos)]
  # The fixtures carry no variant_id column, so the reference set is built in R
  # as the file rows whose (chr, pos, ref, alt) match v in either allele
  # order -- the identity semantics finterface[v] implements.
  ref_rows <- ss_dt()[
    (chr == "chr1" & pos == 100L &
       ((ref == "A" & alt == "G") | (ref == "G" & alt == "A"))) |
      (chr == "chr2" & pos == 300L &
         ((ref == "A" & alt == "G") | (ref == "G" & alt == "A")))
  ][order(pos)]
  expect_equal(by_variant$pos, ref_rows$pos)
  expect_equal(by_variant$alt, ref_rows$alt)
})
