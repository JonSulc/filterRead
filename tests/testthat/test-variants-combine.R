mk <- function(chr, pos, ref, alt, build, ...) {
  new_variants(
    data.table::data.table(chr = chr, pos = pos, ref = ref, alt = alt, ...),
    build = build
  )
}

test_that("rbindlist_variants binds same-build variants, preserving class", {
  out <- rbindlist_variants(list(
    mk("chr1", 100L, "A", "G", "b38"),
    mk("chr2", 200L, "C", "T", "b38")
  ))
  expect_s3_class(out, "variants")
  expect_equal(build(out), "b38")
  expect_equal(out$pos, c(100L, 200L))
  expect_equal(out$defining_build, c("b38", "b38"))
})

test_that("rbind.variants is the pairwise convenience", {
  out <- rbind(
    mk("chr1", 100L, "A", "G", "b38"),
    mk("chr2", 200L, "C", "T", "b38")
  )
  expect_s3_class(out, "variants")
  expect_equal(nrow(out), 2L)
})

test_that("rbindlist_variants lifts inputs to the first input's build", {
  testthat::local_mocked_bindings(
    get_chain_dt = function(from, to) make_fwd_chain(1L, 10000L, 50L, from, to)
  )
  out <- rbindlist_variants(list(
    mk("chr1", 1000L, "A", "G", "b37"),
    mk("chr1", 2000L, "C", "T", "b38")
  ))
  expect_equal(build(out), "b37")
  # row 1 already b37 (no lift); row 2 lifts b38 -> b37 (new = old - 50 = 1950)
  expect_equal(out$pos, c(1000L, 1950L))
  expect_equal(out$defining_build, c("b37", "b38"))
})

test_that("target overrides the first input's build", {
  testthat::local_mocked_bindings(
    get_chain_dt = function(from, to) make_fwd_chain(1L, 10000L, 50L, from, to)
  )
  out <- rbindlist_variants(
    list(mk("chr1", 1000L, "A", "G", "b37")),
    target = "b38"
  )
  expect_equal(build(out), "b38")
  # b37 -> b38 lift: new = old - 50 = 950
  expect_equal(out$pos, 950L)
})

test_that("rbindlist_variants accepts data.tables with a build", {
  dt <- data.table::data.table(chr = "chr2", pos = 200L, ref = "C", alt = "T")
  build(dt) <- "b38"
  out <- rbindlist_variants(list(mk("chr1", 100L, "A", "G", "b38"), dt))
  expect_equal(nrow(out), 2L)
})

test_that("rbindlist_variants rejects a build-less data.table", {
  bare <- data.table::data.table(
    chr = "chr3", pos = 300L, ref = "G", alt = "A"
  )
  expect_error(
    rbindlist_variants(list(mk("chr1", 100L, "A", "G", "b38"), bare)),
    "build"
  )
})

test_that("rbindlist_variants forwards rbindlist options like idcol", {
  out <- rbindlist_variants(
    list(
      mk("chr1", 100L, "A", "G", "b38"),
      mk("chr2", 200L, "C", "T", "b38")
    ),
    idcol = "src"
  )
  expect_equal(out$src, c(1L, 2L))
})

test_that("fill = FALSE on ragged inputs surfaces rbindlist's error", {
  expect_error(
    rbindlist_variants(
      list(
        mk("chr1", 100L, "A", "G", "b38"),
        mk("chr2", 200L, "C", "T", "b38", gene = "X")
      ),
      fill = FALSE
    )
  )
})

test_that("rbindlist_variants on an empty list needs a target", {
  expect_error(rbindlist_variants(list()), "empty list")
})

test_that("rbindlist_variants binds non-native variants", {
  out <- rbindlist_variants(list(
    mk("chr1", 100L, "A", "G", "dahu42"),
    mk("chr2", 200L, "C", "T", "dahu42")
  ))
  expect_equal(build(out), "dahu42")
  expect_equal(out$pos, c(100L, 200L))
})

test_that("rbindlist_variants on an empty list with target is a 0-row variants", {
  out <- rbindlist_variants(list(), target = "b38")
  expect_s3_class(out, "variants")
  expect_equal(nrow(out), 0L)
  expect_equal(build(out), "b38")
})
