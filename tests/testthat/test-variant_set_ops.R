test_that("a subset dropping a coordinate column drops the class and build", {
  subsetted <- make_dummy_variants()[, .(chr, pos, ref)]

  expect_false(inherits(subsetted, "variant_set"))
  expect_null(build(subsetted))
})

test_that("a subset retyping a coordinate column drops the class and build", {
  subsetted <- make_dummy_variants()[, .(chr, pos = pos + 1, ref, alt)]

  expect_false(inherits(subsetted, "variant_set"))
  expect_null(build(subsetted))
})

test_that("a subset keeps the class where a coordinate column is all NA", {
  variants <- make_dummy_variants()
  variants[, c("ref", "alt") := NA_character_][]

  expect_s3_class(variants[1], "variant_set")
  expect_equal(build(variants[1]), "b38")
})

test_that("a subset reordering the coordinate columns stays a variant_set", {
  subsetted <- make_dummy_variants()[, .(alt, ref, pos, chr)]

  expect_s3_class(subsetted, "variant_set")
  expect_equal(build(subsetted), "b38")
})

test_that("a j returning an atomic vector returns the vector untouched", {
  expect_identical(make_dummy_variants()[, pos], c(100L, 200L))
})

test_that("a by whose j keeps the coordinate columns stays a variant_set", {
  grouped <- make_dummy_variants()[
    , .(pos = pos[1], ref = ref[1], alt = alt[1]), by = chr
  ]

  expect_s3_class(grouped, "variant_set")
  expect_equal(build(grouped), "b38")
})

test_that("a subset of a set that carries no build gains none", {
  variants <- make_dummy_variants() |> keep_coordinates_only() |> drop_build()

  expect_s3_class(variants[1], "variant_set")
  expect_null(build(variants[1]))
})

test_that("cbind's result takes a := without a shallow copy", {
  bound <- cbind(make_dummy_variants(), data.table::data.table(extra = c(1, 2)))

  expect_no_warning(bound[, another := 3])
})

test_that("class and build travel together across data.table operations", {
  variants <- make_dummy_variants()
  plain <- data.table::data.table(extra = c(1, 2))
  class_and_build <- function(result) {
    c(class(result)[1], build(result) %||% NA_character_)
  }

  expect_equal(class_and_build(variants[1]), c("variant_set", "b38"))
  expect_equal(
    class_and_build(variants[1, .(chr, pos, ref, alt)]),
    c("variant_set", "b38")
  )
  expect_equal(
    class_and_build(variants[, c(.SD, .(new = 1))]), c("variant_set", "b38")
  )
  expect_equal(class_and_build(variants[, .N, by = chr]), c("data.table", NA))
  expect_equal(class_and_build(cbind(variants, plain)), c("variant_set", "b38"))
  expect_equal(class_and_build(cbind(variants, variants)), c("data.table", NA))
  expect_equal(
    class_and_build(data.table::rbindlist(list(variants, variants))),
    c("data.table", NA)
  )
  expect_equal(
    class_and_build(data.table::copy(variants)), c("variant_set", "b38")
  )
  expect_equal(class_and_build(na.omit(variants)), c("variant_set", "b38"))
  expect_equal(class_and_build(unique(variants)), c("variant_set", "b38"))
  expect_equal(
    class_and_build(variants[data.table::data.table(chr = "chr1"), on = "chr"]),
    c("variant_set", "b38")
  )

  renamed <- data.table::copy(variants)
  data.table::setnames(renamed, "ref", "reference")
  expect_equal(class_and_build(renamed), c("variant_set", "b38"))
  expect_equal(class_and_build(renamed[1]), c("data.table", NA))

  expect_equal(class_and_build(variants[, tmp := 1]), c("variant_set", "b38"))
})

test_that("a coordinate-only subset lifts as the set it came from does", {
  testthat::local_mocked_bindings(
    get_shared_chain = function(from, to)
      make_fwd_chain(1L, 1000L, 50L, from, to)
  )
  variants <- make_dummy_variants()
  subsetted <- variants[, .(chr, pos, ref, alt)]

  expect_equal(liftover(subsetted, "b37")$pos, c(50L, 150L))
  expect_equal(liftover(subsetted, "b37")$pos, liftover(variants, "b37")$pos)
})
