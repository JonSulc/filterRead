test_that("is_non_allele_encoded_column identifies non-allele columns", {
  expect_true(is_non_allele_encoded_column(c("chr", "pos")))
  expect_true(is_non_allele_encoded_column(c("build", "chr", "pos")))
  expect_false(is_non_allele_encoded_column(NULL))
  expect_false(is_non_allele_encoded_column(c("ref", "alt")))
})

test_that("encoded_array_refs builds awk references to split-array elements", {
  expect_equal(
    encoded_array_refs(1L, 1:2),
    c("encoded1[1]", "encoded1[2]")
  )
  expect_equal(encoded_array_refs(2L, 3L), "encoded2[3]")
})
