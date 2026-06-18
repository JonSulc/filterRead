test_that("reverse_complement complements single bases", {
  expect_equal(reverse_complement(c("A", "C", "G", "T")), c("T", "G", "C", "A"))
})

test_that("reverse_complement reverses and complements multi-base alleles", {
  # indel-style alleles: reverse the order, complement each base
  expect_equal(reverse_complement("ATG"), "CAT")
  expect_equal(reverse_complement("AC"), "GT")
})

test_that("reverse_complement is identity on palindromic alleles as a string", {
  # RC("AT") = reverse(complement(A,T)) = reverse(T,A) = "AT"
  expect_equal(reverse_complement("AT"), "AT")
})

test_that("reverse_complement upper-cases and preserves NA and unknown bases", {
  expect_equal(reverse_complement("a"), "T")
  expect_equal(reverse_complement(NA_character_), NA_character_)
  expect_equal(reverse_complement("N"), "N")
  expect_equal(reverse_complement("-"), "-")
})
