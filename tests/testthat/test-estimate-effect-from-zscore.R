test_that("hand-computed example matches Vukcevic 2011 formulas", {
  result <- estimate_effect_from_zscore(
    zscore      = 2.5,
    af          = 0.3,
    sample_size = 10000
  )
  expected_se <- 1 / sqrt((10000 + 2.5^2) * 2 * 0.3 * 0.7)
  expect_equal(result$effect_se, expected_se, tolerance = 1e-8)
  expect_equal(result$effect, 2.5 * expected_se, tolerance = 1e-8)
})

test_that("vectorized inputs of equal length produce vector outputs", {
  zscore <- c(2.5, -1.8, 0.5)
  af     <- c(0.3, 0.1, 0.45)
  n      <- c(10000, 8000, 12000)
  result <- estimate_effect_from_zscore(zscore, af, n)
  expected_se <- 1 / sqrt((n + zscore^2) * 2 * af * (1 - af))
  expect_equal(result$effect_se, expected_se, tolerance = 1e-8)
  expect_equal(result$effect, zscore * expected_se, tolerance = 1e-8)
})

test_that("length-1 inputs apply to every row", {
  zscore <- c(2.5, -1.8, 0.5)
  result <- estimate_effect_from_zscore(zscore, af = 0.3, sample_size = 10000)
  expected_se <- 1 / sqrt((10000 + zscore^2) * 2 * 0.3 * 0.7)
  expect_equal(result$effect_se, expected_se, tolerance = 1e-8)
  expect_equal(result$effect, zscore * expected_se, tolerance = 1e-8)
})

test_that("mismatched non-1 lengths error", {
  expect_error(
    estimate_effect_from_zscore(
      zscore      = c(1, 2),
      af          = c(0.1, 0.2, 0.3),
      sample_size = 1000
    ),
    "same length or length 1"
  )
})

test_that("NA in any input propagates to NA outputs for that row", {
  result <- estimate_effect_from_zscore(
    zscore      = c(2.5, NA, 0.5),
    af          = c(0.3, 0.1, NA),
    sample_size = c(10000, 8000, 12000)
  )
  expect_true(is.na(result$effect[2]))
  expect_true(is.na(result$effect_se[2]))
  expect_true(is.na(result$effect[3]))
  expect_true(is.na(result$effect_se[3]))
  expect_false(is.na(result$effect[1]))
})

test_that("zscore == 0 yields effect == 0 with finite effect_se", {
  result <- estimate_effect_from_zscore(0, af = 0.3, sample_size = 10000)
  expect_equal(result$effect, 0)
  expect_true(is.finite(result$effect_se))
})

test_that("returns a data.table", {
  result <- estimate_effect_from_zscore(1.5, 0.2, 5000)
  expect_true(data.table::is.data.table(result))
  expect_equal(names(result), c("effect", "effect_se"))
})

test_that("works inside dt[, := ...] assignment", {
  dt <- data.table::data.table(
    chr         = c("chr1", "chr2"),
    pos         = c(100L, 200L),
    zscore      = c(2.5, -1.8),
    af          = c(0.3, 0.1),
    sample_size = c(10000, 8000)
  )
  dt[
    ,
    c("effect", "effect_se") := estimate_effect_from_zscore(
      zscore, af, sample_size
    )
  ]
  expect_true(all(c("effect", "effect_se") %in% names(dt)))
  expect_equal(
    dt$effect_se,
    1 / sqrt((dt$sample_size + dt$zscore^2) * 2 * dt$af * (1 - dt$af)),
    tolerance = 1e-8
  )
})
