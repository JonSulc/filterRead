test_that("odds_ratio + pval produces effect and effect_se", {
  dt <- data.table::data.table(
    odds_ratio = c(1.5, 2.0, 0.5),
    pval       = c(0.01, 0.001, 0.05)
  )
  expect_message(
    complete_missing_stats(dt),
    "PMID: 11113947"
  )
  expect_true(all(c("effect", "effect_se") %in% names(dt)))
  expect_equal(
    dt$effect,
    log(c(1.5, 2.0, 0.5)) * sqrt(3) / pi,
    tolerance = 1e-6
  )
  expect_equal(
    dt$effect_se,
    -dt$effect / qnorm(c(0.01, 0.001, 0.05) / 2),
    tolerance = 1e-6
  )
})

test_that("effect + pval produces effect_se", {
  dt <- data.table::data.table(
    effect = c(0.2, -0.3, 0.05),
    pval   = c(0.01, 0.001, 0.05)
  )
  complete_missing_stats(dt)
  expect_true("effect_se" %in% names(dt))
  expect_equal(
    dt$effect_se,
    -abs(c(0.2, -0.3, 0.05)) / qnorm(c(0.01, 0.001, 0.05) / 2),
    tolerance = 1e-6
  )
})

test_that("effect + effect_se produces pval", {
  dt <- data.table::data.table(
    effect    = c(0.2, -0.3, 0.05),
    effect_se = c(0.05, 0.1, 0.02)
  )
  complete_missing_stats(dt)
  expect_true("pval" %in% names(dt))
  expect_equal(
    dt$pval,
    2 * pnorm(-abs(c(0.2, -0.3, 0.05) / c(0.05, 0.1, 0.02))),
    tolerance = 1e-6
  )
})

test_that("All three columns present is a no-op", {
  dt <- data.table::data.table(
    effect    = c(0.2, -0.3),
    effect_se = c(0.05, 0.1),
    pval      = c(0.01, 0.05)
  )
  before <- data.table::copy(dt)
  expect_silent(complete_missing_stats(dt))
  expect_equal(dt, before)
})

test_that("Empty data.table is unchanged", {
  dt <- data.table::data.table(
    effect    = numeric(0),
    effect_se = numeric(0)
  )
  before <- data.table::copy(dt)
  expect_silent(complete_missing_stats(dt))
  expect_equal(dt, before)
})

test_that("Nothing deriveable is silent and a no-op", {
  dt <- data.table::data.table(
    chr = c("chr1", "chr2"),
    pos = c(100L, 200L)
  )
  before <- data.table::copy(dt)
  expect_silent(complete_missing_stats(dt))
  expect_equal(dt, before)
})

test_that("[.file_interface derives effect_se from effect + pval", {
  dt <- data.table::data.table(
    chr    = paste0("chr", 1:4),
    pos    = c(100L, 200L, 300L, 400L),
    ref    = c("A", "G", "C", "T"),
    alt    = c("G", "A", "T", "C"),
    effect = c(0.1, 0.2, 0.3, 0.4),
    pval   = c(0.01, 0.02, 0.03, 0.04)
  )
  local_csv_file("data.csv", dt = dt)
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  result <- finterface[]
  expect_true("effect_se" %in% names(result))
  expect_equal(
    result$effect_se,
    -abs(dt$effect) / qnorm(dt$pval / 2),
    tolerance = 1e-6
  )
})

test_that("head.file_interface also derives missing stats", {
  dt <- data.table::data.table(
    chr    = paste0("chr", 1:4),
    pos    = c(100L, 200L, 300L, 400L),
    ref    = c("A", "G", "C", "T"),
    alt    = c("G", "A", "T", "C"),
    effect = c(0.1, 0.2, 0.3, 0.4),
    pval   = c(0.01, 0.02, 0.03, 0.04)
  )
  local_csv_file("data.csv", dt = dt)
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  result <- head(finterface, 4)
  expect_true("effect_se" %in% names(result))
})
