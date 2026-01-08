test_that("liftover matches known coordinates across builds", {
  chain_dt_b36_b37 <- get_chain_dt(
    from = "b36",
    to = "b37"
  ) |>
    suppressWarnings() # rtracklayer loading has a warning
  chain_dt_b37_b38 <- get_chain_dt(
    from = "b37",
    to = "b38"
  )
  chain_dt_b38_b37 <- get_chain_dt(
    from = "b38",
    to = "b37"
  )

  genomic_ranges_b36 <- data.table::data.table(
    chr = "chr19",
    start = 50103781, # rs429358, b36
    end = 50103919 # rs7412, b36
  )
  genomic_ranges_b37 <- data.table::data.table(
    chr = "chr19",
    start = 45411941, # rs429358, b37
    end = 45412079 # rs7412, b37
  )
  genomic_ranges_b38 <- data.table::data.table(
    chr = "chr19",
    start = 44908684, # rs429358, b38
    end = 44908822 # rs7412, b38
  )

  expect_equal(
    liftover_genomic_ranges(
      genomic_ranges_b36,
      chain_dt_b36_b37
    ),
    genomic_ranges_b37
  )
  expect_equal(
    liftover_genomic_ranges(
      genomic_ranges_b37,
      chain_dt_b37_b38
    ),
    genomic_ranges_b38
  )
  expect_equal(
    liftover_genomic_ranges(
      genomic_ranges_b38,
      chain_dt_b38_b37
    ),
    genomic_ranges_b37
  )
})

test_that("full conversion works", {
  genomic_ranges <- data.table::data.table(
    chr = "chr19",
    start = 45411941, # rs429358, b37
    end = 45412079 # rs7412, b37
  )

  expect_equal(
    convert_to_build(
      genomic_ranges,
      from = "b37",
      to = "b38"
    ) |>
      suppressWarnings(), # rtracklayer loading has a warning
    data.table::data.table(
      chr = "chr19",
      start = 44908684,
      end = 44908822
    )
  )
})

test_that("liftover to the same build returns the object", {
  genomic_ranges <- data.table::data.table(
    chr = "chr19",
    start = 45411941, # rs429358, b37
    end = 45412079 # rs7412, b37
  )

  expect_equal(
    convert_to_build(
      genomic_ranges,
      from = "b37",
      to = "b37"
    ),
    genomic_ranges
  )
  expect_equal(
    convert_to_build(
      genomic_ranges,
      from = "b38",
      to = "b38"
    ),
    genomic_ranges
  )
})

test_that("split regions return additional rows", {
  # There is a mismatch between assemblies at 19:7304090+ (b37)
  genomic_ranges_b37 <- data.table::data.table(
    chr = "chr19",
    start = 7304000,
    end = 7304100
  )
  genomic_ranges_b37_split <- data.table::data.table(
    chr = "chr19",
    start = c(7304000, 7304090),
    end = c(7304089, 7304100)
  )
  expected_ranges_b38 <- data.table::data.table(
    chr = c("chr19", "chr7"),
    start = c(7303989, 101687284),
    end = c(7304078, 101687294)
  )
  chain_dt <- get_chain_dt(from = "b37", to = "b38")

  expect_equal(
    liftover_genomic_ranges(
      genomic_ranges_b37_split,
      chain_dt
    ),
    expected_ranges_b38
  )
  expect_equal(
    liftover_genomic_ranges(
      genomic_ranges_b37,
      chain_dt
    ),
    expected_ranges_b38
  )
})
