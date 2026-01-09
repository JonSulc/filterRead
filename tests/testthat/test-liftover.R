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

  genomic_regions_b36 <- new_genomic_regions(
    chr = "chr19",
    start = 50103781, # rs429358, b36
    end = 50103919, # rs7412, b36
    build = "b36"
  )
  genomic_regions_b37 <- new_genomic_regions(
    chr = "chr19",
    start = 45411941, # rs429358, b37
    end = 45412079, # rs7412, b37
    build = "b37"
  )
  genomic_regions_b38 <- new_genomic_regions(
    chr = "chr19",
    start = 44908684, # rs429358, b38
    end = 44908822, # rs7412, b38
    build = "b38"
  )

  expect_equal(
    liftover(
      genomic_regions_b36,
      chain_dt_b36_b37
    ),
    genomic_regions_b37
  )
  expect_equal(
    liftover(
      genomic_regions_b37,
      chain_dt_b37_b38
    ),
    genomic_regions_b38
  )
  expect_equal(
    liftover(
      genomic_regions_b38,
      chain_dt_b38_b37
    ),
    genomic_regions_b37
  )
})

test_that("liftover to the same build returns the object", {
  genomic_regions <- new_genomic_regions(
    chr = "chr19",
    start = 45411941, # rs429358, b37
    end = 45412079, # rs7412, b37
    build = "b37"
  )

  expect_equal(
    liftover(
      genomic_regions,
      target = "b37"
    ),
    genomic_regions
  )
})

test_that("liftover produces a warning if no build is configured", {
  genomic_regions <- new_genomic_regions(
    chr = "chr19",
    start = 45411941,
    end = 45412079
  )

  expect_warning(
    liftover(
      genomic_regions,
      target = "b37"
    ),
    "No build"
  )
  expect_equal(
    liftover(
      genomic_regions,
      target = "b37"
    ) |>
      suppressWarnings(),
    genomic_regions
  )
})

test_that("split regions return additional rows", {
  # There is a mismatch between assemblies at 19:7304090+ (b37)
  genomic_regions_b37 <- new_genomic_regions(
    chr = "chr19",
    start = 7304000,
    end = 7304100,
    build = "b37"
  )
  genomic_regions_b37_split <- new_genomic_regions(
    chr = "chr19",
    start = c(7304000, 7304090),
    end = c(7304089, 7304100),
    build = "b37"
  )
  expected_regions_b38 <- new_genomic_regions(
    chr = c("chr19", "chr7"),
    start = c(7303989, 101687284),
    end = c(7304078, 101687294),
    build = "b38"
  )
  chain_dt <- get_chain_dt(from = "b37", to = "b38")

  expect_equal(
    liftover(
      genomic_regions_b37_split,
      chain_dt
    ),
    expected_regions_b38
  )
  expect_equal(
    liftover(
      genomic_regions_b37,
      chain_dt
    ),
    expected_regions_b38
  )
})

test_that("full chromosome liftover works", {
  gregions <- new_genomic_regions(
    chr = "chr1",
    build = "b37"
  )
  chain_dt <- get_chain_dt(
    from = "b37",
    to = "b38"
  )
  expected <- chain_dt[
    "chr1",
    .(chr, start, end)
  ] |>
    as_genomic_regions() |>
    liftover(chain_dt)
  expect_equal(
    liftover(
      gregions,
      target = "b38"
    ),
    expected
  )
})

test_that("chromosome-free liftover works", {
  gregions <- new_genomic_regions(
    end = 1e7,
    build = "b37"
  )
  chain_dt <- get_chain_dt(
    from = "b37",
    to = "b38"
  )
  expected <- new_genomic_regions(
    chr = unique(chain_dt$chr),
    start = 1,
    end = 1e7,
    build = "b37"
  ) |>
    liftover("b38")
  expect_equal(
    gregions |>
      liftover("b38"),
    expected
  )
})
