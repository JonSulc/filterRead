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

test_that("Flipped regions work", {
  gregions <- new_genomic_regions(
    chr = 1,
    start = c(317721, 421000),
    end = c(317742, 421421),
    build = "b37"
  )
  expect_equal(
    liftover(
      gregions,
      "b38"
    ),
    new_genomic_regions(
      chr = 1,
      start = c(397916, 501595),
      end = c(398337, 501616),
      build = "b38"
    )
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

test_that("regions with no mapping are dropped", {
  expect_equal(
    new_genomic_regions(
      chr = "chr1",
      end = 1000,
      build = "b37"
    ) |>
      liftover("b38") |>
      nrow(),
    0
  )
})

test_that("empty genomic_regions returns empty", {
  empty_regions <- new_genomic_regions(build = "b37")
  result <- liftover(empty_regions, "b38")
  expect_equal(nrow(result), 0)
  expect_true(is_genomic_regions(result))
})

test_that("reverse strand liftover coordinates are correct", {
  chain_dt <- get_chain_dt(from = "b37", to = "b38")
  # Find a region with reverse strand mapping
  rev_chain <- chain_dt[rev == TRUE]
  skip_if(nrow(rev_chain) == 0, "No reverse strand regions in chain")

  # Use the first reverse strand region
  test_region <- rev_chain[1]
  input_region <- new_genomic_regions(
    chr = test_region$chr,
    start = test_region$start,
    end = test_region$end,
    build = "b37"
  )
  result <- liftover(input_region, chain_dt)

  # Verify result exists and has expected chromosome
  expect_equal(nrow(result), 1)
  expect_equal(result$chr[1], test_region$new_chr)
})

test_that("multiple regions with partial mapping", {
  # Mix of mappable and unmappable regions
  # chr1:1-1000 has no mapping in the chain file
  unmappable <- new_genomic_regions(
    chr = "chr1",
    start = 1,
    end = 1000,
    build = "b37"
  )
  # APOE region is known to map successfully
  mappable <- new_genomic_regions(
    chr = "chr19",
    start = 45411941,
    end = 45412079,
    build = "b37"
  )

  mixed_regions <- rbind(unmappable, mappable)

  # Only the mappable region should appear in result
  expect_equal(
    mixed_regions |>
      liftover("b38"),
    mappable |>
      liftover("b38")
  )
})

# Tests for build() accessor S3 methods
test_that("build() accessor works for different object types", {
  # Test build.default (data.table with attr)
  dt <- data.table::data.table(x = 1)
  attr(dt, "build") <- "b38"
  expect_equal(build(dt), "b38")

  # Test build.character (pass-through)
  expect_equal(build("b37"), "b37")

  # Test build.list
  lst <- list(build = "b38", data = 1:10)
  expect_equal(build(lst), "b38")

  # Test NULL build
  dt_no_build <- data.table::data.table(x = 1)
  expect_null(build(dt_no_build))

  # Test build.file_interface (uses list method)
  finterface <- local_summary_stats_interface() |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(build(finterface), "b36")

  # Test build on genomic_regions
  gr <- new_genomic_regions(chr = "1", start = 100, end = 200, build = "b38")
  expect_equal(build(gr), "b38")
})

test_that("build<-() setter works for different object types", {
  # Test build<-.default
  df <- data.frame(x = 1)
  build(df) <- "b38"
  expect_equal(attr(df, "build"), "b38")

  # Test build<-.data.table (uses setattr for efficiency)
  dt <- data.table::data.table(x = 1)
  build(dt) <- "b37"
  expect_equal(attr(dt, "build"), "b37")

  # Test build<-.list
  lst <- list(data = 1:10)
  build(lst) <- "b38"
  expect_equal(lst$build, "b38")

  # Test setting NULL build
  dt2 <- data.table::data.table(x = 1)
  build(dt2) <- "b38"
  expect_equal(build(dt2), "b38")
  build(dt2) <- NULL
  expect_null(build(dt2))

  # Test setting build on genomic_regions
  gr <- new_genomic_regions(chr = "1", start = 100, end = 200)
  expect_null(build(gr))
  build(gr) <- "b38"
  expect_equal(build(gr), "b38")
})

test_that("build() and build<-() work correctly together", {
  # Round-trip test
  dt <- data.table::data.table(chr = "1", start = 100, end = 200)
  expect_null(build(dt))

  build(dt) <- "b37"
  expect_equal(build(dt), "b37")

  build(dt) <- "b38"
  expect_equal(build(dt), "b38")

  # Test with genomic_regions through operations
  gr1 <- new_genomic_regions(chr = "1", start = 100, end = 200, build = "b38")
  gr2 <- new_genomic_regions(chr = "1", start = 150, end = 250, build = "b38")

  # Build should be preserved through operations
  result <- gr1 & gr2
  expect_equal(build(result), "b38")

  result2 <- gr1 | gr2
  expect_equal(build(result2), "b38")
})

# Tests for normalize_build() and build synonyms
test_that("normalize_build() converts canonical names correctly", {
  expect_equal(normalize_build("b36"), "b36")
  expect_equal(normalize_build("b37"), "b37")
  expect_equal(normalize_build("b38"), "b38")
})

test_that("normalize_build() converts UCSC/hg names correctly", {
  expect_equal(normalize_build("hg18"), "b36")
  expect_equal(normalize_build("hg19"), "b37")
  expect_equal(normalize_build("hg38"), "b38")
})

test_that("normalize_build() converts GRCh names correctly", {
  expect_equal(normalize_build("GRCh36"), "b36")
  expect_equal(normalize_build("GRCh37"), "b37")
  expect_equal(normalize_build("GRCh38"), "b38")

  expect_equal(normalize_build("grch36"), "b36")
  expect_equal(normalize_build("grch37"), "b37")
  expect_equal(normalize_build("grch38"), "b38")
})

test_that("normalize_build() handles NULL correctly", {
  expect_null(normalize_build(NULL))
  expect_null(normalize_build(NULL, allow_null = TRUE))

  expect_error(
    normalize_build(NULL, allow_null = FALSE),
    "Build cannot be NULL"
  )
})

test_that("normalize_build() errors on invalid build names", {
  expect_error(normalize_build("invalid"), "Unknown build")
  expect_error(normalize_build("hg17"), "Unknown build")
  expect_error(normalize_build("b39"), "Unknown build")
  expect_error(normalize_build(""), "Unknown build")
})

test_that("get_chain_filename() accepts build synonyms", {
  # All these should produce the same filename
  expect_equal(
    get_chain_filename("b37", "b38"),
    get_chain_filename("hg19", "hg38")
  )
  expect_equal(
    get_chain_filename("b37", "b38"),
    get_chain_filename("GRCh37", "GRCh38")
  )
  expect_equal(
    get_chain_filename("b37", "b38"),
    get_chain_filename("grch37", "grch38")
  )

  # Verify actual filename format
  expect_equal(
    get_chain_filename("hg19", "hg38"),
    "hg19ToHg38.over.chain"
  )
})

test_that("get_chain_downloads() accepts build synonyms", {
  downloads_canonical <- get_chain_downloads("b37", "b38")
  downloads_ucsc <- get_chain_downloads("hg19", "hg38")
  downloads_grch <- get_chain_downloads("GRCh37", "GRCh38")

  expect_equal(downloads_canonical, downloads_ucsc)
  expect_equal(downloads_canonical, downloads_grch)

  # Verify structure
  expect_equal(downloads_canonical$uncompressed, "hg19ToHg38.over.chain")
  expect_equal(downloads_canonical$filename, "hg19ToHg38.over.chain.gz")
  expect_true(grepl("hgdownload.soe.ucsc.edu", downloads_canonical$url))
})

test_that("liftover() accepts build synonyms as target", {
  gr_b37 <- new_genomic_regions(
    chr = "chr19",
    start = 45411941,
    end = 45412079,
    build = "b37"
  )

  # These should all produce the same result
  result_b38 <- liftover(gr_b37, "b38")
  result_hg38 <- liftover(gr_b37, "hg38")
  result_grch38 <- liftover(gr_b37, "GRCh38")

  expect_equal(result_b38, result_hg38)
  expect_equal(result_b38, result_grch38)

  # Verify build attribute is always canonical
  expect_equal(build(result_b38), "b38")
  expect_equal(build(result_hg38), "b38")
  expect_equal(build(result_grch38), "b38")
})
