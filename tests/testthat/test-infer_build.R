test_that("build_can_be_inferred returns TRUE with required columns", {
  dt <- data.table::data.table(
    chr = "chr1",
    pos = 12345,
    ref = "A",
    alt = "C"
  )
  expect_true(build_can_be_inferred(dt))
})

test_that("build_can_be_inferred returns FALSE with missing columns", {
  # Missing chr
  dt1 <- data.table::data.table(
    pos = 12345,
    ref = "A",
    alt = "C"
  )
  expect_false(build_can_be_inferred(dt1))

  # Missing pos
  dt2 <- data.table::data.table(
    chr = "chr1",
    ref = "A",
    alt = "C"
  )
  expect_false(build_can_be_inferred(dt2))

  # Missing ref
  dt3 <- data.table::data.table(
    chr = "chr1",
    pos = 12345,
    alt = "C"
  )
  expect_false(build_can_be_inferred(dt3))

  # Missing alt
  dt4 <- data.table::data.table(
    chr = "chr1",
    pos = 12345,
    ref = "A"
  )
  expect_false(build_can_be_inferred(dt4))
})

test_that("get_tabix_query generates correct query format", {
  summary_stats <- data.table::data.table(
    chr = c("chr1", "chr2"),
    pos = c(12345, 67890),
    ref = c("A", "C"),
    alt = c("G", "T")
  )

  expect_equal(
    get_tabix_query(summary_stats, "/path/to/ref.vcf.gz"),
    "tabix /path/to/ref.vcf.gz 1:12345-12345 2:67890-67890"
  )
})

test_that("get_tabix_query strips chr prefix correctly", {
  summary_stats <- data.table::data.table(
    chr = c("chr1", "chr22", "chrX"),
    pos = c(100, 200, 300),
    ref = c("A", "C", "G"),
    alt = c("T", "G", "A")
  )

  query <- get_tabix_query(summary_stats, "/path/to/ref.vcf.gz")

  expect_match(query, "1:100-100")
  expect_match(query, "22:200-200")
  expect_match(query, "X:300-300")
  expect_no_match(query, "chr")
})

test_that("infer_build returns NA for data without required columns", {
  dt <- data.table::data.table(
    rsid = "rs123",
    pval = 0.05
  )

  expect_equal(infer_build(dt), NA_character_)
})

test_that("infer_build with return_build_match returns data.table", {
  skip_if_not(
    dir.exists(get_dbsnp_path(warn = FALSE)),
    "dbSNP path not configured"
  )

  dt <- dummy_summary_stats(
    nrows = 100,
    chr = 1,
    start = 1000,
    end = 10000,
    random_names = FALSE
  )

  result <- infer_build(dt, return_build_match = TRUE) |>
    suppressMessages()

  expect_equal(
    result,
    data.table::data.table(
      build = paste0("b", 36:38),
      build_match = c(1, 0, 0)
    )
  )
})

test_that("infer_build without return_build_match returns character", {
  skip_if_not(
    dir.exists(get_dbsnp_path(warn = FALSE)),
    "dbSNP path not configured"
  )

  dt <- dummy_summary_stats(
    nrows = 100,
    chr = 1,
    start = 1000,
    end = 10000,
    random_names = FALSE
  )

  expect_equal(
    infer_build(dt, return_build_match = FALSE) |>
      suppressMessages(),
    "b36"
  )
})

test_that("infer_build prints informative message", {
  skip_if_not(
    dir.exists(get_dbsnp_path(warn = FALSE)),
    "dbSNP path not configured"
  )

  dt <- dummy_summary_stats(
    nrows = 50,
    chr = 1,
    start = 1000,
    end = 10000,
    random_names = FALSE
  )

  expect_message(
    infer_build(dt),
    "Build inferred to be (b36|b37|b38)"
  )
})

test_that(
  "get_build_from_file_interface returns existing build if present",
  {
    finterface <- list(
      build = "b37",
      filename = "test.csv"
    )
    class(finterface) <- c("file_interface", "list")

    expect_equal(
      get_build_from_file_interface(finterface),
      "b37"
    )
  }
)

test_that(
  "get_build_from_file_interface warns for RSID matching files",
  {
    finterface <- local_rsid_summary_stats_interface() |>
      suppressWarnings() |>
      suppressMessages() |>
      withr::with_output_sink(new = "/dev/null")

    # Non-existent RSID build is set to b38 by default
    expect_equal(
      finterface$build,
      "b38"
    )

    # Reset build to force inference
    finterface$build <- NULL

    expect_warning(
      result <- get_build_from_file_interface(finterface),
      "Build inferrence for files that require RSID matching"
    )
    expect_equal(result, NA_character_)
  }
)

test_that(
  "get_build_from_file_interface infers build for standard files",
  {
    skip_if_not(
      dir.exists(get_dbsnp_path(warn = FALSE)),
      "dbSNP path not configured"
    )

    finterface <- local_summary_stats_interface(
      nrows = 50,
      chr = 1,
      start = 1000,
      end = 10000,
      random_names = FALSE
    ) |>
      suppressWarnings() |>
      suppressMessages() |>
      withr::with_output_sink(new = "/dev/null")

    result <- get_build_from_file_interface(finterface, nsnps = 50) |>
      suppressMessages()

    expect_type(result, "character")
    expect_true(result %in% c("b36", "b37", "b38"))
  }
)

test_that("get_tabix_matches handles empty results", {
  skip_if_not(
    dir.exists(get_dbsnp_path(warn = FALSE)),
    "dbSNP path not configured"
  )

  # Create summary stats with positions that shouldn't match anything
  dt <- data.table::data.table(
    chr = "chr999",
    pos = 999999999,
    ref = "A",
    alt = "C"
  )

  ref_filename <- file.path(
    get_dbsnp_path(warn = FALSE),
    get_dbsnp_filename("b37", "common")
  )

  skip_if_not(file.exists(ref_filename), "dbSNP reference file not found")

  result <- get_tabix_matches(dt, ref_filename) |>
    suppressWarnings()

  expect_equal(result, 0)
})

test_that("get_tabix_matches counts matches correctly", {
  skip_if_not(
    dir.exists(get_dbsnp_path(warn = FALSE)),
    "dbSNP path not configured"
  )

  ref_filename <- file.path(
    get_dbsnp_path(warn = FALSE),
    get_dbsnp_filename("b37", "common")
  )

  skip_if_not(file.exists(ref_filename), "dbSNP reference file not found")

  dt <- dummy_summary_stats(
    nrows = 50,
    chr = 1,
    start = 10000,
    end = 20000,
    random_names = FALSE
  )

  result <- get_tabix_matches(dt, ref_filename) |>
    suppressWarnings()

  expect_type(result, "double")
  expect_true(result >= 0)
  expect_true(result <= nrow(dt))
})

test_that("infer_build correctly matches all reference variants", {
  skip_if_not(
    dir.exists(get_dbsnp_path(warn = FALSE)),
    "dbSNP path not configured"
  )

  ref_filenames <- file.path(
    get_dbsnp_path(warn = FALSE),
    get_dbsnp_filename(c("b37", "b38"), "common")
  ) |>
    setNames(c("b37", "b38"))
  ref_filenames <- ref_filenames[
    file.exists(ref_filenames)
  ]

  skip_if_not(length(ref_filenames) != 0, "dbSNP reference file not found")

  for (build in names(ref_filenames)) {
    summary_stats <- new_file_interface(
      ref_filenames[build],
      build = "none"
    ) |>
      head(1000)
    expect_message(
      inferred_build <- infer_build(
        summary_stats
      ),
      sprintf("Build inferred to be %s with a 100.0%% match rate", build)
    )
    expect_equal(
      inferred_build,
      build
    )
  }
})
