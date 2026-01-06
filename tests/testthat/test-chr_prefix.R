test_that("add_chr_prefix adds prefix when chr column has no prefix", {
  column_info <- data.table::data.table(
    standard_name = c("chr", "pos", "ref", "alt"),
    prefix = c(
      NA_character_, NA_character_, NA_character_,
      NA_character_
    )
  )
  data_to_check <- data.table::data.table(
    chr = c("1", "2", "3"),
    pos = c(100, 200, 300),
    ref = c("A", "C", "G"),
    alt = c("T", "G", "A")
  )

  add_chr_prefix(column_info, data_to_check)

  expect_equal(column_info[standard_name == "chr", add_prefix], "chr")
  expect_true(all(is.na(column_info[standard_name != "chr", add_prefix])))
})

test_that(
  "add_chr_prefix does not add prefix when chr column already has one",
  {
    column_info <- data.table::data.table(
      standard_name = c("chr", "pos", "ref", "alt"),
      prefix = c("chr", NA_character_, NA_character_, NA_character_)
    )
    data_to_check <- data.table::data.table(
      chr = c("chr1", "chr2", "chr3"),
      pos = c(100, 200, 300),
      ref = c("A", "C", "G"),
      alt = c("T", "G", "A")
    )

    add_chr_prefix(column_info, data_to_check)

    expect_true(is.na(column_info[standard_name == "chr", add_prefix]))
  }
)

test_that("add_chr_prefix does nothing when chr column is absent", {
  column_info <- data.table::data.table(
    standard_name = c("pos", "ref", "alt"),
    prefix = c(NA_character_, NA_character_, NA_character_)
  )
  data_to_check <- data.table::data.table(
    pos = c(100, 200, 300),
    ref = c("A", "C", "G"),
    alt = c("T", "G", "A")
  )

  original_info <- data.table::copy(column_info)
  add_chr_prefix(column_info, data_to_check)

  expect_equal(
    column_info,
    original_info[
      ,
      c(
        .SD,
        .(add_prefix = NA_character_)
      )
    ]
  )
})

test_that("awk script correctly generates chr prefix assignment", {
  finterface <- list(
    filename = "test.txt",
    gzipped = FALSE,
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t",
    column_info = data.table::data.table(
      name = c("chr", "pos"),
      bash_index = c("$1", "$2"),
      add_prefix = c("chr", NA_character_),
      encoded_names = list(NULL, NULL)
    )
  )

  column_arrays <- get_awk_column_arrays(finterface)

  expect_true(!is.null(column_arrays$after_if))
  expect_true(any(grepl('\\$1 = "chr"\\$1', column_arrays$after_if)))
})

test_that(
  "chr prefix is correctly added in full file_interface workflow",
  {
    local_summary_stats(prefix = NULL, random_names = FALSE)
    finterface <- new_file_interface("data.csv") |>
      suppressMessages() |>
      withr::with_output_sink(new = "/dev/null")

    # Check that add_prefix is set in column_info
    expect_equal(
      finterface$column_info[name == "chr", add_prefix],
      "chr"
    )

    # Read data and verify chr prefix is added
    result <- head(finterface, nlines = 5)
    expect_true(all(grepl("^chr", result$chr)))
  }
)

test_that("chr prefix not added when already present in file", {
  local_summary_stats(
    prefix = list(chr = "chr"),
    random_names = FALSE
  )
  finterface <- new_file_interface("data.csv") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  # Check that add_prefix is NOT set when prefix already exists
  expect_true(is.na(finterface$column_info[name == "chr", add_prefix]))

  # Read data and verify chr prefix is preserved (not doubled)
  result <- head(finterface, nlines = 5)
  expect_true(all(grepl("^chr[0-9XYM]", result$chr)))
  expect_false(any(grepl("^chrchr", result$chr)))
})

test_that("chr prefix works with filtering conditions", {
  local_summary_stats(prefix = NULL, random_names = FALSE)
  finterface <- new_file_interface("data.csv") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  # Apply a filter and check that chr prefix is still added
  result <- finterface[pval < 0.05]

  expect_true(all(grepl("^chr", result$chr)))
  expect_true(all(result$pval < 0.05))
})
