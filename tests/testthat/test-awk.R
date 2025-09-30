# Integration Tests for AWK Command Compilation

test_that("compile_awk_cmds: simple file interface with only_read", {
  finterface <- list(
    filename = "test.txt",
    gzipped = FALSE,
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t"
  )
  result <- compile_awk_cmds(finterface, nlines = 5)
  expected <- paste0(
    "cat test.txt | awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n",
    "  output_lines = 0\n  max_lines = 5\n}",
    "\n{\n  if (++output_lines <= max_lines) print $0; else exit\n}'"
  )
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd: gzipped file head", {
  finterface <- list(
    filename = "test.txt.gz",
    gzipped = TRUE
  )
  result <- awk_load_file_cmd(finterface, nlines = 1, only_read = FALSE)
  expected <- paste0(
    "zcat test.txt.gz | awk"
  )
  expect_equal(result, expected)
})

test_that("compile_awk_cmds: complex case with nlines and prefixes", {
  finterface <- list(
    filename = "data.csv",
    gzipped = FALSE,
    comment_prefix = "^//",
    trim_prefix = "^%",
    sep = ","
  )
  result <- compile_awk_cmds(finterface, nlines = 100)
  expected <- paste0(
    "cat data.csv | awk 'BEGIN{\n  FS = \",\"\n  OFS = \",\"\n",
    "  output_lines = 0\n  max_lines = 100\n}",
    "\n/^\\/\\// { next }\n{\n  gsub(/^%/, \"\", $0)",
    "\n  if (++output_lines <= max_lines) print $0; else exit\n}'"
  )
  expect_equal(result, expected)
})

# Keep existing internal function tests for completeness
test_that("wrap_condition_block: basic functionality", {
  expect_equal(
    wrap_condition_block(
      condition = NULL,
      column_arrays_after_conditions = NULL,
      print_prefix = NULL
    ),
    "print $0"
  )
  expect_equal(
    wrap_condition_block(
      condition = "$1 == 1",
      column_arrays_after_conditions = NULL,
      print_prefix = NULL
    ),
    paste0(
      "if ($1 == 1) {\n",
      "  print $0\n",
      "}"
    )
  )
})

test_that("wrap_main_file_code: basic functionality", {
  expect_equal(
    wrap_main_file_code(
      finterface = list(comment_prefix = NULL, trim_prefix = NULL),
      fcondition_awk_dt = data.table::data.table(index = integer()),
      column_arrays_before_conditions = NULL,
      column_arrays_after_conditions = NULL
    ),
    "print $0"
  )

  # Test with column arrays before conditions
  expect_equal(
    wrap_main_file_code(
      finterface = list(comment_prefix = NULL, trim_prefix = NULL),
      fcondition_awk_dt = data.table::data.table(index = integer()),
      column_arrays_before_conditions = "split($1, encoded1, \":\")",
      column_arrays_after_conditions = NULL
    ),
    "split($1, encoded1, \":\")\n  print $0"
  )
})
