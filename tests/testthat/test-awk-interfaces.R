# Test Level 3: High-Level Interfaces

# Test awk_load_file_cmd with various parameter combinations

test_that("awk_load_file_cmd: simple case, only_read=TRUE, no processing", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL
  )
  result <- awk_load_file_cmd(finterface, only_read = TRUE)
  expect_equal(result, "cat test.txt")
})

test_that("awk_load_file_cmd: simple case, only_read=FALSE, no processing", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL
  )
  result <- awk_load_file_cmd(finterface, only_read = FALSE)
  expect_equal(result, "awk")
})

test_that("awk_load_file_cmd: gzipped, only_read=TRUE, no processing", {
  finterface <- list(
    gzipped = TRUE,
    filename = "test.txt.gz",
    comment_prefix = NULL,
    trim_prefix = NULL
  )
  result <- awk_load_file_cmd(finterface, only_read = TRUE)
  expect_equal(result, "zcat test.txt.gz")
})

test_that("awk_load_file_cmd: gzipped, only_read=FALSE, no processing", {
  finterface <- list(
    gzipped = TRUE,
    filename = "test.txt.gz",
    comment_prefix = NULL,
    trim_prefix = NULL
  )
  result <- awk_load_file_cmd(finterface, only_read = FALSE)
  expect_equal(result, "awk")
})

test_that("awk_load_file_cmd: comment prefix, only_read=TRUE", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = NULL,
    sep = "\t"
  )
  result <- awk_load_file_cmd(finterface, only_read = TRUE)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}",
    "\n/^##/ { next }\n{\n  print $0\n}' test.txt"
  )
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd: comment prefix, only_read=FALSE", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = NULL,
    sep = "\t"
  )
  result <- awk_load_file_cmd(finterface, only_read = FALSE)
  expect_equal(result, "awk")
})

test_that("awk_load_file_cmd: trim prefix, only_read=TRUE", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = "^#",
    sep = "\t"
  )
  result <- awk_load_file_cmd(finterface, only_read = TRUE)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}",
    "\n{\n  gsub(/^#/, \"\", $0)\n  print $0\n}' test.txt"
  )
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd: both prefixes, only_read=TRUE", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = "^#",
    sep = "\t"
  )
  result <- awk_load_file_cmd(finterface, only_read = TRUE)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}",
    "\n/^##/ { next }\n{\n  gsub(/^#/, \"\", $0)",
    "\n  print $0\n}' test.txt"
  )
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd: gzipped with both prefixes, only_read=TRUE", {
  finterface <- list(
    gzipped = TRUE,
    filename = "test.txt.gz",
    comment_prefix = "^##",
    trim_prefix = "^#",
    sep = "\t"
  )
  result <- awk_load_file_cmd(finterface, only_read = TRUE)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}",
    "\n/^##/ { next }\n{\n  gsub(/^#/, \"\", $0)",
    "\n  print $0\n}' <(zcat test.txt.gz)"
  )
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd: nlines only, only_read=TRUE", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t"
  )
  result <- awk_load_file_cmd(finterface, nlines = 100, only_read = TRUE)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n",
    "  output_lines = 0\n  max_lines = 100\n}",
    "\n{\n  if (++output_lines <= max_lines) print $0; else exit\n}'",
    " test.txt"
  )
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd: nlines only, only_read=FALSE", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t"
  )
  result <- awk_load_file_cmd(finterface, nlines = 100, only_read = FALSE)
  expect_equal(result, "awk")
})

test_that("awk_load_file_cmd: prefixes with nlines, only_read=TRUE", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = "^#",
    sep = "\t"
  )
  result <- awk_load_file_cmd(finterface, nlines = 50, only_read = TRUE)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n",
    "  output_lines = 0\n  max_lines = 50\n}",
    "\n/^##/ { next }\n{\n  gsub(/^#/, \"\", $0)",
    "\n  if (++output_lines <= max_lines) print $0; else exit\n}'",
    " test.txt"
  )
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd: gzipped with prefixes and nlines, only_read=TRUE", {
  finterface <- list(
    gzipped = TRUE,
    filename = "test.txt.gz",
    comment_prefix = "^//",
    trim_prefix = "^%",
    sep = ","
  )
  result <- awk_load_file_cmd(finterface, nlines = 25, only_read = TRUE)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \",\"\n  OFS = \",\"\n",
    "  output_lines = 0\n  max_lines = 25\n}",
    "\n/^\\/\\// { next }\n{\n  gsub(/^%/, \"\", $0)",
    "\n  if (++output_lines <= max_lines) print $0; else exit\n}'",
    " <(zcat test.txt.gz)"
  )
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd: complex patterns", {
  finterface <- list(
    gzipped = FALSE,
    filename = "complex.txt",
    comment_prefix = "\\*\\*",
    trim_prefix = "\\*",
    sep = "|"
  )
  result <- awk_load_file_cmd(finterface, only_read = TRUE)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"|\"\n  OFS = \"|\"\n}",
    "\n/\\*\\*/ { next }\n{\n  gsub(/\\*/, \"\", $0)",
    "\n  print $0\n}' complex.txt"
  )
  expect_equal(result, expected)
})

test_that("awk_load_file_cmd: special characters in filename", {
  finterface <- list(
    gzipped = TRUE,
    filename = "file with spaces.txt.gz",
    comment_prefix = NULL,
    trim_prefix = NULL
  )
  result <- awk_load_file_cmd(finterface, only_read = TRUE)
  expect_equal(result, "zcat file with spaces.txt.gz")
})

test_that("awk_load_file_cmd: edge case with empty patterns", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = "",
    trim_prefix = "",
    sep = "\t"
  )
  result <- awk_load_file_cmd(finterface, only_read = TRUE)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}",
    "\n// { next }\n{\n  gsub(//, \"\", $0)",
    "\n  print $0\n}' test.txt"
  )
  expect_equal(result, expected)
})
