# Test Level 2: Component Assemblers

test_that("build_read_only_awk_script with no prefixes or nlines", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t",
    gzipped = FALSE
  )
  result <- build_read_only_awk_script(finterface)
  expected <- paste(
    "awk",
    "'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}\n{\n  print $0\n}'",
    "test.txt"
  )
  expect_equal(result, expected)
})

test_that("build_read_only_awk_script with comment prefix only", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = NULL,
    sep = "\t",
    gzipped = FALSE
  )
  result <- build_read_only_awk_script(finterface)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}",
    "\n/^##/ { next }\n{\n  print $0\n}' test.txt"
  )
  expect_equal(result, expected)
})

test_that("build_read_only_awk_script with trim prefix only", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = "^#",
    sep = "\t",
    gzipped = FALSE
  )
  result <- build_read_only_awk_script(finterface)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}",
    "\n{\n  gsub(/^#/, \"\", $0)\n  print $0\n}' test.txt"
  )
  expect_equal(result, expected)
})

test_that("build_read_only_awk_script with both prefixes", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = "^#",
    sep = "\t",
    gzipped = FALSE
  )
  result <- build_read_only_awk_script(finterface)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}",
    "\n/^##/ { next }\n{\n  gsub(/^#/, \"\", $0)",
    "\n  print $0\n}' test.txt"
  )
  expect_equal(result, expected)
})

test_that("build_read_only_awk_script with nlines only", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t",
    gzipped = FALSE
  )
  result <- build_read_only_awk_script(finterface, nlines = 100)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n",
    "  output_lines = 0\n  max_lines = 100\n}",
    "\n{\n  if (++output_lines <= max_lines) print $0; else exit\n}'",
    " test.txt"
  )
  expect_equal(result, expected)
})

test_that("build_read_only_awk_script with prefixes and nlines", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = "^#",
    sep = "\t",
    gzipped = FALSE
  )
  result <- build_read_only_awk_script(finterface, nlines = 100)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n",
    "  output_lines = 0\n  max_lines = 100\n}",
    "\n/^##/ { next }\n{\n  gsub(/^#/, \"\", $0)",
    "\n  if (++output_lines <= max_lines) print $0; else exit\n}'",
    " test.txt"
  )
  expect_equal(result, expected)
})

test_that("build_read_only_awk_script handles different separators", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = ",",
    gzipped = FALSE
  )
  result <- build_read_only_awk_script(finterface)
  expected <- paste(
    "awk",
    "'BEGIN{\n  FS = \",\"\n  OFS = \",\"\n}\n{\n  print $0\n}'",
    "test.txt"
  )
  expect_equal(result, expected)
})

test_that("build_read_only_awk_script with complex patterns", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = "^<!--",
    trim_prefix = "^//",
    sep = "|",
    gzipped = FALSE
  )
  result <- build_read_only_awk_script(finterface)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"|\"\n  OFS = \"|\"\n}",
    "\n/^<!--/ { next }\n{\n  gsub(/^\\/\\//, \"\", $0)",
    "\n  print $0\n}' test.txt"
  )
  expect_equal(result, expected)
})

