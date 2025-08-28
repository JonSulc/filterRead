# Test Level 2: Component Assemblers

test_that("build_read_only_awk_script with no prefixes or nlines", {
  finterface <- list(
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t"
  )
  result <- build_read_only_awk_script(finterface)
  expected <- "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}\n{\n  print $0\n}'"
  expect_equal(result, expected)
})

test_that("build_read_only_awk_script with comment prefix only", {
  finterface <- list(
    comment_prefix = "^##",
    trim_prefix = NULL,
    sep = "\t"
  )
  result <- build_read_only_awk_script(finterface)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}",
    "\n/^##/ { next }\n{\n  print $0\n}'"
  )
  expect_equal(result, expected)
})

test_that("build_read_only_awk_script with trim prefix only", {
  finterface <- list(
    comment_prefix = NULL,
    trim_prefix = "^#",
    sep = "\t"
  )
  result <- build_read_only_awk_script(finterface)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}",
    "\n{\n  gsub(/^#/, \"\", $0)\n  print $0\n}'"
  )
  expect_equal(result, expected)
})

test_that("build_read_only_awk_script with both prefixes", {
  finterface <- list(
    comment_prefix = "^##",
    trim_prefix = "^#",
    sep = "\t"
  )
  result <- build_read_only_awk_script(finterface)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}",
    "\n/^##/ { next }\n{\n  gsub(/^#/, \"\", $0)",
    "\n  print $0\n}'"
  )
  expect_equal(result, expected)
})

test_that("build_read_only_awk_script with nlines only", {
  finterface <- list(
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t"
  )
  result <- build_read_only_awk_script(finterface, nlines = 100)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n",
    "  output_lines = 0\n  max_lines = 100\n}",
    "\n{\n  if (++output_lines <= max_lines) print $0; else exit\n}'"
  )
  expect_equal(result, expected)
})

test_that("build_read_only_awk_script with prefixes and nlines", {
  finterface <- list(
    comment_prefix = "^##",
    trim_prefix = "^#",
    sep = "\t"
  )
  result <- build_read_only_awk_script(finterface, nlines = 100)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n",
    "  output_lines = 0\n  max_lines = 100\n}",
    "\n/^##/ { next }\n{\n  gsub(/^#/, \"\", $0)",
    "\n  if (++output_lines <= max_lines) print $0; else exit\n}'"
  )
  expect_equal(result, expected)
})

test_that("build_read_only_awk_script handles different separators", {
  finterface <- list(
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = ","
  )
  result <- build_read_only_awk_script(finterface)
  expected <- "awk 'BEGIN{\n  FS = \",\"\n  OFS = \",\"\n}\n{\n  print $0\n}'"
  expect_equal(result, expected)
})

test_that("build_read_only_awk_script with complex patterns", {
  finterface <- list(
    comment_prefix = "^<!--",
    trim_prefix = "^//",
    sep = "|"
  )
  result <- build_read_only_awk_script(finterface)
  expected <- paste0(
    "awk 'BEGIN{\n  FS = \"|\"\n  OFS = \"|\"\n}",
    "\n/^<!--/ { next }\n{\n  gsub(/^\\/\\//, \"\", $0)",
    "\n  print $0\n}'"
  )
  expect_equal(result, expected)
})

test_that("build_pipeline_cmd creates correct pipeline", {
  file_cmd <- "cat test.txt"
  awk_script <- "awk '{print $1}'"
  result <- build_pipeline_cmd(file_cmd, awk_script)
  expect_equal(result, "cat test.txt | awk '{print $1}'")
})

test_that("build_pipeline_cmd with gzipped file", {
  file_cmd <- "zcat test.txt.gz"
  awk_script <- "awk 'BEGIN{OFS=\"\\t\"}{print}'"
  result <- build_pipeline_cmd(file_cmd, awk_script)
  expect_equal(result, "zcat test.txt.gz | awk 'BEGIN{OFS=\"\\t\"}{print}'")
})

test_that("build_pipeline_cmd with complex awk script", {
  file_cmd <- "cat data.txt"
  awk_script <- paste0(
    "awk 'BEGIN{OFS=\"\\t\"}",
    "/^#/ { next }",
    "{gsub(/prefix/, \"\"); print}'"
  )
  result <- build_pipeline_cmd(file_cmd, awk_script)
  expected <- paste0(
    "cat data.txt | awk 'BEGIN{OFS=\"\\t\"}",
    "/^#/ { next }",
    "{gsub(/prefix/, \"\"); print}'"
  )
  expect_equal(result, expected)
})
