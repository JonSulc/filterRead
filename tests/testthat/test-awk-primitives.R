# Test Level 1: Primitive Builders (No Dependencies)

test_that("build_file_read_cmd works with non-gzipped files", {
  result <- build_file_read_cmd(list(filename = "test.txt", gzipped = FALSE))
  expect_equal(result, "cat test.txt")
})

test_that("build_file_read_cmd works with gzipped files", {
  result <- build_file_read_cmd(list(filename = "test.txt.gz", gzipped = TRUE))
  expect_equal(result, "zcat test.txt.gz")
})

test_that("build_comment_filter_pattern returns NULL for NULL input", {
  result <- build_comment_filter_pattern(NULL)
  expect_null(result)
})

test_that("build_comment_filter_pattern creates correct pattern", {
  result <- build_comment_filter_pattern("^##")
  expect_equal(result, "/^##/ { next }")
})

test_that("build_comment_filter_pattern handles special characters", {
  result <- build_comment_filter_pattern("\\*\\*")
  expect_equal(result, "/\\*\\*/ { next }")
})

test_that("build_comment_filter_pattern escapes forward slashes", {
  result <- build_comment_filter_pattern("^//")
  expect_equal(result, "/^\\/\\// { next }")
})

test_that("build_comment_filter_pattern escapes brackets", {
  result <- build_comment_filter_pattern("^[test]")
  expect_equal(result, "/^\\[test\\]/ { next }")
})

test_that("build_comment_filter_pattern handles complex patterns", {
  result <- build_comment_filter_pattern("^<!--")
  expect_equal(result, "/^<!--/ { next }")
})

test_that("build_trim_prefix_code returns NULL for NULL input", {
  result <- build_trim_prefix_code(NULL)
  expect_null(result)
})

test_that("build_trim_prefix_code creates correct gsub command", {
  result <- build_trim_prefix_code("^#")
  expect_equal(result, 'gsub(/^#/, "", $0)')
})

test_that("build_trim_prefix_code handles special regex characters", {
  result <- build_trim_prefix_code("\\*")
  expect_equal(result, 'gsub(/\\*/, "", $0)')
})

test_that("build_trim_prefix_code escapes forward slashes", {
  result <- build_trim_prefix_code("^//")
  expect_equal(result, 'gsub(/^\\/\\//, "", $0)')
})

test_that("build_trim_prefix_code escapes brackets", {
  result <- build_trim_prefix_code("^[prefix]")
  expect_equal(result, 'gsub(/^\\[prefix\\]/, "", $0)')
})

test_that("build_trim_prefix_code handles complex patterns", {
  result <- build_trim_prefix_code("^\\/\\/")
  expect_equal(result, 'gsub(/^\\\\/\\\\//, "", $0)')
})

test_that("build_line_limit_code returns NULL for NULL input", {
  result <- build_line_limit_code(NULL)
  expect_null(result)
})

test_that("build_line_limit_code creates correct line counting code", {
  result <- build_line_limit_code(100)
  expected <- "if (++output_lines <= max_lines) print $0; else exit"
  expect_equal(result, expected)
})

test_that("build_awk_begin_block works without nlines", {
  result <- build_awk_begin_block("\t")
  expect_equal(result, "BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}")
})

test_that("build_awk_begin_block returns NULL when no sep or nlines", {
  result <- build_awk_begin_block(NULL)
  expect_null(result)
})

test_that("build_awk_begin_block works with nlines only (no sep)", {
  result <- build_awk_begin_block(NULL, 50)
  expected <- "BEGIN{\n  output_lines = 0\n  max_lines = 50\n}"
  expect_equal(result, expected)
})

test_that("build_awk_begin_block with command line FS (only OFS in BEGIN)", {
  result <- build_awk_begin_block("\t", NULL, use_command_line_fs = TRUE)
  expect_equal(result, "BEGIN{\n  OFS = \"\t\"\n}")
})

test_that("build_awk_begin_block with command line FS and nlines", {
  result <- build_awk_begin_block(",", 100, use_command_line_fs = TRUE)
  expected <- paste0(
    "BEGIN{\n  OFS = \",\"\n",
    "  output_lines = 0\n  max_lines = 100\n}"
  )
  expect_equal(result, expected)
})

test_that("build_awk_begin_block works with nlines", {
  result <- build_awk_begin_block("\t", 100)
  expected <- paste0(
    "BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n",
    "  output_lines = 0\n  max_lines = 100\n}"
  )
  expect_equal(result, expected)
})

test_that("build_awk_begin_block handles different separators", {
  result <- build_awk_begin_block(",")
  expect_equal(result, "BEGIN{\n  FS = \",\"\n  OFS = \",\"\n}")
})

test_that("build_awk_begin_block handles separator with special chars", {
  result <- build_awk_begin_block("|")
  expect_equal(result, "BEGIN{\n  FS = \"|\"\n  OFS = \"|\"\n}")
})

# Tests for escape_awk_regex helper function
test_that("escape_awk_regex handles NULL input", {
  result <- escape_awk_regex(NULL)
  expect_null(result)
})

test_that("escape_awk_regex escapes forward slashes", {
  result <- escape_awk_regex("^//")
  expect_equal(result, "^\\/\\/")
})

test_that("escape_awk_regex escapes backslashes", {
  result <- escape_awk_regex("^\\")
  expect_equal(result, "^\\")
})

test_that("escape_awk_regex escapes square brackets", {
  result <- escape_awk_regex("^[test]")
  expect_equal(result, "^\\[test\\]")
})

test_that("escape_awk_regex escapes parentheses", {
  result <- escape_awk_regex("^(test)")
  expect_equal(result, "^\\(test\\)")
})

test_that("escape_awk_regex escapes curly braces", {
  result <- escape_awk_regex("^{3}")
  expect_equal(result, "^\\{3\\}")
})

test_that("escape_awk_regex escapes quantifiers", {
  result <- escape_awk_regex("^test+?")
  expect_equal(result, "^test\\+\\?")
})

test_that("escape_awk_regex escapes pipe character", {
  result <- escape_awk_regex("^test|alt")
  expect_equal(result, "^test\\|alt")
})

test_that("escape_awk_regex handles multiple special characters", {
  result <- escape_awk_regex("^[/\\]+?{test}")
  expect_equal(result, "^\\[\\/\\\\]\\+\\?\\{test\\}")
})

test_that("escape_awk_regex preserves anchors and wildcards", {
  result <- escape_awk_regex("^test.*$")
  expect_equal(result, "^test.*$")
})
