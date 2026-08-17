# Test Level 1: Primitive Builders (No Dependencies)

test_that("build_file_read_cmd works with non-gzipped files", {
  result <- build_file_read_cmd(list(filename = "test.txt", gzipped = FALSE))
  expect_equal(result, "cat 'test.txt'")
})

test_that("build_file_read_cmd works with gzipped files", {
  result <- build_file_read_cmd(list(filename = "test.txt.gz", gzipped = TRUE))
  expect_equal(result, "zcat 'test.txt.gz'")
})

test_that("build_file_read_cmd quotes paths with shell-special chars", {
  result <- build_file_read_cmd(list(filename = "with space.txt", gzipped = FALSE))
  expect_equal(result, "cat 'with space.txt'")
  # shQuote wraps in double quotes when input contains a single quote;
  # $, \ and ` get backslash-escaped inside.
  result_q <- build_file_read_cmd(list(filename = "weird'name.txt", gzipped = FALSE))
  expect_equal(result_q, "cat \"weird'name.txt\"")
  # $ alone (no single quote) stays in single-quoted form, so no expansion.
  result_d <- build_file_read_cmd(list(filename = "weird$name.txt", gzipped = FALSE))
  expect_equal(result_d, "cat 'weird$name.txt'")
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

test_that("build_comment_filter_pattern keeps a bracket expression", {
  result <- build_comment_filter_pattern("^[test]")
  expect_equal(result, "/^[test]/ { next }")
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

test_that("build_trim_prefix_code keeps a bracket expression", {
  result <- build_trim_prefix_code("^[prefix]")
  expect_equal(result, 'gsub(/^[prefix]/, "", $0)')
})

test_that("build_trim_prefix_code keeps an escaped backslash", {
  expect_equal(
    build_trim_prefix_code("^\\\\"),
    'gsub(/^\\\\/, "", $0)'
  )
})

test_that("build_comment_filter_pattern keeps an escaped metacharacter", {
  expect_equal(
    build_comment_filter_pattern("^\\{"),
    "/^\\{/ { next }"
  )
})

test_that("build_comment_filter_pattern escapes the regex delimiter", {
  expect_equal(
    build_comment_filter_pattern("^\\{/"),
    "/^\\{\\// { next }"
  )
})

test_that("build_trim_prefix_code keeps an escaped metacharacter", {
  expect_equal(
    build_trim_prefix_code("^\\{"),
    'gsub(/^\\{/, "", $0)'
  )
})

test_that("build_trim_prefix_code escapes the regex delimiter", {
  expect_equal(
    build_trim_prefix_code("^\\{/"),
    'gsub(/^\\{\\//, "", $0)'
  )
})

test_that("build_line_limit_code returns NULL for NULL input", {
  expect_null(
    build_line_limit_code(NULL)
  )
  expect_null(
    build_line_limit_code(integer(0))
  )
})

test_that("build_line_limit_code creates correct line counting code", {
  expect_equal(
    build_line_limit_code(100),
    "if (++output_lines <= max_lines) print $0; else exit"
  )
})

test_that("build_awk_begin_block works without nlines", {
  expect_equal(
    build_awk_begin_block("\t", skip_header = FALSE),
    "BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}"
  )
  expect_equal(
    build_awk_begin_block("\t", nlines = integer(0), skip_header = FALSE),
    "BEGIN{\n  FS = \"\t\"\n  OFS = \"\t\"\n}"
  )
})

test_that("build_awk_begin_block returns NULL when no sep or nlines", {
  expect_null(build_awk_begin_block(NULL, skip_header = FALSE))
  expect_null(build_awk_begin_block(NULL, nlines = integer(0), skip_header = FALSE))
})

test_that("build_awk_begin_block works with nlines only (no sep)", {
  result <- build_awk_begin_block(NULL, 50, skip_header = FALSE)
  expected <- "BEGIN{\n  output_lines = 0\n  max_lines = 50\n}"
  expect_equal(result, expected)
})

test_that("build_awk_begin_block with command line FS (only OFS in BEGIN)", {
  result <- build_awk_begin_block(
    "\t", NULL, skip_header = FALSE, use_command_line_fs = TRUE
  )
  expect_equal(result, "BEGIN{\n  OFS = \"\t\"\n}")
})

test_that("build_awk_begin_block with command line FS and nlines", {
  expect_equal(
    build_awk_begin_block(
      ",", 100, skip_header = FALSE, use_command_line_fs = TRUE
    ),
    "BEGIN{
  OFS = \",\"
  output_lines = 0
  max_lines = 100
}"
  )
})

test_that("build_awk_begin_block works with nlines", {
  expect_equal(
    build_awk_begin_block("\t", 100, skip_header = FALSE),
    "BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
  output_lines = 0
  max_lines = 100
}"
  )
})

test_that("build_awk_begin_block handles different separators", {
  result <- build_awk_begin_block(",", skip_header = FALSE)
  expect_equal(result, "BEGIN{\n  FS = \",\"\n  OFS = \",\"\n}")
})

test_that("build_awk_begin_block handles separator with special chars", {
  result <- build_awk_begin_block("|", skip_header = FALSE)
  expect_equal(result, "BEGIN{\n  FS = \"|\"\n  OFS = \"|\"\n}")
})


test_that("escape_awk_string escapes backslashes and double quotes", {
  expect_equal(escape_awk_string("plain"), "plain")
  expect_equal(escape_awk_string('a"b'), 'a\\"b')
  expect_equal(escape_awk_string("a\\b"), "a\\\\b")
  expect_equal(escape_awk_string('a"b\\c"d'), 'a\\"b\\\\c\\"d')
})
