test_that("escape_awk_regex handles NULL", {
  expect_null(escape_awk_regex(NULL))
})

test_that("escape_awk_regex escapes special characters", {
  expect_equal(escape_awk_regex("/"), "\\/")
  expect_equal(escape_awk_regex("[test]"), "\\[test\\]")
  expect_equal(escape_awk_regex("(a+b)?"), "\\(a\\+b\\)\\?")
  expect_equal(escape_awk_regex("{1,2}"), "\\{1,2\\}")
  expect_equal(escape_awk_regex("a|b"), "a\\|b")
  expect_equal(escape_awk_regex("plain"), "plain")
})

test_that("increase_indent adds indentation after newlines", {
  expect_equal(increase_indent("line1"), "line1")
  expect_equal(increase_indent("line1\nline2"), "line1\n  line2")
  expect_equal(
    increase_indent("a\nb\nc"),
    "a\n  b\n  c"
  )
})

test_that("awk_split_column generates correct split command", {
  expect_equal(
    awk_split_column("$1", "arr", ":"),
    'split($1, arr, ":")'
  )
  expect_equal(
    awk_split_column("$5", "encoded1", " "),
    'split($5, encoded1, " ")'
  )
})

test_that("awk_combine_split_for_output combines array elements", {
  expect_equal(
    awk_combine_split_for_output("$1", "arr", 1),
    "$1 = arr[1]"
  )
  expect_equal(
    awk_combine_split_for_output("$1", "arr", 2),
    "$1 = arr[1] OFS arr[2]"
  )
  expect_equal(
    awk_combine_split_for_output("$1", "arr", 3),
    "$1 = arr[1] OFS arr[2] OFS arr[3]"
  )
})

test_that("wrap_filename returns filename for non-gzipped", {
  fi <- list(filename = "test.txt", gzipped = FALSE)
  expect_equal(wrap_filename(fi), "test.txt")
})

test_that("wrap_filename uses process substitution for gzipped", {
  fi <- list(filename = "test.txt.gz", gzipped = TRUE)
  expect_equal(wrap_filename(fi), "<(zcat test.txt.gz)")
})
