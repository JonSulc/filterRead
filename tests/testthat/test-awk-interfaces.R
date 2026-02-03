# Test Level 3: High-Level Interfaces

# Test awk_load_file_cmd with various parameter combinations

test_that("awk_load_file_cmd: simple case, only_read=TRUE, no processing", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL
  )
  # With skip_header=FALSE, returns simple cat command
  result <- awk_load_file_cmd(finterface, only_read = TRUE, skip_header = FALSE)
  expect_equal(result, "cat test.txt")

  # Default skip_header=TRUE returns awk command with header skip
  result_skip <- awk_load_file_cmd(finterface, only_read = TRUE)
  expect_match(result_skip, "header_skipped")
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
  # With skip_header=FALSE, returns simple zcat command
  result <- awk_load_file_cmd(finterface, only_read = TRUE, skip_header = FALSE)
  expect_equal(result, "zcat test.txt.gz")

  # Default skip_header=TRUE returns awk command with header skip
  result_skip <- awk_load_file_cmd(finterface, only_read = TRUE)
  expect_match(result_skip, "header_skipped")
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
  expect_equal(
    awk_load_file_cmd(finterface, only_read = TRUE, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
}
/^##/ { next }
{
  print $0
}' test.txt"
  )
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
  expect_equal(
    awk_load_file_cmd(finterface, only_read = TRUE, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
}
{
  gsub(/^#/, \"\", $0)
  print $0
}' test.txt"
  )
})

test_that("awk_load_file_cmd: both prefixes, only_read=TRUE", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = "^#",
    sep = "\t"
  )
  expect_equal(
    awk_load_file_cmd(finterface, only_read = TRUE, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
}
/^##/ { next }
{
  gsub(/^#/, \"\", $0)
  print $0
}' test.txt"
  )
})

test_that("awk_load_file_cmd: gzipped with both prefixes, only_read=TRUE", {
  finterface <- list(
    gzipped = TRUE,
    filename = "test.txt.gz",
    comment_prefix = "^##",
    trim_prefix = "^#",
    sep = "\t"
  )
  expect_equal(
    awk_load_file_cmd(finterface, only_read = TRUE, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
}
/^##/ { next }
{
  gsub(/^#/, \"\", $0)
  print $0
}' <(zcat test.txt.gz)"
  )
})

test_that("awk_load_file_cmd: nlines only, only_read=TRUE", {
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t"
  )
  expect_equal(
    awk_load_file_cmd(
      finterface, nlines = 100, only_read = TRUE, skip_header = FALSE
    ),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
  output_lines = 0
  max_lines = 100
}
{
  if (++output_lines <= max_lines) print $0; else exit
}' test.txt"
  )
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
  expect_equal(
    awk_load_file_cmd(
      finterface, nlines = 50, only_read = TRUE, skip_header = FALSE
    ),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
  output_lines = 0
  max_lines = 50
}
/^##/ { next }
{
  gsub(/^#/, \"\", $0)
  if (++output_lines <= max_lines) print $0; else exit
}' test.txt"
  )
})

test_that("awk_load_file_cmd: gzipped with prefixes and nlines, only_read=TRUE", {
  finterface <- list(
    gzipped = TRUE,
    filename = "test.txt.gz",
    comment_prefix = "^//",
    trim_prefix = "^%",
    sep = ","
  )
  expect_equal(
    awk_load_file_cmd(
      finterface, nlines = 25, only_read = TRUE, skip_header = FALSE
    ),
    "awk 'BEGIN{
  FS = \",\"
  OFS = \",\"
  output_lines = 0
  max_lines = 25
}
/^\\/\\// { next }
{
  gsub(/^%/, \"\", $0)
  if (++output_lines <= max_lines) print $0; else exit
}' <(zcat test.txt.gz)"
  )
})

test_that("awk_load_file_cmd: complex patterns", {
  finterface <- list(
    gzipped = FALSE,
    filename = "complex.txt",
    comment_prefix = "\\*\\*",
    trim_prefix = "\\*",
    sep = "|"
  )
  expect_equal(
    awk_load_file_cmd(finterface, only_read = TRUE, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"|\"
  OFS = \"|\"
}
/\\*\\*/ { next }
{
  gsub(/\\*/, \"\", $0)
  print $0
}' complex.txt"
  )
})

test_that("awk_load_file_cmd: special characters in filename", {
  finterface <- list(
    gzipped = TRUE,
    filename = "file with spaces.txt.gz",
    comment_prefix = NULL,
    trim_prefix = NULL
  )
  result <- awk_load_file_cmd(finterface, only_read = TRUE, skip_header = FALSE)
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
  expect_equal(
    awk_load_file_cmd(finterface, only_read = TRUE, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
}
// { next }
{
  gsub(//, \"\", $0)
  print $0
}' test.txt"
  )
})
