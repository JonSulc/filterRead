# Test Level 3: High-Level Interfaces

test_that(
  "resolve_read_cmd reads a plain file with cat, and with awk where a header is skipped",
  {
    finterface <- list(
      gzipped = FALSE,
      filename = "test.txt",
      comment_prefix = NULL,
      trim_prefix = NULL
    )
    # With skip_header=FALSE, returns simple cat command
    result <- resolve_read_cmd(finterface, skip_header = FALSE)
    expect_equal(result, "cat 'test.txt'")

    # Default skip_header=TRUE returns awk command with header skip
    result_skip <- resolve_read_cmd(finterface)
    expect_match(result_skip, "header_skipped")
  }
)

test_that(
  "resolve_read_cmd reads a gzipped file with zcat, and with awk where a header is skipped",
  {
    finterface <- list(
      gzipped = TRUE,
      filename = "test.txt.gz",
      comment_prefix = NULL,
      trim_prefix = NULL
    )
    # With skip_header=FALSE, returns simple zcat command
    result <- resolve_read_cmd(finterface, skip_header = FALSE)
    expect_equal(result, "zcat 'test.txt.gz'")

    # Default skip_header=TRUE returns awk command with header skip
    result_skip <- resolve_read_cmd(finterface)
    expect_match(result_skip, "header_skipped")
  }
)

test_that("resolve_read_cmd emits a comment filter", {
  local_awk()
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = NULL,
    sep = "\t"
  )
  expect_equal(
    resolve_read_cmd(finterface, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
}
/^##/ { next }
{
  print $0
}' 'test.txt'"
  )
})

test_that("resolve_read_cmd emits a trim substitution", {
  local_awk()
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = "^#",
    sep = "\t"
  )
  expect_equal(
    resolve_read_cmd(finterface, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
}
{
  gsub(/^#/, \"\", $0)
  print $0
}' 'test.txt'"
  )
})

test_that(
  "resolve_read_cmd emits a comment filter and a trim substitution together",
  {
    local_awk()
    finterface <- list(
      gzipped = FALSE,
      filename = "test.txt",
      comment_prefix = "^##",
      trim_prefix = "^#",
      sep = "\t"
    )
    expect_equal(
      resolve_read_cmd(finterface, skip_header = FALSE),
      "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
}
/^##/ { next }
{
  gsub(/^#/, \"\", $0)
  print $0
}' 'test.txt'"
    )
  }
)

test_that(
  "resolve_read_cmd reads a gzipped file through process substitution",
  {
    local_awk()
    finterface <- list(
      gzipped = TRUE,
      filename = "test.txt.gz",
      comment_prefix = "^##",
      trim_prefix = "^#",
      sep = "\t"
    )
    expect_equal(
      resolve_read_cmd(finterface, skip_header = FALSE),
      "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
}
/^##/ { next }
{
  gsub(/^#/, \"\", $0)
  print $0
}' <(zcat 'test.txt.gz')"
    )
  }
)

test_that("resolve_read_cmd emits a record cap", {
  local_awk()
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t"
  )
  expect_equal(
    resolve_read_cmd(finterface, nlines = 100, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
  output_lines = 0
  max_lines = 100
}
{
  if (++output_lines <= max_lines) print $0; else exit
}' 'test.txt'"
  )
})

test_that("resolve_read_cmd emits the prefixes and the record cap together", {
  local_awk()
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = "^#",
    sep = "\t"
  )
  expect_equal(
    resolve_read_cmd(finterface, nlines = 50, skip_header = FALSE),
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
}' 'test.txt'"
  )
})

test_that(
  "resolve_read_cmd emits the prefixes and the record cap for a gzipped file",
  {
    local_awk()
    finterface <- list(
      gzipped = TRUE,
      filename = "test.txt.gz",
      comment_prefix = "^//",
      trim_prefix = "^%",
      sep = ","
    )
    expect_equal(
      resolve_read_cmd(finterface, nlines = 25, skip_header = FALSE),
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
}' <(zcat 'test.txt.gz')"
    )
  }
)

test_that(
  "resolve_read_cmd carries a regex prefix and a pipe separator into the program",
  {
    local_awk()
    finterface <- list(
      gzipped = FALSE,
      filename = "complex.txt",
      comment_prefix = "\\*\\*",
      trim_prefix = "\\*",
      sep = "|"
    )
    expect_equal(
      resolve_read_cmd(finterface, skip_header = FALSE),
      "awk 'BEGIN{
  FS = \"|\"
  OFS = \"|\"
}
/\\*\\*/ { next }
{
  gsub(/\\*/, \"\", $0)
  print $0
}' 'complex.txt'"
    )
  }
)

test_that("resolve_read_cmd quotes a filename holding spaces", {
  finterface <- list(
    gzipped = TRUE,
    filename = "file with spaces.txt.gz",
    comment_prefix = NULL,
    trim_prefix = NULL
  )
  result <- resolve_read_cmd(finterface, skip_header = FALSE)
  expect_equal(result, "zcat 'file with spaces.txt.gz'")
})

test_that("resolve_read_cmd emits an empty prefix as an empty regex", {
  local_awk()
  finterface <- list(
    gzipped = FALSE,
    filename = "test.txt",
    comment_prefix = "",
    trim_prefix = "",
    sep = "\t"
  )
  expect_equal(
    resolve_read_cmd(finterface, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
}
// { next }
{
  gsub(//, \"\", $0)
  print $0
}' 'test.txt'"
  )
})
