# Test Level 2: Component Assemblers

test_that("build_read_only_awk_cmd prints every record, skipping the header where asked", {
  local_awk()
  finterface <- list(
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t",
    gzipped = FALSE
  )
  
  expect_equal(
    build_read_only_awk_cmd(finterface, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
}
{
  print $0
}' 'test.txt'"
  )
  expect_equal(
    build_read_only_awk_cmd(finterface, skip_header = TRUE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
  header_skipped = 0
}
!header_skipped { header_skipped = 1; next }
{
  print $0
}' 'test.txt'"
  )
})

test_that("build_read_only_awk_cmd drops comment records", {
  local_awk()
  finterface <- list(
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = NULL,
    sep = "\t",
    gzipped = FALSE
  )
  expect_equal(
    build_read_only_awk_cmd(finterface, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
}
/^##/ { next }
{
  print $0
}' 'test.txt'"
  )
  expect_equal(
    build_read_only_awk_cmd(finterface, skip_header = TRUE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
  header_skipped = 0
}
/^##/ { next }
!header_skipped { header_skipped = 1; next }
{
  print $0
}' 'test.txt'"
  )
})

test_that("build_read_only_awk_cmd trims a prefix from every record", {
  local_awk()
  finterface <- list(
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = "^#",
    sep = "\t",
    gzipped = FALSE
  )
  expect_equal(
    build_read_only_awk_cmd(finterface, skip_header = FALSE),
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

test_that("build_read_only_awk_cmd drops comment records and trims the rest", {
  local_awk()
  finterface <- list(
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = "^#",
    sep = "\t",
    gzipped = FALSE
  )
  expect_equal(
    build_read_only_awk_cmd(finterface, skip_header = FALSE),
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
})

test_that("build_read_only_awk_cmd stops at the record cap", {
  local_awk()
  finterface <- list(
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t",
    gzipped = FALSE
  )
  expect_equal(
    build_read_only_awk_cmd(finterface, nlines = 100, skip_header = FALSE),
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
  expect_equal(
    build_read_only_awk_cmd(finterface, nlines = 100),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
  output_lines = 0
  max_lines = 100
  header_skipped = 0
}
!header_skipped { header_skipped = 1; next }
{
  if (++output_lines <= max_lines) print $0; else exit
}' 'test.txt'"
  )
})

test_that("build_read_only_awk_cmd counts only records the comment filter keeps", {
  local_awk()
  finterface <- list(
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = "^#",
    sep = "\t",
    gzipped = FALSE
  )
  expect_equal(
    build_read_only_awk_cmd(finterface, nlines = 100, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
  output_lines = 0
  max_lines = 100
}
/^##/ { next }
{
  gsub(/^#/, \"\", $0)
  if (++output_lines <= max_lines) print $0; else exit
}' 'test.txt'"
  )
  expect_equal(
    build_read_only_awk_cmd(finterface, nlines = 100, skip_header = TRUE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
  output_lines = 0
  max_lines = 100
  header_skipped = 0
}
/^##/ { next }
!header_skipped { header_skipped = 1; next }
{
  gsub(/^#/, \"\", $0)
  if (++output_lines <= max_lines) print $0; else exit
}' 'test.txt'"
  )
})

test_that("build_read_only_awk_cmd sets FS and OFS to the file's separator", {
  local_awk()
  finterface <- list(
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = ",",
    gzipped = FALSE
  )
  expect_equal(
    build_read_only_awk_cmd(finterface, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \",\"
  OFS = \",\"
}
{
  print $0
}' 'test.txt'"
  )
})

test_that("build_read_only_awk_cmd escapes a slash in a trim prefix", {
  local_awk()
  finterface <- list(
    filename = "test.txt",
    comment_prefix = "^<!--",
    trim_prefix = "^//",
    sep = "|",
    gzipped = FALSE
  )
  expect_equal(
    build_read_only_awk_cmd(finterface, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"|\"
  OFS = \"|\"
}
/^<!--/ { next }
{
  gsub(/^\\/\\//, \"\", $0)
  print $0
}' 'test.txt'"
  )
})

