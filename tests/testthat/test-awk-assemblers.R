# Test Level 2: Component Assemblers

test_that("build_read_only_awk_script with no prefixes or nlines", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t",
    gzipped = FALSE
  )
  
  expect_equal(
    build_read_only_awk_script(finterface, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
}
{
  print $0
}' test.txt"
  )
  expect_equal(
    build_read_only_awk_script(finterface, skip_header = TRUE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
  header_skipped = 0
}
!header_skipped { header_skipped = 1; next }
{
  print $0
}' test.txt"
  )
})

test_that("build_read_only_awk_script with comment prefix only", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = NULL,
    sep = "\t",
    gzipped = FALSE
  )
  expect_equal(
    build_read_only_awk_script(finterface, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
}
/^##/ { next }
{
  print $0
}' test.txt"
  )
  expect_equal(
    build_read_only_awk_script(finterface, skip_header = TRUE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
  header_skipped = 0
}
/^##/ { next }
!header_skipped { header_skipped = 1; next }
{
  print $0
}' test.txt"
  )
})

test_that("build_read_only_awk_script with trim prefix only", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = "^#",
    sep = "\t",
    gzipped = FALSE
  )
  expect_equal(
    build_read_only_awk_script(finterface, skip_header = FALSE),
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

test_that("build_read_only_awk_script with both prefixes", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = "^#",
    sep = "\t",
    gzipped = FALSE
  )
  expect_equal(
    build_read_only_awk_script(finterface, skip_header = FALSE),
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

test_that("build_read_only_awk_script with nlines only", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t",
    gzipped = FALSE
  )
  expect_equal(
    build_read_only_awk_script(finterface, nlines = 100, skip_header = FALSE),
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
  expect_equal(
    build_read_only_awk_script(finterface, nlines = 100),
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
}' test.txt"
  )
})

test_that("build_read_only_awk_script with prefixes and nlines", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = "^##",
    trim_prefix = "^#",
    sep = "\t",
    gzipped = FALSE
  )
  expect_equal(
    build_read_only_awk_script(finterface, nlines = 100, skip_header = FALSE),
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
}' test.txt"
  )
  expect_equal(
    build_read_only_awk_script(finterface, nlines = 100, skip_header = TRUE),
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
}' test.txt"
  )
})

test_that("build_read_only_awk_script handles different separators", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = ",",
    gzipped = FALSE
  )
  expect_equal(
    build_read_only_awk_script(finterface, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \",\"
  OFS = \",\"
}
{
  print $0
}' test.txt"
  )
})

test_that("build_read_only_awk_script with complex patterns", {
  finterface <- list(
    filename = "test.txt",
    comment_prefix = "^<!--",
    trim_prefix = "^//",
    sep = "|",
    gzipped = FALSE
  )
  expect_equal(
    build_read_only_awk_script(finterface, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"|\"
  OFS = \"|\"
}
/^<!--/ { next }
{
  gsub(/^\\/\\//, \"\", $0)
  print $0
}' test.txt"
  )
})

