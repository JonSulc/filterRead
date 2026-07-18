# Integration Tests for AWK Command Compilation

test_that("compile_awk_cmds: simple file interface with only_read", {
  finterface <- list(
    filename = "test.txt",
    gzipped = FALSE,
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t"
  )
  expect_equal(
    compile_awk_cmds(finterface, nlines = 5, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \"\t\"
  OFS = \"\t\"
  output_lines = 0
  max_lines = 5
}
{
  if (++output_lines <= max_lines) print $0; else exit
}' 'test.txt'"
  )
})

test_that("awk_load_file_cmd: gzipped file head", {
  finterface <- list(
    filename = "test.txt.gz",
    gzipped = TRUE
  )
  expect_equal(
    awk_load_file_cmd(finterface, nlines = 1, only_read = FALSE),
    "awk"
  )
})

test_that("compile_awk_cmds: complex case with nlines and prefixes", {
  finterface <- list(
    filename = "data.csv",
    gzipped = FALSE,
    comment_prefix = "^//",
    trim_prefix = "^%",
    sep = ","
  )
  expect_equal(
    compile_awk_cmds(finterface, nlines = 100, skip_header = FALSE),
    "awk 'BEGIN{
  FS = \",\"
  OFS = \",\"
  output_lines = 0
  max_lines = 100
}
/^\\/\\// { next }
{
  gsub(/^%/, \"\", $0)
  if (++output_lines <= max_lines) print $0; else exit
}' 'data.csv'"
  )
})

test_that("compile_awk_cmds: case with empty nlines", {
  finterface <- list(
    filename = "data.csv",
    gzipped = FALSE,
    comment_prefix = "^//",
    trim_prefix = "^%",
    sep = ","
  )
  expect_equal(
    compile_awk_cmds(finterface, nlines = integer(0), skip_header = FALSE),
    "awk 'BEGIN{
  FS = \",\"
  OFS = \",\"
}
/^\\/\\// { next }
{
  gsub(/^%/, \"\", $0)
  print $0
}' 'data.csv'"
  )
})
