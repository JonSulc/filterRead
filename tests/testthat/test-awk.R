# Integration Tests for AWK Command Compilation

test_that("compile_awk_cmd emits a record cap for a read needing no filter", {
  local_awk()
  finterface <- list(
    filename = "test.txt",
    gzipped = FALSE,
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t"
  )
  expect_equal(
    compile_awk_cmd(finterface, nlines = 5, skip_header = FALSE),
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

test_that("compile_awk_cmd applies both prefixes under a record cap", {
  local_awk()
  finterface <- list(
    filename = "data.csv",
    gzipped = FALSE,
    comment_prefix = "^//",
    trim_prefix = "^%",
    sep = ","
  )
  expect_equal(
    compile_awk_cmd(finterface, nlines = 100, skip_header = FALSE),
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

test_that("compile_awk_cmd treats an empty nlines as no record cap", {
  local_awk()
  finterface <- list(
    filename = "data.csv",
    gzipped = FALSE,
    comment_prefix = "^//",
    trim_prefix = "^%",
    sep = ","
  )
  expect_equal(
    compile_awk_cmd(finterface, nlines = integer(0), skip_header = FALSE),
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

test_that("a two-membership condition emits the match branch once", {
  fi <- local_file_interface(
    dt = data.table::data.table(
      chr = c("chr1", "chr1", "chr2"), pos = c(100L, 200L, 300L),
      ref = c("A", "C", "G"), alt = c("G", "T", "A"), pval = c(0.1, 0.2, 0.3)
    ),
    build = "b38"
  )
  cmd <- fi[ref %in% c("A", "C") & alt %in% c("G", "T"), return_only_cmd = TRUE]
  expect_equal(
    lengths(regmatches(cmd, gregexpr("print $0", cmd, fixed = TRUE))),
    1L
  )
  expect_equal(
    lengths(regmatches(cmd, gregexpr("if (FILENAME == ", cmd, fixed = TRUE))),
    2L
  )
})

test_that("single-file filtered read initializes header_skipped in BEGIN", {
  finterface <- local_summary_stats_interface()
  cmd <- finterface[pval < 0.5, return_only_cmd = TRUE]
  expect_match(cmd, "header_skipped = 0", fixed = TRUE)
  expect_match(
    cmd, "!header_skipped { header_skipped = 1; next }", fixed = TRUE
  )
})

test_that("tabix path omits FS from BEGIN", {
  finterface <- local_rsid_summary_stats_interface(build = "b38")
  cmd <- finterface[
    pval < .05 & chr == 1 & 123 <= pos & pos <= 12345,
    return_only_cmd = TRUE
  ]
  begin <- strsplit(cmd, "}", fixed = TRUE)[[1]][1]
  expect_match(begin, "\n  OFS = ", fixed = TRUE)
  expect_no_match(begin, "\n  FS = ", fixed = TRUE)
})

test_that("a filtered read names the configured executable", {
  finterface <- local_summary_stats_interface()
  local_awk("custom-awk")
  expect_match(
    finterface[pval < 0.5, return_only_cmd = TRUE],
    "^custom-awk '"
  )
})

test_that("a read-only command names the configured executable", {
  local_awk("custom-awk")
  finterface <- list(
    filename = "test.txt",
    gzipped = FALSE,
    comment_prefix = NULL,
    trim_prefix = NULL,
    sep = "\t"
  )
  expect_match(
    build_read_only_awk_cmd(finterface, nlines = NULL, skip_header = TRUE),
    "^custom-awk '"
  )
})
