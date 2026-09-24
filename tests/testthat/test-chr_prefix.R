test_that("no code is emitted for a bare chr column", {
  finterface <- local_file_interface(
    dt = data.table::data.table(
      chr = c("1", "2"), pos = c(1000L, 2000L), pval = c(0.1, 0.2)
    ),
    build = "b38"
  )
  expect_null(get_awk_column_arrays(finterface))
})

test_that("a prefixed chr column in the first field substitutes on $0", {
  finterface <- local_file_interface(
    dt = data.table::data.table(
      chr = c("chr1", "chr2"), pos = c(1000L, 2000L), pval = c(0.1, 0.2)
    ),
    build = "b38"
  )
  expect_equal(
    get_awk_column_arrays(finterface)$after_if,
    'sub(/^chr/, "")'
  )
})

test_that("a prefixed chr column elsewhere substitutes on its field", {
  finterface <- local_file_interface(
    dt = data.table::data.table(
      pos = c(1000L, 2000L), chr = c("chr1", "chr2"), pval = c(0.1, 0.2)
    ),
    build = "b38"
  )
  expect_equal(
    get_awk_column_arrays(finterface)$after_if,
    'sub(/^chr/, "", $2)'
  )
  expect_equal(finterface[]$chr, c("1", "2"))
})

test_that("a quoted prefixed chr column reads back bare", {
  finterface <- local_file_interface(
    dt = data.table::data.table(
      rsid = c("rs1", "rs2"), chr = c("chr1", "chr2"),
      pos = c(1000L, 2000L), pval = c(0.1, 0.2)
    ),
    quote = TRUE,
    build = "b38"
  )
  expect_equal(
    get_awk_column_arrays(finterface)$after_if,
    'sub(/^"chr/, "\\"", $2)'
  )
  expect_equal(finterface[]$chr, c("1", "2"))
})

test_that("a quoted parent's decoded chr reads back bare", {
  finterface <- local_file_interface(
    dt = make_decoded_chr_stats(),
    quote = TRUE,
    build = "b38"
  )
  expect_equal(finterface[]$chr, c("1", "1"))
})

test_that("a bare chr column records no prefix and reads back bare", {
  local_summary_stats(prefix = NULL, random_names = FALSE)
  finterface <- new_file_interface("data.csv") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_true(is.na(finterface$column_info[name == "chr", dropped_prefix]))
  expect_true(all(head(finterface, nlines = 5)$chr %in% as.character(1:22)))
  expect_type(head(finterface, nlines = 5)$chr, "character")
})

test_that("a prefixed chr column records its prefix and reads back bare", {
  local_summary_stats(
    prefix = list(chr = "chr"),
    random_names = FALSE
  )
  finterface <- new_file_interface("data.csv") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_equal(finterface$column_info[name == "chr", dropped_prefix], "chr")
  expect_true(
    all(is.na(finterface$column_info[name != "chr", dropped_prefix]))
  )
  expect_true(all(head(finterface, nlines = 5)$chr %in% as.character(1:22)))
})

test_that("a blank chromosome field stays blank when its column is prefixed", {
  fi <- local_file_interface(
    dt = data.table::data.table(
      chr  = c("chr1", "", "chr2"),
      pos  = c(1000L, 1500L, 2000L),
      ref  = c("A", "C", "G"),
      alt  = c("G", "T", "A"),
      pval = c(0.1, 0.2, 0.3)
    ),
    quote = FALSE,
    build = "b38"
  )
  expect_equal(fi[]$chr, c("1", "", "2"))
})

test_that("a blank chromosome field stays blank when its column is bare", {
  fi <- local_file_interface(
    dt = data.table::data.table(
      chr  = c("1", "", "2"),
      pos  = c(1000L, 1500L, 2000L),
      ref  = c("A", "C", "G"),
      alt  = c("G", "T", "A"),
      pval = c(0.1, 0.2, 0.3)
    ),
    quote = FALSE,
    build = "b38"
  )
  expect_equal(fi[]$chr, c("1", "", "2"))
})

test_that("an inconsistently prefixed chr column reads back bare", {
  finterface <- local_file_interface(
    dt = data.table::data.table(
      chr = c("chr1", "2"), pos = c(1000L, 2000L), pval = c(0.1, 0.2)
    ),
    build = "b38"
  )
  expect_equal(finterface$column_info[name == "chr", dropped_prefix], "chr")
  expect_identical(finterface[]$chr, c("1", "2"))
})

test_that("a chr column holding a scaffold name reads it back unchanged", {
  finterface <- local_file_interface(
    dt = data.table::data.table(
      chr = c("chr1", "GL000209.1"), pos = c(1000L, 2000L),
      pval = c(0.1, 0.2)
    ),
    build = "b38"
  )
  expect_equal(
    finterface$column_info[name == "chr", dropped_prefix], "chr"
  )
  expect_identical(finterface[]$chr, c("1", "GL000209.1"))
})

test_that("a decoded chr carrying the prefix records it and filters", {
  finterface <- local_file_interface(
    dt = make_decoded_chr_stats(), build = "b38"
  )
  expect_equal(finterface$column_info[name == "chr", dropped_prefix], "chr")

  regions <- region_set(
    chr = "chr1", start = 1L, end = 1500000L, build = "b38"
  )
  expect_equal(finterface[regions]$MarkerName, "chr1:1000000:SNP")
  expect_equal(finterface[chr %in% "chr1"]$pos, c(1000000L, 2000000L))
})

test_that("a decoded chr carrying no prefix reads back the package's", {
  finterface <- local_file_interface(
    dt = make_decoded_chr_stats(chr = "1"), build = "b38"
  )
  regions <- region_set(
    chr = "chr1", start = 1L, end = 1500000L, build = "b38"
  )
  expect_identical(finterface[regions]$chr, "1")
  expect_identical(finterface[]$chr, c("1", "1"))
})

test_that("a chr column carrying two declared prefixes errors", {
  declarations <- data.table::copy(summary_stats_standard_names_dt)
  declarations[
    standard_name == "chr",
    possible_prefixes := .(.(c("chr", "Chr")))
  ][]
  expect_error(
    local_file_interface(
      dt = data.table::data.table(
        chr = c("chr1", "Chr2"), pos = c(1000L, 2000L), pval = c(0.1, 0.2)
      ),
      standard_names_dt = declarations,
      build = "b38"
    ),
    "carries more than one declared prefix"
  )
})

test_that("an inconsistently prefixed column other than chr errors", {
  declarations <- data.table::copy(summary_stats_standard_names_dt)
  declarations[
    standard_name == "rsid",
    possible_prefixes := .(.("rs"))
  ][]
  expect_error(
    local_file_interface(
      dt = data.table::data.table(
        chr = c("1", "2"), pos = c(1000L, 2000L),
        rsid = c("rs1", "2"), pval = c(0.1, 0.2)
      ),
      standard_names_dt = declarations,
      build = "b38"
    ),
    "on some values and not others"
  )
})

test_that("region_set drops the prefix it is given", {
  expect_equal(
    region_set(chr = c("ch1", "chr2"), chr_prefix = "ch", build = "b38")$chr,
    c("1", "r2")
  )
})
