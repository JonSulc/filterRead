test_that("awk returns the records R returns for every filter expression", {
  # Every filter expression is read through awk and evaluated in R over the
  # same records, so a constraint the generator drops shows up as a
  # record-set difference rather than needing an expected awk string per
  # expression.
  condition_fcalls <- list(
    quote(chr == "chr1" & pos <= 200),
    quote(chr == "chr1" | chr == "chr2"),
    quote(pos <= 200 | 400 <= pos),
    quote(ref == "A" & alt == "T"),
    quote(ref == "A" | alt == "G"),
    quote(chr == "chr1" & ref == "A"),
    quote(chr == "chr1" | ref == "A"),
    quote(pos <= 200 & (ref == "A" | alt == "G")),
    quote((ref == "A" | alt == "G") & pos <= 200),
    quote(chr == "chr1" & (ref == "A" | alt == "G")),
    quote(ref == "A" & (pos <= 200 | 400 <= pos)),
    quote((chr == "chr1" | chr == "chr2") & (pos <= 200 | 400 <= pos)),
    quote((chr == "chr1" & pos <= 200) | (chr == "chr2" & 300 <= pos)),
    quote(
      (chr == "chr1" & 150 <= pos & pos <= 350) |
        (chr == "chr2" & 250 <= pos & pos <= 450)
    ),
    quote((chr == "chr1" & ref == "A") | (chr == "chr2" & alt == "G")),
    quote((chr == "chr1") | (pval < 0.5)),
    quote(pval < 0.05 & (chr == "chr1" | chr == "chr2")),
    quote(chr == "chr1" & ref == "A" | chr == "chr2" & alt == "G"),
    quote(
      (chr == "chr1" & (pos <= 200 | 400 <= pos)) | (chr == "chr2" & ref == "C")
    ),
    quote(((chr == "chr1" | chr == "chr2") & pos <= 200) & ref == "A"),
    quote((pval < 0.5 | ref == "A") & chr == "chr1" & pos <= 300),
    quote(chr == "chr1" | chr == "chr1" & pval < 0.5),
    quote(100 <= pos & pos <= 300 | 100 <= pos & pos <= 300 & pval < 0.5)
  )
  finterface <- local_file_interface(dt = dummy_fixed_summary_stats())
  for (fcall in condition_fcalls) {
    expect_read_matches_r(finterface, fcall)
  }
})

test_that("a condition naming a scaffold matches it in a prefixed file", {
  finterface <- local_file_interface(
    dt = data.table::data.table(
      chr  = c("chr1", "GL000209.1"),
      pos  = c(1000L, 2000L),
      pval = c(0.1, 0.2)
    ),
    build = "b38"
  )
  expect_equal(finterface[chr == "GL000209.1"]$pos, 2000L)
})

test_that("a condition matches every spelling the chr column declares", {
  declarations <- data.table::copy(summary_stats_standard_names_dt)
  declarations[
    standard_name == "chr",
    possible_prefixes := .(.(c("chr", "Chr")))
  ][]
  expect_warning(
    finterface <- local_file_interface(
      dt = data.table::data.table(
        chr = c("Chr1", "2"), pos = c(1000L, 2000L), ref = c("A", "C"),
        alt = c("G", "T"), pval = c(0.1, 0.2)
      ),
      standard_names_dt = declarations,
      build = "b38"
    ),
    "agree on a prefix"
  )
  regions <- new_region_set(chr = c(1, 2), build = "b38")
  variants <- new_variant_set(
    data.table::data.table(
      chr = c("1", "2"), pos = c(1000L, 2000L), ref = c("A", "C"),
      alt = c("G", "T")
    ),
    build = "b38"
  )
  expect_equal(finterface[chr == 1]$pos, 1000L)
  expect_equal(finterface[chr %in% c(1, 2)]$pos, c(1000L, 2000L))
  expect_equal(finterface[regions]$pos, c(1000L, 2000L))
  expect_equal(finterface[variants]$pos, c(1000L, 2000L))
})

test_that("a variant set matches a file whose ref declares a prefix", {
  declarations <- data.table::copy(summary_stats_standard_names_dt)
  declarations[standard_name == "ref", possible_prefixes := .(.("_"))][]
  finterface <- local_file_interface(
    dt = data.table::data.table(
      chr = c("1", "2"), pos = c(1000L, 2000L), ref = c("_A", "_C"),
      alt = c("G", "T"), pval = c(0.1, 0.2)
    ),
    standard_names_dt = declarations,
    build = "b38"
  )
  variants <- new_variant_set(
    data.table::data.table(chr = "1", pos = 1000L, ref = "A", alt = "G"),
    build = "b38"
  )
  expect_equal(finterface[variants]$pos, 1000L)
})

test_that("a variant missing an allele warns that one variant is dropped", {
  finterface <- local_file_interface(
    dt = data.table::data.table(
      chr = c("chr1", "chr2"), pos = c(1000L, 2000L), ref = c("A", "C"),
      alt = c("G", "T"), pval = c(0.1, 0.2)
    ),
    quote = TRUE,
    build = "b38"
  )
  variants <- new_variant_set(
    data.table::data.table(
      chr = c("1", "1"), pos = c(1000L, 2000L), ref = c("A", NA),
      alt = c("G", "G")
    ),
    build = "b38"
  )
  expect_warning(
    result <- finterface[variants],
    "^1 variant\\(s\\) dropped"
  )
  expect_equal(result$pos, 1000L)
})
