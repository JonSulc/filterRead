test_that("is_non_allele_encoded_column identifies non-allele columns", {
  expect_true(is_non_allele_encoded_column(c("chr", "pos")))
  expect_true(is_non_allele_encoded_column(c("build", "chr", "pos")))
  expect_false(is_non_allele_encoded_column(NULL))
  expect_false(is_non_allele_encoded_column(c("ref", "alt")))
})

test_that("encoded_array_refs builds awk references to split-array elements", {
  expect_equal(
    encoded_array_refs(1L, 1:2),
    c("encoded1[1]", "encoded1[2]")
  )
  expect_equal(encoded_array_refs(2L, 3L), "encoded2[3]")
})

real_and_encoded_dt <- function() {
  data.table::data.table(
    chr = c("chr1", "chr2"), pos = c(100L, 300L),
    chr_colon_pos = c("1:100", "2:300"),
    ref = c("A", "G"), alt = c("G", "A"), pval = c(0.1, 0.3)
  )
}

test_that("a fully-redundant encoded parent is demoted to a plain column", {
  fi <- local_file_interface(dt = real_and_encoded_dt(), build = "b38")
  expect_equal(
    column_names(fi),
    c("chr", "pos", "chr_colon_pos", "ref", "alt", "pval")
  )
  parent <- fi$column_info[input_name == "chr_colon_pos"]
  expect_true(is.null(parent$encoded_names[[1]]))
  expect_true(is.na(parent$recode_columns))
  expect_true(is.na(parent$split_encoding_column))
})

test_that("check_post_processing works with a redundant encoded column", {
  fi <- local_file_interface(dt = real_and_encoded_dt(), build = "b38")
  expect_equal(
    check_post_processing(c("chr1", "chr2"), "chr", fi, to_write = TRUE),
    c("chr1", "chr2")
  )
})

test_that("positional filtering works with a redundant encoded column", {
  fi <- local_file_interface(dt = real_and_encoded_dt(), build = "b38")
  result <- fi[chr == 1 & pos < 500]
  expect_equal(nrow(result), 1)
  expect_equal(result$pos, 100L)
})

test_that("the demoted parent is read through raw with no per-row split", {
  fi <- local_file_interface(dt = real_and_encoded_dt(), build = "b38")
  arrays <- get_awk_column_arrays(fi)
  expect_false(any(grepl("encoded1", unlist(arrays))))
  result <- fi[chr == 1 & pos < 500]
  expect_equal(result$chr_colon_pos, "1:100")
})

test_that("a partially-redundant parent keeps only the surviving child", {
  dt <- data.table::data.table(
    chr = c("chr1", "chr2"),
    chr_colon_pos = c("1:100", "2:300"),
    ref = c("A", "G"), alt = c("G", "A"), pval = c(0.1, 0.3)
  )
  fi <- local_file_interface(dt = dt, build = "b38")
  expect_equal(
    column_names(fi),
    c("chr", "chr_colon_pos", "pos", "ref", "alt", "pval")
  )
  expect_false(any(duplicated(column_names(fi))))
  pos_row <- fi$column_info[standard_name == "pos"]
  expect_equal(nrow(pos_row), 1L)
  expect_equal(pos_row$bash_index, "encoded1[2]")
  parent <- fi$column_info[input_name == "chr_colon_pos"]
  expect_equal(parent$recode_columns, "$2 = $2 OFS encoded1[2]")
  result <- fi[pos < 250]
  expect_equal(nrow(result), 1L)
  expect_equal(result$chr, "chr1")
})

test_that("a build-encoded parent beside real chr/pos keeps only build", {
  dt <- data.table::data.table(
    chr = c("chr1", "chr2"), pos = c(100L, 300L),
    MarkerName = c("b37-c1:100-123", "b37-c2:300-123"),
    ref = c("A", "G"), alt = c("G", "A"), pval = c(0.1, 0.3)
  )
  fi <- local_file_interface(dt = dt, build = "b38")
  expect_true("build" %in% column_names(fi))
  expect_false(any(duplicated(column_names(fi))))
  expect_equal(fi$column_info[standard_name == "chr", bash_index], "$1")
  expect_equal(fi$column_info[standard_name == "pos", bash_index], "$2")
  expect_equal(
    fi$column_info[standard_name == "build", bash_index], "encoded1[1]"
  )
})
