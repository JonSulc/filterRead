test_that("RSID parsing required is correctly detected", {
  finterface <- local_summary_stats_interface()
  expect_false(needs_rsid_matching(finterface))
  finterface_rn <- local_summary_stats_interface("data_rn.csv",
                                                 random_names = TRUE)
  expect_false(needs_rsid_matching(finterface_rn))

  finterface_rsid <- local_rsid_summary_stats_interface("data_rsid.csv")
  expect_true(needs_rsid_matching(finterface_rsid))
  finterface_rsid_rn <- local_rsid_summary_stats_interface("data_rsid_rn.csv",
                                                           random_names = TRUE)
  expect_true(needs_rsid_matching(finterface_rsid_rn))

  finterface_enc <- local_summary_stats_interface("data_enc.csv",
                                                  encode_columns = TRUE)
  expect_false(needs_rsid_matching(finterface_enc))
  finterface_enc_rn <- local_summary_stats_interface("data_enc_rn.csv",
                                                     random_names   = TRUE,
                                                     encode_columns = TRUE)
  expect_false(needs_rsid_matching(finterface_enc_rn))
})

test_that("tabix process substitution works", {
  expect_error(
    get_tabix_process_substitution(1:2, 123, 234)
  )
  expect_error(
    get_tabix_process_substitution(1, 123:124, 234)
  )
  expect_error(
    get_tabix_process_substitution(1, 123, 234:235)
  )

  expect_equal(
    get_tabix_process_substitution(1, 123, 234),
    sprintf("<(tabix %s NC_000001.11:123-234)", dbsnp_file)
  )
  expect_equal(
    get_tabix_process_substitution(1, c(123, 456), c(234, 567)),
    sprintf("<(tabix %s NC_000001.11:123-234 NC_000001.11:456-567)", dbsnp_file)
  )
  expect_equal(
    get_tabix_process_substitution(1:2, c(123, 456), c(234, 567)),
    sprintf("<(tabix %s NC_000001.11:123-234 NC_000002.12:456-567)", dbsnp_file)
  )
})

test_that("File reading works", {
  finterface <- local_rsid_summary_stats_interface()
  expect_true(needs_rsid_matching(finterface))
  expect_no_error(head(finterface))
  expect_true(all(finterface[pval < .05]$pval < .05))
  expect_equal(colnames(finterface[pval < .05]),
               c("rsid", "ref", "alt", "effect", "pval"))
  expect_equal(colnames(finterface[pval < .05 & chr == 1 & 123 <= pos & pos <= 12345]),
               c("chr", "pos", "rsid", "ref", "alt", "effect", "pval"))
  expect_equal(
    finterface[pval < .05 & chr == 1 & 123 <= pos & pos <= 12345,
               return_only_cmd = TRUE],
    paste0(
      "awk 'BEGIN{
  OFS = \",\"
} {
  if (NR == FNR) {
    rsid0[$3]=$1 OFS $2
  }
  else {
    if ($1 in rsid0) {
      if ($5 < 0.05) {
        print rsid0[$1] OFS $0
      }
    }
  }
}' FS=\"\\t\" <(tabix ~/rcp_storage/common/Users/abadreddine/data/dbSNP/GCF_000001405.40.gz NC_000001.11:123-12345) FS=\",\" data.csv"
    )
  )
})

test_that("Genomic blocks are correctly identified", {
  finterface <- local_rsid_summary_stats_interface()
  expect_true(
    new_filter_condition(rlang::expr(pval < .05),
                         finterface) |>
      is_single_genomic_range_block()
  )
  expect_true(
    new_filter_condition(rlang::expr(chr == 1),
                         finterface) |>
      is_single_genomic_range_block()
  )
  expect_true(
    new_filter_condition(rlang::expr(chr == 1 & pval < .05),
                         finterface) |>
      is_single_genomic_range_block()
  )
  expect_true(
    new_filter_condition(rlang::expr(chr == 1 | pval < .05),
                         finterface) |>
      is_single_genomic_range_block()
  )
  expect_true(
    new_filter_condition(rlang::expr(chr == 1 & pos < 123 | pval < .05),
                         finterface) |>
      is_single_genomic_range_block()
  )
  expect_true(
    new_filter_condition(rlang::expr(chr == 1 & 123 < pos & pos < 234 | pval < .05),
                         finterface) |>
      is_single_genomic_range_block()
  )
  expect_true(
    new_filter_condition(rlang::expr(chr == 1 & 123 < pos & pos < 234 & pval < .05),
                         finterface) |>
      is_single_genomic_range_block()
  )
  expect_true(
    new_filter_condition(rlang::expr(chr == 1 & 123 < pos & pos < 234
                                     & pval < .05 & ref == "A"),
                         finterface) |>
      is_single_genomic_range_block()
  )
  expect_true(
    new_filter_condition(rlang::expr(chr == 1 & 123 < pos & pos < 234
                                     | chr == 2 & 21 < pos & pos < 42),
                         finterface) |>
      is_single_genomic_range_block()
  )

  expect_false(
    new_filter_condition(rlang::expr(chr == 1 & 123 < pos | pos < 234 & pval < .05),
                         finterface) |>
      is_single_genomic_range_block()
  )
  expect_false(
    new_filter_condition(rlang::expr(chr == 1 | 123 < pos & pos < 234 & pval < .05),
                         finterface) |>
      is_single_genomic_range_block()
  )
  expect_false(
    new_filter_condition(rlang::expr(chr == 1 & 123 < pos & pos < 234 & pval < .05
                                     | chr == 2 & 21 < pos & pos < 42),
                         finterface) |>
      is_single_genomic_range_block()
  )
  expect_false(
    new_filter_condition(rlang::expr(chr == 1 & 123 < pos & pos < 234
                                     | chr == 2 & 21 < pos & pos < 42 & pval < .01),
                         finterface) |>
      is_single_genomic_range_block()
  )
  expect_false(
    new_filter_condition(rlang::expr(chr == 1 & 123 < pos & pos < 234 & pval < .05
                                     | chr == 2 & 21 < pos & pos < 42 & pval < .01),
                         finterface) |>
      is_single_genomic_range_block()
  )
})

test_that("Multiple genomic range-other condition combinations can be handled", {
  finterface <- local_rsid_summary_stats_interface()
  expect_equal(
    new_filter_condition(rlang::expr((chr == 1 & 123 < pos & pos < 234 & pval < .05)
                                     | (chr == 2 & 21 < pos & pos < 42 & pval < .01)),
                         finterface) |>
      fcondition_to_awk(),
    "awk 'BEGIN{
  OFS = \",\"
} {
  if (NR == FNR) {
    rsid0[$3]=$1 OFS $2
  }
  else if (NR == FNR + 1) {
    rsid1[$3]=$1 OFS $2
  }
  else {
    if ($1 in rsid0) {
      if ($5 < 0.05) {
        print rsid0[$1] OFS $0
      }
    }
    else if ($1 in rsid1) {
      if ($5 < 0.01) {
        print rsid1[$1] OFS $0
      }
    }
  }
}' FS=\"\\t\" <(tabix ~/rcp_storage/common/Users/abadreddine/data/dbSNP/GCF_000001405.40.gz NC_000001.11:124-233) <(tabix ~/rcp_storage/common/Users/abadreddine/data/dbSNP/GCF_000001405.40.gz NC_000002.12:22-41) FS=\",\" data.csv"
  )
})

test_that("Genomic ranges are correctly identified", {
  finterface <- local_rsid_summary_stats_interface()
  expect_equal(
    new_filter_condition(rlang::expr(chr == 1 & pos < 123), finterface) |>
      attr("genomic_range"),
    data.table::data.table(chr = "1", start = NA_real_, end = 122)
  )
})

test_that("Genomic ranges are correctly structured", {
  finterface <- local_rsid_summary_stats_interface()
  expect_equal(
    new_filter_condition(rlang::expr(pos < 123), finterface) |>
      attr("genomic_range"),
    data.table::data.table(chr = as.character(c(1:22, "X", "Y", "MT")),
                           start = NA_real_, end = 122)
  )
})
