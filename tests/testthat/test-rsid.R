test_that("RSID parsing required is correctly detected", {
  finterface <- local_summary_stats_interface()
  expect_false(needs_rsid_matching(finterface))
  finterface_rn <- local_summary_stats_interface(
    "data_rn.csv",
    random_names = TRUE
  )
  expect_false(needs_rsid_matching(finterface_rn))

  finterface_rsid <- local_rsid_summary_stats_interface("data_rsid.csv")
  expect_true(needs_rsid_matching(finterface_rsid))
  finterface_rsid_rn <- local_rsid_summary_stats_interface(
    "data_rsid_rn.csv",
    random_names = TRUE
  )
  expect_true(needs_rsid_matching(finterface_rsid_rn))

  finterface_enc <- local_summary_stats_interface(
    "data_enc.csv",
    encode_columns = TRUE
  )
  expect_false(needs_rsid_matching(finterface_enc))
  finterface_enc_rn <- local_summary_stats_interface(
    "data_enc_rn.csv",
    random_names   = TRUE,
    encode_columns = TRUE
  )
  expect_false(needs_rsid_matching(finterface_enc_rn))
})

test_that("tabix process substitution works", {
  expect_error(
    get_tabix_process_substitution(1:2, 123, 234, "dbsnp_file.vcf.gz")
  )
  expect_error(
    get_tabix_process_substitution(1, 123:124, 234, "dbsnp_file.vcf.gz")
  )
  expect_error(
    get_tabix_process_substitution(1, 123, 234:235, "dbsnp_file.vcf.gz")
  )

  expect_equal(
    get_tabix_process_substitution(1, 123, 234, "dbsnp_file.vcf.gz"),
    "<(tabix dbsnp_file.vcf.gz 1:123-234)"
  )
  expect_equal(
    get_tabix_process_substitution(1, c(123, 456), c(234, 567), "dbsnp_file.vcf.gz"),
    "<(tabix dbsnp_file.vcf.gz 1:123-234 1:456-567)"
  )
  expect_equal(
    get_tabix_process_substitution(1:2, c(123, 456), c(234, 567), "dbsnp_file.vcf.gz"),
    "<(tabix dbsnp_file.vcf.gz 1:123-234 2:456-567)"
  )
})

test_that("File reading works", {
  finterface <- local_rsid_summary_stats_interface(build = "b38")
  expect_true(needs_rsid_matching(finterface))
  expect_no_error(head(finterface))
  expect_true(all(finterface[pval < .05]$pval < .05))
  expect_equal(
    colnames(finterface[pval < .05]),
    c("rsid", "ref", "alt", "effect", "pval")
  )
  expect_equal(
    colnames(finterface[pval < .05 & chr == 1 & 123 <= pos & pos <= 123456]),
    c("chr", "pos", "rsid", "ref", "alt", "effect", "pval")
  )
  expect_equal(
    finterface[pval < .05 & chr == 1 & 123 <= pos & pos <= 12345,
      return_only_cmd = TRUE
    ],
    "awk 'BEGIN{
  FS = \",\"
  OFS = \",\"
  header_skipped = 0
}
{
  if (NR == FNR) {
    rsid0[$3]=$1 OFS $2
  }
  else {
    if (FNR == 1) next
      if ($1 in rsid0) {
      if ($5 < 0.05) {
        print rsid0[$1] OFS $0
      }
    }
  }
}' FS=\"\\t\" <(tabix /home/sulc/rcp_storage/common/Users/sulc/data/dbsnp/00-common_all_b38.vcf.gz 1:123-12345) FS=\",\" data.csv"
  )
})

test_that("Genomic blocks are correctly identified", {
  finterface <- local_rsid_summary_stats_interface()
  expect_true(
    new_filter_condition(
      rlang::quo(pval < .05),
      finterface
    ) |>
      is_single_genomic_block()
  )
  expect_true(
    new_filter_condition(
      rlang::quo(chr == 1),
      finterface
    ) |>
      is_single_genomic_block()
  )
  expect_true(
    new_filter_condition(
      rlang::quo(chr == 1 & pval < .05),
      finterface
    ) |>
      is_single_genomic_block()
  )
  expect_false(
    new_filter_condition(
      rlang::quo(chr == 1 | pval < .05),
      finterface
    ) |>
      is_single_genomic_block()
  )
  expect_false(
    new_filter_condition(
      rlang::quo(chr == 1 & pos < 123 | pval < .05),
      finterface
    ) |>
      is_single_genomic_block()
  )
  expect_false(
    new_filter_condition(
      rlang::quo(chr == 1 & 123 < pos & pos < 234 | pval < .05),
      finterface
    ) |>
      is_single_genomic_block()
  )
  expect_true(
    new_filter_condition(
      rlang::quo(chr == 1 & 123 < pos & pos < 234 & pval < .05),
      finterface
    ) |>
      is_single_genomic_block()
  )
  expect_true(
    new_filter_condition(
      rlang::quo(chr == 1 & 123 < pos & pos < 234 &
        pval < .05 & ref == "A"),
      finterface
    ) |>
      is_single_genomic_block()
  )
  expect_true(
    new_filter_condition(
      rlang::quo(chr == 1 & 123 < pos & pos < 234 |
        chr == 2 & 21 < pos & pos < 42),
      finterface
    ) |>
      is_single_genomic_block()
  )

  expect_false(
    new_filter_condition(
      rlang::quo(chr == 1 & 123 < pos | pos < 234 & pval < .05),
      finterface
    ) |>
      is_single_genomic_block()
  )
  expect_false(
    new_filter_condition(
      rlang::quo(chr == 1 | 123 < pos & pos < 234 & pval < .05),
      finterface
    ) |>
      is_single_genomic_block()
  )
  expect_false(
    new_filter_condition(
      rlang::quo(chr == 1 & 123 < pos & pos < 234 & pval < .05 |
        chr == 2 & 21 < pos & pos < 42),
      finterface
    ) |>
      is_single_genomic_block()
  )
  expect_false(
    new_filter_condition(
      rlang::quo(chr == 1 & 123 < pos & pos < 234 |
        chr == 2 & 21 < pos & pos < 42 & pval < .01),
      finterface
    ) |>
      is_single_genomic_block()
  )
  expect_false(
    new_filter_condition(
      rlang::quo(chr == 1 & 123 < pos & pos < 234 & pval < .05 |
        chr == 2 & 21 < pos & pos < 42 & pval < .01),
      finterface
    ) |>
      is_single_genomic_block()
  )
})

test_that("Multiple genomic range-other condition combinations can be handled", {
  finterface <- local_rsid_summary_stats_interface(build = "b38")
  expect_equal(
    new_filter_condition(
      rlang::quo(
        (chr == 1 & 123 < pos & pos < 234 & pval < .05) |
          (chr == 2 & 21 < pos & pos < 42 & pval < .01)
      ),
      finterface
    ) |>
      fcondition_to_awk(return_only_cmd = TRUE),
    "awk 'BEGIN{
  FS = \",\"
  OFS = \",\"
  header_skipped = 0
}
{
  if (NR == FNR) {
    rsid0[$3]=$1 OFS $2
  }
  else if (NR == FNR + 1) {
    rsid1[$3]=$1 OFS $2
  }
  else {
    if (FNR == 1) next
      if ($1 in rsid0) {
      if (($5 < 0.05)) {
        print rsid0[$1] OFS $0
      }
    }
    else if ($1 in rsid1) {
      if (($5 < 0.01)) {
        print rsid1[$1] OFS $0
      }
    }
  }
}' FS=\"\\t\" <(tabix /home/sulc/rcp_storage/common/Users/sulc/data/dbsnp/00-common_all_b38.vcf.gz 1:124-233) <(tabix /home/sulc/rcp_storage/common/Users/sulc/data/dbsnp/00-common_all_b38.vcf.gz 2:22-41) FS=\",\" data.csv"
  )
})

test_that("Genomic ranges are correctly identified", {
  finterface <- local_rsid_summary_stats_interface(build = "b38")
  expect_equal(
    new_filter_condition(rlang::quo(chr == 1 & pos < 123), finterface) |>
      genomic_regions(),
    new_genomic_regions(chr = "1", start = NA_real_, end = 122, build = "b38")
  )
})

test_that("Genomic ranges are correctly structured", {
  finterface <- local_rsid_summary_stats_interface(build = "b38")
  expect_equal(
    new_filter_condition(rlang::quo(pos < 123), finterface) |>
      genomic_regions(),
    new_genomic_regions(
      end = 122,
      build = "b38"
    )
  )
})

test_that("RSID-based position filtering uses the correct build reference", {
  rsidt <- data.table::data.table(
    rsid = c("rs429358", "rs7412", "rs12913832"),
    ref = c("T", "C", "A"),
    alt = c("C", "T", "G")
  )
  local_csv_file("data.csv", rsidt)

  rsid37 <- new_file_interface("data.csv", build = "b37")
  rsid38 <- new_file_interface("data.csv", build = "b38")

  # Filtering based on b37 coordinates
  expect_equal(
    rsid37[chr == 19 & 45e6 < pos],
    data.table::data.table(
      chr = 19,
      pos = c(45411941, 45412079),
      rsid = c("rs429358", "rs7412"),
      ref = c("T", "C"),
      alt = c("C", "T")
    )
  )
  expect_warning(
    rsid38_results <- rsid38[chr == 19 & 45e6 < pos],
    "File .* has size 0[.]"
  )
  expect_equal(
    rsid38_results,
    data.table::data.table()
  )

  expect_equal(
    rsid37[chr == 15 & pos == 28365618],
    data.table::data.table(
      chr = 15,
      pos = 28365618,
      rsid = "rs12913832",
      ref = "A",
      alt = "G"
    )
  )
  expect_warning(
    rsid38_results <- rsid38[chr == 15 & pos == 28365618],
    "File .* has size 0[.]"
  )
  expect_equal(
    rsid38_results,
    data.table::data.table()
  )

  # Filtering based on b38 coordinates
  expect_equal(
    rsid38[chr == 19 & pos < 45e6],
    data.table::data.table(
      chr = 19,
      pos = c(44908684, 44908822),
      rsid = c("rs429358", "rs7412"),
      ref = c("T", "C"),
      alt = c("C", "T")
    )
  )
  expect_warning(
    rsid37_results <- rsid37[chr == 19 & pos < 45e6],
    "File .* has size 0[.]"
  )
  expect_equal(
    rsid37_results,
    data.table::data.table()
  )

  expect_equal(
    rsid38[chr == 15 & pos == 28120472],
    data.table::data.table(
      chr = 15,
      pos = 28120472,
      rsid = "rs12913832",
      ref = "A",
      alt = "G"
    )
  )
  expect_warning(
    rsid37_results <- rsid37[chr == 15 & pos == 28120472],
    "File .* has size 0[.]"
  )
  expect_equal(
    rsid37_results,
    data.table::data.table()
  )
})
