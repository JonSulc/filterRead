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
  expect_true(colnames(finterface[pval < .05, rsid_condition = data.table::data.table(chr = 1, start = 123, end = 12345)]),
              c("chr", "pos", "rsid", "ref", "alt", "effect", "pval"))
})
