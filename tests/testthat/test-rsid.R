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
