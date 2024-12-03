# test_that("Base file reading works", {
#   expect_true(
#     grepl("^zcat ", cl_read_dbgap_file_template("test", "mvp"))
#   )
#   expect_true(
#     grepl("^awk ", cl_read_dbgap_file_template("test", "charge"))
#   )
# })

test_that("Seeing whether a file is gzipped works", {
  expect_true(
    is_gzipped("test.txt.gz")
  )
  expect_false(
    is_gzipped("test.txt")
  )
})
