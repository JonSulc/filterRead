test_that("Reading JSON file works", {
  expect_no_error(
    read_json_file(
      "~/rcp_storage/common/Users/sulc/data/dbsnp/refsnp-chr22.json.bz2",
      nlines = 10
    )
  )
  expect_equal(
    read_json_file(
      "~/rcp_storage/common/Users/sulc/data/dbsnp/refsnp-chr22.json.bz2",
      nlines = 10
    )[0],
    data.table::data.table(
      build = character(0),
      chr   = character(0),
      pos   = numeric(0),
      ref   = character(0),
      alt   = character(0)
    )
  )
})
