test_that("is_gzipped detects .gz extension", {
  expect_true(is_gzipped("file.txt.gz"))
  expect_true(is_gzipped("path/to/file.csv.gz"))
  expect_false(is_gzipped("file.txt"))
  expect_false(is_gzipped("file.gzip"))
  expect_false(is_gzipped("file.gz.txt"))
})

test_that("is_file_interface checks class correctly", {
  fi <- structure(list(), class = c("file_interface", "character"))
  expect_true(is_file_interface(fi))
  expect_false(is_file_interface(list()))
  expect_false(is_file_interface("string"))
  expect_false(is_file_interface(NULL))
})

test_that("is.filter_condition checks class correctly", {
  fc <- structure(list(), class = "filter_condition")
  expect_true(is.filter_condition(fc))
  expect_false(is.filter_condition(list()))
  expect_false(is.filter_condition(NULL))
})
