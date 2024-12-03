test_that("Dummy csv initialization works", {
  expect_false(file.exists(filename = "data.csv"))
  local_csv_file(filename = "data.csv")
  expect_true(file.exists(filename = "data.csv"))
})
test_that("Dummy csv cleanup works", {
  expect_false(file.exists(filename = "data.csv"))
})

test_that("Non-quoted values work properly", {
  local_csv_file(filename = "data.csv")
  test <- data.table::fread("data.csv", quote = "")
  expect_false(any(stringr::str_detect(names(test), "\"")))
  expect_equal(test,
               dummy_dt())
})

test_that("Quoted values work", {
  local_csv_file(filename = "data.csv", quote = TRUE)
  test <- data.table::fread("data.csv", quote = "")
  expect_true(all(stringr::str_detect(names(test), "\"")))
  expect_equal(test,
               dummy_dt(values_are_quoted = TRUE))
})

test_that("Prefixes work", {
  local_csv_file(filename = "data.csv", prefix = c(char = "chr"))
  test <- data.table::fread("data.csv", quote = "")
  expect_equal(test,
               dummy_dt(prefix = c(char = "chr")))
})
