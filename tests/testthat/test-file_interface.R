gz_filename <- "~/Databases/MVP/release/Submissions/sub20221024/CART.EUR.MVP.NatMed2022.txt.gz"

test_that("Initialization works", {
  expect_no_error(
    new_file_interface(gz_filename)
  )
  expect_error(
    new_file_interface("test")
  )
  expect_error(
    new_file_interface(123)
  )
  finterface <- new_file_interface(gz_filename)
  expect_equal(class(finterface),
               c("file_interface", "character"))
})

test_that("Validation works", {
  finterface <- new_file_interface(gz_filename)
  expect_no_error(validate_file_interface)

  expect_error(validate_file_interface(gz_filename))
  expect_error(
    validate_file_interface(structure(finterface, gzipped = 1))
  )
  expect_error(
    validate_file_interface(structure(finterface, values_are_quoted = "test"))
  )
})

test_that("Non-quoted values work properly", {
  local_csv_file(filename = "data.csv")
  finterface <- new_file_interface("data.csv") |>
    suppressWarnings()
  expect_false(all(finterface$values_are_quoted))
  expect_false(finterface$gzipped)
})

test_that("Quoted values work", {
  local_csv_file(filename = "data.csv", quote = TRUE)
  finterface <- new_file_interface("data.csv") |>
    suppressWarnings()
  expect_true(any(finterface$values_are_quoted))
  expect_false(finterface$gzipped)
})

test_that("Gzipped files are handled", {
  local_csv_file("data.csv.gz")
  finterface <- new_file_interface("data.csv.gz")
  expect_false(all(finterface$values_are_quoted))
  expect_true(finterface$gzipped)
})

test_that("Head works", {
  local_csv_file("data.csv")
  finterface <- new_file_interface("data.csv")
  expect_equal(head(finterface, 1),
               head(dummy_dt(), 1))
  expect_equal(head(finterface, 2),
               head(dummy_dt(), 2))

  local_csv_file("data.csv.gz")
  finterface <- new_file_interface("data.csv.gz")
  expect_equal(head(finterface, 1),
               head(dummy_dt(), 1))
  expect_equal(head(finterface, 2),
               head(dummy_dt(), 2))

  local_csv_file("data_quoted.csv", quote = TRUE)
  finterface <- new_file_interface("data_quoted.csv") |>
    suppressWarnings()
  expect_equal(head(finterface, 1),
               head(dummy_dt(), 1))
  expect_equal(head(finterface, 2),
               head(dummy_dt(), 2))
})

test_that("Math conditions work", {
  finterface <- local_file_interface("data.csv")
  expect_equal(finterface[num < 3],
               dummy_dt()[num < 3])
  expect_equal(finterface[num > 3],
               dummy_dt()[num > 3])
  expect_equal(finterface[3 <= num],
               dummy_dt()[3 <= num])
  expect_equal(finterface[3 >= num],
               dummy_dt()[3 >= num])
})

test_that("Combining conditions works", {
  finterface <- local_file_interface("data.csv")
  expect_equal(finterface[3 <= num & num <= 5],
               dummy_dt()[3 <= num & num <= 5])
  expect_equal(finterface[1 < num & num < 3 | 5 < num],
               dummy_dt()[1 < num & num < 3 | 5 < num])
})
