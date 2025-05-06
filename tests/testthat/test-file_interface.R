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

test_that("Non-quoted values work properly", {
  local_csv_file(filename = "data.csv")
  finterface <- new_file_interface("data.csv")
  expect_equal(finterface$column_info$quoted,
               rep(FALSE, 2))
  expect_false(finterface$gzipped)
})

test_that("Quoted values work", {
  local_csv_file(filename = "data.csv", quote = TRUE)
  finterface <- new_file_interface("data.csv")
  expect_equal(finterface$column_info$quoted,
               c(TRUE, FALSE))
  expect_false(finterface$gzipped)
})

test_that("Gzipped files are handled", {
  local_csv_file("data.csv.gz")
  finterface <- new_file_interface("data.csv.gz")
  expect_equal(finterface$column_info$quote,
               rep(FALSE, 2))
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

  local_csv_file("data.tsv", sep = "\t")
  finterface <- new_file_interface("data.tsv")
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
  expect_equal(finterface[char == "a"],
               dummy_dt()[char == "a"])
  expect_equal(finterface[num == 3],
               dummy_dt()[num == 3])

  fquoted <- local_file_interface("data_quoted.csv", quote = TRUE)
  expect_equal(fquoted[num < 3],
               dummy_dt()[num < 3])
  expect_equal(fquoted[num > 3],
               dummy_dt()[num > 3])
  expect_equal(fquoted[3 <= num],
               dummy_dt()[3 <= num])
  expect_equal(fquoted[3 >= num],
               dummy_dt()[3 >= num])
  expect_equal(fquoted[char == "a"],
               dummy_dt()[char == "a"])
  expect_equal(fquoted[num == 3],
               dummy_dt()[num == 3])
})

test_that("Set belonging works", {
  finterface <- local_file_interface("data.csv")
  expect_equal(finterface[char %in% "a"],
               dummy_dt()[char %in% "a"])
  expect_equal(finterface[char %in% c("a", "b")],
               dummy_dt()[char %in% c("a", "b")])
  expect_equal(finterface[char %in% c("a", 1)],
               dummy_dt()[char %in% c("a", 1)])
  expect_equal(finterface[num %in% 1],
               dummy_dt()[num %in% 1])
  expect_equal(finterface[num %in% c("a", 1)],
               dummy_dt()[num %in% c("a", 1)])
  expect_equal(finterface[num %in% 1:4],
               dummy_dt()[num %in% 1:4])
})

test_that("Combining conditions works", {
  finterface <- local_file_interface("data.csv")
  expect_equal(finterface[3 <= num & num <= 5],
               dummy_dt()[3 <= num & num <= 5])
  expect_equal(finterface[1 < num & num < 3 | 5 < num],
               dummy_dt()[1 < num & num < 3 | 5 < num])

  expect_equal(finterface[3 <= num & num <= 5],
               dummy_dt()[3 <= num & num <= 5])
  expect_equal(finterface[1 < num & num < 3 | 5 < num],
               dummy_dt()[1 < num & num < 3 | 5 < num])

  fquoted <- local_file_interface("data_quoted.csv", quote = TRUE)
  expect_equal(fquoted[3 <= num & num <= 5],
               dummy_dt()[3 <= num & num <= 5])
  expect_equal(fquoted[1 < num & num < 3 | 5 < num],
               dummy_dt()[1 < num & num < 3 | 5 < num])

  expect_equal(fquoted[3 <= num | num %in% c("a", 1) & num <= 5][order(char)],
               dummy_dt()[3 <= num | num %in% c("a", 1) & num <= 5])
  expect_equal(fquoted[1 < num | num %in% 1:4 & num < 3 | 5 < num][order(char)] |>
                 unique(),
               dummy_dt()[1 < num | num %in% 1:4 & num < 3 | 5 < num])
})

test_that("Full command line with gz detection works", {
  expect_equal(
    new_file_interface(
      gz_filename
    )[chr == 10, return_only_cmd = TRUE],
    paste0("zcat ~/Databases/MVP/release/Submissions/sub20221024/CART.EUR.MVP.NatMed2022.txt.gz  | ",
           "awk 'BEGIN{\n",
           "  FS = \"\\t\"\n",
           "  OFS = FS\n",
           "} {\n",
           "  if ($2 == 10) {\n",
           "    print $0\n",
           "  }\n",
           "}' ")
  )
})

test_that("Prefixes are handled correctly", {
  finterface_b <- local_summary_stats_interface(
    "data.csv",
    chr    = 1,
    prefix = list(chr = "chr")
  )
  finterface_q <- local_summary_stats_interface(
    "data_q.csv",
    chr    = 1,
    prefix = list(chr = "chr"),
    values_are_quoted = TRUE
  )
  finterface_gz <- local_summary_stats_interface(
    "data.csv.gz",
    chr    = 1,
    prefix = list(chr = "chr")
  )

  for (finterface in list(finterface_b, finterface_q, finterface_gz)) {
    data_to_check <- head(finterface, 500)
    expect_equal(
      check_single_column_prefix(prefixes = "chr", data_to_check$chr),
      "chr"
    )
    expect_equal(
      check_single_column_prefix(prefixes = "incorrect", data_to_check$chr),
      NULL
    )

    expect_equal(
      finterface[chr == 1],
      finterface[chr == "chr1"]
    )
  }
})
