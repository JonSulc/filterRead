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
  expect_equal(finterface$column_info$quoted_values,
               list(char = FALSE, num = FALSE))
  expect_false(finterface$gzipped)
})

test_that("Quoted values work", {
  local_csv_file(filename = "data.csv", quote = TRUE)
  finterface <- new_file_interface("data.csv") |>
    suppressWarnings()
  expect_equal(finterface$column_info$quoted_values,
               list(char = TRUE, num = FALSE))
  expect_false(finterface$gzipped)
})

test_that("Gzipped files are handled", {
  local_csv_file("data.csv.gz")
  finterface <- new_file_interface("data.csv.gz")
  expect_equal(finterface$column_info$quoted_values,
               list(char = FALSE, num = FALSE))
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

test_that("Flipping column names works", {
  expect_equal(
    flip_column_names(setNames(1:3, letters[1:3])),
    setNames(as.list(letters[1:3]), 1:3)
  )

  expect_equal(
    flip_column_names(list(test = letters[1:3])),
    list(a = "test", b = "test", c = "test")
  )
  expect_equal(
    flip_column_names(list(test1 = letters[1:3],
                           test2 = letters[4:6])),
    list(a = "test1", b = "test1", c = "test1",
         d = "test2", e = "test2", f = "test2")
  )
})

test_that("Swapping in the column names works", {
  column_names <- list(
    chr = c("chr", "char"),
    pos = c("num", "position")
  )
  expect_equal(
    swap_in_column_names(
      list(test1 = "$1",
           test2 = "$2")
    ),
    list(test1 = "$1",
         test2 = "$2")
  )
  expect_equal(
    swap_in_column_names(
      list(test1 = "$1",
           test2 = "$2"),
      column_names
    ),
    list(test1 = "$1",
         test2 = "$2")
  )
  expect_equal(
    swap_in_column_names(
      list(char  = "$1",
           test2 = "$2"),
      column_names
    ),
    list(chr   = "$1",
         test2 = "$2")
  )
  expect_equal(
    swap_in_column_names(
      list(char  = "$1",
           num   = "$2"),
      column_names
    ),
    list(chr = "$1",
         pos = "$2")
  )
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

test_that("Quoting works properly", {
  expect_equal(
    check_quotes(1, FALSE, base_enquote = TRUE),
    1
  )
  expect_equal(
    check_quotes("a", FALSE, base_enquote = TRUE),
    "\"a\""
  )
  expect_equal(
    check_quotes(1, TRUE, base_enquote = FALSE),
    "\"1\""
  )
  expect_equal(
    check_quotes("a", TRUE, base_enquote = TRUE),
    "\"\\\"a\\\"\""
  )
})

test_that("Full command line with gz detection works", {
  expect_equal(
    new_file_interface(
      gz_filename,
      column_names = list(chr = "Chromsome")
    )[chr == 10, return_only_cmd = TRUE],
    list("zcat ~/Databases/MVP/release/Submissions/sub20221024/CART.EUR.MVP.NatMed2022.txt.gz | awk '$2 == 10'")
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
    expect_equal(
      check_single_column_prefix(finterface,
                                 file_colname = "$1",
                                 prefix       = "chr"),
      "chr"
    )
    expect_equal(
      check_single_column_prefix(finterface,
                                 file_colname = "$1",
                                 prefix       = "incorrect"),
      NULL
    )

    expect_equal(
      get_prefixes(
        finterface,
        file_colnames = list(chr = "$1"),
        prefixes      = list(chr = "chr")
      ),
      list(chr = "chr")
    )
    expect_equal(
      get_prefixes(
        finterface,
        file_colnames = list(chr = "$1"),
        prefixes      = list(chr = "chr"),
        nrows_to_check = NULL
      ),
      list(chr = "chr")
    )
    expect_equal(
      get_prefixes(
        finterface,
        file_colnames = list(chr = "$1"),
        prefixes      = list(chr = "incorrect")
      ),
      list() |>
        setNames(character())
    )

    expect_equal(
      finterface[chr == 1],
      finterface[chr == "chr1"]
    )
  }
})
