test_that("Base column_info works", {
  local_summary_stats()
  finterface <- structure(list(filename = "data.csv",
                               gzipped  = FALSE,
                               sep      = ","),
                          class = c("file_interface", "list"))
  expect_equal(
    get_base_column_info(finterface)[
      ,
      .SD,
      .SDcols = -"input_name"
    ],
    data.table::data.table(
      standard_name     = c("chr", "pos", "ref", "alt", "effect", "pval"),
      possible_prefixes = c("chr", rep(NA_character_, 5)),
      pattern           = NA_character_,
      regex             = NA_character_,
      encoded_names     = list(),
      delimiter         = NA_character_,
      input_index       = 1:6,
      bash_index        = paste0("$", 1:6)
    )
  )
})

test_that("Regex matching works", {
  local_summary_stats(encode_columns = TRUE)
  finterface <- structure(list(filename = "data.csv",
                               gzipped  = FALSE,
                               sep      = ","),
                          class = c("file_interface", "list"))
  column_info <- get_base_column_info(finterface)
  data_to_check <- head(finterface, 500)

  column_info <- filter_regex_matches(column_info, data_to_check)

  expect_true(
    all(grepl(column_info$regex[1], data_to_check[[1]]))
  )
})

test_that("Unquoted column detection works", {
  local_summary_stats(values_are_quoted = FALSE, random_names = FALSE)
  finterface <- structure(list(filename = "data.csv",
                               gzipped  = FALSE,
                               sep      = ","),
                          class = c("file_interface", "list"))
  expect_equal(are_values_quoted(finterface),
               c(chr = FALSE, pos = FALSE, ref = FALSE, alt = FALSE,
                 effect = FALSE, pval = FALSE))
})
test_that("Quoted column detection works", {
  local_summary_stats(values_are_quoted = TRUE, random_names = FALSE)
  finterface <- structure(list(filename = "data.csv",
                               gzipped  = FALSE,
                               sep      = ","),
                          class = c("file_interface", "list"))
  expect_equal(are_values_quoted(finterface),
               c(chr = FALSE, pos = FALSE, ref = TRUE, alt = TRUE,
                 effect = FALSE, pval = FALSE))
})

test_that("Prefix detection works", {
  summary_stats <- dummy_summary_stats()
  expect_null(check_single_column_prefix(prefixes = "chr", summary_stats[[1]]))

  summary_stats <- dummy_summary_stats(prefixes = list(chr = "chr"))
  expect_equal(check_single_column_prefix(prefixes = "chr", summary_stats[[1]]),
               "chr")
})

test_that("Encoded columns are detected", {
  local_summary_stats()
  finterface <- structure(list(filename = "data.csv",
                               gzipped  = FALSE,
                               sep      = ","),
                          class = c("file_interface", "list"))
  column_info <- get_base_column_info(finterface)
  expect_equal(expand_encoded_columns(column_info),
               column_info)
  suppressMessages(withr::deferred_run())

  local_summary_stats(encode_columns = TRUE)
  finterface <- structure(list(filename = "data.csv",
                               gzipped  = FALSE,
                               sep      = ","),
                          class = c("file_interface", "list"))
  column_info <- get_base_column_info(finterface)
  data_to_check <- head(finterface, 500)
  column_info <- filter_regex_matches(column_info, data_to_check)

  add_quoted_column(column_info, finterface)

  add_prefix_column(column_info, data_to_check)

  add_encoding_columns(column_info)
  expect_false(any(is.na(column_info$input_name)))
  expect_true(any(is.na(expand_encoded_columns(column_info)$input_name)))
  expect_true(all(c("chr", "pos") %in% expand_encoded_columns(column_info)$standard_name))
})
