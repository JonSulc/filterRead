test_that("Encoded columns are correctly detected", {
  finterface <- local_summary_stats_interface() |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  finterface_enc <- local_summary_stats_interface(
    "encoded.csv",
    encode_columns = summary_stats_standard_names_dt[
      list(input_name = "MarkerName", delimiter = "-c|:|-"),
      on = c("input_name", "delimiter")
    ]
  ) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_true(all(is.na(finterface$column_info$regex)))
  expect_true(all(sapply(finterface$column_info$encoded_names, \(x) length(x) == 0)))
  expect_true(all(is.na(finterface$column_info$delimiter)))
  expect_true(all(is.na(finterface$column_info$encoded_column_index)))
  expect_equal(
    finterface_enc$column_info[
      c("build", "chr", "pos"),
      .(regex, delimiter, bash_index, encoding_column, split_encoding_column, recode_columns),
      on = "name"
    ],
    data.table::data.table(
      regex = "^(b3[6-8])-c([^:]{1,2}):([0-9]+)-[0-9]+$",
      delimiter = "-c|:|-",
      bash_index = sprintf("encoded1[%i]", 1:3),
      encoding_column = "MarkerName",
      split_encoding_column = "split($1, encoded1, \"-c|:|-\")",
      recode_columns = "$1 = encoded1[1] OFS encoded1[2] OFS encoded1[3]"
    )
  )
  expect_equal(
    get_awk_column_arrays(finterface_enc),
    list(
      before_if = character(0),
      after_if = c(
        "split($1, encoded1, \"-c|:|-\")",
        "$1 = encoded1[1] OFS encoded1[2] OFS encoded1[3]"
      )
    )
  )


  expect_true(finterface_enc$column_info[
    !c("MarkerName", "build", "chr", "pos"),
    all(is.na(regex)),
    on = "name"
  ])
  expect_true(finterface_enc$column_info[
    !c("MarkerName", "build", "chr", "pos"),
    all(is.na(delimiter)),
    on = "name"
  ])
  expect_true(finterface_enc$column_info[
    !c("MarkerName", "build", "chr", "pos"),
    all(is.na(split_encoding_column)),
    on = "name"
  ])
  expect_true(finterface_enc$column_info[
    !c("MarkerName", "build", "chr", "pos"),
    all(is.na(recode_columns)),
    on = "name"
  ])
  expect_true(finterface_enc$column_info[
    !c("MarkerName"),
    all(sapply(encoded_names, \(x) length(x) == 0)),
    on = "name"
  ])
  expect_true(finterface_enc$column_info[
    !c("MarkerName"),
    all(is.na(encoded_column_index)),
    on = "name"
  ])
  expect_true(finterface_enc$column_info[
    !c("build", "chr", "pos"),
    all(is.na(encoding_column)),
    on = "name"
  ])
  expect_equal(
    finterface_enc$column_info[
      "MarkerName",
      .(regex, encoded_names, delimiter, encoded_column_index, split_encoding_column, recode_columns),
      on = "name"
    ],
    data.table::data.table(
      regex                 = "^(b3[6-8])-c([^:]{1,2}):([0-9]+)-[0-9]+$",
      encoded_names         = list(c("build", "chr", "pos")),
      delimiter             = "-c|:|-",
      encoded_column_index  = 1,
      split_encoding_column = "split($1, encoded1, \"-c|:|-\")",
      recode_columns        = "$1 = encoded1[1] OFS encoded1[2] OFS encoded1[3]"
    )
  )
  expect_equal(
    finterface_enc$column_info[
      c("build", "chr", "pos"),
      .(regex, delimiter, bash_index, encoding_column, split_encoding_column, recode_columns),
      on = "name"
    ],
    data.table::data.table(
      regex = "^(b3[6-8])-c([^:]{1,2}):([0-9]+)-[0-9]+$",
      delimiter = "-c|:|-",
      bash_index = sprintf("encoded1[%i]", 1:3),
      encoding_column = "MarkerName",
      split_encoding_column = "split($1, encoded1, \"-c|:|-\")",
      recode_columns = "$1 = encoded1[1] OFS encoded1[2] OFS encoded1[3]"
    )
  )
  expect_equal(
    get_awk_column_arrays(finterface_enc),
    list(
      before_if = character(0),
      after_if = c(
        "split($1, encoded1, \"-c|:|-\")",
        "$1 = encoded1[1] OFS encoded1[2] OFS encoded1[3]"
      )
    )
  )
})

test_that("Encoded columns are correctly parsed and loaded", {
  finterface_enc <- local_summary_stats_interface(
    "encoded.csv",
    encode_columns = summary_stats_standard_names_dt[
      list(input_name = "MarkerName", delimiter = "-c|:|-"),
      on = c("input_name", "delimiter")
    ],
    random_names = FALSE
  ) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  data_raw <- data.table::fread("encoded.csv")
  expect_equal(
    names(data_raw),
    c("MarkerName", "ref", "alt", "effect", "pval")
  )

  expect_equal(
    column_names(finterface_enc),
    c("build", "chr", "pos", "ref", "alt", "effect", "pval")
  )
  expect_equal(
    names(head(finterface_enc)),
    c("build", "chr", "pos", "ref", "alt", "effect", "pval")
  )
  expect_true(
    finterface_enc[chr == 1][, all(chr == 1)]
  )
})
