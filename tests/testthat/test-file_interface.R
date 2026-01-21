test_that("Initialization works", {
  local_csv_file("data.csv.gz")
  expect_no_error(
    new_file_interface("data.csv.gz") |>
      suppressMessages() |>
      withr::with_output_sink(new = "/dev/null")
  )
  expect_error(
    new_file_interface("test")
  )
  expect_error(
    new_file_interface(123)
  )
  finterface <- new_file_interface("data.csv.gz") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    class(finterface),
    c("file_interface", "character")
  )
})

test_that("Non-quoted values work properly", {
  local_csv_file(filename = "data.csv")
  finterface <- new_file_interface("data.csv") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    finterface$column_info$quoted,
    rep(FALSE, 2)
  )
  expect_false(finterface$gzipped)
})

test_that("Quoted values work", {
  local_csv_file(filename = "data.csv", quote = TRUE)
  finterface <- new_file_interface("data.csv") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    finterface$column_info$quoted,
    c(TRUE, FALSE)
  )
  expect_false(finterface$gzipped)
})

test_that("Gzipped files are handled", {
  local_csv_file("data.csv.gz")
  finterface <- new_file_interface("data.csv.gz") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    finterface$column_info$quote,
    rep(FALSE, 2)
  )
  expect_true(finterface$gzipped)
})

test_that("Head works", {
  local_csv_file("data.csv")
  finterface <- new_file_interface("data.csv") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    head(finterface, 1),
    head(dummy_dt(), 1)
  )
  expect_equal(
    head(finterface, 2),
    head(dummy_dt(), 2)
  )

  local_csv_file("data.csv.gz")
  finterface <- new_file_interface("data.csv.gz") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    head(finterface, 1),
    head(dummy_dt(), 1)
  )
  expect_equal(
    head(finterface, 2),
    head(dummy_dt(), 2)
  )

  local_csv_file("data_quoted.csv", quote = TRUE)
  finterface <- new_file_interface("data_quoted.csv") |>
    suppressWarnings() |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    head(finterface, 1),
    head(dummy_dt(), 1)
  )
  expect_equal(
    head(finterface, 2),
    head(dummy_dt(), 2)
  )

  local_csv_file("data.tsv", sep = "\t")
  finterface <- new_file_interface("data.tsv") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    head(finterface, 1),
    head(dummy_dt(), 1)
  )
  expect_equal(
    head(finterface, 2),
    head(dummy_dt(), 2)
  )
})

test_that("Math conditions work", {
  finterface <- local_file_interface("data.csv") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    finterface[num < 3],
    dummy_dt()[num < 3]
  )
  expect_equal(
    finterface[num > 3],
    dummy_dt()[num > 3]
  )
  expect_equal(
    finterface[3 <= num],
    dummy_dt()[3 <= num]
  )
  expect_equal(
    finterface[3 >= num],
    dummy_dt()[3 >= num]
  )
  expect_equal(
    finterface[char == "a"],
    dummy_dt()[char == "a"]
  )
  expect_equal(
    finterface[num == 3],
    dummy_dt()[num == 3]
  )

  fquoted <- local_file_interface("data_quoted.csv", quote = TRUE) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    fquoted[num < 3],
    dummy_dt()[num < 3]
  )
  expect_equal(
    fquoted[num > 3],
    dummy_dt()[num > 3]
  )
  expect_equal(
    fquoted[3 <= num],
    dummy_dt()[3 <= num]
  )
  expect_equal(
    fquoted[3 >= num],
    dummy_dt()[3 >= num]
  )
  expect_equal(
    fquoted[char == "a"],
    dummy_dt()[char == "a"]
  )
  expect_equal(
    fquoted[num == 3],
    dummy_dt()[num == 3]
  )
})

test_that("Set belonging works", {
  finterface <- local_file_interface("data.csv") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    finterface[char %in% "a"],
    dummy_dt()[char %in% "a"]
  )
  expect_equal(
    finterface[char %in% c("a", "b")],
    dummy_dt()[char %in% c("a", "b")]
  )
  expect_equal(
    finterface[char %in% c("a", 1)],
    dummy_dt()[char %in% c("a", 1)]
  )
  expect_equal(
    finterface[num %in% 1],
    dummy_dt()[num %in% 1]
  )
  expect_equal(
    finterface[num %in% c("a", 1)],
    dummy_dt()[num %in% c("a", 1)]
  )
  expect_equal(
    finterface[num %in% 1:4],
    dummy_dt()[num %in% 1:4]
  )
})

test_that("Combining conditions works", {
  finterface <- local_file_interface("data.csv") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    finterface[3 <= num & num <= 5],
    dummy_dt()[3 <= num & num <= 5]
  )
  expect_equal(
    finterface[1 < num & num < 3 | 5 < num],
    dummy_dt()[1 < num & num < 3 | 5 < num]
  )

  expect_equal(
    finterface[3 <= num & num <= 5],
    dummy_dt()[3 <= num & num <= 5]
  )
  expect_equal(
    finterface[1 < num & num < 3 | 5 < num],
    dummy_dt()[1 < num & num < 3 | 5 < num]
  )

  fquoted <- local_file_interface("data_quoted.csv", quote = TRUE) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    fquoted[3 <= num & num <= 5],
    dummy_dt()[3 <= num & num <= 5]
  )
  expect_equal(
    fquoted[1 < num & num < 3 | 5 < num],
    dummy_dt()[1 < num & num < 3 | 5 < num]
  )

  expect_equal(
    fquoted[3 <= num | num %in% c("a", 1) & num <= 5][order(char)],
    dummy_dt()[3 <= num | num %in% c("a", 1) & num <= 5]
  )
  expect_equal(
    fquoted[1 < num | num %in% 1:4 & num < 3 | 5 < num][order(char)] |>
      unique(),
    dummy_dt()[1 < num | num %in% 1:4 & num < 3 | 5 < num]
  )
})

test_that("Allele1/Allele2 to NEA deduction generates correct awk", {
  local_summary_stats(
    "data.csv",
    alleles_as_a1_a2_alt = TRUE,
    random_names = FALSE
  )
  finterface <- new_file_interface("data.csv") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  cmd <- finterface[chr == 1, return_only_cmd = TRUE]

  # Verify NEA deduction logic is present
  expect_match(cmd, "# Deduce NEA based on EA vs Allele1/Allele2")
  expect_match(cmd, "nea = ")
  expect_match(cmd, "nea OFS")
})

test_that("Prefixes are handled correctly", {
  finterface_b <- local_summary_stats_interface(
    "data.csv",
    chr    = 1,
    prefix = list(chr = "chr")
  ) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  finterface_q <- local_summary_stats_interface(
    "data_q.csv",
    chr = 1,
    prefix = list(chr = "chr"),
    values_are_quoted = TRUE
  ) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  finterface_gz <- local_summary_stats_interface(
    "data.csv.gz",
    chr    = 1,
    prefix = list(chr = "chr")
  ) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

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

test_that("sep is correctly detected in complicated files", {
  finterface <- list(
    filename = "/home/sulc/rcp_storage/common/Users/sulc/data/dbsnp/00-common_all_b38.vcf.gz",
    gzipped = TRUE,
    comment_prefix = "^##",
    trim_prefix = "^#"
  ) |>
    structure(class = c("file_interface", "list"))

  expect_equal(
    get_file_separator(finterface),
    "\t"
  )
})

# Tests consolidated from test-comment.R
test_that("Commented lines are correctly ignored", {
  local_file_with_comments(
    "commented.vcf.gz",
    comment_lines = c(
      "##fileformat=VCFv4.2",
      "##INFO=<ID=AF,Number=A,Type=Float,Description=\"Allele Frequency\">"
    ),
    header_prefix = "#",
    content_dt = data.table::data.table(
      CHROM = "chr1",
      POS = 10177,
      ID = "rs123",
      REF = "AC",
      ALT = "A",
      QUAL = ".",
      FILTER = "PASS",
      INFO = "AF=0.601001",
      FORMAT = "ES:SE:LP:AF:ID",
      sample = "-0.00481799:0.00310296:0.920819:0.601001:rs123"
    )
  )

  result <- new_file_interface("commented.vcf.gz", ieugwas_parsing = FALSE) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null") |>
    head()

  # Verify comments were skipped and # prefix was trimmed
  expect_equal(names(result)[1], "chr")
  expect_equal(nrow(result), 1)
  expect_equal(result$chr, "chr1")
  expect_equal(result$pos, 10177)
})

# Tests consolidated from test-column_encoding.R
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
  expect_true(all(sapply(
    finterface$column_info$encoded_names,
    \(x) length(x) == 0
  )))
  expect_true(all(is.na(finterface$column_info$delimiter)))
  expect_true(all(is.na(finterface$column_info$encoded_column_index)))
  expect_equal(
    finterface_enc$column_info[
      c("build", "chr", "pos"),
      .(
        regex, delimiter, bash_index, encoding_column,
        split_encoding_column, recode_columns
      ),
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
      .(
        regex, encoded_names, delimiter, encoded_column_index,
        split_encoding_column, recode_columns
      ),
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
      .(
        regex, delimiter, bash_index, encoding_column,
        split_encoding_column, recode_columns
      ),
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
