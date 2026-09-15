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
  finterface <- local_file_interface("data.csv")
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

  fquoted <- local_file_interface("data_quoted.csv", quote = TRUE)
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
  finterface <- local_file_interface("data.csv")
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
  finterface <- local_file_interface("data.csv")
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

  fquoted <- local_file_interface("data_quoted.csv", quote = TRUE)
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
  expect_match(cmd, "OFS nea")
})

test_that("Prefixes are handled correctly", {
  finterface_b <- local_summary_stats_interface(
    "data.csv",
    chr    = 1,
    prefix = list(chr = "chr")
  )
  finterface_q <- local_summary_stats_interface(
    "data_q.csv",
    chr = 1,
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

test_that("Space-separated files are read correctly", {
  local_csv_file("data.txt", sep = " ")
  finterface <- new_file_interface("data.txt") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_equal(get_file_separator(finterface), " ")
  expect_equal(head(finterface, 2), head(dummy_dt(), 2))
  expect_equal(finterface[num < 3], dummy_dt()[num < 3])
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

  result <- new_file_interface("commented.vcf.gz", gwas_vcf_parsing = FALSE) |>
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
  finterface <- local_summary_stats_interface()
  finterface_enc <- local_summary_stats_interface(
    "encoded.csv",
    encode_columns = summary_stats_standard_names_dt[
      list(input_name = "MarkerName", delimiter = "-c|:|-"),
      on = c("input_name", "delimiter")
    ]
  )

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
      recode_columns = "$1 = $1 OFS encoded1[1] OFS encoded1[2] OFS encoded1[3]"
    )
  )
  expect_equal(
    get_awk_column_arrays(finterface_enc),
    list(
      before_if = c("$2 = toupper($2)", "$3 = toupper($3)"),
      after_if = c(
        "split($1, encoded1, \"-c|:|-\")",
        "$1 = $1 OFS encoded1[1] OFS encoded1[2] OFS encoded1[3]"
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
      recode_columns        =
        "$1 = $1 OFS encoded1[1] OFS encoded1[2] OFS encoded1[3]"
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
      recode_columns = "$1 = $1 OFS encoded1[1] OFS encoded1[2] OFS encoded1[3]"
    )
  )
  expect_equal(
    get_awk_column_arrays(finterface_enc),
    list(
      before_if = c("$2 = toupper($2)", "$3 = toupper($3)"),
      after_if = c(
        "split($1, encoded1, \"-c|:|-\")",
        "$1 = $1 OFS encoded1[1] OFS encoded1[2] OFS encoded1[3]"
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
  )

  data_raw <- data.table::fread("encoded.csv")
  expect_equal(
    names(data_raw),
    c("MarkerName", "ref", "alt", "effect", "pval")
  )

  expect_equal(
    column_names(finterface_enc),
    c("MarkerName", "build", "chr", "pos", "ref", "alt", "effect", "pval")
  )
  expect_equal(
    names(head(finterface_enc)),
    c("MarkerName", "build", "chr", "pos", "ref", "alt", "effect", "pval")
  )
  expect_true(
    finterface_enc[chr == 1][, all(chr == 1)]
  )
})

test_that("nlines parameter limits output rows", {
  finterface <- local_file_interface("data.csv")

  # nlines with no filtering
  expect_equal(
    finterface[nlines = 3],
    head(dummy_dt(), 3)
  )
  expect_equal(
    finterface[nlines = 1],
    head(dummy_dt(), 1)
  )

  # nlines with filtering conditions
  expect_equal(
    finterface[2 < num, nlines = 2],
    head(dummy_dt()[num > 2], 2)
  )
  expect_equal(
    finterface[num < 5, nlines = 3],
    head(dummy_dt()[num < 5], 3)
  )

  # nlines larger than file returns all rows
  expect_equal(
    finterface[nlines = 100],
    dummy_dt()
  )

  # nlines with gzipped file
  finterface_gz <- local_file_interface("data.csv.gz")
  expect_equal(
    finterface_gz[nlines = 2],
    head(dummy_dt(), 2)
  )
  expect_equal(
    finterface_gz[num > 3, nlines = 2],
    head(dummy_dt()[num > 3], 2)
  )
})

test_that("Empty brackets read full file", {
  finterface <- local_file_interface("data.csv")

  # Empty brackets return all rows
  expect_equal(
    finterface[],
    dummy_dt()
  )

  # Works with gzipped files
  finterface_gz <- local_file_interface("data.csv.gz")
  expect_equal(
    finterface_gz[],
    dummy_dt()
  )

  # Works with quoted values
  fquoted <- local_file_interface("data_quoted.csv", quote = TRUE)
  expect_equal(
    fquoted[],
    dummy_dt()
  )
})

test_that("read_file_interface reads full file", {
  finterface <- local_file_interface("data.csv")

  # read_file_interface returns all rows
  expect_equal(
    read_file_interface(finterface),
    dummy_dt()
  )

  # Equivalent to empty brackets
  expect_equal(
    read_file_interface(finterface),
    finterface[]
  )

  # Works with gzipped files
  finterface_gz <- local_file_interface("data.csv.gz")
  expect_equal(
    read_file_interface(finterface_gz),
    dummy_dt()
  )
})

test_that("Header is consistently skipped regardless of filter condition", {
  finterface <- local_file_interface("data.csv")

  # Previously, nlines interacted with headers passing the filter condition
  # For instance, for [num < 5], awk substitutes num for 0, yielding 0 < 5,
  # in which case the header was counted as one of the lines of output.
  # In cases where the header did not match, e.g., 2 < num (=> 2 < 0), the
  # header was not included, leading to inconsistent behavior.

  expect_equal(
    nrow(finterface[num < 5, nlines = 2]),
    2
  )
  expect_equal(
    nrow(finterface[num < 5, nlines = 1]),
    1
  )
  expect_equal(
    nrow(finterface[2 < num, nlines = 2]),
    2
  )
  expect_equal(
    nrow(finterface[2 < num, nlines = 1]),
    1
  )
})

test_that("[.file_interface cleans up the awk script tempfile", {
  finterface <- local_file_interface()
  before <- list.files(tempdir(), full.names = TRUE)
  result <- finterface[num < 3]
  after <- list.files(tempdir(), full.names = TRUE)
  new_files <- setdiff(after, before)
  expect_length(new_files[grepl("\\.awk$", new_files)], 0)
})

test_that("[.file_interface cleans up %in% tempfiles", {
  finterface <- local_file_interface()
  before <- list.files(tempdir(), full.names = TRUE)
  result <- finterface[char %in% c("a", "b", "c")]
  after <- list.files(tempdir(), full.names = TRUE)
  new_files <- setdiff(after, before)
  expect_length(new_files, 0)
})

test_that("[.file_interface preserves tempfiles when return_only_cmd = TRUE", {
  finterface <- local_file_interface()
  cmd <- finterface[char %in% c("a", "b"), return_only_cmd = TRUE]
  additional_files <- attr(cmd, "additional_files")
  expect_true(length(additional_files) > 0)
  expect_true(all(file.exists(additional_files)))
  unlink(additional_files)
})

test_that("head(finterface, 0) returns an empty data.table with full schema", {
  finterface <- local_file_interface()
  result <- head(finterface, 0)
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), names(head(finterface, 1)))
  expect_equal(
    sapply(result, class),
    sapply(head(finterface, 1), class)
  )
})

test_that("[.file_interface handles filenames with spaces", {
  base_dir <- file.path(tempdir(), "dir with space")
  dir.create(base_dir, showWarnings = FALSE)
  withr::defer(unlink(base_dir, recursive = TRUE))
  filename <- file.path(base_dir, "data.csv")
  dt <- dummy_dt()
  data.table::fwrite(dt, filename)
  finterface <- new_file_interface(filename)
  expect_equal(
    nrow(finterface[num < 3]),
    nrow(dt[num < 3])
  )
})

test_that("head.file_interface handles filenames with spaces", {
  base_dir <- file.path(tempdir(), "dir with space")
  dir.create(base_dir, showWarnings = FALSE)
  withr::defer(unlink(base_dir, recursive = TRUE))
  filename <- file.path(base_dir, "data.csv")
  dt <- dummy_dt()
  data.table::fwrite(dt, filename)
  finterface <- new_file_interface(filename)
  expect_equal(nrow(head(finterface, 2)), 2)
})

test_that("[.file_interface short-circuits empty region_set without awk", {
  testthat::local_mocked_bindings(
    fcondition_to_awk = function(...) {
      stop("fcondition_to_awk should not be called for empty regions")
    }
  )
  finterface <- local_summary_stats_interface(prefixes = c(chr = "chr"))
  result <- finterface[chr == 1 & chr != 1]
  expect_equal(nrow(result), 0)
  expect_equal(names(result), names(head(finterface, 1)))
})

test_that("Tilde in filename is expanded before shelling out", {
  home <- withr::local_tempdir()
  withr::local_envvar(HOME = home)
  local_csv_file(file.path(home, "tilde_test.csv.gz"))

  finterface <- new_file_interface("~/tilde_test.csv.gz") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_equal(finterface$filename, file.path(home, "tilde_test.csv.gz"))
  expect_equal(nrow(head(finterface, nlines = 6L)), 6L)
})

test_that("[.file_interface returns the full schema when nothing matches", {
  finterface <- local_summary_stats_interface()

  expect_no_warning(result <- finterface[pval < 0])
  expect_equal(result, head(finterface, 0))
})

test_that("dimnames agrees with what a read returns", {
  finterface <- local_summary_stats_interface(build = "b38")

  expect_equal(dimnames(finterface)[[2]], column_names(finterface))
  expect_equal(names(head(finterface, 1)), column_names(finterface))
  expect_equal(
    names(read_file_interface(finterface)),
    column_names(finterface)
  )
})

test_that("Reads of a header-only file are silent", {
  finterface <- local_header_only_interface()

  expect_silent(head(finterface, 1))
  expect_silent(head(finterface, 0))
  expect_silent(read_file_interface(finterface))
})

test_that("A header-only file reads back as its standardized columns", {
  finterface <- local_header_only_interface()

  expect_equal(
    names(head(finterface, 1)),
    c("chr", "pos", "ref", "alt", "effect", "pval")
  )
  expect_equal(names(head(finterface, 1)), column_names(finterface))
  expect_equal(names(head(finterface, 0)), column_names(finterface))
  expect_equal(
    names(read_file_interface(finterface)),
    column_names(finterface)
  )
  expect_equal(nrow(read_file_interface(finterface)), 0L)
  expect_equal(
    vapply(read_file_interface(finterface), class, character(1)),
    c(
      chr    = "logical",
      pos    = "logical",
      ref    = "logical",
      alt    = "logical",
      effect = "logical",
      pval   = "logical"
    )
  )
})

test_that("A no-match read on a populated file keeps fread's types", {
  finterface <- local_summary_stats_interface(build = "b38")

  expect_no_warning(result <- finterface[pval < 0])
  expect_equal(nrow(result), 0L)
  expect_equal(
    vapply(result, class, character(1)),
    c(
      chr    = "character",
      pos    = "integer",
      ref    = "character",
      alt    = "character",
      effect = "numeric",
      pval   = "numeric"
    )
  )
})

test_that("A row of only character fields is read as data", {
  dt <- data.table::data.table(
    chr = c("chr1", "chr2"),
    ref = c("A", "G"),
    alt = c("G", "A")
  )
  local_csv_file("characters.csv", dt = dt)
  finterface <- new_file_interface("characters.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  first_row <- head(finterface, 1)
  expect_equal(nrow(first_row), 1L)
  expect_equal(first_row$chr, "chr1")
  expect_equal(
    vapply(first_row, class, character(1)),
    c(chr = "character", ref = "character", alt = "character")
  )

  filtered <- finterface[ref == "A"]
  expect_equal(nrow(filtered), 1L)
  expect_equal(filtered$alt, "G")
})

test_that("field_shapes_agree compares the dominant separator", {
  expect_true(field_shapes_agree("chr\tpos\tref", "1\t100\tA"))
  expect_false(field_shapes_agree("chr\tpos", "1\t100\tA"))
  expect_true(field_shapes_agree("chr\tpos\tinfo", "1\t100\tAF=0.6;DP=10"))
  expect_true(field_shapes_agree("chr,pos,ref", "1,100,A"))
  expect_true(field_shapes_agree("chr pos ref", "1 100 A"))
  expect_true(is.na(field_shapes_agree("ID", "rs1")))
})

test_that("field_shapes_agree takes the separator from its first argument", {
  expect_false(
    field_shapes_agree("see doi:10.1/x, table 2, note", "chr,pos,ref")
  )
  expect_true(
    field_shapes_agree("chr,pos,ref", "see doi:10.1/x, table 2, note")
  )
  expect_false(field_shapes_agree("chr,pos,ref", "rs1"))
  expect_false(field_shapes_agree("ID", "rs1,rs2"))
})

test_that("escape_extended_regex escapes every metacharacter", {
  expect_equal(escape_extended_regex("#CHROM"), "#CHROM")
  expect_equal(
    escape_extended_regex(".*+?^$()[]{}|"),
    "\\.\\*\\+\\?\\^\\$\\(\\)\\[\\]\\{\\}\\|"
  )
  expect_equal(escape_extended_regex("[test]"), "\\[test\\]")
  expect_equal(escape_extended_regex("(a+b)?"), "\\(a\\+b\\)\\?")
  expect_equal(escape_extended_regex("{1,2}"), "\\{1,2\\}")
  expect_equal(escape_extended_regex("a|b"), "a\\|b")
  expect_equal(escape_extended_regex("\\"), "\\\\")
})

test_that("escape_extended_regex escapes the backslash before the rest", {
  expect_equal(escape_extended_regex("\\."), "\\\\\\.")
  expect_equal(escape_extended_regex("\\$"), "\\\\\\$")
  expect_true(grepl(escape_extended_regex("a\\b"), "a\\b"))
})

character_summary_stats <- function() {
  data.table::data.table(
    chr    = c("1", "2"),
    pos    = c("100", "200"),
    ref    = c("A", "G"),
    alt    = c("G", "A"),
    effect = c("0.1", "-0.2"),
    pval   = c("0.01", "0.2")
  )
}

test_that("A prefixed header with no comment above it is a header", {
  local_file_with_comments(
    "prefixed_header.tsv",
    comment_lines = character(0),
    header_prefix = "#",
    content_dt = character_summary_stats()
  )
  finterface <- new_file_interface("prefixed_header.tsv", build = "b38")

  expect_null(finterface$comment_prefix)
  expect_equal(finterface$trim_prefix, "^#")
  expect_equal(
    column_names(finterface),
    c("chr", "pos", "ref", "alt", "effect", "pval")
  )
  expect_equal(nrow(read_file_interface(finterface)), 2L)
})

test_that("Comments above an unprefixed header are dropped", {
  local_file_with_comments(
    "unprefixed_header.tsv",
    header_prefix = "",
    content_dt = character_summary_stats()
  )
  finterface <- new_file_interface("unprefixed_header.tsv", build = "b38")

  expect_equal(finterface$comment_prefix, "^##")
  expect_null(finterface$trim_prefix)
  expect_equal(
    column_names(finterface),
    c("chr", "pos", "ref", "alt", "effect", "pval")
  )
  expect_equal(nrow(read_file_interface(finterface)), 2L)
})

test_that("A header-only file whose header carries a prefix standardizes", {
  local_file_with_comments(
    "prefixed_empty.tsv",
    comment_lines = character(0),
    header_prefix = "#",
    content_dt = character_summary_stats()[0]
  )
  finterface <- new_file_interface("prefixed_empty.tsv", build = "b38")

  expect_null(finterface$comment_prefix)
  expect_equal(finterface$trim_prefix, "^#")
  expect_equal(
    column_names(finterface),
    c("chr", "pos", "ref", "alt", "effect", "pval")
  )
  expect_equal(nrow(read_file_interface(finterface)), 0L)
})

test_that("A prefixed header above a single-column file is rejected", {
  local_file_with_comments(
    "single_column.tsv",
    comment_lines = character(0),
    header_prefix = "#",
    content_dt = data.table::data.table(ID = c("rs1", "rs2"))
  )

  expect_error(
    new_file_interface("single_column.tsv", build = "b38"),
    "is a header or a comment"
  )
})

test_that("A file of nothing but comments is rejected", {
  writeLines(
    c(
      "##fileformat=VCFv4.2",
      "##INFO=<ID=AF,Number=A,Type=Float,Description=\"Allele Frequency\">"
    ),
    "comments_only.vcf"
  )
  withr::defer(file.remove("comments_only.vcf"))

  expect_error(
    new_file_interface("comments_only.vcf", build = "b38"),
    "leaving no header to read"
  )
})

test_that("A prose comment sharing the header's comma count is a comment", {
  local_file_with_comments(
    "prose_comment.csv",
    comment_lines = "#see doi:10.1/x, table 2, note",
    header_prefix = "",
    content_dt = data.table::data.table(
      chr = "1",
      pos = "100",
      ref = "A"
    ),
    sep = ","
  )
  finterface <- new_file_interface("prose_comment.csv", build = "b38")

  expect_equal(finterface$comment_prefix, "^#")
  expect_null(finterface$trim_prefix)
  expect_equal(column_names(finterface), c("chr", "pos", "ref"))
  expect_equal(nrow(read_file_interface(finterface)), 1L)
})

test_that("A prefixed header above a comma-rich data row is a header", {
  local_file_with_comments(
    "comma_rich_info.vcf",
    comment_lines = character(0),
    header_prefix = "#",
    content_dt = data.table::data.table(
      CHROM = c("1", "2"),
      POS   = c("100", "200"),
      ID    = c("rs1", "rs2"),
      REF   = c("A", "C"),
      ALT   = c("G", "T"),
      INFO  = c("CSQ=a,b,c,d,e,f,g,h", "CSQ=i,j,k")
    )
  )
  finterface <- new_file_interface("comma_rich_info.vcf", build = "b38")

  expect_null(finterface$comment_prefix)
  expect_equal(finterface$trim_prefix, "^#")
  expect_equal(
    column_names(finterface),
    c("chr", "pos", "rsid", "ref", "alt", "INFO")
  )
  expect_equal(nrow(read_file_interface(finterface)), 2L)
})

test_that("read_leading_lines skips only lines starting with the pattern", {
  writeLines(
    c(
      "##fileformat=VCFv4.2",
      "chr,pos,note",
      "1,100,see##here"
    ),
    "mid_line_prefix.csv"
  )
  withr::defer(file.remove("mid_line_prefix.csv"))
  finterface <- new_file_interface("mid_line_prefix.csv", build = "b38")

  expect_equal(
    read_leading_lines(finterface, nlines = 3L, skip_pattern = "^##"),
    c("chr,pos,note", "1,100,see##here")
  )
})

test_that("extract_prefix_pattern anchors and escapes the leading run", {
  expect_null(extract_prefix_pattern(character(0)))
  expect_null(extract_prefix_pattern(""))
  expect_null(extract_prefix_pattern("chr\tpos\tref"))
  expect_equal(extract_prefix_pattern("#CHROM\tPOS"), "^#")
  expect_equal(extract_prefix_pattern("##fileformat=VCFv4.2"), "^##")
  expect_equal(extract_prefix_pattern("{chr,pos,ref"), "^\\{")
  expect_equal(extract_prefix_pattern("$.*chr,pos"), "^\\$\\.\\*")
  expect_equal(extract_prefix_pattern(c("#CHROM\tPOS", "1\t100")), "^#")
})

test_that("A header starting with a regex metacharacter is read", {
  writeLines(
    c(
      "{chr,pos,ref",
      "1,100,A"
    ),
    "metacharacter_header.csv"
  )
  withr::defer(file.remove("metacharacter_header.csv"))
  finterface <- new_file_interface("metacharacter_header.csv", build = "b38")

  expect_null(finterface$comment_prefix)
  expect_equal(finterface$trim_prefix, "^\\{")
  expect_equal(nrow(read_file_interface(finterface)), 1L)
})

test_that("A header starting with an apostrophe is detected", {
  # Covers prefix detection only. Building a file_interface on this file
  # fails further down: the read-only awk script is wrapped in single quotes
  # for the shell, which an apostrophe in the trim prefix closes early.
  writeLines(
    c(
      "'chr,pos,ref",
      "1,100,A"
    ),
    "apostrophe_header.csv"
  )
  withr::defer(file.remove("apostrophe_header.csv"))
  finterface <- structure(
    list(filename = "apostrophe_header.csv", gzipped = FALSE),
    class = c("file_interface", "character")
  )

  expect_equal(
    read_leading_lines(finterface, nlines = 2L, skip_pattern = "^'"),
    "1,100,A"
  )
  expect_equal(
    detect_line_prefixes(finterface),
    list(comment = NULL, trim = "^'")
  )
})

test_that("A metacharacter header's prefix is stripped from the names", {
  writeLines(
    c(
      "{chr,pos,ref",
      "1,100,A",
      "2,200,G"
    ),
    "metacharacter_names.csv"
  )
  withr::defer(file.remove("metacharacter_names.csv"))
  finterface <- new_file_interface("metacharacter_names.csv", build = "b38")

  expect_equal(column_names(finterface), c("chr", "pos", "ref"))
  expect_equal(nrow(read_file_interface(finterface)), 2L)
})

test_that("A single comma-rich data row reads with the detected separator", {
  local_file_with_comments(
    "single_comma_rich_row.vcf",
    comment_lines = character(0),
    header_prefix = "#",
    content_dt = data.table::data.table(
      CHROM = "1",
      POS   = "100",
      ID    = "rs1",
      REF   = "A",
      ALT   = "G",
      INFO  = "CSQ=a,b,c,d,e,f,g,h"
    )
  )
  finterface <- new_file_interface("single_comma_rich_row.vcf", build = "b38")

  expect_equal(finterface$sep, "\t")
  expect_equal(
    column_names(finterface),
    c("chr", "pos", "rsid", "ref", "alt", "INFO")
  )
  expect_equal(nrow(read_file_interface(finterface)), 1L)
  expect_equal(head(finterface, 1), read_file_interface(finterface))
})

test_that("Every read path agrees on a comma-rich file", {
  local_file_with_comments(
    "comma_rich_paths.vcf",
    comment_lines = character(0),
    header_prefix = "#",
    content_dt = data.table::data.table(
      CHROM = c("1", "1"),
      POS   = c("1000", "2000"),
      ID    = c("rs1", "rs2"),
      REF   = c("A", "C"),
      ALT   = c("G", "T"),
      INFO  = c("CSQ=a,b,c,d,e,f,g,h", "CSQ=i,j,k,l,m,n,o,p")
    )
  )
  finterface <- new_file_interface("comma_rich_paths.vcf", build = "b38")

  expect_equal(
    names(head(finterface, 1)),
    c("chr", "pos", "rsid", "ref", "alt", "INFO")
  )
  expect_equal(nrow(head(finterface, 1)), 1L)
  expect_equal(head(finterface, 1)$INFO, "CSQ=a,b,c,d,e,f,g,h")
  expect_equal(
    names(finterface[5000 < pos]),
    c("chr", "pos", "rsid", "ref", "alt", "INFO")
  )
  expect_equal(nrow(finterface[5000 < pos]), 0L)
})

test_that("head does not split a row on a separator inside a field", {
  local_file_with_comments(
    "semicolon_field.tsv",
    comment_lines = character(0),
    header_prefix = "",
    content_dt = data.table::data.table(
      chr  = "1",
      pos  = "1000",
      rsid = "rs1",
      ref  = "A",
      alt  = "G",
      INFO = "a;b;c;d;e;f"
    )
  )
  finterface <- new_file_interface("semicolon_field.tsv", build = "b38")

  expect_equal(head(finterface, 1), read_file_interface(finterface))
  expect_equal(head(finterface, 1)$INFO, "a;b;c;d;e;f")
})

test_that("A caller's separator reaches both read paths", {
  local_file_with_comments(
    "caller_sep.csv",
    comment_lines = character(0),
    header_prefix = "",
    content_dt = data.table::data.table(
      chr = c("1", "2"),
      pos = c("100", "200"),
      ref = c("A", "G")
    ),
    sep = ","
  )
  finterface <- new_file_interface("caller_sep.csv", build = "b38")

  expect_equal(finterface[, sep = ","], finterface[])
  expect_equal(head(finterface, 1, sep = ","), head(finterface, 1))
})

test_that("A condition covering the whole genome reads every row", {
  fixture <- dummy_fixed_summary_stats()
  finterface <- local_file_interface(dt = fixture)
  expect_equal(
    nrow(finterface[chr == "chr1" | 123 < pos | pos < 456]),
    nrow(fixture)
  )
})
