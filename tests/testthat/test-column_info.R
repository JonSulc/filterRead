test_that("Base column_info works", {
  local_summary_stats()
  finterface <- structure(
    list(
      filename = "data.csv",
      gzipped = FALSE,
      sep = ","
    ),
    class = c("file_interface", "list")
  )
  expect_equal(
    get_base_column_info(finterface)[
      ,
      .SD,
      .SDcols = -"input_name"
    ],
    data.table::data.table(
      standard_name     = c("chr", "pos", "ref", "alt", "effect", "pval"),
      possible_prefixes = c(list("chr"), rep(list(NULL), 5)),
      class             = c("character", rep(NA_character_, 5)),
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
  finterface <- structure(
    list(
      filename = "data.csv",
      gzipped = FALSE,
      sep = ","
    ),
    class = c("file_interface", "list")
  )
  column_info <- get_base_column_info(finterface)
  data_to_check <- head(finterface, 500)

  column_info <- filter_regex_matches(column_info, data_to_check)

  expect_true(
    all(grepl(column_info$regex[1], data_to_check[[1]]))
  )
})

test_that("Unquoted column detection works", {
  local_summary_stats(values_are_quoted = FALSE, random_names = FALSE)
  finterface <- structure(
    list(
      filename = "data.csv",
      gzipped = FALSE,
      sep = ","
    ),
    class = c("file_interface", "list")
  )
  expect_equal(
    are_values_quoted(finterface),
    c(
      chr = FALSE, pos = FALSE, ref = FALSE, alt = FALSE,
      effect = FALSE, pval = FALSE
    )
  )
})
test_that("Quoted column detection works", {
  local_summary_stats(values_are_quoted = TRUE, random_names = FALSE)
  finterface <- structure(
    list(
      filename = "data.csv",
      gzipped = FALSE,
      sep = ","
    ),
    class = c("file_interface", "list")
  )
  expect_equal(
    are_values_quoted(finterface),
    c(
      chr = FALSE, pos = FALSE, ref = TRUE, alt = TRUE,
      effect = FALSE, pval = FALSE
    )
  )
})

test_that("a column is quoted where every value it carries is", {
  local_csv_file(
    filename = "sparse.csv",
    dt = data.table::data.table(
      chr_colon_pos = c(NA_character_, "2:200"),
      pval = c(0.01, 0.2)
    ),
    quote = TRUE
  )
  finterface <- structure(
    list(filename = "sparse.csv", gzipped = FALSE, sep = ","),
    class = c("file_interface", "list")
  )
  expect_equal(
    are_values_quoted(finterface),
    c(chr_colon_pos = TRUE, pval = FALSE)
  )
})

test_that("resolve_dropped_prefix separates a carried prefix from none", {
  summary_stats <- dummy_summary_stats()
  expect_equal(
    resolve_dropped_prefix(
      prefixes = "chr",
      values = as.character(summary_stats$chr),
      column = "chr",
      allow_inconsistent = TRUE
    ),
    NA_character_
  )

  summary_stats <- dummy_summary_stats(prefixes = list(chr = "chr"))
  expect_equal(
    resolve_dropped_prefix(
      prefixes = "chr",
      values = summary_stats$chr,
      column = "chr",
      allow_inconsistent = TRUE
    ),
    "chr"
  )
})

test_that("resolve_dropped_prefix finds the prefix beside a scaffold name", {
  expect_equal(
    resolve_dropped_prefix(
      prefixes = "chr",
      values = c("chr10", "GL000209.1"),
      column = "chr",
      allow_inconsistent = TRUE
    ),
    "chr"
  )
  expect_equal(
    resolve_dropped_prefix(
      prefixes = "chr",
      values = c("GL000209.1", "KI270220.1"),
      column = "chr",
      allow_inconsistent = TRUE
    ),
    NA_character_
  )
})

test_that("a declared prefix is dropped whatever else the declaration holds", {
  finterface <- local_file_interface(prefix = list(char = "test"))
  expect_equal(finterface$column_info[name == "char", dropped_prefix], "test")
  expect_equal(finterface[]$char, letters[1:6])
})

test_that("Encoded columns are detected", {
  local_summary_stats()
  finterface <- structure(
    list(
      filename = "data.csv",
      gzipped = FALSE,
      sep = ","
    ),
    class = c("file_interface", "list")
  )
  column_info <- get_base_column_info(finterface)
  expect_equal(
    expand_encoded_columns(column_info),
    column_info
  )
  suppressMessages(withr::deferred_run())

  local_summary_stats(encode_columns = TRUE)
  finterface <- structure(
    list(
      filename = "data.csv",
      gzipped = FALSE,
      sep = ","
    ),
    class = c("file_interface", "list")
  )
  column_info <- get_base_column_info(finterface)
  data_to_check <- head(finterface, 500)
  column_info <- filter_regex_matches(column_info, data_to_check)

  add_quoted_column(column_info, finterface)

  add_encoding_columns(column_info)
  expect_false(any(is.na(column_info$input_name)))
  expect_true(any(is.na(expand_encoded_columns(column_info)$input_name)))
  expect_true(all(c("chr", "pos") %in% expand_encoded_columns(column_info)$standard_name))
})

test_that("match_a1_a2_to_ref generates correct awk code", {
  # Typical case
  code <- match_a1_a2_to_ref("$4", "$5", "$6")
  expect_true(grepl("if \\(\\$4 == \\$6\\)", code))
  expect_true(grepl("nea = \\$5", code))
  expect_true(grepl("nea = \\$4", code))
})

test_that("Parsing allele1, allele2, alt works", {
  local_summary_stats(alleles_as_a1_a2_alt = TRUE)
  finterface <- structure(
    list(
      filename = "data.csv",
      gzipped = FALSE,
      sep = ","
    ),
    class = c("file_interface", "list")
  )
  column_info <- get_base_column_info(
    finterface,
    standard_names_dt = summary_stats_standard_names_dt
  )

  data_to_check <- head(finterface, 500)
  column_info <- filter_regex_matches(column_info, data_to_check)
  add_quoted_column(column_info, finterface)
  add_encoding_columns(column_info)
  column_info <- expand_encoded_columns(column_info)

  expect_true(needs_a1_a2_to_ref_matching(column_info))

  finterface <- new_file_interface("data.csv") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    column_names(finterface),
    c("chr", "pos", "alt", "ref", "effect", "pval", "allele1", "allele2")
  )
  expect_equal(
    head(finterface) |>
      colnames(),
    c("chr", "pos", "alt", "ref", "effect", "pval", "allele1", "allele2")
  )
})

test_that("An encoded column with no records to verify is left packed", {
  patterns <- summary_stats_standard_names_dt[!sapply(delimiter, is.na)]

  for (index in seq_len(nrow(patterns))) {
    pattern <- patterns[index]
    finterface <- local_header_only_interface(
      sprintf("encoded_empty_%d.csv", index),
      encode_columns = pattern
    )
    result <- read_file_interface(finterface)

    expect_false(any(pattern$encoded_names[[1]] %in% column_names(finterface)))
    expect_true(pattern$input_name %in% column_names(finterface))
    expect_equal(names(result), column_names(finterface))
    expect_equal(nrow(result), 0L)
  }
})

test_that("Handling of Chr_Pos works", {
  column_info <- summary_stats_standard_names_dt[
    "Chr_Pos",
    on = "input_name"
  ]
  data_to_check <- data.table(
    Chr_Pos = c(
      "1:12345", "2:23456", "X:34567", "Y:45678", "MT:56789",
      "1:12345:ID", "2:23456:ID", "X:34567:ID", "Y:45678:ID", "MT:56789:ID"
    )
  )
  expect_false(
    is.na(filter_regex_matches(column_info, data_to_check)$regex)
  )
})

test_that("resolve_column_values splits a parent on its delimiter pattern", {
  data_to_check <- data.table::data.table(
    MarkerName = c("b37-c1:1000-123", "b37-c2:2000-456")
  )
  expect_equal(
    resolve_column_values(
      input_name = NA_character_,
      encoding_column = "MarkerName",
      delimiter = "-c|:|-",
      decoded_index = 2L,
      data_to_check = data_to_check
    ),
    c("1", "2")
  )
  expect_equal(
    resolve_column_values(
      input_name = "MarkerName",
      encoding_column = NA_character_,
      delimiter = NA_character_,
      decoded_index = NA_integer_,
      data_to_check = data_to_check
    ),
    c("b37-c1:1000-123", "b37-c2:2000-456")
  )
})

test_that("a decoded column takes the metadata its standard name declares", {
  finterface <- local_file_interface(
    dt = make_decoded_chr_stats(), build = "b38"
  )
  expect_equal(
    finterface$column_info[standard_name == "chr", possible_prefixes],
    list("chr")
  )
  expect_equal(
    finterface$column_info[standard_name == "chr", class],
    "character"
  )
  expect_true(is.na(finterface$column_info[name == "pos", dropped_prefix]))
})

test_that("a caller's standard_names_dt governs a decoded column", {
  standard_names_dt <- data.table::copy(summary_stats_standard_names_dt)
  standard_names_dt[
    standard_name == "chr",
    possible_prefixes := .(.(c("ch", "Ch")))
  ][]

  finterface <- local_file_interface(
    dt = make_decoded_chr_stats(chr = "ch1"),
    build = "b38",
    standard_names_dt = standard_names_dt
  )
  expect_equal(
    finterface$column_info[name == "chr", possible_prefixes],
    list(c("ch", "Ch"))
  )
  expect_equal(finterface$column_info[name == "chr", dropped_prefix], "ch")
})

test_that(
  "a build-encoded parent yields a bare chr and an undeclared build",
  {
    finterface <- local_file_interface(
      dt = data.table::data.table(
        MarkerName = c("b37-c1:1000-123", "b37-c2:2000-456"),
        pval = c(0.1, 0.2)
      ),
      build = "b37"
    )
    expect_true(is.na(finterface$column_info[name == "chr", dropped_prefix]))
    expect_equal(
      finterface$column_info[name == "build", possible_prefixes], list(NULL)
    )
    expect_identical(finterface[]$chr, c("1", "2"))
    expect_identical(finterface[]$build, c("b37", "b37"))
  }
)
