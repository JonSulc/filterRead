test_in_fc <- function(
  fcall,
  finterface,
  expected_condition,
  file_contents
) {
  awk_condition_list <- new_filter_condition(
    fcall,
    finterface = finterface
  ) |>
    eval_fcondition(finterface = finterface)
  expect_true(
    file.exists(awk_condition_list$additional_files)
  )
  expect_equal(
    readLines(awk_condition_list$additional_files),
    file_contents
  )
  filepath <- awk_condition_list$additional_files
  random_code <- gsub("^/tmp/Rtmp[^/]+/file",
                      "",
                      awk_condition_list$additional_files)
  expect_equal(
    awk_condition_list$condition,
    sprintf(expected_condition,
            random_code)
  )
  expect_equal(
    awk_condition_list$variable_arrays,
    paste0(
      "if (FILENAME == \"%s\") {\n",
      "    var%s[$0] = 1\n",
      "    next\n",
      "  }"
    ) |>
      sprintf(filepath, random_code)
  )
}


test_that("Basic filter_condition initialization works", {
  finterface <- local_file_interface()
  expect_no_error(new_filter_condition(rlang::expr(x < 3),
                                       finterface = finterface))
  expect_no_error(new_filter_condition(rlang::expr(x > 3),
                                       finterface = finterface))
  expect_no_error(new_filter_condition(rlang::expr(x <= 3),
                                       finterface = finterface))
  expect_no_error(new_filter_condition(rlang::expr(x >= 3),
                                       finterface = finterface))
  expect_no_error(new_filter_condition(rlang::expr(x == 3),
                                       finterface = finterface))

  # expect_error(new_filter_condition(rlang::expr(x == 1:3),
  #                                   finterface = finterface))

  finterface_env <- {
    env <- new.env(parent = emptyenv())
    env$finterface <- finterface
    env
  }

  expect_equal(new_filter_condition(rlang::expr(x < 3),
                                    finterface = finterface),
               structure(
                 rlang::expr(lt_filter_condition(x, 3)),
                 class = c("filter_condition", "call"),
                 finterface_env = finterface_env
               ))
  expect_equal(new_filter_condition(rlang::expr(x > 3),
                                    finterface = finterface),
               structure(
                 rlang::expr(gt_filter_condition(x, 3)),
                 class = c("filter_condition", "call"),
                 finterface_env = finterface_env
               ))
  expect_equal(new_filter_condition(rlang::expr(x <= 3),
                                    finterface = finterface),
               structure(
                 rlang::expr(lte_filter_condition(x, 3)),
                 class = c("filter_condition", "call"),
                 finterface_env = finterface_env
               ))
  expect_equal(new_filter_condition(rlang::expr(x >= 3),
                                    finterface = finterface),
               structure(
                 rlang::expr(gte_filter_condition(x, 3)),
                 class = c("filter_condition", "call"),
                 finterface_env = finterface_env
               ))
  expect_equal(new_filter_condition(rlang::expr(x == 3),
                                    finterface = finterface),
               structure(
                 rlang::expr(eq_filter_condition(x, 3)),
                 class = c("filter_condition", "call"),
                 finterface_env = finterface_env
               ))
})

test_that("Passing variables or complex objects works", {
  finterface <- local_summary_stats_interface()
  my_chr <- 3
  expect_equal(new_filter_condition(rlang::expr(chr == my_chr),
                                    finterface = finterface),
               new_filter_condition(rlang::expr(chr == 3),
                                    finterface = finterface))
  expect_equal(new_filter_condition(rlang::expr(chr == 2*my_chr),
                                    finterface = finterface),
               new_filter_condition(rlang::expr(chr == 6),
                                    finterface = finterface))
})

test_that("%in% parsing works", {
  finterface <- local_file_interface()
  expect_equal(
    new_filter_condition(
      rlang::expr(x %in% letters[1:5]),
      finterface = finterface
    ),
    structure(
      {
        fcall <- rlang::expr(in_filter_condition(x))
        fcall[[3]] <- c("a", "b", "c", "d", "e")
        fcall
      },
      class = c("filter_condition", "call"),
      finterface_env = {
        env <- new.env(parent = emptyenv())
        env$finterface <- finterface
        env
      }
    )
  )

  test_in_fc(rlang::expr(char %in% letters[1:5]),
             finterface         = finterface,
             expected_condition = "($1 in var%s)",
             file_contents      = letters[1:5])

  expect_true(
    grepl(
      paste0(
        "^awk [']BEGIN[{]\n",
        "  OFS = \",\"\n",
        "[}] [{]\n",
        "  if [(]FILENAME == \\\"/tmp/Rtmp([^/]+)/file([a-f0-9]+)\\\"[)] [{]\n",
        "    var\\2[[][$]0[]] = 1\n",
        "    next\n",
        "  [}]\n",
        "  else [{]\n",
        "    if [(][(][$]1 in var\\2[)][)] [{]\n",
        "    print [$]0\n",
        "  [}]\n",
        "  [}]\n",
        "[}]['] /tmp/Rtmp\\1/file\\2 FS=\",\" data[.]csv"
      ),
      new_filter_condition(
        rlang::expr(char %in% letters[1:5]),
        finterface = finterface
      ) |>
        fcondition_to_awk()
    )
  )
})

test_that("Parsing to command line works", {
  finterface <- local_file_interface()
  expect_equal(
    new_filter_condition(rlang::expr(num < 3),
                         finterface = finterface) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 < 3")
  )
  expect_equal(
    new_filter_condition(rlang::expr(num > 3),
                         finterface = finterface) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 > 3")
  )
  expect_equal(
    new_filter_condition(rlang::expr(num <= 3),
                         finterface = finterface) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 <= 3")
  )
  expect_equal(
    new_filter_condition(rlang::expr(num >= 3),
                         finterface = finterface) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 >= 3")
  )
  expect_equal(
    new_filter_condition(rlang::expr(num == 3),
                         finterface = finterface) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 == 3")
  )
  expect_equal(
    new_filter_condition(rlang::expr(char == "a"),
                         finterface = finterface) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$1 == \"a\"")
  )

  expect_equal(
    new_filter_condition(rlang::expr(char < 3 | num > 42),
                         finterface = finterface) |>
      eval_fcondition(finterface = finterface),
    list(variable_arrays = NULL,
         condition = "$1 < 3 || $2 > 42",
         additional_files = NULL)
  )
  expect_equal(
    new_filter_condition(rlang::expr(char < 3 & num > 42),
                         finterface = finterface) |>
      eval_fcondition(finterface = finterface),
    list(variable_arrays = NULL,
         condition = "$1 < 3 && $2 > 42",
         additional_files = NULL)
  )
})

test_that("Parentheses work as expected", {
  finterface <- local_file_interface()
  expect_equal(
    new_filter_condition(rlang::expr((num < 3 & char == "a")),
                         finterface = finterface) |>
      eval_fcondition(finterface = finterface),
    list(
      variable_arrays  = NULL,
      condition        = "($2 < 3 && $1 == \"a\")",
      additional_files = NULL
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::expr((num < 3 & char == "a") | (123 < num & num < 234 & char == "b")),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(
      variable_arrays = NULL,
      condition = "($2 < 3 && $1 == \"a\") || (123 < $2 && $2 < 234 && $1 == \"b\")",
      additional_files = NULL
    )
  )

  test_in_fc(
    rlang::expr((num < 3 & char == "a") | (num < 12 & char %in% letters[1:3])),
    finterface = finterface,
    expected_condition = "($2 < 3 && $1 == \"a\") || ($2 < 12 && ($1 in var%s))",
    file_contents = letters[1:3]
  )

  test_in_fc(
    rlang::expr((num < 3 & char == "a") |
                  (123 < num & num < 234 & char == "b") |
                  (num < 12 & char %in% letters[1:3])),
    finterface = finterface,
    expected_condition = paste(
      "($2 < 3 && $1 == \"a\")",
      "|| (123 < $2 && $2 < 234 && $1 == \"b\")",
      "|| ($2 < 12 && ($1 in var%s))"
    ),
    file_contents = letters[1:3]
  )

  test_in_fc(
    rlang::expr((num < 3 & char == "a") |
                  (num < 12 & char %in% letters[1:3]) |
                  (123 < num & num < 234 & char == "b")),
    finterface = finterface,
    expected_condition = paste(
      "($2 < 3 && $1 == \"a\")",
      "|| ($2 < 12 && ($1 in var%s))",
      "|| (123 < $2 && $2 < 234 && $1 == \"b\")"
    ),
    file_contents = letters[1:3]
  )

  test_in_fc(
    rlang::expr((3 <= num | num %in% c("a", 1)) & num <= 5),
    finterface = finterface,
    expected_condition = "(3 <= $2 || ($2 in var%s)) && $2 <= 5",
    file_contents = c("a", 1)
  )
})

test_that("Quoted values are handled correctly", {
  finterface <- local_file_interface(quote = TRUE)
  expect_equal(
    new_filter_condition(rlang::expr(char == "a"),
                         finterface = finterface) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$1 == \"\\\"a\\\"\"")
  )
  expect_equal(
    new_filter_condition(rlang::expr(num == "a"),
                         finterface = finterface) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 == \"a\"")
  )


  test_in_fc(
    rlang::expr(char %in% c("a", 1)),
    finterface = finterface,
    expected_condition = "($1 in var%s)",
    file_contents = sprintf("\"%s\"", c("a", 1))
  )
  test_in_fc(
    rlang::expr(num %in% c("a", 1)),
    finterface = finterface,
    expected_condition = "($2 in var%s)",
    file_contents = c("a", 1)
  )
})

test_that("Prefixes are handled properly",  {
  finterface <- local_file_interface(prefix = list(char = "test"))
  expect_equal(
    new_filter_condition(
      rlang::expr(char == 1),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$1 == \"test1\"")
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(char == "test1"),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$1 == \"test1\"")
  )

  test_in_fc(
    rlang::expr(num %in% 1:3),
    finterface = finterface,
    expected_condition = "($2 in var%s)",
    file_contents = as.character(1:3)
  )
  test_in_fc(
    rlang::expr(char %in% 1:3),
    finterface = finterface,
    expected_condition = "($1 in var%s)",
    file_contents = paste0("test", 1:3)
  )
  test_in_fc(
    rlang::expr(char %in% paste0("test", 1:3)),
    finterface = finterface,
    expected_condition = "($1 in var%s)",
    file_contents = paste0("test", 1:3)
  )
})

test_that("And-block detection works", {
  finterface <- local_summary_stats_interface()
  expect_true(is_and_block(1))
  expect_true(is_and_block("1"))
  expect_true(is_and_block(new_filter_condition(rlang::expr(chr == 1), finterface)))
  expect_true(is_and_block(new_filter_condition(rlang::expr(pos < 1), finterface)))
  expect_true(is_and_block(new_filter_condition(rlang::expr(123 < pos & pos < 456), finterface)))
  expect_true(is_and_block(new_filter_condition(rlang::expr(chr == 1 & 123 < pos & pos < 456), finterface)))

  expect_false(is_and_block(new_filter_condition(rlang::expr(chr == 1 | 123 < pos & pos < 456), finterface)))
  expect_false(is_and_block(new_filter_condition(rlang::expr(chr == 1 & 123 < pos | pos < 456), finterface)))
  expect_false(is_and_block(new_filter_condition(rlang::expr(chr == 1 | 123 < pos | pos < 456), finterface)))
})

test_that("Genomic position conditions are correctly detected", {
  expect_true(is_genomic_symbol(rlang::expr(chr)))
  expect_true(is_genomic_symbol(rlang::expr(pos)))
  expect_false(is_genomic_symbol(rlang::expr(ref)))
  expect_false(is_genomic_symbol(1))

  finterface <- local_summary_stats_interface()

  expect_false(
    has_chromosome_condition(
      new_filter_condition(rlang::expr(ref == "A"),
                           finterface)
    )
  )
  expect_false(
    has_chromosome_condition(
      new_filter_condition(rlang::expr(pval < .05),
                           finterface)
    )
  )
  expect_false(
    has_chromosome_condition(
      new_filter_condition(rlang::expr(123 < pos & pos < 456 & pval < .05),
                           finterface)
    )
  )
  expect_true(
    has_chromosome_condition(
      new_filter_condition(rlang::expr(chr == 1 & 123 < pos & pos < 456 & pval < .05),
                           finterface)
    )
  )
  expect_true(
    has_chromosome_condition(
      new_filter_condition(rlang::expr(chr == 1 | 123 < pos & pos < 456 & pval < .05),
                           finterface)
    )
  )

  expect_false(
    has_non_genomic_condition(
      new_filter_condition(rlang::expr(chr == 1),
                           finterface)
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(rlang::expr(pos < 1),
                           finterface)
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(rlang::expr(123 < pos & pos < 456),
                           finterface)
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(rlang::expr(chr == 1 & 123 < pos & pos < 456),
                           finterface)
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(rlang::expr(chr == 1 | 123 < pos & pos < 456),
                           finterface)
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(rlang::expr(chr == 1 & 123 < pos | pos < 456),
                           finterface)
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(rlang::expr(chr == 1 | 123 < pos | pos < 456),
                           finterface)
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(rlang::expr(ref == "A"),
                           finterface)
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(rlang::expr(pval < .05),
                           finterface)
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(rlang::expr(chr == 1 & (pos < 123 | 345 < pos)),
                           finterface)
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(rlang::expr(123 < pos & pos < 456 & pval < .05),
                           finterface)
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(rlang::expr(chr == 1 & 123 < pos & pos < 456 & pval < .05),
                           finterface)
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(rlang::expr(chr == 1 | 123 < pos & pos < 456 & pval < .05),
                           finterface)
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(rlang::expr(chr == 1 & 123 < pos | pos < 456 & pval < .05),
                           finterface)
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(rlang::expr(chr == 1 | 123 < pos | pos < 456 & pval < .05),
                           finterface)
    )
  )

  expect_true(
    is_only_genomic_position_condition(
      new_filter_condition(rlang::expr(chr == 1),
                           finterface)
    )
  )
  expect_false(
    is_only_genomic_position_condition(
      new_filter_condition(rlang::expr(pos < 1),
                           finterface)
    )
  )
  expect_false(
    is_only_genomic_position_condition(
      new_filter_condition(rlang::expr(123 < pos & pos < 456),
                           finterface)
    )
  )
  expect_true(
    is_only_genomic_position_condition(
      new_filter_condition(rlang::expr(chr == 1 & 123 < pos & pos < 456),
                           finterface)
    )
  )
  expect_true(
    is_only_genomic_position_condition(
      new_filter_condition(rlang::expr(chr == 1 | 123 < pos & pos < 456),
                           finterface)
    )
  )
  expect_true(
    is_only_genomic_position_condition(
      new_filter_condition(rlang::expr(chr == 1 & 123 < pos | pos < 456),
                           finterface)
    )
  )
  expect_true(
    is_only_genomic_position_condition(
      new_filter_condition(rlang::expr(chr == 1 | 123 < pos | pos < 456),
                           finterface)
    )
  )
  expect_false(
    is_only_genomic_position_condition(
      new_filter_condition(rlang::expr(ref == "A"),
                           finterface)
    )
  )
  expect_false(
    is_only_genomic_position_condition(
      new_filter_condition(rlang::expr(pval < .05),
                           finterface)
    )
  )
  expect_false(
    is_only_genomic_position_condition(
      new_filter_condition(rlang::expr(123 < pos & pos < 456 & pval < .05),
                           finterface)
    )
  )
  expect_false(
    is_only_genomic_position_condition(
      new_filter_condition(rlang::expr(chr == 1 & 123 < pos & pos < 456 & pval < .05),
                           finterface)
    )
  )
  expect_false(
    is_only_genomic_position_condition(
      new_filter_condition(rlang::expr(chr == 1 | 123 < pos & pos < 456 & pval < .05),
                           finterface)
    )
  )
  expect_false(
    is_only_genomic_position_condition(
      new_filter_condition(rlang::expr(chr == 1 & 123 < pos | pos < 456 & pval < .05),
                           finterface)
    )
  )
  expect_false(
    is_only_genomic_position_condition(
      new_filter_condition(rlang::expr(chr == 1 | 123 < pos | pos < 456 & pval < .05),
                           finterface)
    )
  )
})

test_that("Can detect conditions that require isolation of genomic part", {
  finterface_norm <- local_summary_stats_interface()
  finterface_rsid <- local_rsid_summary_stats_interface("rsid.csv")
  expect_false(
    new_filter_condition(rlang::expr(chr == 1),
                         finterface_norm) |>
      needs_genomic_condition_isolation()
  )
  expect_false(
    new_filter_condition(rlang::expr(pval < .05),
                         finterface_norm) |>
      needs_genomic_condition_isolation()
  )
  expect_false(
    new_filter_condition(rlang::expr(chr == 1 & (pos < 123 | 345 < pos)),
                         finterface_norm) |>
      needs_genomic_condition_isolation()
  )
  expect_false(
    new_filter_condition(rlang::expr(chr == 1 & pval < .05),
                         finterface_norm) |>
      needs_genomic_condition_isolation()
  )
  expect_false(
    new_filter_condition(rlang::expr(chr == 1 & (pos < 123 | 345 < pos) & pval < .05),
                         finterface_norm) |>
      needs_genomic_condition_isolation()
  )

  expect_false(
    new_filter_condition(rlang::expr(chr == 1),
                         finterface_rsid) |>
      needs_genomic_condition_isolation()
  )
  expect_false(
    new_filter_condition(rlang::expr(pval < .05),
                         finterface_norm) |>
      needs_genomic_condition_isolation()
  )
  expect_false(
    new_filter_condition(rlang::expr(chr == 1 & (pos < 123 | 345 < pos)),
                         finterface_rsid) |>
      needs_genomic_condition_isolation()
  )
  expect_true(
    new_filter_condition(rlang::expr(chr == 1 & pval < .05),
                         finterface_rsid) |>
      needs_genomic_condition_isolation()
  )
  expect_true(
    new_filter_condition(rlang::expr(chr == 1 & (pos < 123 | 345 < pos) & pval < .05),
                         finterface_rsid) |>
      needs_genomic_condition_isolation()
  )
  expect_true(
    new_filter_condition(rlang::expr(chr == 1 & (pos < 123 | 345 < pos) | pval < .05),
                         finterface_rsid) |>
      needs_genomic_condition_isolation()
  )
})

test_that("Parenthesis stripping works", {
  finterface <- local_summary_stats_interface()

  expect_equal(new_filter_condition(rlang::expr((chr == 1)),
                                    finterface)[[1]],
               as.symbol("lp_filter_condition"))
  expect_equal(new_filter_condition(rlang::expr(chr == 1),
                                    finterface)[[1]],
               as.symbol("eq_filter_condition"))
  expect_equal(new_filter_condition(rlang::expr((chr == 1)),
                                    finterface) |>
                 strip_parentheses(),
               new_filter_condition(rlang::expr(chr == 1),
                                    finterface))
})

test_that("Getting genomic regions works", {
  finterface <- local_summary_stats_interface()
  expect_equal(
    new_filter_condition(rlang::expr(chr == 1 & 123 <= pos & pos <= 234),
                         finterface) |>
      make_genomic_ranges(),
    data.table::data.table(chr = "1", start = 123, end = 234)
  )
  expect_equal(
    new_filter_condition(rlang::expr(123 <= pos & pos <= 234),
                         finterface) |>
      make_genomic_ranges(),
    data.table::data.table(chr = character(), start = numeric(), end = numeric())
  )
  expect_equal(
    new_filter_condition(rlang::expr(chr == 1 & 123 < pos & pos < 234),
                         finterface) |>
      make_genomic_ranges(),
    data.table::data.table(chr = "1", start = 124, end = 233)
  )
  expect_equal(
    new_filter_condition(rlang::expr(chr < 2 & 123 < pos & pos < 234),
                         finterface) |>
      make_genomic_ranges(),
    data.table::data.table(chr = "1", start = 124, end = 233)
  )
  expect_equal(
    new_filter_condition(rlang::expr(chr == 1 & 123 <= pos & pos <= 234
                                     | chr == "X" & 21 <= pos & pos <= 42),
                         finterface) |>
      make_genomic_ranges(),
    data.table::data.table(chr = c("1", "X"), start = c(123, 21), end = c(234, 42))
  )
  expect_equal(
    new_filter_condition(rlang::expr((chr == 1 | chr == "X") & 123 <= pos & pos <= 234),
                         finterface) |>
      make_genomic_ranges(),
    data.table::data.table(chr = c("1", "X"), start = 123, end = 234)
  )
  expect_equal(
    new_filter_condition(rlang::expr((chr == 1 & chr == "X") & 123 <= pos & pos <= 234),
                         finterface) |>
      make_genomic_ranges(),
    data.table::data.table(chr = character(), start = numeric(), end = numeric())
  )
  expect_equal(
    new_filter_condition(rlang::expr((chr == 1 | chr == "X") & 234 <= pos & pos <= 123),
                         finterface) |>
      make_genomic_ranges(),
    data.table::data.table(chr = c("1", "X"), start = 234, end = 123)
  )
  expect_equal(
    new_filter_condition(rlang::expr((chr == 1 | chr == "X")
                                     & 42 < pos & 123 <= pos
                                     & pos <= 234 & pos < 512),
                         finterface) |>
      make_genomic_ranges(),
    data.table::data.table(chr = c("1", "X"), start = 123, end = 234)
  )
})
