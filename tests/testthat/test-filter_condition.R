test_that("Basic filter_condition initialization works with dummy variables", {
  finterface <- local_file_interface()
  expect_no_error(new_filter_condition(rlang::quo(x < 3),
    finterface = finterface
  ))
  expect_no_error(new_filter_condition(rlang::quo(x > 3),
    finterface = finterface
  ))
  expect_no_error(new_filter_condition(rlang::quo(x <= 3),
    finterface = finterface
  ))
  expect_no_error(new_filter_condition(rlang::quo(x >= 3),
    finterface = finterface
  ))
  expect_no_error(new_filter_condition(rlang::quo(x == 3),
    finterface = finterface
  ))

  finterface_env <- {
    env <- new.env(parent = emptyenv())
    env$finterface <- finterface
    env
  }

  expect_equal(
    new_filter_condition(rlang::quo(num < 3),
      finterface = finterface
    ),
    structure(
      rlang::quo(lt_filter_condition(num, 3)),
      class = c("lt_filter_condition", "filter_condition", "quosure", "formula"),
      finterface_env = finterface_env,
      build = "b38",
      genomic_regions = full_genomic_regions(build = "b38")
    )
  )
  expect_equal(
    new_filter_condition(rlang::quo(num > 3),
      finterface = finterface
    ),
    structure(
      rlang::quo(gt_filter_condition(num, 3)),
      class = c("gt_filter_condition", "filter_condition", "quosure", "formula"),
      finterface_env = finterface_env,
      build = "b38",
      genomic_regions = full_genomic_regions(build = "b38")
    )
  )
  expect_equal(
    new_filter_condition(rlang::quo(num <= 3),
      finterface = finterface
    ),
    structure(
      rlang::quo(lte_filter_condition(num, 3)),
      class = c("lte_filter_condition", "filter_condition", "quosure", "formula"),
      finterface_env = finterface_env,
      build = "b38",
      genomic_regions = full_genomic_regions(build = "b38")
    )
  )
  expect_equal(
    new_filter_condition(rlang::quo(num >= 3),
      finterface = finterface
    ),
    structure(
      rlang::quo(gte_filter_condition(num, 3)),
      class = c("gte_filter_condition", "filter_condition", "quosure", "formula"),
      finterface_env = finterface_env,
      build = "b38",
      genomic_regions = full_genomic_regions(build = "b38")
    )
  )
  expect_equal(
    new_filter_condition(rlang::quo(num == 3),
      finterface = finterface
    ),
    structure(
      rlang::quo(eq_filter_condition(num, 3)),
      class = c("eq_filter_condition", "filter_condition", "quosure", "formula"),
      finterface_env = finterface_env,
      build = "b38",
      genomic_regions = full_genomic_regions(build = "b38")
    )
  )
})

test_that("Basic filter_condition initialization works with column names", {
  finterface <- local_file_interface()
  expect_no_error(new_filter_condition(rlang::quo(num < 3),
    finterface = finterface
  ))
  expect_no_error(new_filter_condition(rlang::quo(num > 3),
    finterface = finterface
  ))
  expect_no_error(new_filter_condition(rlang::quo(num <= 3),
    finterface = finterface
  ))
  expect_no_error(new_filter_condition(rlang::quo(num >= 3),
    finterface = finterface
  ))
  expect_no_error(new_filter_condition(rlang::quo(num == 3),
    finterface = finterface
  ))

  finterface_env <- {
    env <- new.env(parent = emptyenv())
    env$finterface <- finterface
    env
  }

  expect_equal(
    new_filter_condition(rlang::quo(num < 3),
      finterface = finterface
    ),
    structure(
      rlang::quo(lt_filter_condition(num, 3)),
      class = c("lt_filter_condition", "filter_condition", "quosure", "formula"),
      finterface_env = finterface_env,
      build = "b38",
      genomic_regions = full_genomic_regions(build = "b38")
    )
  )
  expect_equal(
    new_filter_condition(rlang::quo(num > 3),
      finterface = finterface
    ),
    structure(
      rlang::quo(gt_filter_condition(num, 3)),
      class = c("gt_filter_condition", "filter_condition", "quosure", "formula"),
      finterface_env = finterface_env,
      build = "b38",
      genomic_regions = full_genomic_regions(build = "b38")
    )
  )
  expect_equal(
    new_filter_condition(rlang::quo(num <= 3),
      finterface = finterface
    ),
    structure(
      rlang::quo(lte_filter_condition(num, 3)),
      class = c("lte_filter_condition", "filter_condition", "quosure", "formula"),
      finterface_env = finterface_env,
      build = "b38",
      genomic_regions = full_genomic_regions(build = "b38")
    )
  )
  expect_equal(
    new_filter_condition(rlang::quo(num >= 3),
      finterface = finterface
    ),
    structure(
      rlang::quo(gte_filter_condition(num, 3)),
      class = c("gte_filter_condition", "filter_condition", "quosure", "formula"),
      finterface_env = finterface_env,
      build = "b38",
      genomic_regions = full_genomic_regions(build = "b38")
    )
  )
  expect_equal(
    new_filter_condition(rlang::quo(num == 3),
      finterface = finterface
    ),
    structure(
      rlang::quo(eq_filter_condition(num, 3)),
      class = c("eq_filter_condition", "filter_condition", "quosure", "formula"),
      finterface_env = finterface_env,
      build = "b38",
      genomic_regions = full_genomic_regions(build = "b38")
    )
  )
})

test_that("Passing variables or complex objects works", {
  finterface <- local_summary_stats_interface()
  my_chr <- 3
  expect_equal(
    new_filter_condition(rlang::quo(chr == my_chr),
      finterface = finterface
    ),
    new_filter_condition(rlang::quo(chr == 3),
      finterface = finterface
    )
  )
  expect_equal(
    new_filter_condition(rlang::quo(chr == 2 * my_chr),
      finterface = finterface
    ),
    new_filter_condition(rlang::quo(chr == 6),
      finterface = finterface
    )
  )
})

test_that("Quoted values are handled correctly", {
  finterface <- local_file_interface(quote = TRUE)
  expect_equal(
    new_filter_condition(rlang::quo(char == "a"),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$1 == \"\\\"a\\\"\"")
  )
  expect_equal(
    new_filter_condition(rlang::quo(num == "a"),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 == \"a\"")
  )


  test_in_fc(
    rlang::quo(char %in% c("a", 1)),
    finterface = finterface,
    expected_condition = "($1 in var%s)",
    file_contents = sprintf("\"%s\"", c("a", 1))
  )
  test_in_fc(
    rlang::quo(num %in% c("a", 1)),
    finterface = finterface,
    expected_condition = "($2 in var%s)",
    file_contents = c("a", 1)
  )
})

test_that("Prefixes are handled properly", {
  finterface <- local_file_interface(prefix = list(char = "test"))
  expect_equal(
    new_filter_condition(
      rlang::quo(char == 1),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$1 == \"test1\"")
  )
  expect_equal(
    new_filter_condition(
      rlang::quo(char == "test1"),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$1 == \"test1\"")
  )

  test_in_fc(
    rlang::quo(num %in% 1:3),
    finterface = finterface,
    expected_condition = "($2 in var%s)",
    file_contents = as.character(1:3)
  )
  test_in_fc(
    rlang::quo(char %in% 1:3),
    finterface = finterface,
    expected_condition = "($1 in var%s)",
    file_contents = paste0("test", 1:3)
  )
  test_in_fc(
    rlang::quo(char %in% paste0("test", 1:3)),
    finterface = finterface,
    expected_condition = "($1 in var%s)",
    file_contents = paste0("test", 1:3)
  )
})

test_that("And-block detection works", {
  finterface <- local_summary_stats_interface()
  expect_true(is_and_block(1))
  expect_true(is_and_block("1"))
  expect_true(
    is_and_block(new_filter_condition(
      rlang::quo(chr == 1),
      finterface
    ))
  )
  expect_true(
    is_and_block(new_filter_condition(
      rlang::quo(pos < 1),
      finterface
    ))
  )
  expect_true(
    is_and_block(new_filter_condition(
      rlang::quo(123 < pos & pos < 456),
      finterface
    ))
  )
  expect_true(
    is_and_block(new_filter_condition(
      rlang::quo(chr == 1 & 123 < pos & pos < 456),
      finterface
    ))
  )

  expect_false(
    is_and_block(new_filter_condition(
      rlang::quo(chr == 1 | 123 < pos & pos < 456),
      finterface
    ))
  )
  expect_false(
    is_and_block(new_filter_condition(
      rlang::quo(chr == 1 & 123 < pos | pos < 456),
      finterface
    ))
  )
  expect_false(
    is_and_block(new_filter_condition(
      rlang::quo(chr == 1 | 123 < pos | pos < 456),
      finterface
    ))
  )
})

test_that("Genomic position conditions are correctly detected", {
  expect_true(is_genomic_symbol(rlang::quo(chr)))
  expect_true(is_genomic_symbol(rlang::quo(pos)))
  expect_false(is_genomic_symbol(rlang::quo(ref)))
  expect_false(is_genomic_symbol(1))

  finterface <- local_summary_stats_interface()

  expect_false(
    has_chromosome_condition(
      new_filter_condition(
        rlang::quo(NULL),
        finterface
      )
    )
  )
  expect_false(
    has_chromosome_condition(
      new_filter_condition(
        rlang::quo(ref == "A"),
        finterface
      )
    )
  )
  expect_false(
    has_chromosome_condition(
      new_filter_condition(
        rlang::quo(pval < .05),
        finterface
      )
    )
  )
  expect_false(
    has_chromosome_condition(
      new_filter_condition(
        rlang::quo(123 < pos & pos < 456 & pval < .05),
        finterface
      )
    )
  )
  expect_true(
    has_chromosome_condition(
      new_filter_condition(
        rlang::quo(chr == 1 & 123 < pos & pos < 456 & pval < .05),
        finterface
      )
    )
  )
  expect_true(
    has_chromosome_condition(
      new_filter_condition(
        rlang::quo(chr == 1 | 123 < pos & pos < 456 & pval < .05),
        finterface
      )
    )
  )

  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::quo(chr == 1),
        finterface
      )
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::quo(pos < 1),
        finterface
      )
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::quo(123 < pos & pos < 456),
        finterface
      )
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::quo(chr == 1 & 123 < pos & pos < 456),
        finterface
      )
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::quo(chr == 1 | 123 < pos & pos < 456),
        finterface
      )
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::quo(chr == 1 & 123 < pos | pos < 456),
        finterface
      )
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::quo(chr == 1 | 123 < pos | pos < 456),
        finterface
      )
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::quo(ref == "A"),
        finterface
      )
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::quo(pval < .05),
        finterface
      )
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::quo(chr == 1 & (pos < 123 | 345 < pos)),
        finterface
      )
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::quo(123 < pos & pos < 456 & pval < .05),
        finterface
      )
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::quo(chr == 1 & 123 < pos & pos < 456 & pval < .05),
        finterface
      )
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::quo(chr == 1 | 123 < pos & pos < 456 & pval < .05),
        finterface
      )
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::quo(chr == 1 & 123 < pos | pos < 456 & pval < .05),
        finterface
      )
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::quo(chr == 1 | 123 < pos | pos < 456 & pval < .05),
        finterface
      )
    )
  )
})

test_that("Parenthesis stripping works", {
  finterface <- local_summary_stats_interface()

  expect_equal(
    new_filter_condition(
      rlang::quo((chr == 1)),
      finterface
    )[[1]],
    as.symbol("lp_filter_condition")
  )
  expect_equal(
    new_filter_condition(
      rlang::quo(chr == 1),
      finterface
    ) |>
      length(),
    0
  )
  expect_equal(
    new_filter_condition(
      rlang::quo((chr == 1)),
      finterface
    ) |>
      strip_parentheses(),
    new_filter_condition(
      rlang::quo(chr == 1),
      finterface
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::quo(((chr == 1))),
      finterface
    ) |>
      strip_parentheses(recursive = TRUE),
    new_filter_condition(
      rlang::quo(chr == 1),
      finterface
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::quo(((chr == 1))),
      finterface
    ) |>
      strip_parentheses(recursive = FALSE),
    new_filter_condition(
      rlang::quo(chr == 1),
      finterface
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::quo(((chr == 1) & (123 < pos & pos < 234))),
      finterface
    ) |>
      strip_parentheses(recursive = FALSE) |>
      rlang::get_expr(),
    new_filter_condition(
      rlang::quo((chr == 1) & (123 < pos & pos < 234)),
      finterface
    ) |>
      rlang::get_expr()
  )
  expect_equal(
    new_filter_condition(
      rlang::quo(((chr == 1) & (pos < 123 | 234 < pos))),
      finterface
    ) |>
      strip_parentheses(recursive = TRUE),
    new_filter_condition(
      rlang::quo(chr == 1 & pos < 123 | chr == 1 & 234 < pos),
      finterface
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::quo(((chr == 1) & (123 < pos & pos < 234))),
      finterface
    ) |>
      strip_parentheses(recursive = FALSE) |>
      rlang::get_expr(),
    new_filter_condition(
      rlang::quo((chr == 1) & (123 < pos & pos < 234)),
      finterface
    ) |>
      rlang::get_expr()
  )
})

test_that("empty_fc | empty_fc does not inherit or_filter_condition_class", {
  finterface <- local_summary_stats_interface()
  fcondition1 <- empty_filter_condition(finterface_env = finterface)
  fcondition2 <- empty_filter_condition(
    finterface_env = attr(fcondition1, "finterface_env")
  )
  expect_equal(
    class(fcondition1 | fcondition2),
    c("filter_condition", "list")
  )
})

test_that("AND code block handling works", {
  finterface <- local_summary_stats_interface()
  expect_no_error(
    new_filter_condition(
      rlang::quo(chr == 1 | chr == 2),
      finterface
    ) |>
      eval_fcondition_w_gregions()
  )
  expect_no_error(
    new_filter_condition(
      rlang::quo(pval < .05 & (chr == 1 | chr == 2)),
      finterface
    ) |>
      eval_fcondition_w_gregions()
  )
})

test_that("Getting genomic regions works", {
  finterface <- local_summary_stats_interface()
  expect_equal(
    new_filter_condition(
      rlang::quo(chr == 1 & 123 <= pos & pos <= 234),
      finterface,
      build = NULL
    ) |>
      genomic_regions(),
    new_genomic_regions(chr = "1", start = 123, end = 234)
  )
  expect_equal(
    new_filter_condition(
      rlang::quo(123 <= pos & pos <= 234),
      finterface
    ) |>
      genomic_regions(),
    new_genomic_regions(start = 123, end = 234)
  )
  expect_equal(
    new_filter_condition(
      rlang::quo(chr == 1 & 123 < pos & pos < 234),
      finterface
    ) |>
      genomic_regions(),
    new_genomic_regions(chr = "1", start = 124, end = 233)
  )
  expect_equal(
    new_filter_condition(
      rlang::quo(chr < 2 & 123 < pos & pos < 234),
      finterface
    ) |>
      genomic_regions(),
    new_genomic_regions(chr = "1", start = 124, end = 233)
  )
  expect_equal(
    new_filter_condition(
      rlang::quo(chr == 1 & 123 <= pos & pos <= 234 |
        chr == "X" & 21 <= pos & pos <= 42),
      finterface
    ) |>
      genomic_regions(),
    new_genomic_regions(
      chr = c("1", "X"),
      start = c(123, 21),
      end = c(234, 42)
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::quo((chr == 1 | chr == "X") & 123 <= pos & pos <= 234),
      finterface
    ) |>
      genomic_regions(),
    new_genomic_regions(
      chr = c("1", "X"),
      start = 123,
      end = 234
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::quo((chr == 1 & chr == "X") & 123 <= pos & pos <= 234),
      finterface
    ) |>
      genomic_regions(),
    empty_genomic_regions()
  )
  expect_equal(
    new_filter_condition(
      rlang::quo((chr == 1 | chr == "X") & 234 <= pos & pos <= 123),
      finterface
    ) |>
      genomic_regions(),
    empty_genomic_regions()
  )
  expect_equal(
    new_filter_condition(
      rlang::quo((chr == 1 | chr == "X") &
        42 < pos & 123 <= pos &
        pos <= 234 & pos < 512),
      finterface
    ) |>
      genomic_regions(),
    new_genomic_regions(
      chr = c("1", "X"),
      start = 123,
      end = 234
    )
  )
})

# Tests consolidated from test-filter_condition_internal.R
test_that("Quoting works properly", {
  expect_equal(
    check_quotes(1, FALSE),
    1
  )
  expect_equal(
    check_quotes("a", FALSE),
    "\"a\""
  )
  expect_equal(
    check_quotes(1, TRUE),
    "\"\\\"1\\\"\""
  )
  expect_equal(
    check_quotes("a", TRUE),
    "\"\\\"a\\\"\""
  )

  expect_equal(
    check_quotes_to_write(1, FALSE),
    1
  )
  expect_equal(
    check_quotes_to_write("a", FALSE),
    "a"
  )
  expect_equal(
    check_quotes_to_write(1, TRUE),
    "\"1\""
  )
  expect_equal(
    check_quotes_to_write("a", TRUE),
    "\"a\""
  )
})

# Tests consolidated from test-fc_condition_functions.R
test_that("Parsing to command line works", {
  finterface <- local_file_interface()
  expect_equal(
    new_filter_condition(rlang::quo(num < 3),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 < 3")
  )
  expect_equal(
    new_filter_condition(rlang::quo(num > 3),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 > 3")
  )
  expect_equal(
    new_filter_condition(rlang::quo(num <= 3),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 <= 3")
  )
  expect_equal(
    new_filter_condition(rlang::quo(num >= 3),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 >= 3")
  )
  expect_equal(
    new_filter_condition(rlang::quo(num == 3),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 == 3")
  )
  expect_equal(
    new_filter_condition(rlang::quo(char == "a"),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$1 == \"a\"")
  )

  expect_equal(
    new_filter_condition(rlang::quo(char < 3 | num > 42),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(
      variable_arrays = NULL,
      condition = "$1 < 3 || $2 > 42",
      additional_files = NULL
    )
  )
  expect_equal(
    new_filter_condition(rlang::quo(char < 3 & num > 42),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(
      variable_arrays = NULL,
      condition = "$1 < 3 && $2 > 42",
      additional_files = NULL
    )
  )
})

test_that("%in% parsing works", {
  finterface <- local_file_interface()
  expect_equal(
    new_filter_condition(
      rlang::quo(char %in% letters[1:5]),
      finterface = finterface
    ),
    structure(
      {
        fcall <- rlang::quo(in_filter_condition(char))
        fexpr <- rlang::get_expr(fcall)
        fexpr[[3]] <- c("a", "b", "c", "d", "e")
        rlang::set_expr(fcall, fexpr)
      },
      class = c("in_filter_condition", "filter_condition", "quosure", "formula"),
      finterface_env = {
        env <- new.env(parent = emptyenv())
        env$finterface <- finterface
        env
      },
      build = "b38",
      genomic_regions = full_genomic_regions(build = "b38")
    )
  )

  test_in_fc(rlang::quo(char %in% letters[1:5]),
    finterface         = finterface,
    expected_condition = "($1 in var%s)",
    file_contents      = letters[1:5]
  )

  expect_true(
    grepl(
      paste0(
        "^awk 'BEGIN\\{\n",
        "  FS = \",\"\n",
        "  OFS = \",\"\n",
        "\\}\n",
        "\\{\n",
        "  if \\(FILENAME == \\\"[^\\\"]+/file([a-f0-9]+)\\\"\\) \\{\n",
        "    var\\1\\[\\$0\\] = 1\n",
        "    next\n",
        "  \\}\n",
        "  else \\{\n",
        "    if \\(FNR == 1\\) next\n",
        "      if \\(\\(\\$1 in var\\1\\)\\) \\{\n",
        "      print \\$0\n",
        "    \\}\n",
        "  \\}\n",
        "\\}' [^[:space:]]+/file\\1 data\\.csv"
      ),
      new_filter_condition(
        rlang::quo(char %in% letters[1:5]),
        finterface = finterface
      ) |>
        fcondition_to_awk(return_only_cmd = TRUE)
    )
  )
})

test_that("Parentheses work as expected", {
  finterface <- local_file_interface()
  expect_equal(
    new_filter_condition(
      rlang::quo((num < 3 & char == "a")),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(
      variable_arrays  = NULL,
      condition        = "($2 < 3 && $1 == \"a\")",
      additional_files = NULL
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::quo((num < 3 & char == "a") | (123 < num & num < 234 & char == "b")),
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
    rlang::quo((num < 3 & char == "a") | (num < 12 & char %in% letters[1:3])),
    finterface = finterface,
    expected_condition = "($2 < 3 && $1 == \"a\") || ($2 < 12 && ($1 in var%s))",
    file_contents = letters[1:3]
  )

  test_in_fc(
    rlang::quo((num < 3 & char == "a") |
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
    rlang::quo((num < 3 & char == "a") |
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
    rlang::quo((3 <= num | num %in% c("a", 1)) & num <= 5),
    finterface = finterface,
    expected_condition = "(3 <= $2 || ($2 in var%s)) && $2 <= 5",
    file_contents = c("a", 1)
  )
})

# Tests for new_filter_condition.genomic_regions constructor
test_that("new_filter_condition.genomic_regions creates filter from regions", {
  finterface <- local_summary_stats_interface()

  gr <- new_genomic_regions(
    chr = "1",
    start = 100,
    end = 200,
    build = "b36"
  )

  fc <- new_filter_condition(gr, finterface)

  expect_true(is.filter_condition(fc))
  expect_equal(build(fc), "b36")
  expect_equal(genomic_regions(fc), gr)
})

test_that("new_filter_condition.genomic_regions handles build parameter", {
  finterface <- local_summary_stats_interface(build = "b36")

  gr <- new_genomic_regions(
    chr = "1",
    start = 100,
    end = 200,
    build = "b37"
  )

  # With explicit build = NULL, should use finterface build
  fc <- new_filter_condition(gr, finterface, build = NULL)
  expect_equal(build(fc), "b36")

  # With explicit build, should liftover the regions
  fc_lifted <- new_filter_condition(gr, finterface, build = "b38")
  expect_equal(build(fc_lifted), "b38")
})

test_that("new_filter_condition.genomic_regions works with multiple regions", {
  finterface <- local_summary_stats_interface()

  gr <- new_genomic_regions(
    chr = c("1", "2"),
    start = c(100, 500),
    end = c(200, 600),
    build = "b36"
  )

  fc <- new_filter_condition(gr, finterface)

  expect_true(is.filter_condition(fc))
  expect_equal(nrow(genomic_regions(fc)), 2)
})

test_that("new_filter_condition.genomic_regions handles empty regions", {
  finterface <- local_summary_stats_interface()

  empty_gr <- empty_genomic_regions(build = "b36")

  fc <- new_filter_condition(empty_gr, finterface)

  expect_true(is.filter_condition(fc))
  expect_equal(nrow(genomic_regions(fc)), 0)
})

test_that("new_filter_condition.name evaluates name to get value", {
  finterface <- local_summary_stats_interface()

  gr <- new_genomic_regions(
    chr = "1",
    start = 100,
    end = 200,
    build = "b36"
  )

  fc <- new_filter_condition(rlang::quo(gr), finterface)

  expect_true(is.filter_condition(fc))
  expect_equal(genomic_regions(fc), gr)
})

test_that("get_used_columns fuctions within nested conditions", {
  finterface <- local_summary_stats_interface()
  fcondition <- new_filter_condition(
    rlang::quo((chr == 1 | pval < .05) | (alt == "A" & ref == "C")),
    finterface
  )
  expect_equal(
    sort(get_used_columns(fcondition)),
    sort(c("alt", "chr", "pval", "ref"))
  )
})

test_that("Not-equal operator works with numeric values", {
  finterface <- local_file_interface()

  expect_no_error(new_filter_condition(
    rlang::quo(num != 3),
    finterface = finterface
  ))

  fc <- new_filter_condition(rlang::quo(num != 3), finterface = finterface)
  expect_equal(fc[[1]], as.symbol("neq_filter_condition"))
  expect_s3_class(fc, "neq_filter_condition")

  result <- eval_fcondition(fc, finterface = finterface)
  expect_equal(result$condition, "$2 != 3")
})

test_that("Not-equal operator works with string values", {
  finterface <- local_file_interface()

  fc <- new_filter_condition(
    rlang::quo(char != "a"),
    finterface = finterface
  )
  expect_equal(fc[[1]], as.symbol("neq_filter_condition"))

  result <- eval_fcondition(fc, finterface = finterface)
  expect_equal(result$condition, "$1 != \"a\"")
})

test_that("Not-equal operator works with quoted values", {
  finterface <- local_file_interface(quote = TRUE)

  result <- new_filter_condition(
    rlang::quo(char != "a"),
    finterface = finterface
  ) |>
    eval_fcondition(finterface = finterface)
  expect_equal(result$condition, "$1 != \"\\\"a\\\"\"")
})

test_that("Not-equal operator combines with other conditions", {
  finterface <- local_file_interface()

  fc <- new_filter_condition(
    rlang::quo(num != 3 & char != "a"),
    finterface = finterface
  )
  result <- eval_fcondition(fc, finterface = finterface)
  expect_equal(result$condition, "$2 != 3 && $1 != \"a\"")

  fc_or <- new_filter_condition(
    rlang::quo(num != 3 | char == "a"),
    finterface = finterface
  )
  result_or <- eval_fcondition(fc_or, finterface = finterface)
  expect_equal(result_or$condition, "$2 != 3 || $1 == \"a\"")
})

test_that("Not-equal operator works with genomic conditions", {
  skip("Not yet implemented")
  finterface <- local_summary_stats_interface()
  fcondition <- new_filter_condition(
    rlang::quo(chr != 1),
    finterface
  )
})
