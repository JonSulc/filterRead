test_that("Basic filter_condition initialization works", {
  finterface <- local_file_interface() |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_no_error(new_filter_condition(rlang::expr(x < 3),
    finterface = finterface
  ))
  expect_no_error(new_filter_condition(rlang::expr(x > 3),
    finterface = finterface
  ))
  expect_no_error(new_filter_condition(rlang::expr(x <= 3),
    finterface = finterface
  ))
  expect_no_error(new_filter_condition(rlang::expr(x >= 3),
    finterface = finterface
  ))
  expect_no_error(new_filter_condition(rlang::expr(x == 3),
    finterface = finterface
  ))

  finterface_env <- {
    env <- new.env(parent = emptyenv())
    env$finterface <- finterface
    env
  }

  expect_equal(
    new_filter_condition(rlang::expr(x < 3),
      finterface = finterface
    ),
    structure(
      rlang::expr(lt_filter_condition(x, 3)),
      class = c("filter_condition", "call"),
      finterface_env = finterface_env
    )
  )
  expect_equal(
    new_filter_condition(rlang::expr(x > 3),
      finterface = finterface
    ),
    structure(
      rlang::expr(gt_filter_condition(x, 3)),
      class = c("filter_condition", "call"),
      finterface_env = finterface_env
    )
  )
  expect_equal(
    new_filter_condition(rlang::expr(x <= 3),
      finterface = finterface
    ),
    structure(
      rlang::expr(lte_filter_condition(x, 3)),
      class = c("filter_condition", "call"),
      finterface_env = finterface_env
    )
  )
  expect_equal(
    new_filter_condition(rlang::expr(x >= 3),
      finterface = finterface
    ),
    structure(
      rlang::expr(gte_filter_condition(x, 3)),
      class = c("filter_condition", "call"),
      finterface_env = finterface_env
    )
  )
  expect_equal(
    new_filter_condition(rlang::expr(x == 3),
      finterface = finterface
    ),
    structure(
      rlang::expr(eq_filter_condition(x, 3)),
      class = c("filter_condition", "call"),
      finterface_env = finterface_env
    )
  )
})

test_that("Passing variables or complex objects works", {
  finterface <- local_summary_stats_interface() |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  my_chr <- 3
  expect_equal(
    new_filter_condition(rlang::expr(chr == my_chr),
      finterface = finterface
    ),
    new_filter_condition(rlang::expr(chr == 3),
      finterface = finterface
    )
  )
  expect_equal(
    new_filter_condition(rlang::expr(chr == 2 * my_chr),
      finterface = finterface
    ),
    new_filter_condition(rlang::expr(chr == 6),
      finterface = finterface
    )
  )
})

test_that("Quoted values are handled correctly", {
  finterface <- local_file_interface(quote = TRUE) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    new_filter_condition(rlang::expr(char == "a"),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$1 == \"\\\"a\\\"\"")
  )
  expect_equal(
    new_filter_condition(rlang::expr(num == "a"),
      finterface = finterface
    ) |>
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

test_that("Prefixes are handled properly", {
  finterface <- local_file_interface(prefix = list(char = "test")) |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
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
  finterface <- local_summary_stats_interface() |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
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

  finterface <- local_summary_stats_interface() |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_false(
    has_chromosome_condition(
      new_filter_condition(
        rlang::expr(NULL),
        finterface
      )
    )
  )
  expect_false(
    has_chromosome_condition(
      new_filter_condition(
        rlang::expr(ref == "A"),
        finterface
      )
    )
  )
  expect_false(
    has_chromosome_condition(
      new_filter_condition(
        rlang::expr(pval < .05),
        finterface
      )
    )
  )
  expect_false(
    has_chromosome_condition(
      new_filter_condition(
        rlang::expr(123 < pos & pos < 456 & pval < .05),
        finterface
      )
    )
  )
  expect_true(
    has_chromosome_condition(
      new_filter_condition(
        rlang::expr(chr == 1 & 123 < pos & pos < 456 & pval < .05),
        finterface
      )
    )
  )
  expect_true(
    has_chromosome_condition(
      new_filter_condition(
        rlang::expr(chr == 1 | 123 < pos & pos < 456 & pval < .05),
        finterface
      )
    )
  )

  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::expr(chr == 1),
        finterface
      )
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::expr(pos < 1),
        finterface
      )
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::expr(123 < pos & pos < 456),
        finterface
      )
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::expr(chr == 1 & 123 < pos & pos < 456),
        finterface
      )
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::expr(chr == 1 | 123 < pos & pos < 456),
        finterface
      )
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::expr(chr == 1 & 123 < pos | pos < 456),
        finterface
      )
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::expr(chr == 1 | 123 < pos | pos < 456),
        finterface
      )
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::expr(ref == "A"),
        finterface
      )
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::expr(pval < .05),
        finterface
      )
    )
  )
  expect_false(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::expr(chr == 1 & (pos < 123 | 345 < pos)),
        finterface
      )
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::expr(123 < pos & pos < 456 & pval < .05),
        finterface
      )
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::expr(chr == 1 & 123 < pos & pos < 456 & pval < .05),
        finterface
      )
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::expr(chr == 1 | 123 < pos & pos < 456 & pval < .05),
        finterface
      )
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::expr(chr == 1 & 123 < pos | pos < 456 & pval < .05),
        finterface
      )
    )
  )
  expect_true(
    has_non_genomic_condition(
      new_filter_condition(
        rlang::expr(chr == 1 | 123 < pos | pos < 456 & pval < .05),
        finterface
      )
    )
  )
})

test_that("Parenthesis stripping works", {
  finterface <- local_summary_stats_interface() |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_equal(
    new_filter_condition(
      rlang::expr((chr == 1)),
      finterface
    )[[1]],
    as.symbol("lp_filter_condition")
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(chr == 1),
      finterface
    )[[1]],
    as.symbol("eq_filter_condition")
  )
  expect_equal(
    new_filter_condition(
      rlang::expr((chr == 1)),
      finterface
    ) |>
      strip_parentheses(),
    new_filter_condition(
      rlang::expr(chr == 1),
      finterface
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(((chr == 1))),
      finterface
    ) |>
      strip_parentheses(recursive = TRUE),
    new_filter_condition(
      rlang::expr(chr == 1),
      finterface
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(((chr == 1))),
      finterface
    ) |>
      strip_parentheses(recursive = FALSE),
    new_filter_condition(
      rlang::expr(chr == 1),
      finterface
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(((chr == 1) & (123 < pos & pos < 234))),
      finterface
    ) |>
      strip_parentheses(recursive = FALSE),
    new_filter_condition(
      rlang::expr((chr == 1) & (123 < pos & pos < 234)),
      finterface
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(((chr == 1) & (pos < 123 | 234 < pos))),
      finterface
    ) |>
      strip_parentheses(recursive = TRUE),
    new_filter_condition(
      rlang::expr(chr == 1 & pos < 123 | chr == 1 & 234 < pos),
      finterface
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(((chr == 1) & (123 < pos & pos < 234))),
      finterface
    ) |>
      strip_parentheses(recursive = FALSE),
    new_filter_condition(
      rlang::expr((chr == 1) & (123 < pos & pos < 234)),
      finterface
    )
  )
})

test_that("Getting genomic regions works", {
  finterface <- local_summary_stats_interface() |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  expect_equal(
    new_filter_condition(
      rlang::expr(chr == 1 & 123 <= pos & pos <= 234),
      finterface
    ) |>
      make_genomic_ranges(),
    data.table::data.table(chr = "1", start = 123, end = 234)
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(123 <= pos & pos <= 234),
      finterface
    ) |>
      make_genomic_ranges(),
    data.table::data.table(chr = c(1:22, "X", "Y", "MT"), start = 123, end = 234)
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(chr == 1 & 123 < pos & pos < 234),
      finterface
    ) |>
      make_genomic_ranges(),
    data.table::data.table(chr = "1", start = 124, end = 233)
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(chr < 2 & 123 < pos & pos < 234),
      finterface
    ) |>
      make_genomic_ranges(),
    data.table::data.table(chr = "1", start = 124, end = 233)
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(chr == 1 & 123 <= pos & pos <= 234 |
        chr == "X" & 21 <= pos & pos <= 42),
      finterface
    ) |>
      make_genomic_ranges(),
    data.table::data.table(chr = c("1", "X"), start = c(123, 21), end = c(234, 42))
  )
  expect_equal(
    new_filter_condition(
      rlang::expr((chr == 1 | chr == "X") & 123 <= pos & pos <= 234),
      finterface
    ) |>
      make_genomic_ranges(),
    data.table::data.table(chr = c("1", "X"), start = 123, end = 234)
  )
  expect_equal(
    new_filter_condition(
      rlang::expr((chr == 1 & chr == "X") & 123 <= pos & pos <= 234),
      finterface
    ) |>
      make_genomic_ranges(),
    data.table::data.table(chr = character(), start = numeric(), end = numeric())
  )
  expect_equal(
    new_filter_condition(
      rlang::expr((chr == 1 | chr == "X") & 234 <= pos & pos <= 123),
      finterface
    ) |>
      make_genomic_ranges(),
    data.table::data.table(chr = c("1", "X"), start = 234, end = 123)
  )
  expect_equal(
    new_filter_condition(
      rlang::expr((chr == 1 | chr == "X") &
        42 < pos & 123 <= pos &
        pos <= 234 & pos < 512),
      finterface
    ) |>
      make_genomic_ranges(),
    data.table::data.table(chr = c("1", "X"), start = 123, end = 234)
  )
})

test_that("Combining genomic ranges works", {
  expect_equal(
    combine_genomic_ranges(
      data.table::data.table(chr = 1, start = 123, end = 234),
      data.table::data.table(chr = 1, start = 42, end = 221)
    ),
    data.table::data.table(chr = 1, start = 123, end = 221)
  )
  expect_equal(
    combine_genomic_ranges(
      data.table::data.table(chr = 1, start = 123, end = 234),
      data.table::data.table(chr = 2, start = 42, end = 221)
    ),
    data.table::data.table(chr = numeric(), start = numeric(), end = numeric())
  )
  expect_equal(
    combine_genomic_ranges(
      NULL,
      data.table::data.table(chr = 2, start = 42, end = 221)
    ),
    data.table::data.table(chr = 2, start = 42, end = 221)
  )
  expect_equal(
    combine_genomic_ranges(
      data.table::data.table(chr = 1, start = 123, end = 234),
      NULL
    ),
    data.table::data.table(chr = 1, start = 123, end = 234)
  )
  expect_equal(
    combine_genomic_ranges(
      data.table::data.table(chr = "1", start = NA_real_, end = NA_real_),
      data.table::data.table(chr = as.character(c(1:22, "X", "Y", "MT")), start = NA_real_, end = 122)
    ),
    data.table::data.table(chr = "1", start = NA_real_, end = 122)
  )
})
