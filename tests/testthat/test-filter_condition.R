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
  random_code <- gsub("^/tmp/Rtmp[a-zA-Z]+/file",
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

  expect_equal(new_filter_condition(rlang::expr(x < 3),
                                    finterface = finterface),
               structure(
                 rlang::expr(lt_filter_condition(x, 3)),
                 class = c("filter_condition", "call")
               ))
  expect_equal(new_filter_condition(rlang::expr(x > 3),
                                    finterface = finterface),
               structure(
                 rlang::expr(gt_filter_condition(x, 3)),
                 class = c("filter_condition", "call")
               ))
  expect_equal(new_filter_condition(rlang::expr(x <= 3),
                                    finterface = finterface),
               structure(
                 rlang::expr(lte_filter_condition(x, 3)),
                 class = c("filter_condition", "call")
               ))
  expect_equal(new_filter_condition(rlang::expr(x >= 3),
                                    finterface = finterface),
               structure(
                 rlang::expr(gte_filter_condition(x, 3)),
                 class = c("filter_condition", "call")
               ))
  expect_equal(new_filter_condition(rlang::expr(x == 3),
                                    finterface = finterface),
               structure(
                 rlang::expr(eq_filter_condition(x, 3)),
                 class = c("filter_condition", "call")
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
      class = c("filter_condition", "call")
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
        "  FS = \",\"\n",
        "  OFS = FS\n[}] [{]\n",
        "  if [(]FILENAME == \\\"/tmp/Rtmp([a-zA-Z]+)/file([a-f0-9]+)\\\"[)] [{]\n",
        "    var\\2[[][$]0[]] = 1\n",
        "    next\n",
        "  [}]\n",
        "  \n",
        "  if [(][(][$]1 in var\\2[)][)] [{]\n",
        "    print [$]0\n",
        "  [}]\n",
        "[}]['] /tmp/Rtmp\\1/file\\2 data[.]csv"
      ),
      new_filter_condition(
        rlang::expr(char %in% letters[1:5]),
        finterface = finterface
      ) |>
        fcondition_to_awk(finterface = finterface)
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


  # expect_equal(
  #   new_filter_condition(rlang::expr(num <= 5 & (3 <= num | num %in% c("a", 1))),
  #                        finterface = finterface) |>
  #     to_awk(finterface = finterface) |>
  #     flatten_cl_bits(),
  #   list(
  #     structure(c("$1 <= 5 && 3 <= $1"),
  #               chainable = TRUE,
  #               encoded   = FALSE),
  #     c("$1 <= 5",
  #       "BEGIN {split(\"a 1\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}")
  #   )
  # )
  # expect_equal(
  #   new_filter_condition(rlang::expr((3 <= num | num < 4) & num <= 5),
  #                        finterface = finterface) |>
  #     to_awk(finterface = finterface) |>
  #     flatten_cl_bits(),
  #   list(
  #     structure("(3 <= $1 || $1 < 4) && $1 <= 5",
  #               chainable = TRUE,
  #               encoded   = FALSE)
  #   )
  # )
  # expect_equal(
  #   new_filter_condition(rlang::expr(num <= 5 & (3 <= num | num < 4)),
  #                        finterface = finterface) |>
  #     to_awk(finterface = finterface) |>
  #     flatten_cl_bits(),
  #   list(
  #     structure("$1 <= 5 && (3 <= $1 || $1 < 4)",
  #               chainable = TRUE,
  #               encoded   = FALSE)
  #   )
  # )
  # expect_equal(
  #   new_filter_condition(rlang::expr((3 <= num | num < 4) & char %in% letters[1:3]),
  #                        finterface = finterface) |>
  #     to_awk(finterface = finterface) |>
  #     flatten_cl_bits(),
  #   list(
  #     c("(3 <= $1 || $1 < 4)",
  #       "BEGIN {split(\"a b c\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}")
  #   )
  # )
  # expect_equal(
  #   new_filter_condition(rlang::expr(char %in% letters[1:3] & (3 <= num | num < 4)),
  #                        finterface = finterface) |>
  #     to_awk(finterface = finterface) |>
  #     flatten_cl_bits(),
  #   list(
  #     c("BEGIN {split(\"a b c\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}",
  #       "(3 <= $1 || $1 < 4)")
  #   )
  # )
  #
  # expect_equal(
  #   new_filter_condition(rlang::expr(3 <= num & (num <= 5 & char %in% "a")),
  #                        finterface = finterface) |>
  #     to_awk(finterface = finterface) |>
  #     flatten_cl_bits(),
  #   list(c("3 <= $1",
  #          "$1 <= 5",
  #          "BEGIN {split(\"a\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}"))
  # )
  #
  # expect_equal(
  #   new_filter_condition(
  #     rlang::expr((3 <= num | num %in% c("a", 1)) & (num <= 5 & char == "a")),
  #     finterface = finterface
  #   ) |>
  #     to_awk(finterface = finterface) |>
  #     flatten_cl_bits(),
  #   list(
  #     structure(c("3 <= $1 && ($1 <= 5 && $2 == \"a\")"),
  #               chainable = TRUE,
  #               encoded   = FALSE),
  #     c("BEGIN {split(\"a 1\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}",
  #       "($1 <= 5 && $2 == \"a\")")
  #   )
  # )
  # expect_equal(
  #   new_filter_condition(
  #     rlang::expr((3 <= num | num %in% c("a", 1)) & (num <= 5 & char %in% "a")),
  #     finterface = finterface
  #   ) |>
  #     to_awk(finterface = finterface) |>
  #     flatten_cl_bits(),
  #   list(
  #     c("3 <= $1",
  #       "$1 <= 5",
  #       "BEGIN {split(\"a\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}"),
  #     c("BEGIN {split(\"a 1\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}",
  #       "$1 <= 5",
  #       "BEGIN {split(\"a\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}")
  #   )
  # )
  # expect_equal(
  #   new_filter_condition(
  #     rlang::expr((3 <= num | num %in% c("a", 1)) & (num <= 5 | char %in% "a")),
  #     finterface = finterface
  #   ) |>
  #     to_awk(finterface = finterface) |>
  #     flatten_cl_bits(),
  #   list(
  #     structure(
  #       "3 <= $1 && $1 <= 5",
  #       chainable = TRUE,
  #       encoded   = FALSE
  #     ),
  #     structure(
  #       c("3 <= $1",
  #         "BEGIN {split(\"a\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}")
  #     ),
  #     structure(
  #       c("BEGIN {split(\"a 1\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}",
  #         "$1 <= 5")
  #     ),
  #     structure(
  #       c("BEGIN {split(\"a 1\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}",
  #         "BEGIN {split(\"a\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}")
  #     )
  #   )
  # )
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
