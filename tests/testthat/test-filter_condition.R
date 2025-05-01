
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
    new_filter_condition(rlang::expr(x < 3 | y > 42),
                         finterface = finterface) |>
      to_awk(finterface = finterface),
    structure("$1 < 3 || $2 > 42", chainable = TRUE, encoded = FALSE)
  )
  expect_equal(
    new_filter_condition(rlang::expr(x < 3 & y > 42),
                         finterface = finterface) |>
      to_awk(finterface = finterface),
    structure("$1 < 3 && $2 > 42", chainable = TRUE, encoded = FALSE)
  )

  expect_equal(
    new_filter_condition(
      rlang::expr(x %in% letters[1:5]),
      finterface = finterface
    ) |>
      to_awk(finterface = finterface),
    structure(
      "BEGIN {split(\"a b c d e\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}",
      chainable = FALSE,
      encoded = FALSE
    )
  )
})

test_that("Flattening cl_bits works", {
  and <- \(a, b) {
    structure(list(a, b),
              operation = "and_filter_condition")
  }
  or <- \(a, b) {
    structure(list(a, b),
              operation = "or_filter_condition")
  }
  expect_equal(
    and("a", "a") |>
      flatten_cl_bits(),
    list(c("a", "a"))
  )
  expect_equal(
    and("a", and("a", "a")) |>
      flatten_cl_bits(),
    list(rep("a", 3))
  )
  expect_equal(
    and(and("a", "a"), "a") |>
      flatten_cl_bits(),
    list(rep("a", 3))
  )
  expect_equal(
    and(and("a", "a"), and("a", "a")) |>
      flatten_cl_bits(),
    list(rep("a", 4))
  )
  expect_equal(
    and(and("a", and("a", "a")), "a") |>
      flatten_cl_bits(),
    list(rep("a", 4))
  )
  expect_equal(
    and("a", "a") |>
      or("b") |>
      flatten_cl_bits(),
    list(rep("a", 2),
         "b")
  )
  expect_equal(
    and("a", "a") |>
      or(and("b", "b")) |>
      flatten_cl_bits(),
    list(rep("a", 2),
         rep("b", 2))
  )
  expect_equal(
    and(and("a", "a"), "a") |>
      or(and("b", and("b", "b"))) |>
      flatten_cl_bits(),
    list(rep("a", 3),
         rep("b", 3))
  )
  expect_equal(
    and(and("a", "a"), "a") |>
      or(and("b", and("b", "b"))) |>
      or(and("c", and("c", "c"))) |>
      flatten_cl_bits(),
    list(rep("a", 3),
         rep("b", 3),
         rep("c", 3))
  )

  expect_equal(
    and(list("a"), list("a")) |>
      flatten_cl_bits(),
    list(c("a", "a"))
  )
})

test_that("as_command_line splits/merges conditions where necessary", {
  expect_equal(
    new_filter_condition(rlang::expr(x < 3 & y < 2),
                         finterface = finterface) |>
      as_command_line(finterface = finterface),
    list("awk '$1 < 3 && $2 < 2' data.csv")
  )
  expect_equal(
    new_filter_condition(rlang::expr(x < 3 & y %in% letters[1:3]),
                         finterface = finterface) |>
      as_command_line(finterface = finterface),
    list("awk '$1 < 3' data.csv | awk 'BEGIN {split(\"a b c\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}'")
  )
  expect_equal(
    new_filter_condition(rlang::expr(x < 3 & x > 5 & y %in% letters[1:3]),
                         finterface = finterface) |>
      as_command_line(finterface = finterface),
    list("awk '$1 < 3 && $1 > 5' data.csv | awk 'BEGIN {split(\"a b c\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}'")
  )
  expect_equal(
    new_filter_condition(rlang::expr(x < 3 | x > 5 & y %in% letters[1:3]),
                         finterface = finterface) |>
      as_command_line(finterface = finterface),
    list("awk '$1 < 3' data.csv",
         "awk '$1 > 5' data.csv | awk 'BEGIN {split(\"a b c\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}'")
  )
  expect_equal(
    new_filter_condition(rlang::expr(x < 3 | x > 5 & y < 2 & y %in% letters[1:3]),
                         finterface = finterface) |>
      as_command_line(finterface = finterface),
    list("awk '$1 < 3' data.csv",
         "awk '$1 > 5 && $2 < 2' data.csv | awk 'BEGIN {split(\"a b c\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}'")
  )
  expect_equal(
    new_filter_condition(rlang::expr(x < 3 & y %in% letters[1:3] & x > 5),
                         finterface = finterface) |>
      as_command_line(finterface = finterface),
    list("awk '$1 < 3' data.csv | awk 'BEGIN {split(\"a b c\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}' | awk '$1 > 5'")
  )

  expect_equal(
    new_filter_condition(rlang::expr(x < 3 & y %in% letters[1:3] | x > 5),
                         finterface = finterface) |>
      as_command_line(finterface = finterface),
    list("awk '$1 < 3' data.csv | awk 'BEGIN {split(\"a b c\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}'",
         "awk '$1 > 5' data.csv")
  )
  expect_equal(
    new_filter_condition(rlang::expr(x < 3 | y %in% letters[1:3] | x > 5),
                         finterface = finterface) |>
      as_command_line(finterface = finterface),
    list("awk '$1 < 3' data.csv",
         "awk 'BEGIN {split(\"a b c\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}' data.csv",
         "awk '$1 > 5' data.csv")
  )
})

test_that("Parentheses work as expected", {
  expect_equal(
    new_filter_condition(rlang::expr((x < 3 & y == "a")),
                         finterface = finterface) |>
      to_awk(finterface = finterface),
    structure(
      "($1 < 3 && $2 == \"a\")",
      chainable = TRUE,
      encoded   = FALSE
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::expr((x < 3 & y == "a") | (123 < x & x < 234 & y == "b")),
      finterface = finterface
    ) |>
      to_awk(finterface = finterface),
    structure(
      "($1 < 3 && $2 == \"a\") || (123 < $1 && $1 < 234 && $2 == \"b\")",
      chainable = TRUE,
      encoded   = FALSE
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::expr((x < 3 & y == "a") | (x < 12 & y %in% letters[1:3])),
      finterface = finterface
    ) |>
      to_awk(finterface = finterface) |>
      flatten_cl_bits(),
    structure(
      list(structure("($1 < 3 && $2 == \"a\")",
                     chainable = TRUE,
                     encoded   = FALSE),
           c("$1 < 12",
             "BEGIN {split(\"a b c\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}"))
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::expr((x < 3 & y == "a") | (123 < x & x < 234 & y == "b") | (x < 12 & y %in% letters[1:3])),
      finterface = finterface
    ) |>
      to_awk(finterface = finterface) |>
      flatten_cl_bits(),
    structure(
      list(structure("($1 < 3 && $2 == \"a\") || (123 < $1 && $1 < 234 && $2 == \"b\")",
                     chainable = TRUE,
                     encoded   = FALSE),
           c("$1 < 12", "BEGIN {split(\"a b c\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}"))
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::expr((x < 3 & y == "a") | (x < 12 & y %in% letters[1:3]) | (123 < x & x < 234 & y == "b")),
      finterface = finterface
    ) |>
      to_awk(finterface = finterface) |>
      flatten_cl_bits(),
    structure(
      list(structure("($1 < 3 && $2 == \"a\")",
                     chainable = TRUE,
                     encoded   = FALSE),
           c("$1 < 12", "BEGIN {split(\"a b c\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}"),
           structure("(123 < $1 && $1 < 234 && $2 == \"b\")",
                     chainable = TRUE,
                     encoded   = FALSE))
    )
  )

  expect_equal(
    new_filter_condition(rlang::expr((3 <= x | x %in% c("a", 1)) & x <= 5),
                         finterface = finterface) |>
      to_awk(finterface = finterface) |>
      flatten_cl_bits(),
    list(
      structure(c("3 <= $1 && $1 <= 5"),
                chainable = TRUE,
                encoded   = FALSE),
      c("BEGIN {split(\"a 1\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}",
        "$1 <= 5")
    )
  )
  expect_equal(
    new_filter_condition(rlang::expr(x <= 5 & (3 <= x | x %in% c("a", 1))),
                         finterface = finterface) |>
      to_awk(finterface = finterface) |>
      flatten_cl_bits(),
    list(
      structure(c("$1 <= 5 && 3 <= $1"),
                chainable = TRUE,
                encoded   = FALSE),
      c("$1 <= 5",
        "BEGIN {split(\"a 1\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}")
    )
  )
  expect_equal(
    new_filter_condition(rlang::expr((3 <= x | x < 4) & x <= 5),
                         finterface = finterface) |>
      to_awk(finterface = finterface) |>
      flatten_cl_bits(),
    list(
      structure("(3 <= $1 || $1 < 4) && $1 <= 5",
                chainable = TRUE,
                encoded   = FALSE)
    )
  )
  expect_equal(
    new_filter_condition(rlang::expr(x <= 5 & (3 <= x | x < 4)),
                         finterface = finterface) |>
      to_awk(finterface = finterface) |>
      flatten_cl_bits(),
    list(
      structure("$1 <= 5 && (3 <= $1 || $1 < 4)",
                chainable = TRUE,
                encoded   = FALSE)
    )
  )
  expect_equal(
    new_filter_condition(rlang::expr((3 <= x | x < 4) & y %in% letters[1:3]),
                         finterface = finterface) |>
      to_awk(finterface = finterface) |>
      flatten_cl_bits(),
    list(
      c("(3 <= $1 || $1 < 4)",
        "BEGIN {split(\"a b c\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}")
    )
  )
  expect_equal(
    new_filter_condition(rlang::expr(y %in% letters[1:3] & (3 <= x | x < 4)),
                         finterface = finterface) |>
      to_awk(finterface = finterface) |>
      flatten_cl_bits(),
    list(
      c("BEGIN {split(\"a b c\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}",
        "(3 <= $1 || $1 < 4)")
    )
  )

  expect_equal(
    new_filter_condition(rlang::expr(3 <= x & (x <= 5 & y %in% "a")),
                         finterface = finterface) |>
      to_awk(finterface = finterface) |>
      flatten_cl_bits(),
    list(c("3 <= $1",
           "$1 <= 5",
           "BEGIN {split(\"a\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}"))
  )

  expect_equal(
    new_filter_condition(
      rlang::expr((3 <= x | x %in% c("a", 1)) & (x <= 5 & y == "a")),
      finterface = finterface
    ) |>
      to_awk(finterface = finterface) |>
      flatten_cl_bits(),
    list(
      structure(c("3 <= $1 && ($1 <= 5 && $2 == \"a\")"),
                chainable = TRUE,
                encoded   = FALSE),
      c("BEGIN {split(\"a 1\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}",
        "($1 <= 5 && $2 == \"a\")")
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::expr((3 <= x | x %in% c("a", 1)) & (x <= 5 & y %in% "a")),
      finterface = finterface
    ) |>
      to_awk(finterface = finterface) |>
      flatten_cl_bits(),
    list(
      c("3 <= $1",
        "$1 <= 5",
        "BEGIN {split(\"a\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}"),
      c("BEGIN {split(\"a 1\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}",
        "$1 <= 5",
        "BEGIN {split(\"a\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}")
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::expr((3 <= x | x %in% c("a", 1)) & (x <= 5 | y %in% "a")),
      finterface = finterface
    ) |>
      to_awk(finterface = finterface) |>
      flatten_cl_bits(),
    list(
      structure(
        "3 <= $1 && $1 <= 5",
        chainable = TRUE,
        encoded   = FALSE
      ),
      structure(
        c("3 <= $1",
          "BEGIN {split(\"a\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}")
      ),
      structure(
        c("BEGIN {split(\"a 1\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}",
          "$1 <= 5")
      ),
      structure(
        c("BEGIN {split(\"a 1\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}",
          "BEGIN {split(\"a\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}")
      )
    )
  )
})

test_that("Comma-separated values are handled correctly", {
  expect_equal(
    new_filter_condition(
      rlang::expr(x %in% letters[1:5]),
      finterface = dummy_finterface(sep = ",")
    ) |>
      as_command_line(finterface = dummy_finterface(sep = ",")),
    list(
      "awk -F',' 'BEGIN {split(\"a b c d e\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}' data.csv"
    )
  )
})

test_that("Quoted values are handled correctly", {
  expect_equal(
    new_filter_condition(rlang::expr(x == "a"),
                         finterface = finterface) |>
      as_command_line(finterface = finterface),
    list("awk '$1 == \"a\"' data.csv")
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(x == "a"),
      finterface = dummy_finterface(quoted_values = list(test = TRUE, x = FALSE))
    ) |>
      as_command_line(
        finterface = dummy_finterface(quoted_values = list(test = TRUE, x = FALSE))
      ),
    list("awk '$1 == \"a\"' data.csv")
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(x == "a"),
      finterface = dummy_finterface(quoted_values = list(test = FALSE, x = TRUE))
    ) |>
      as_command_line(
        finterface = dummy_finterface(quoted_values = list(test = FALSE, x = TRUE))
      ),
    list("awk '$1 == \"\\\"a\\\"\"' data.csv")
  )
  expect_equal(
    new_filter_condition(
      rlang::expr("a" == x),
      finterface = dummy_finterface(quoted_values = list(test = TRUE, x = FALSE))
    ) |>
      as_command_line(
        finterface = dummy_finterface(quoted_values = list(test = TRUE, x = FALSE))
      ),
    list("awk '\"a\" == $1' data.csv")
  )
  expect_equal(
    new_filter_condition(
      rlang::expr("a" == x),
      finterface = dummy_finterface(quoted_values = list(test = FALSE, x = TRUE))
    ) |>
      as_command_line(
        finterface = dummy_finterface(quoted_values = list(test = FALSE, x = TRUE)
      )),
    list("awk '\"\\\"a\\\"\" == $1' data.csv")
  )

  expect_equal(
    new_filter_condition(
      rlang::expr(x %in% c("a", 1)),
      finterface = dummy_finterface(quoted_values = list(test = FALSE, x = TRUE))
    ) |>
      as_command_line(
        finterface = dummy_finterface(quoted_values = list(test = FALSE, x = TRUE))
      ),
    list("awk 'BEGIN {split(\"\\\"a\\\" \\\"1\\\"\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}' data.csv")
  )
})

test_that("Prefixes are handled properly",  {
  finterface <- local_file_interface()
  expect_equal(
    new_filter_condition(
      rlang::expr(x == 1),
      finterface = dummy_finterface(prefixes = list(x = "test"))
    ) |>
      as_command_line(finterface = finterface),
    list("awk '$1 == \"test1\"' data.csv")
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(x == "test1"),
      finterface = dummy_finterface(prefixes = list(x = "test"))
    ) |>
      as_command_line(finterface = finterface),
    list("awk '$1 == \"test1\"' data.csv")
  )

  expect_equal(
    new_filter_condition(
      rlang::expr(x %in% 1:3),
      finterface = dummy_finterface(prefixes = list(x = "test"))
    ) |>
      as_command_line(finterface = finterface),
    list("awk 'BEGIN {split(\"test1 test2 test3\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}' data.csv")
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(x %in% paste0("test", 1:3)),
      finterface = dummy_finterface(prefixes = list(x = "test"))
    ) |>
      as_command_line(finterface = finterface),
    list("awk 'BEGIN {split(\"test1 test2 test3\", vals, \" \"); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}' data.csv")
  )
})
