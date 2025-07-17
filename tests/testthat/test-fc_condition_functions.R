test_that("Parsing to command line works", {
  finterface <- local_file_interface()
  expect_equal(
    new_filter_condition(rlang::expr(num < 3),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 < 3")
  )
  expect_equal(
    new_filter_condition(rlang::expr(num > 3),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 > 3")
  )
  expect_equal(
    new_filter_condition(rlang::expr(num <= 3),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 <= 3")
  )
  expect_equal(
    new_filter_condition(rlang::expr(num >= 3),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 >= 3")
  )
  expect_equal(
    new_filter_condition(rlang::expr(num == 3),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$2 == 3")
  )
  expect_equal(
    new_filter_condition(rlang::expr(char == "a"),
      finterface = finterface
    ) |>
      eval_fcondition(finterface = finterface),
    list(condition = "$1 == \"a\"")
  )

  expect_equal(
    new_filter_condition(rlang::expr(char < 3 | num > 42),
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
    new_filter_condition(rlang::expr(char < 3 & num > 42),
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
    file_contents      = letters[1:5]
  )

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
        "      print [$]0\n",
        "    [}]\n",
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

test_that("Parentheses work as expected", {
  finterface <- local_file_interface()
  expect_equal(
    new_filter_condition(rlang::expr((num < 3 & char == "a")),
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
