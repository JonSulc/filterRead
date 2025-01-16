
test_that("Basic filter_condition initialization works", {
  expect_no_error(new_filter_condition(rlang::expr(x < 3)))
  expect_no_error(new_filter_condition(rlang::expr(x > 3)))
  expect_no_error(new_filter_condition(rlang::expr(x <= 3)))
  expect_no_error(new_filter_condition(rlang::expr(x >= 3)))
  expect_no_error(new_filter_condition(rlang::expr(x == 3)))

  expect_equal(new_filter_condition(rlang::expr(x < 3)),
               structure(
                 rlang::expr(lt_filter_condition(x, 3)),
                 class = c("filter_condition", "call"),
                 chainable = TRUE
               ))
  expect_equal(new_filter_condition(rlang::expr(x > 3)),
               structure(
                 rlang::expr(gt_filter_condition(x, 3)),
                 class = c("filter_condition", "call"),
                 chainable = TRUE
               ))
  expect_equal(new_filter_condition(rlang::expr(x <= 3)),
               structure(
                 rlang::expr(lte_filter_condition(x, 3)),
                 class = c("filter_condition", "call"),
                 chainable = TRUE
               ))
  expect_equal(new_filter_condition(rlang::expr(x >= 3)),
               structure(
                 rlang::expr(gte_filter_condition(x, 3)),
                 class = c("filter_condition", "call"),
                 chainable = TRUE
               ))
  expect_equal(new_filter_condition(rlang::expr(x == 3)),
               structure(
                 rlang::expr(eq_filter_condition(x, 3)),
                 class = c("filter_condition", "call"),
                 chainable = TRUE
               ))
})

test_that("%in% parsing works", {
  expect_equal(
    new_filter_condition(
      rlang::expr(x %in% letters[1:5])
    ),
    structure(
      rlang::expr(in_filter_condition(x, letters[1:5])),
      class = c("filter_condition", "call"),
      chainable = FALSE
    )
  )
})

test_that("Column name substitution works", {
  expect_equal(
    new_filter_condition(
      rlang::expr(x < 123),
      column_names = list(x = "pos")
    ),
    new_filter_condition(
      rlang::expr(pos < 123)
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(x < 123),
      column_names = list(x = "pos", y = "foo", z = "bar")
    ),
    new_filter_condition(
      rlang::expr(pos < 123)
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(x < 123 | x > 234 & y == "a"),
      column_names = list(x = "pos", y = "foo", z = "bar")
    ),
    new_filter_condition(
      rlang::expr(pos < 123 | pos > 234 & foo == "a")
    )
  )
  expect_equal(
    new_filter_condition(
      rlang::expr(x < 123 | x > 234 & y == "a" | baz <= 22),
      column_names = list(x = "pos", y = "foo", z = "bar")
    ),
    new_filter_condition(
      rlang::expr(pos < 123 | pos > 234 & foo == "a" | baz <= 22)
    )
  )
})

test_that("Parsing to command line works", {
  column_indices <- get_indices_from_column_names(letters[24:26])
  expect_equal(
    new_filter_condition(rlang::expr(x < 3)) |>
      to_awk(column_indices = column_indices),
    structure("$1 < 3", chainable = TRUE)
  )
  expect_equal(
    new_filter_condition(rlang::expr(x > 3)) |>
      to_awk(column_indices = column_indices),
    structure("3 < $1", chainable = TRUE)
  )
  expect_equal(
    new_filter_condition(rlang::expr(x <= 3)) |>
      to_awk(column_indices = column_indices),
    structure("$1 <= 3", chainable = TRUE)
  )
  expect_equal(
    new_filter_condition(rlang::expr(x >= 3)) |>
      to_awk(column_indices = column_indices),
    structure("3 <= $1", chainable = TRUE)
  )
  expect_equal(
    new_filter_condition(rlang::expr(x == 3)) |>
      to_awk(column_indices = column_indices),
    structure("$1 == 3", chainable = TRUE)
  )

  expect_equal(
    new_filter_condition(rlang::expr(x < 3 | y > 42)) |>
      to_awk(column_indices = column_indices),
    structure("$1 < 3 || 42 < $2", chainable = TRUE)
  )
  expect_equal(
    new_filter_condition(rlang::expr(x < 3 & y > 42)) |>
      to_awk(column_indices = column_indices),
    structure("$1 < 3 && 42 < $2", chainable = TRUE)
  )

  expect_equal(
    new_filter_condition(
      rlang::expr(x %in% letters[1:5])
    ) |>
      to_awk(column_indices = column_indices),
    structure(
      "BEGIN {split(\"a b c d e\", vals); for (i in vals) arr[vals[i]]} {if ($1 in arr) print $0}",
      chainable = FALSE
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
    as_command_line(
      new_filter_condition(rlang::expr(
        x < 3 & y < 2
      )),
      filename = "data.csv",
      column_indices = list(x = "$1", y = "$2")
    ),
    list("awk '$1 < 3 && $2 < 2' data.csv")
  )
  expect_equal(
    as_command_line(
      new_filter_condition(rlang::expr(
        x < 3 & y %in% letters[1:3]
      )),
      filename = "data.csv",
      column_indices = list(x = "$1", y = "$2")
    ),
    list("awk '$1 < 3' data.csv | awk 'BEGIN {split(\"a b c\", vals); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}'")
  )
  expect_equal(
    as_command_line(
      new_filter_condition(rlang::expr(
        x < 3 & x > 5 & y %in% letters[1:3]
      )),
      filename = "data.csv",
      column_indices = list(x = "$1", y = "$2")
    ),
    list("awk '$1 < 3 && 5 < $1' data.csv | awk 'BEGIN {split(\"a b c\", vals); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}'")
  )
  expect_equal(
    as_command_line(
      new_filter_condition(rlang::expr(
        x < 3 | x > 5 & y %in% letters[1:3]
      )),
      filename = "data.csv",
      column_indices = list(x = "$1", y = "$2")
    ),
    list("awk '$1 < 3 || 5 < $1' data.csv | awk 'BEGIN {split(\"a b c\", vals); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}'")
  )
  expect_equal(
    as_command_line(
      new_filter_condition(rlang::expr(
        x < 3 | x > 5 & y < 2 & y %in% letters[1:3]
      )),
      filename = "data.csv",
      column_indices = list(x = "$1", y = "$2")
    ),
    list("awk '$1 < 3 || 5 < $1 && $2 < 2' data.csv | awk 'BEGIN {split(\"a b c\", vals); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}'")
  )
  expect_equal(
    as_command_line(
      new_filter_condition(rlang::expr(
        x < 3 & y %in% letters[1:3] & x > 5
      )),
      filename = "data.csv",
      column_indices = list(x = "$1", y = "$2")
    ),
    list("awk '$1 < 3' data.csv | awk 'BEGIN {split(\"a b c\", vals); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}' | awk '5 < $1'")
  )

  expect_equal(
    as_command_line(
      new_filter_condition(rlang::expr(
        x < 3 & y %in% letters[1:3] | x > 5
      )),
      filename = "data.csv",
      column_indices = list(x = "$1", y = "$2")
    ),
    list("awk '$1 < 3' data.csv | awk 'BEGIN {split(\"a b c\", vals); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}'",
         "awk '5 < $1' data.csv")
  )
  expect_equal(
    as_command_line(
      new_filter_condition(rlang::expr(
        x < 3 | y %in% letters[1:3] | x > 5
      )),
      filename = "data.csv",
      column_indices = list(x = "$1", y = "$2")
    ),
    list("awk '$1 < 3' data.csv",
         "awk 'BEGIN {split(\"a b c\", vals); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}' data.csv",
         "awk '5 < $1' data.csv")
  )
})

test_that("Parenthese work as expected", {
  column_indices <- get_indices_from_column_names(letters[24:26])

  expect_equal(
    new_filter_condition(rlang::expr(
      (x < 3 & y == "a")
    )) |>
      to_awk(column_indices),
    structure(
      "($1 < 3 && $2 == a)",
      chainable = TRUE
    )
  )
  expect_equal(
    new_filter_condition(rlang::expr(
      (x < 3 & y == "a") | (123 < x & x < 234 & y == "b")
    )) |>
      to_awk(column_indices),
    structure(
      "($1 < 3 && $2 == a) || (123 < $1 && $1 < 234 && $2 == b)",
      chainable = TRUE
    )
  )
  expect_equal(
    new_filter_condition(rlang::expr(
      (x < 3 & y == "a") | (x < 12 & y %in% letters[1:3])
    )) |>
      to_awk(column_indices) |>
      flatten_cl_bits(),
    structure(
      list(structure("($1 < 3 && $2 == a)",
                     chainable = TRUE),
           c("$1 < 12", "BEGIN {split(\"a b c\", vals); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}"))
    )
  )
  expect_equal(
    new_filter_condition(rlang::expr(
      (x < 3 & y == "a") | (123 < x & x < 234 & y == "b") | (x < 12 & y %in% letters[1:3])
    )) |>
      to_awk(column_indices) |>
      flatten_cl_bits(),
    structure(
      list(structure("($1 < 3 && $2 == a) || (123 < $1 && $1 < 234 && $2 == b)",
                     chainable = TRUE),
           c("$1 < 12", "BEGIN {split(\"a b c\", vals); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}"))
    )
  )
  expect_equal(
    new_filter_condition(rlang::expr(
      (x < 3 & y == "a") | (x < 12 & y %in% letters[1:3]) | (123 < x & x < 234 & y == "b")
    )) |>
      to_awk(column_indices) |>
      flatten_cl_bits(),
    structure(
      list(structure("($1 < 3 && $2 == a)",
                     chainable = TRUE),
           c("$1 < 12", "BEGIN {split(\"a b c\", vals); for (i in vals) arr[vals[i]]} {if ($2 in arr) print $0}"),
           structure("(123 < $1 && $1 < 234 && $2 == b)",
                     chainable = TRUE))
    )
  )
})
