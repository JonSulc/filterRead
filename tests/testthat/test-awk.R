test_that("Wrapping the condition block works", {
  expect_equal(
    wrap_condition_block(
      condition                     = NULL,
      column_arrays_after_condition = NULL,
      print_prefix                  = NULL
    ),
    "print $0"
  )
  expect_equal(
    wrap_condition_block(
      condition                     = NULL,
      column_arrays_after_condition = "$1 = encoded1[1] OFS encoded1[2]",
      print_prefix                  = NULL
    ),
    "$1 = encoded1[1] OFS encoded1[2]
print $0"
  )
  expect_equal(
    wrap_condition_block(
      condition                     = "$1 == 1",
      column_arrays_after_condition = NULL,
      print_prefix                  = NULL
    ),
    paste0(
      "if ($1 == 1) {\n",
      "  print $0\n",
      "}"
    )
  )
  expect_equal(
    wrap_condition_block(
      condition                     = "$1 == 1",
      column_arrays_after_condition = "$1 = encoded1[1] OFS encoded1[2]",
      print_prefix                  = NULL
    ),
    "if ($1 == 1) {
  $1 = encoded1[1] OFS encoded1[2]
  print $0
}"
  )
  expect_equal(
    wrap_condition_block(
      condition                     = NULL,
      column_arrays_after_condition = NULL,
      print_prefix                  = "tabix[$1] OFS "
    ),
    "print tabix[$1] OFS $0"
  )
  expect_equal(
    wrap_condition_block(
      condition                     = "$1 == 1",
      column_arrays_after_condition = NULL,
      print_prefix                  = "tabix[$1] OFS "
    ),
    "if ($1 == 1) {
  print tabix[$1] OFS $0
}"
  )
  expect_equal(
    wrap_condition_block(
      condition                     = "$1 == 1",
      column_arrays_after_condition = "$1 = encoded1[1] OFS encoded1[2]",
      print_prefix                  = "tabix[$1] OFS "
    ),
    "if ($1 == 1) {
  $1 = encoded1[1] OFS encoded1[2]
  print tabix[$1] OFS $0
}"
  )
})

test_that("Wrapping the main file code works", {
  expect_equal(
    wrap_main_file_code(
      fcondition_awk_dt               = data.table::data.table(index = integer()),
      column_arrays_before_conditions = NULL,
      column_arrays_after_conditions  = NULL
    ),
    "print $0"
  )
  expect_equal(
    wrap_main_file_code(
      fcondition_awk_dt               = data.table::data.table(index = integer()),
      column_arrays_before_conditions = "split($1, encoded1, \":\")",
      column_arrays_after_conditions  = NULL
    ),
    "split($1, encoded1, \":\")
  print $0"
  )
})

test_that("Wrapping the full code block works", {
  expect_equal(
    wrap_full_code_block(
      fcondition_awk_dt = data.table::data.table(index = integer()),
      main_file_code    = NULL
    ),
    "{\n  \n}"
  )
  expect_equal(
    wrap_full_code_block(
      fcondition_awk_dt = data.table::data.table(index = integer()),
      main_file_code    = "print $0"
    ),
    "{
  print $0
}"
  )
  expect_equal(
    wrap_full_code_block(
      fcondition_awk_dt = data.table::data.table(
        awk_code_block  = "if (NR == FNR) {tabix stuff}",
        variable_arrays = "if (FILENAME == dont_read_42.txt) {oops}"
      ),
      main_file_code = "if (something) {print $0}"
    ),
    "{
  if (NR == FNR) {tabix stuff}
  else if (FILENAME == dont_read_42.txt) {oops}
  else if (something) {print $0}
}"
  )
  expect_equal(
    wrap_full_code_block(
      fcondition_awk_dt = data.table::data.table(
        awk_code_block = "if (NR == FNR) {tabix stuff}",
        variable_arrays = list(c(
          "if (FILENAME == dont_read_42.txt) {oops}",
          "if (FILENAME == read_this.txt) {good}"
        ))
      ),
      main_file_code = "if (something) {print $0}"
    ),
    "{
  if (NR == FNR) {tabix stuff}
  else if (FILENAME == dont_read_42.txt) {oops}
  else if (FILENAME == read_this.txt) {good}
  else if (something) {print $0}
}"
  )
})
