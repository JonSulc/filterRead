# Test Internal AWK Code Generation Functions
# These are comprehensive tests for the internal wrapping functions

test_that("wrap_condition_block works with all parameter combinations", {
  # Basic case: no conditions, arrays, or prefixes
  expect_equal(
    wrap_condition_block(
      condition = NULL,
      column_arrays_after_conditions = NULL,
      print_prefix = NULL
    ),
    "print $0"
  )

  # With column arrays only
  expect_equal(
    wrap_condition_block(
      condition = NULL,
      column_arrays_after_conditions = "$1 = encoded1[1] OFS encoded1[2]",
      print_prefix = NULL
    ),
    "$1 = encoded1[1] OFS encoded1[2]\nprint $0"
  )

  # With condition only
  expect_equal(
    wrap_condition_block(
      condition = "$1 == 1",
      column_arrays_after_conditions = NULL,
      print_prefix = NULL
    ),
    paste0(
      "if ($1 == 1) {\n",
      "  print $0\n",
      "}"
    )
  )

  # With condition and column arrays
  expect_equal(
    wrap_condition_block(
      condition = "$1 == 1",
      column_arrays_after_conditions = "$1 = encoded1[1] OFS encoded1[2]",
      print_prefix = NULL
    ),
    "if ($1 == 1) {\n  $1 = encoded1[1] OFS encoded1[2]\n  print $0\n}"
  )

  # With print prefix only (tabix functionality)
  expect_equal(
    wrap_condition_block(
      condition = NULL,
      column_arrays_after_conditions = NULL,
      print_prefix = "tabix[$1] OFS "
    ),
    "print tabix[$1] OFS $0"
  )

  # With condition and print prefix
  expect_equal(
    wrap_condition_block(
      condition = "$1 == 1",
      column_arrays_after_conditions = NULL,
      print_prefix = "tabix[$1] OFS "
    ),
    "if ($1 == 1) {\n  print tabix[$1] OFS $0\n}"
  )

  # All parameters combined
  expect_equal(
    wrap_condition_block(
      condition = "$1 == 1",
      column_arrays_after_conditions = "$1 = encoded1[1] OFS encoded1[2]",
      print_prefix = "tabix[$1] OFS "
    ),
    paste0(
      "if ($1 == 1) {\n",
      "  $1 = encoded1[1] OFS encoded1[2]\n",
      "  print tabix[$1] OFS $0\n",
      "}"
    )
  )
})

test_that("wrap_condition_block works with nlines parameter", {
  # With nlines only
  expect_equal(
    wrap_condition_block(
      condition = NULL,
      column_arrays_after_conditions = NULL,
      print_prefix = NULL,
      nlines = 100
    ),
    "if (++output_lines <= max_lines) print $0; else exit"
  )

  # With condition and nlines
  expect_equal(
    wrap_condition_block(
      condition = "$1 == 1",
      column_arrays_after_conditions = NULL,
      print_prefix = NULL,
      nlines = 50
    ),
    paste0(
      "if ($1 == 1) {\n",
      "  if (++output_lines <= max_lines) print $0; else exit\n",
      "}"
    )
  )

  # With all parameters including nlines
  expect_equal(
    wrap_condition_block(
      condition = "$2 > 100",
      column_arrays_after_conditions = "$3 = encoded2[1] OFS encoded2[2]",
      print_prefix = "rsid0[$4] OFS ",
      nlines = 25
    ),
    paste0(
      "if ($2 > 100) {\n",
      "  $3 = encoded2[1] OFS encoded2[2]\n",
      "  if (++output_lines <= max_lines) print rsid0[$4] OFS $0; else exit\n",
      "}"
    )
  )
})

test_that("wrap_condition_block works with rsid_condition parameter", {
  # With rsid condition only
  expect_equal(
    wrap_condition_block(
      condition = NULL,
      column_arrays_after_conditions = NULL,
      print_prefix = NULL,
      rsid_condition = "$3 in rsid0"
    ),
    "if ($3 in rsid0) {\n  print $0\n}"
  )

  # With regular condition and rsid condition
  expect_equal(
    wrap_condition_block(
      condition = "$1 == 1",
      column_arrays_after_conditions = NULL,
      print_prefix = NULL,
      rsid_condition = "$3 in rsid0"
    ),
    paste0(
      "if ($3 in rsid0) {\n",
      "  if ($1 == 1) {\n",
      "    print $0\n",
      "  }\n",
      "}"
    )
  )
})

test_that("wrap_main_file_code works with various parameter combinations", {
  # Basic case
  expect_equal(
    wrap_main_file_code(
      finterface = list(comment_prefix = NULL, trim_prefix = NULL),
      fcondition_awk_dt = data.table::data.table(index = integer()),
      column_arrays_before_conditions = NULL,
      column_arrays_after_conditions = NULL
    ),
    "print $0"
  )

  # With before conditions arrays
  expect_equal(
    wrap_main_file_code(
      finterface = list(comment_prefix = NULL, trim_prefix = NULL),
      fcondition_awk_dt = data.table::data.table(index = integer()),
      column_arrays_before_conditions = "split($1, encoded1, \":\")",
      column_arrays_after_conditions = NULL
    ),
    "split($1, encoded1, \":\")\n  print $0"
  )

  # With prefix handling
  expect_equal(
    wrap_main_file_code(
      finterface = list(comment_prefix = "^##", trim_prefix = "^#"),
      fcondition_awk_dt = data.table::data.table(index = integer()),
      column_arrays_before_conditions = NULL,
      column_arrays_after_conditions = NULL
    ),
    "gsub(/^#/, \"\", $0)\n  print $0"
  )

  # With nlines
  expect_equal(
    wrap_main_file_code(
      finterface = list(comment_prefix = NULL, trim_prefix = NULL),
      fcondition_awk_dt = data.table::data.table(index = integer()),
      column_arrays_before_conditions = NULL,
      column_arrays_after_conditions = NULL,
      nlines = 100
    ),
    "if (++output_lines <= max_lines) print $0; else exit"
  )
})

test_that("wrap_full_code_block works with empty nlines", {
  expect_equal(
    wrap_condition_block(
      column_arrays_after_conditions = "$1 = \"chr\"$1",
      nlines = NULL
    ),
    "$1 = \"chr\"$1\nprint $0"
  )
  # Because NULL + 1 = integer(0), the function also neeed to handle that
  expect_equal(
    wrap_condition_block(
      column_arrays_after_conditions = "$1 = \"chr\"$1",
      nlines = integer(0)
    ),
    "$1 = \"chr\"$1\nprint $0"
  )
})

test_that("wrap_full_code_block works with complex data structures", {
  # Basic case with no conditions
  expect_equal(
    wrap_full_code_block(
      fcondition_awk_dt = data.table::data.table(index = integer()),
      main_file_code = NULL
    ),
    "{\n  \n}"
  )

  # Basic case with main file code
  expect_equal(
    wrap_full_code_block(
      fcondition_awk_dt = data.table::data.table(index = integer()),
      main_file_code = "print $0"
    ),
    "{\n  print $0\n}"
  )

  # With awk_code_block and variable_arrays
  expect_equal(
    wrap_full_code_block(
      fcondition_awk_dt = data.table::data.table(
        awk_code_block = "if (NR == FNR) {tabix stuff}",
        variable_arrays = "if (FILENAME == dont_read_42.txt) {oops}"
      ),
      main_file_code = "if (something) {print $0}"
    ),
    paste0(
      "{\n",
      "  if (NR == FNR) {tabix stuff}\n",
      "  else if (FILENAME == dont_read_42.txt) {oops}\n",
      "  else if (something) {print $0}\n",
      "}"
    )
  )

  # With nested arrays in variable_arrays
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
    paste0(
      "{\n",
      "  if (NR == FNR) {tabix stuff}\n",
      "  else if (FILENAME == dont_read_42.txt) {oops}\n",
      "  else if (FILENAME == read_this.txt) {good}\n",
      "  else if (something) {print $0}\n",
      "}"
    )
  )

  # Complex case with multiple rows in fcondition_awk_dt
  expect_equal(
    wrap_full_code_block(
      fcondition_awk_dt = data.table::data.table(
        awk_code_block = c(
          "if (NR == FNR) {rsid0[$3]=$1 OFS $2}",
          "if (NR == FNR + 1) {rsid1[$3]=$1 OFS $2}"
        ),
        variable_arrays = list(
          "if (FILENAME == file1.txt) {var1[$0] = 1; next}",
          "if (FILENAME == file2.txt) {var2[$0] = 1; next}"
        )
      ),
      main_file_code = "if ($3 in rsid0 || $3 in rsid1) {print rsid0[$3] OFS $0}"
    ),
    paste0(
      "{\n",
      "  if (NR == FNR) {rsid0[$3]=$1 OFS $2}\n",
      "  else if (NR == FNR + 1) {rsid1[$3]=$1 OFS $2}\n",
      "  else if (FILENAME == file1.txt) {var1[$0] = 1; next}\n",
      "  else if (FILENAME == file2.txt) {var2[$0] = 1; next}\n",
      "  else if ($3 in rsid0 || $3 in rsid1) {print rsid0[$3] OFS $0}\n",
      "}"
    )
  )
})
