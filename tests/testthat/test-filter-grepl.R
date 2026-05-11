test_that("grepl filters by regex on a non-standard column", {
  dt <- data.table::data.table(
    chr    = paste0("chr", 1:4),
    pos    = c(100L, 200L, 300L, 400L),
    Gene   = c(
      "ENSG00000001626.16",
      "ENSG00000001626.15",
      "ENSG00000999999.1",
      "ENSG00000123456.7"
    ),
    effect = c(0.1, 0.2, 0.3, 0.4),
    pval   = c(0.01, 0.02, 0.03, 0.04)
  )
  local_csv_file("data.csv", dt = dt)
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  result <- finterface[grepl("^ENSG00000001626\\.", Gene)]
  expect_equal(
    result$Gene,
    c("ENSG00000001626.16", "ENSG00000001626.15")
  )
})

test_that("%like% is a synonym for grepl with flipped operand order", {
  dt <- data.table::data.table(
    chr    = paste0("chr", 1:3),
    pos    = c(100L, 200L, 300L),
    Gene   = c("ENSG00000001626.16", "ENSG00000999999.1", "ENSG00000123456.7"),
    effect = c(0.1, 0.2, 0.3),
    pval   = c(0.01, 0.02, 0.03)
  )
  local_csv_file("data.csv", dt = dt)
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  result <- finterface[Gene %like% "^ENSG00000001626"]
  expect_equal(result$Gene, "ENSG00000001626.16")
})

test_that("grepl composes with other conditions via &", {
  dt <- data.table::data.table(
    chr    = c("chr1", "chr1", "chr2", "chr2"),
    pos    = c(100L, 200L, 300L, 400L),
    Gene   = c(
      "ENSG00000001626.16",
      "ENSG00000999999.1",
      "ENSG00000001626.15",
      "ENSG00000123456.7"
    ),
    effect = c(0.1, 0.2, 0.3, 0.4),
    pval   = c(0.01, 0.02, 0.03, 0.04)
  )
  local_csv_file("data.csv", dt = dt)
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  result <- finterface[chr == "chr1" & grepl("^ENSG00000001626", Gene)]
  expect_equal(result$Gene, "ENSG00000001626.16")
  expect_equal(result$pos, 100L)
})

test_that("grepl escapes the / delimiter and passes regex syntax through", {
  dt <- data.table::data.table(
    chr    = c("chr1", "chr2"),
    pos    = c(100L, 200L),
    Gene   = c("ENSG00000001626.16", "ENSG_OTHER"),
    effect = c(0.1, 0.2),
    pval   = c(0.01, 0.02)
  )
  local_csv_file("data.csv", dt = dt)
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  # `\.` in the R-side pattern must reach awk as `\.` (literal dot).
  cmd <- finterface[
    grepl("ENSG00000001626\\.16", Gene),
    return_only_cmd = TRUE
  ]
  expect_match(cmd, "ENSG00000001626\\.16", fixed = TRUE)

  # The literal dot really only matches a dot, not arbitrary chars.
  result <- finterface[grepl("ENSG00000001626\\.16", Gene)]
  expect_equal(result$Gene, "ENSG00000001626.16")

  # `/` in the R-side pattern reaches awk as `\/` so the regex literal
  # is not prematurely closed.
  cmd_slash <- finterface[
    grepl("a/b", Gene),
    return_only_cmd = TRUE
  ]
  expect_match(cmd_slash, "a\\/b", fixed = TRUE)
})

test_that("grepl emit uses the ~ operator", {
  dt <- data.table::data.table(
    chr    = "chr1",
    pos    = 100L,
    Gene   = "ENSG00000001626.16",
    effect = 0.1,
    pval   = 0.01
  )
  local_csv_file("data.csv", dt = dt)
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  cmd <- finterface[grepl("^ENSG", Gene), return_only_cmd = TRUE]
  expect_match(cmd, "\\$[0-9]+ ~ /\\^ENSG/", fixed = FALSE)
})

test_that("grepl rejects non-scalar character patterns", {
  dt <- data.table::data.table(
    chr    = "chr1",
    pos    = 100L,
    Gene   = "ENSG00000001626.16",
    effect = 0.1,
    pval   = 0.01
  )
  local_csv_file("data.csv", dt = dt)
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_error(
    finterface[grepl(c("^ENSG", "^rs"), Gene)],
    "single character pattern"
  )
})

test_that("grepl on chr is treated as non-genomic and falls through to awk", {
  dt <- data.table::data.table(
    chr    = c("chr1", "chr10", "chrX"),
    pos    = c(100L, 200L, 300L),
    Gene   = c("a", "b", "c"),
    effect = c(0.1, 0.2, 0.3),
    pval   = c(0.01, 0.02, 0.03)
  )
  local_csv_file("data.csv", dt = dt)
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  result <- finterface[grepl("^chr[0-9]+$", chr)]
  expect_equal(sort(result$chr), c("chr1", "chr10"))
})

test_that("is_regex_filter_condition errors on unexpected input types", {
  expect_error(
    is_regex_filter_condition("not a call"),
    "expects a quosure or call"
  )
  expect_error(
    is_regex_filter_condition(list()),
    "expects a quosure or call"
  )
})
