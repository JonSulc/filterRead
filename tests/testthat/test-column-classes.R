test_that("EXTRA is read as character even when most rows are NA", {
  # REGENIE writes an optional EXTRA column that is NA on most rows.
  # Without the class override, fread infers it as logical and rbind
  # across files breaks.
  dt <- data.table::data.table(
    chr    = paste0("chr", 1:5),
    pos    = c(100L, 200L, 300L, 400L, 500L),
    ref    = c("A", "C", "G", "T", "A"),
    alt    = c("G", "T", "A", "C", "G"),
    effect = c(0.1, 0.2, 0.3, 0.4, 0.5),
    pval   = c(0.01, 0.02, 0.03, 0.04, 0.05),
    EXTRA  = c(NA, NA, NA, NA, "REGENIE_FLAG_VALUE")
  )
  local_csv_file("data.csv", dt = dt)

  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  result <- finterface[]
  expect_true("EXTRA" %in% names(result))
  expect_type(result$EXTRA, "character")
  expect_equal(result$EXTRA[5], "REGENIE_FLAG_VALUE")
})

test_that("column_class_overrides returns the EXTRA pin", {
  dt <- data.table::data.table(
    chr   = "chr1",
    pos   = 100L,
    ref   = "A",
    alt   = "G",
    EXTRA = NA_character_
  )
  local_csv_file("data.csv", dt = dt)
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  overrides <- column_class_overrides(finterface)
  expect_named(overrides, "character")
  # EXTRA is the last output column in the fixture.
  expect_equal(
    overrides$character,
    length(column_names(finterface))
  )
})

test_that("column_class_overrides is NULL when no overrides apply", {
  dt <- data.table::data.table(
    chr    = "chr1",
    pos    = 100L,
    ref    = "A",
    alt    = "G",
    effect = 0.1,
    pval   = 0.01
  )
  local_csv_file("data.csv", dt = dt)
  finterface <- new_file_interface("data.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  expect_null(column_class_overrides(finterface))
})

test_that("Two REGENIE-shaped files rbind cleanly after read", {
  dt1 <- data.table::data.table(
    chr    = paste0("chr", 1:3),
    pos    = c(100L, 200L, 300L),
    ref    = c("A", "C", "G"),
    alt    = c("G", "T", "A"),
    effect = c(0.1, 0.2, 0.3),
    pval   = c(0.01, 0.02, 0.03),
    EXTRA  = c(NA, NA, NA)
  )
  dt2 <- data.table::data.table(
    chr    = paste0("chr", 4:5),
    pos    = c(400L, 500L),
    ref    = c("T", "A"),
    alt    = c("C", "G"),
    effect = c(0.4, 0.5),
    pval   = c(0.04, 0.05),
    EXTRA  = c("SOME_TAG", NA)
  )
  local_csv_file("a.csv", dt = dt1)
  local_csv_file("b.csv", dt = dt2)

  fi1 <- new_file_interface("a.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")
  fi2 <- new_file_interface("b.csv", build = "b38") |>
    suppressMessages() |>
    withr::with_output_sink(new = "/dev/null")

  combined <- rbind(fi1[], fi2[])
  expect_equal(nrow(combined), 5)
  expect_type(combined$EXTRA, "character")
  expect_equal(combined[chr == "chr4", EXTRA], "SOME_TAG")
})
