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
