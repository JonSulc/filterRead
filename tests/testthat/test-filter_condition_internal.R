test_that("And combinations work", {
  column_indices <- get_indices_from_column_names(letters[24:26])
  chainable_fc <- new_filter_condition(rlang::expr(x < 2))
  non_chainable_fc <- new_filter_condition(rlang::expr(y %in% letters[1:3]))
  non_pipable_fc <- new_filter_condition(rlang::expr(x < 2 | y == 3))

  and_fc <- function(fc1, fc2) {
    ab <- rlang::expr(and_filter_condition())
    ab[2:3] <- list(fc1, fc2)
    class(ab) <- c("filter_condition", "call")
    ab
  }

  expect_equal(
    distributive_and(
      non_chainable_fc,
      chainable_fc
    ),
    and_fc(non_chainable_fc, chainable_fc)
  )
  expect_equal(
    distributive_and(
      chainable_fc,
      chainable_fc
    ),
    and_fc(chainable_fc, chainable_fc)
  )

  expect_equal(
    distributive_and(
      non_pipable_fc,
      non_chainable_fc
    ),
    new_filter_condition(rlang::expr(x < 2 & y %in% letters[1:3] | y == 3 & y %in% letters[1:3]))
  )
})
