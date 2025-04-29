test_that("And combinations work", {
  chainable_fc <- new_filter_condition(rlang::expr(x < 2),
                                       finterface = dummy_finterface())
  non_chainable_fc <- new_filter_condition(rlang::expr(y %in% letters[1:3]),
                                           finterface = dummy_finterface())
  non_pipable_fc <- new_filter_condition(rlang::expr(x < 2 | y == 3),
                                         finterface = dummy_finterface())

  and_fc <- function(fc1, fc2) {
    ab <- rlang::expr(and_filter_condition())
    ab[2:3] <- list(fc1, fc2)
    class(ab) <- c("filter_condition", "call")
    ab
  }

  expect_equal(
    distribute_and_fc(
      non_chainable_fc,
      chainable_fc
    ),
    and_fc(non_chainable_fc, chainable_fc)
  )
  expect_equal(
    distribute_and_fc(
      chainable_fc,
      chainable_fc
    ),
    and_fc(chainable_fc, chainable_fc)
  )

  expect_equal(
    distribute_and_fc(
      non_pipable_fc,
      non_chainable_fc
    ),
    new_filter_condition(rlang::expr(x < 2 & y %in% letters[1:3] | y == 3 & y %in% letters[1:3]),
                         finterface = dummy_finterface())
  )
})
