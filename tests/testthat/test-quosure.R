test_that("variable resolution works within functions", {
  # Initial implementation using exprs failed to correctly resolve variables
  # when passed through functions
  finterface <- local_summary_stats_interface()

  get_each_chromosome <- function(
    finterface,
    chr_names = 1:22
  ) {
    lapply(
      chr_names,
      \(chr_name) finterface[chr == chr_name]
    )
  }
  expect_no_error(
    get_each_chromosome(finterface)
  )
})
