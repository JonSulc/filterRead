#' @import data.table

and_combine_fconditions <- function(
  fcondition1,
  fcondition2
) {
  if (as.character(fcondition1[[1]]) == "lp_filter_condition") {
    fcondition1[[2]] <- distributive_and(fcondition1[[2]], fcondition2)
  }
}

distributive_and <- function(
  lp_contents,
  fcondition_to_distribute
) {
  if (is_chainable(lp_contents)) {
    to_return <- rlang::expr(and_filter_condition())
    to_return[2:3] <- list(lp_contents, fcondition_to_distribute)
    class(to_return) <- c("filter_condition", class(to_return)) |>
      unique()
    return(to_return)
  }

  if (!is_chainable(lp_contents)) {
    if (as.character(lp_contents[[2]]) %in% combine_filter_condition_functions) {
      lp_contents[-1] <- lapply(lp_contents[-1],
                                       distributive_and,
                                       fcondition_to_distribute)
      return(lp_contents)
    } else {
      to_return <- rlang::expr(and_filter_condition())
      to_return[2:3] <- list(lp_contents, fcondition_to_distribute)
      class(to_return) <- c("filter_condition", class(to_return)) |>
        unique()
      return(to_return)
    }
  }
  to_return <- rlang::expr(and_filter_condition())
  to_return[2:3] <- list(lp_contents, fcondition_to_distribute)
  class(to_return) <- c("filter_condition", class(to_return)) |>
    unique()
  to_return
}
