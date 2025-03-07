#' @import data.table

and_combine_fconditions <- function(
  fcondition1,
  fcondition2
) {
  if (needs_parenthesis_handling(fcondition1)) {
    return(distributive_and(fcondition1[[2]], fcondition2))
  } else if (needs_parenthesis_handling(fcondition2)) {
    return(distributive_and(fcondition2[[2]], fcondition1))
  }
  if (fcondition1[[1]] == as.symbol("lp_filter_condition")) {
    return(and_combine_fconditions(fcondition1[[2]], fcondition2))
  } else if (fcondition2[[1]] == as.symbol("lp_filter_condition")) {
    return(and_combine_fconditions(fcondition1, fcondition2[[2]]))
  }
  # stop("This shouldn't be happening.")
  to_return <- rlang::expr(and_filter_condition())
  to_return[2:3] <- list(fcondition1, fcondition2)
  to_return |>
    as_filter_condition()
}

distributive_and <- function(
  lp_contents,
  fcondition_to_distribute
) {
  if (lp_contents[[1]] == as.symbol("or_filter_condition")) {
    to_return <- lp_contents[1]
    to_return[2:3] <- lapply(lp_contents[2:3], distributive_and, fcondition_to_distribute)
    attr(to_return, "pipable") <- FALSE
  # } else if (lp_contents[[1]] == as.symbol("lp_filter_condition")) {
  #   return(distributive_and(lp_contents[[2]], fcondition_to_distribute))
  } else {
    to_return <- rlang::expr(and_filter_condition())
    to_return[[2]] <- lp_contents
    to_return[[3]] <- fcondition_to_distribute
  }
  # class(to_return) <- c("filter_condition", class(to_return)) |>
  #   unique()
  to_return |>
    as_filter_condition()
#
#
#
#   if (is_chainable(lp_contents)) {
#     to_return <- rlang::expr(and_filter_condition())
#     to_return[2:3] <- list(lp_contents, fcondition_to_distribute)
#     class(to_return) <- c("filter_condition", class(to_return)) |>
#       unique()
#     return(to_return)
#   }
#
#   if (!is_chainable(lp_contents)) {
#     if (as.character(lp_contents[[2]]) %in% combine_filter_condition_functions) {
#       lp_contents[-1] <- lapply(lp_contents[-1],
#                                        distributive_and,
#                                        fcondition_to_distribute)
#       return(lp_contents)
#     } else {
#       to_return <- rlang::expr(and_filter_condition())
#       to_return[2:3] <- list(lp_contents, fcondition_to_distribute)
#       class(to_return) <- c("filter_condition", class(to_return)) |>
#         unique()
#       return(to_return)
#     }
#   }
#   to_return <- rlang::expr(and_filter_condition())
#   to_return[2:3] <- list(lp_contents, fcondition_to_distribute)
#   class(to_return) <- c("filter_condition", class(to_return)) |>
#     unique()
#   to_return
}
