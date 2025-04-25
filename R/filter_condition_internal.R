#' @import data.table
#' @importFrom stringr str_detect

lp_to_fc <- function(
  fcall,
  ...
) {
  stopifnot(fcall[[1]] == as.symbol("("))
  fcall[[1]] <- as.symbol("lp_filter_condition")
  fcall[-1] <- lapply(fcall[-1], new_filter_condition, ...)
  fcall |>
    as_filter_condition()
}

and_to_fc <- function(
  fcall,
  ...
) {
  stopifnot(fcall[[1]] == as.symbol("&"))
  fcall[-1] <- lapply(fcall[-1], new_filter_condition, ...)
  if (needs_and_distributed(fcall[[2]])
      | needs_and_distributed(fcall[[3]])) {
    return(distribute_and_fc(fcall[[2]], fcall[[3]]))
  }
  fcall[[1]] <- as.symbol("and_filter_condition")
  fcall |>
    as_filter_condition()
}

needs_and_distributed <- function(fcall) {
  if (!is.call(fcall)) return(FALSE)
  if (fcall[[1]] == as.symbol("lp_filter_condition")) {
    return(!is_pipable(fcall))
  } else {
    return(any(sapply(fcall[-1], needs_parenthesis_handling)))
  }
}

distribute_and_fc <- function(
  fcondition1,
  fcondition2,
  ...
) {
  if (needs_and_distributed(fcondition1))
    fcondition1 <- strip_parentheses(fcondition1)
  if (needs_and_distributed(fcondition2))
    fcondition2 <- strip_parentheses(fcondition2)

  done_distributing <- function(fcall) {
    !as.character(fcall[[1]]) %in% c("and_filter_condition", "or_filter_condition")
  }

  if (!done_distributing(fcondition1)) {
    to_return <- fcondition1[1] |>
      as_filter_condition(pipable = FALSE)
    to_return[2:3] <- lapply(fcondition1[2:3], distribute_and_fc, fcondition2, ...)
    return(to_return)
  }
  if (!done_distributing(fcondition2)) {
    to_return <- fcondition2[1] |>
      as_filter_condition(pipable = FALSE)
    to_return[2:3] <- lapply(fcondition2[2:3], distribute_and_fc, fcondition1 = fcondition1, ...)
    return(to_return)
  }
  to_return <- rlang::expr(and_filter_condition())
  to_return[2:3] <- list(fcondition1, fcondition2)
  to_return |>
    as_filter_condition()
}


or_to_fc <- function(
  fcall,
  ...
) {
  stopifnot(fcall[[1]] == as.symbol("|"))
  fcall[[1]] <- as.symbol("or_filter_condition")
  fcall[-1] <- lapply(fcall[-1],
                      new_filter_condition,
                      ...)
  fcall |>
    as_filter_condition(pipable = is_chainable(fcall))
}

strip_parentheses <- function(fcondition) {
  if (fcondition[[1]] != as.symbol("lp_filter_condition")) return(fcondition)
  strip_parentheses(fcondition[[2]])
}

lt_to_fc <- function(fcall, ...) {
  chainable_to_fc(fcall, "<", ...)
}
lte_to_fc <- function(fcall, ...) {
  chainable_to_fc(fcall, "<=", ...)
}
gt_to_fc <- function(fcall, ...) {
  chainable_to_fc(fcall, ">", ...)
}
gte_to_fc <- function(fcall, ...) {
  chainable_to_fc(fcall, ">=", ...)
}
chainable_to_fc <- function(
  fcall,
  operator,
  encoded_pattern,
  ...
) {
  stopifnot(operator %in% c("<", "<=", ">", ">="))

  fcall[[1]] <- list(
    "<"  = as.symbol("lt_filter_condition"),
    "<=" = as.symbol("lte_filter_condition"),
    ">"  = as.symbol("gt_filter_condition"),
    ">=" = as.symbol("gte_filter_condition")
  )[[operator]]
  attr(fcall, "chainable") <- TRUE
  attr(fcall, "pipable") <- TRUE
  fcall[length(fcall) + 1] <- list(
    encoded = is_encoded(fcall, ...)
  )
  fcall
}


check_prefixes <- function(
  fcallm1,
  prefixes
) {
  if (is.symbol(fcallm1[[1]])) {
    if (as.character(fcallm1[[1]]) %in% names(prefixes)) {
      if (!stringr::str_detect(fcallm1[[2]],
                               paste0("^", prefixes[[as.character(fcallm1[[1]])]]))) {
        fcallm1[[2]] <- paste0(prefixes[[as.character(fcallm1[[1]])]], fcallm1[[2]])
      }
    }
  } else if (is.symbol(fcallm1[[2]])) {
    if (as.character(fcallm1[[2]]) %in% names(prefixes)) {
      if (!stringr::str_detect(fcallm1[[1]],
                               paste0("^", prefixes[[as.character(fcallm1[[2]])]]))) {
        fcallm1[[1]] <- paste0(prefixes[[as.character(fcallm1[[2]])]], fcallm1[[1]])
      }
    }
  }
  fcallm1
}


eq_to_fc <- function(
  fcall,
  quoted_values,
  prefixes,
  encoded_column_names = NULL,
  encoded_pattern = NULL,
  ...
) {
  stopifnot(fcall[[1]] == as.symbol("=="))

  if (1L < length(fcall[[2]]) | 1L < length(fcall[[3]])) {
    stop("More than 1 element on a side of '==':\n", fcall)
  }
  fcall[[1]] <- as.symbol("eq_filter_condition")

  if (!is.null(prefixes)) {
    fcall[-1] <- check_prefixes(fcall[-1], prefixes)
  }

  if (is.character(fcall[[3]])) {
    stopifnot(is.symbol(fcall[[2]]))
    fcall[[3]] <- check_quotes(
      fcall[[3]],
      ifelse(as.character(fcall[[2]]) %in% names(quoted_values),
             quoted_values[[as.character(fcall[[2]])]],
             FALSE)
    )
  } else if (is.character(fcall[[2]])) {
    stopifnot(is.symbol(fcall[[3]]))
    fcall[[2]] <- check_quotes(
      fcall[[2]],
      ifelse(as.character(fcall[[3]]) %in% names(quoted_values),
             quoted_values[[as.character(fcall[[3]])]],
             FALSE)
    )
  }
  attr(fcall, "chainable") <- TRUE
  attr(fcall, "pipable") <- TRUE
  fcall[length(fcall) + 1] <- list(
    encoded = is_encoded(fcall, encoded_column_names = encoded_column_names)
  )
  fcall
}


in_to_fc <- function(
  fcall,
  sep,
  quoted_values,
  prefixes
) {
  fcall[[1]] <- as.symbol("in_filter_condition")
  fcall$sep <- sep
  if (as.character(fcall[[2]]) %in% names(quoted_values)) {
    fcall$values_need_to_be_quoted <- quoted_values[[as.character(fcall[[2]])]]
  }
  if (as.character(fcall[[2]]) %in% names(prefixes)) {
    fcall$prefix <- prefixes[[as.character(fcall[[2]])]]
  }
  attr(fcall, "chainable") <- FALSE
  attr(fcall, "pipable") <- TRUE
  fcall
}
