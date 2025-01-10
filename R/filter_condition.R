#' @import data.table
#' @importFrom stringr str_detect str_match str_trim
#' @importFrom rlang enexpr caller_env env

is.filter_condition <- function(x) inherits(x, "filter_condition")


new_filter_condition <- function(
  fcall,
  chainable
) {
  # fcall <- substitute(fcall)
  if (is.call(fcall)) {
    class(fcall) <- c("filter_condition", class(fcall))
    setattr(fcall, "chainable", chainable)
  }
  fcall
}

get_indices_from_column_names <- function(
  column_names
) {
  if (is.null(column_names)) return(NULL)
  setNames(
    paste0("$", seq_along(column_names)),
    column_names
  ) |>
    as.list()
}


as_filter_condition <- function(
  fc, column_indices, new = TRUE
) {
  if (new) {
    fc <- eval(substitute(
      substitute2(.filter_condition, column_indices),
      list(.filter_condition = substitute(fc))
    ))
  }
  if (length(fc) > 1) {
    if (is.call(fc)) {
      class(fc) <- c("filter_condition", class(fc)) |>
        unique()
      for (index in seq_along(fc)[-1L]){
        fc[[index]] <- as_filter_condition(fc[[index]], column_indices = column_indices, new = FALSE)
      }
    }
  }
  fc
}





lt_filter_condition <- function(
    column_name,
    value,
    env = NULL
) {
  if (length(value) > 1) value <- as.numeric(min(value))
  if (is.null(env)) {
    fcall <- substitute(column_name < value)
  } else {
    fcall <- eval(substitute(
      substitute2(.filter_condition, env),
      list(.filter_condition = substitute(column_name < value))
    ))
  }
  new_filter_condition(
    fcall,
    chainable = TRUE
  )
}
lte_filter_condition <- function(
    column_name,
    value
) {
  # TODO Check vector length
  sprintf("%s <= %s", column_name, value) |>
    new_filter_condition(chainable = TRUE)
}
gt_filter_condition <- function(
    column_name,
    value
) {
  # TODO Check vector length
  sprintf("%s > %s", column_name, value) |>
    new_filter_condition(chainable = TRUE)
}
gte_filter_condition <- function(
    column_name,
    value
) {
  # TODO Check vector length
  sprintf("%s >= %s", column_name, value) |>
    new_filter_condition(chainable = TRUE)
}
eq_filter_condition <- function(
    column_name,
    value,
    quotes_needed = withr::with_environment(
      rlang::caller_env(),
      {
        print(column_name)
        print(quoted_values)
        quoted_values[[as.character(rlang::enexpr(column_name))]]
      }
    )
) {
  # TODO Check vector length
  sprintf("%s == %s",
          column_name,
          check_quotes(value,
                       quotes_needed)) |>
    new_filter_condition(chainable = TRUE)
}
in_filter_condition <- function(
  column_name,
  values
) {
  sprintf(
    paste("BEGIN {split(\"%s\", vals);",
          "for (i in vals) arr[vals[i]]}",
          "{if (%s in arr) print $0}"),
    paste(
      withr::with_environment(
        rlang::caller_env(),
        {
          # sapply(values, check_quotes, quoted_values[[rlang::enexpr(column_name)]])
          check_quotes(values, quoted_values[[rlang::enexpr(column_name)]])
        }
      ),
      collapse = " "
    ),
    column_name
  ) |>
    new_filter_condition(chainable = FALSE)
}
and_filter_condition <- function(
    condition1,
    condition2
) {
  # TODO Check vector length
  if (is_chainable_condition(condition1) &
      is_chainable_condition(condition2)) {
    if (length(condition1) > 1L) {
      condition1[[length(condition1)]] <- paste(
        condition1[[length(condition1)]], "&&", condition2
      )
      return(condition1)
    } else {
      conditions12 <- paste(condition1, "&&", condition2)
      attr(conditions12, "chainable") <- TRUE
      return(conditions12)
    }
  }
  list(condition1, condition2)
}
or_filter_condition <- function(
    condition1,
    condition2
) {
  # TODO Check vector length
  if (is_chainable_condition(condition1) &
      is_chainable_condition(condition2)) {
    if (length(condition1) > 1L) {
      condition1[[length(condition1)]] <- paste(
        condition1[[length(condition1)]], "||", condition2
      )
      return(condition1)
    } else {
      conditions12 <- paste(condition1, "||", condition2)
      attr(conditions12, "chainable") <- TRUE
      return(conditions12)
    }
  }
  list(condition1, condition2)
}

is_chainable_condition <- function(
  fcondition
) {
  if (!is.null(attr(fcondition, "chainable"))) {
    if (attr(fcondition, "chainable"))
      return(TRUE)
  } else if (length(fcondition) > 1L) {
    return(is_chainable_condition(fcondition[[length(fcondition)]]))
  }
  FALSE
}

substitute_filter_condition <- function(filter_condition, column_names) {
  eval(substitute(
    substitute2(.filter_condition, filter_functions),
    list(.filter_condition = substitute(filter_condition))
  ))
}
s2 <- function(filter_condition, column_names) {
  eval(substitute(
    substitute2(.filter_condition, column_names),
    list(.filter_condition = substitute(filter_condition))
  ))
}
s3 <- function(filter_condition, column_names) {
  data.table:::eval_with_cols(filter_condition, column_names)
}
# substitute_filter_condition(x < 3, finterface$column_indices)


# replace_filter_condition_operations <- function(e) {
#   if (is.call(e) && !is.function(e[[1L]])) {
#     if (e[[1L]] == "<") e[[1L]] = quote(lt_filter_condition)
#     for (i in seq_along(e)[-1L]) {
#       if (!is.null(e[[i]])) e[[i]] = replace_filter_condition_operations(e[[i]])
#     }
#   }
#   e
# }

filter_functions <- list(
  `<`    = quote( lt_filter_condition),
  `<=`   = quote(lte_filter_condition),
  `>`    = quote( gt_filter_condition),
  `>=`   = quote(gte_filter_condition),
  `==`   = quote( eq_filter_condition),
  `%in%` = quote( in_filter_condition),
  `&`    = quote(and_filter_condition),
  `|`    = quote( or_filter_condition),
  char   = "test"
)
