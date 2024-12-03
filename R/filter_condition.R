#' @import data.table
#' @importFrom stringr str_detect str_match str_trim
#' @importFrom rlang enexpr caller_env env

new_filter_condition <- function(
  cmd,
  chainable
) {
  structure(
    cmd,
    chainable = chainable
  )
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

lt_filter_condition <- function(
    column_name,
    value
) {
  sprintf("%s < %s", column_name, value) |>
    new_filter_condition(chainable = TRUE)
}
lte_filter_condition <- function(
    column_name,
    value
) {
  sprintf("%s <= %s", column_name, value) |>
    new_filter_condition(chainable = TRUE)
}
gt_filter_condition <- function(
    column_name,
    value
) {
  sprintf("%s > %s", column_name, value) |>
    new_filter_condition(chainable = TRUE)
}
gte_filter_condition <- function(
    column_name,
    value
) {
  sprintf("%s >= %s", column_name, value) |>
    new_filter_condition(chainable = TRUE)
}
eq_filter_condition <- function(
    column_name,
    value
) {
  quotes_needed <- withr::with_environment(
    rlang::caller_env(),
    quoted_values[[as.character(rlang::enexpr(column_name))]]
  )
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
      check_quotes(values, quoted_values[column_name]),
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
  if (is_chainable_condition(condition1) &
      is_chainable_condition(condition2)) {
    if (length(condition1) > 1) {
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
  if (is_chainable_condition(condition1) &
      is_chainable_condition(condition2)) {
    if (length(condition1) > 1) {
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
  } else if (length(fcondition) > 1) {
    return(is_chainable_condition(fcondition[[length(fcondition)]]))
  }
  FALSE
}
