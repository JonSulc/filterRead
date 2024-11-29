#' @import data.table
#' @importFrom stringr str_detect str_match str_trim
#' @importFrom rlang enexpr caller_env env

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

# validate_column_indices <- function(
#   column_indices
# ) {
#   if (is.null(column_indices)) return()
#   stopifnot(is.list(column_indices))
#   stopifnot(all(sapply(column_indices, stringr::str_detect, "^[$][0-9]+$")))
# }
#
# new_filter_condition <- function(
#   filter_condition,
#   finterface_env = NULL
# ) {
#
#   filter_condition <- rlang::enexpr(filter_condition)
#
#   if (class(filter_condition) != "call") {
#     if (class(filter_condition) == "name") {
#       filter_condition <- as.symbol(
#         paste0(filter_condition, ".filter_condition")
#       )
#     }
#     return(filter_condition)
#   }
#
#   if (filter_condition[[1]] == as.symbol("&")
#       | filter_condition[[1]] == as.symbol("|")) {
#     filter_condition[[2]] <- new_filter_condition(!!filter_condition[[2]])
#     filter_condition[[3]] <- new_filter_condition(!!filter_condition[[3]])
#     return(filter_condition)
#   }
#
#   filter_condition <- structure(
#     filter_condition,
#     class = c("filter_condition",
#               class(filter_condition)),
#     # column_indices = column_indices
#     finterface_env = finterface_env
#   )
#   attr(filter_condition, "chainable") <- filter_condition[[1]] != as.symbol("%in%")
#
#   filter_condition
# }

#' @export
`<.filter_condition` <- function(
    column_name,
    value
) {
  fcondition <- sprintf("%s < %s", column_name, value)
  attr(fcondition, "chainable") <- TRUE
  fcondition
}
#' @export
`<=.filter_condition` <- function(
    column_name,
    value
) {
  fcondition <- sprintf("%s <= %s", column_name, value)
  attr(fcondition, "chainable") <- TRUE
  fcondition
}
#' @export
`>.filter_condition` <- function(
    column_name,
    value
) {
  fcondition <- sprintf("%s > %s", column_name, value)
  attr(fcondition, "chainable") <- TRUE
  fcondition
}
#' @export
`>=.filter_condition` <- function(
    column_name,
    value
) {
  fcondition <- sprintf("%s >= %s", column_name, value)
  attr(fcondition, "chainable") <- TRUE
  fcondition
}
#' @export
`==.filter_condition` <- function(
    column_name,
    value
) {
  fcondition <- sprintf("%s == %s", column_name, value)
  attr(fcondition, "chainable") <- TRUE
  fcondition
}
#' @export
`%in%.filter_condition` <- function(
  column_name,
  values
) {
  fcondition <- sprintf(
    paste("BEGIN {split(\"%s\", vals);",
          "for (i in vals) arr[vals[i]]}",
          "{if (%s in arr) print $0}"),
    paste(
      c(rlang::enexpr(column_name),
        values),
      collapse = " "
    ),
    column_name
  )
  attr(fcondition, "chainable") <- FALSE
  fcondition
}
#' @export
`&.filter_condition` <- function(
  condition1,
  condition2
) {
  if (is_chainable_condition(condition1) &
      is_chainable_condition(condition2)) {
    if (length(condition1) > 1) {
      condition1[[length(condition1)]] <- paste(
        condition1[[length(condition1)]], "&", condition2
      )
      return(condition1)
    } else {
      conditions12 <- paste(condition1, "&", condition2)
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
