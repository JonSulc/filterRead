lp_to_fc <- function(
  fcall,
  ...
) {
  stopifnot(fcall[[1]] == as.symbol("("))
  fcall[[1]] <- as.symbol("lp_filter_condition")
  fcall[[2]] <- new_filter_condition(fcall[[2]], ...)
  class(fcall) <- c("lp_filter_condition", class(fcall)) |>
    unique()
  fcall
}
lp_wrap_fcondition <- function(
  fcall
) {
  fcondition <- rlang::expr(lp_filter_condition())
  fcondition[[2]] <- fcall
  fcondition <- as_filter_condition(fcondition)
  class(fcondition) <- c("lp_filter_condition", class(fcondition)) |>
    unique()
  attr(fcondition, "build") <- attr(fcall, "build")
  genomic_regions(fcondition) <- genomic_regions(fcall)
  fcondition
}

and_to_fc <- function(
  fcall,
  ...
) {
  new_filter_condition(fcall[[2]], ...) &
    new_filter_condition(fcall[[3]], ...)
}


or_to_fc <- function(
  fcall,
  ...
) {
  new_filter_condition(fcall[[2]], ...) |
    new_filter_condition(fcall[[3]], ...)
}


chainable_to_fc <- function(
  fcall,
  ...
) {
  fcall[[1]] <- list(
    "<"  = as.symbol("lt_filter_condition"),
    "<=" = as.symbol("lte_filter_condition"),
    ">"  = as.symbol("gt_filter_condition"),
    ">=" = as.symbol("gte_filter_condition")
  )[[as.character(fcall[[1]])]]
  class(fcall) <- c(
    as.character(fcall[[1]]),
    class(fcall)
  ) |> unique()
  fcall
}


eq_to_fc <- function(
  fcall,
  finterface_env,
  env,
  ...
) {
  stopifnot(fcall[[1]] == as.symbol("=="))
  fcall[[1]] <- as.symbol("eq_filter_condition")

  # To work with genomic_regions, post-processing will have to be done later
  if (is_column_symbol(fcall[[2]], finterface_env$finterface) &&
    !is_column_symbol(fcall[[3]], finterface_env$finterface)) {
    fcall[[3]] <- eval(fcall[[3]], env) |>
      check_post_processing(fcall[[2]], finterface_env$finterface)
  } else if (is_column_symbol(fcall[[3]], finterface_env$finterface) &&
    !is_column_symbol(fcall[[2]], finterface_env$finterface)) {
    fcall[[2]] <- eval(fcall[[2]], env) |>
      check_post_processing(fcall[[3]], finterface_env$finterface)
  }

  class(fcall) <- c("eq_filter_condition", class(fcall)) |>
    unique()

  fcall
}


in_to_fc <- function(
  fcall,
  finterface_env,
  env,
  ...
) {
  fcall[[1]] <- as.symbol("in_filter_condition")

  fcall[[3]] <- eval(fcall[[3]], env) |>
    check_post_processing(fcall[[2]], finterface_env$finterface, to_write = TRUE)

  class(fcall) <- c("in_filter_condition", class(fcall)) |>
    unique()

  fcall
}

is_column_symbol <- function(
  fcall,
  finterface
) {
  if (length(fcall) != 1) {
    return(FALSE)
  }
  as.character(fcall) %in% finterface$column_info$name
}

check_post_processing <- function(
  values,
  column_symbol,
  finterface,
  to_write = FALSE,
  check_quotes_function = ifelse(
    to_write,
    check_quotes_to_write,
    check_quotes
  )
) {
  column_name <- as.character(column_symbol)
  if (!column_name %in% finterface$column_info$name) {
    return(values)
  }

  post_processing_to_check <- finterface$column_info[
    column_name,
    .(quoted, prefix),
    on = "name"
  ]

  values |>
    check_prefix(post_processing_to_check$prefix) |>
    check_quotes_function(post_processing_to_check$quoted)
}

check_prefix <- function(
  values,
  prefix
) {
  if (is.na(prefix)) {
    return(values)
  }
  data.table::fifelse(
    grepl(paste0("^", prefix), values),
    as.character(values),
    paste0(prefix, values)
  )
}

check_quotes <- function(
  values,
  quoted
) {
  if (!isTRUE(quoted)) {
    if (all(is.numeric(values))) {
      return(values)
    }
    return(sprintf("\"%s\"", values))
  }
  # Strip existing quotation
  values <- gsub("^(?:\")?(?:\\\")?(.*)(?:\\\")?(?:\")?$", "\\1", values)
  sprintf(
    "\"\\\"%s\\\"\"",
    values
  )
}
check_quotes_to_write <- function(
  values,
  quoted
) {
  if (!isTRUE(quoted)) {
    return(values)
  }
  data.table::fifelse(
    grepl("^\".*\"$", values),
    as.character(values),
    sprintf("\"%s\"", values)
  )
}

evaluate_non_column_variables <- function(
  fcondition,
  finterface,
  env
) {
  fcondition[-1] <- lapply(
    fcondition[-1],
    \(x) {
      if (is_column_symbol(x, finterface)) return(x)
      eval(x, env)
    }
  )
  fcondition
}
