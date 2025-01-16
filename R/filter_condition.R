#' @import data.table
#' @importFrom stringr str_detect str_match str_trim
#' @importFrom rlang enexpr caller_env env

is.filter_condition <- function(x) inherits(x, "filter_condition")

chainable_filter_condition_functions <- list(
  "<"  =  "lt_filter_condition",
  "<=" = "lte_filter_condition",
  ">"  =  "gt_filter_condition",
  ">=" = "gte_filter_condition"
)

combine_filter_condition_functions <- list(
  "&" = "and_filter_condition",
  "|" =  "or_filter_condition"
)


new_filter_condition <- function(
  fcall,
  column_names = NULL,
  sep = " ",
  values_are_quoted = FALSE
) {
  if (!is.null(column_names)) {
    fcall <- eval(substitute(
      substitute(.fcall, lapply(column_names, as.symbol)),
      list(.fcall = fcall)
    ))
  }

  if (!is.call(fcall)) return(fcall)
  class(fcall) <- c("filter_condition", class(fcall))

  fcall1 <- as.character(fcall[[1]])
  if (fcall1 == "(") {
    fcall[[1]] <- as.symbol("lp_filter_condition")
    fcall[-1] <- lapply(fcall[-1], new_filter_condition)
  } else if (fcall1 %chin% names(combine_filter_condition_functions)) {
    fcall[-1] <- lapply(fcall[-1], new_filter_condition)
    fcall[[1]] <- combine_filter_condition_functions[[fcall1]] |>
      as.symbol()
    # attr(fcall, "chainable") <- is_chainable(fcall[[2]])
  } else if (fcall1 %chin% names(chainable_filter_condition_functions)) {
    fcall[[1]] <- chainable_filter_condition_functions[[fcall1]] |>
      as.symbol()
    attr(fcall, "chainable") <- TRUE
  } else if (fcall1 == "==") {
    fcall[[1]] <- as.symbol("eq_filter_condition")
    fcall$values_are_quoted <- values_are_quoted
    attr(fcall, "chainable") <- TRUE
  } else if (fcall1 %chin% c("%in%", "%chin%")) {
    fcall[[1]] <- as.symbol("in_filter_condition")
    fcall$sep <- sep
    attr(fcall, "chainable") <- FALSE
  }
  fcall
}

is_chainable <- function(
  fcall
) {
  if (is.null(attr(fcall, "chainable"))) {
    if (fcall[[1]] == "lp_filter_condition") {
      return(all(sapply(fcall[-1], is_chainable)))
    }
    if (1L < length(fcall)) {
      return(all(sapply(fcall[-1], is_chainable)))
    }
    warning("NULL value chainable, defaulting to non-chainable")
    return(FALSE)
  }
  attr(fcall, "chainable")
}

are_chainable <- function(
  left,
  right
) {
  if (!is.null(attr(left, "chainable")) & !is.null(attr(right, "chainable"))) {
    return(is_chainable(left) & is_chainable(right))
  }

  if (1L < length(left)) {
    return(are_chainable(left[[length(left)]], right))
  }
  if (1L < length(right)) {
    return(are_chainable(left, right[[1]]))
  }
  return(is_chainable(left) & is_chainable(right))
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

to_awk <- function(
  fcall,
  column_indices
) {
  with(
    column_indices,
    eval(fcall)
  )
}

as_command_line <- function(
  fcall,
  filename,
  column_indices,
  ...
) {
  cl_bits <- to_awk(
    fcall,
    column_indices
  ) |>
    flatten_cl_bits()

  lapply(cl_bits, wrap_awk, filename, ...)
}

flatten_cl_bits <- function(
  cl_bits
) {
  if (length(cl_bits) == 1L) {
    if (!is.list(cl_bits)) {
      return(list(cl_bits))
    }
    return(cl_bits)
  }
  if (2L < length(cl_bits)) stop("cl_bits has too many components")

  cl_bits1 <- flatten_cl_bits(cl_bits[[1]])
  cl_bits2 <- flatten_cl_bits(cl_bits[[2]])

  if (attr(cl_bits, "operation") == "and_filter_condition") {
    return(
      c(
        cl_bits1[-length(cl_bits1)],
        list(c(
          cl_bits1[[length(cl_bits1)]],
          cl_bits2[[1L]]
        )),
        cl_bits2[-1L]
      )
    )
  } else {
    return(
      c(cl_bits1, cl_bits2)
    )
  }
}

wrap_awk <- function(
  awk_cl,
  ...
) {
  if (length(awk_cl) == 1) {
    return(wrap_first_awk(awk_cl, ...))
  }



  awk_cl[[1L]] <- wrap_first_awk(
    awk_cl[[1L]],
    ...
  )
  awk_cl[-1L] <- lapply(awk_cl[-1L], wrap_next_awk, ...)
  unlist(awk_cl) |>
    paste(collapse = " | ")
}

wrap_first_awk <- function(
  single_awk_cl,
  filename,
  sep = " ",
  gzipped = FALSE
) {
  if (1L < length(single_awk_cl)) {
    single_awk_cl[[1L]] <- wrap_first_awk(single_awk_cl[[1L]],
                                         filename,
                                         sep = sep,
                                         gzipped = gzipped)
    single_awk_cl[-1L] <- wrap_next_awk(single_awk_cl[-1L], sep = sep)
    return(single_awk_cl)
  }
  if (gzipped) {
    return(
      sprintf("zcat %s | awk%s '%s'",
              filename,
              ifelse(sep == ",", " -F','", ""),
              single_awk_cl)
    )
  }
  sprintf("awk%s '%s' %s",
          ifelse(sep == ",", " -F','", ""),
          single_awk_cl,
          filename)
}

wrap_next_awk <- function(
  single_awk_cl,
  sep = " ",
  ...
) {
  sprintf("awk%s '%s'",
          ifelse(sep == ",", " -F','", ""),
          single_awk_cl)
}

and_cl_bit <- function(
  awk_cl1,
  awk_cl2,
  ...
) {
  if (1L < length(awk_cl1)) {
    return(
      and_cl_bit(
        do.call(attr(awk_cl1, "operation"), awk_cl1),
        awk_cl2
      )
    )
  }
  if (1L < length(awk_cl2)) {
    return(
      and_cl_bit(
        awk_cl1,
        do.call(attr(awk_cl2, "operation"), awk_cl2)
      )
    )
  }
  paste(awk_cl1, "|", wrap_next_awk(awk_cl2, ...))
}

or_cl_bit <- function(
  awk_cl1,
  awk_cl2
) {
  list(awk_cl1, awk_cl2)
}



lt_filter_condition <- function(
    column_name,
    value,
    env = NULL
) {
  # TODO Check vector length
  cmd <- sprintf("%s < %s", column_name, value)
  attr(cmd, "chainable") <- TRUE
  cmd
}
lte_filter_condition <- function(
    column_name,
    value
) {
  # TODO Check vector length
  cmd <- sprintf("%s <= %s", column_name, value)
  attr(cmd, "chainable") <- TRUE
  cmd
}
gt_filter_condition <- function(
    column_name,
    value
) {
  lt_filter_condition(value, column_name)
  # # TODO Check vector length
  # sprintf("%s > %s", column_name, value) |>
  #   new_filter_condition(chainable = TRUE)
}
gte_filter_condition <- function(
    column_name,
    value
) {
  lte_filter_condition(value, column_name)
  # # TODO Check vector length
  # sprintf("%s >= %s", column_name, value) |>
  #   new_filter_condition(chainable = TRUE)
}
eq_filter_condition <- function(
    column_name,
    value,
    values_are_quoted
) {
  # TODO Check vector length
  if (substr(column_name, 1, 1) == "$") {
    if (is.character(value)) {
      value <- check_quotes(value, values_are_quoted)
    }
  }
  cmd <- sprintf("%s == %s",
                 column_name,
                 value)
  attr(cmd, "chainable") <- TRUE
  cmd
}
in_filter_condition <- function(
  column_name,
  values,
  sep
) {
  cmd <- sprintf(
    paste("BEGIN {split(\"%s\", vals);",
          "for (i in vals) arr[vals[i]]}",
          "{if (%s in arr) print $0}"),
    paste(
      values,
      collapse = sep
    ),
    column_name
  )
  attr(cmd, "chainable") <- FALSE
  cmd
}
and_filter_condition <- function(
    condition1,
    condition2
) {
  # TODO Check vector length
  if (are_chainable(condition1, condition2)) {
    if (length(condition1) > 1L) {
      if (length(condition2) > 1L) {
        condition1[[length(condition1)]] <- paste(
          condition1[[length(condition1)]], "&&", condition2[[1]]
        )
        attr(condition1, "chainable") <- TRUE
        return(get(attr(condition2, "operation"))(condition1, condition2[[2]]))
      }
      condition1[[length(condition1)]] <- paste(
        condition1[[length(condition1)]], "&&", condition2
      )
      attr(condition1, "chainable") <- TRUE
      return(condition1)
    } else {
      if (length(condition2) > 1L) {
        condition12 <- paste(
          condition12, "&&", condition2[[1]]
        )
        attr(condition12, "chainable") <- TRUE
        return(get(attr(condition2, "operation"))(condition12, condition2[[2]]))
      }
      conditions12 <- paste(condition1, "&&", condition2)
      attr(conditions12, "chainable") <- TRUE
      return(conditions12)
    }
  }
  structure(
    list(condition1, condition2),
    operation = "and_filter_condition"
  )
}
or_filter_condition <- function(
  condition1,
  condition2
) {
  # TODO Check vector length
  if (are_chainable(condition1, condition2)) {
    if (length(condition1) > 1L) {
      if (length(condition2) > 1L) {
        condition1[[length(condition1)]] <- paste(
          condition1[[length(condition1)]], "||", condition2[[1]]
        )
        attr(condition1, "chainable") <- TRUE
        return(get(attr(condition2, "operation"))(condition1, condition2[[2]]))
      }
      condition1[[length(condition1)]] <- paste(
        condition1[[length(condition1)]], "||", condition2
      )
      attr(condition1, "chainable") <- TRUE
      return(condition1)
    }
    if (length(condition2) > 1L) {
      condition1[[length(condition1)]] <- paste(
        condition1[[length(condition1)]], "||", condition2[[1]]
      )
      attr(condition1, "chainable") <- TRUE
      return(get(attr(condition2, "operation"))(condition1, condition2[[2]]))
    }
    conditions12 <- paste(condition1, "||", condition2)
    attr(conditions12, "chainable") <- TRUE
    return(conditions12)
  }
  structure(
    list(condition1, condition2),
    operation = "or_filter_condition"
  )
}

lp_filter_condition <- function(
  filter_condition
) {
  if (is_chainable(filter_condition)) {
    return(
      structure(sprintf("(%s)", eval(filter_condition)),
                chainable = TRUE)
    )
  }
  structure(eval(filter_condition),
            chainable = FALSE)
}
