#' @import data.table
#' @importFrom stringr str_detect str_match str_trim
#' @importFrom rlang enexpr caller_env env
#' @include filter_condition_internal.R

is.filter_condition <- function(x) inherits(x, "filter_condition")

fc_convert <- list(
  "<"      = lt_to_fc,
  "<="     = lte_to_fc,
  ">"      = gt_to_fc,
  ">="     = gte_to_fc,
  "=="     = eq_to_fc,
  "&"      = and_to_fc,
  "|"      = or_to_fc,
  "("      = lp_to_fc,
  "%in%"   = in_to_fc,
  "%chin%" = in_to_fc
)


as_filter_condition <- function(
  fcall,
  ...
) {
  if (!is.call(fcall)) {
    warning("Attempting to set non-call object to filter_condition")
    return(fcall)
  }
  structure(
    fcall,
    class = c("filter_condition", class(fcall)) |>
      unique(),
    ...
  )
}


new_filter_condition <- function(
  fcall,
  finterface = NULL
) {
  if (!is.call(fcall)) return(fcall)

  fcall <- as_filter_condition(fcall)

  if (!as.character(fcall[[1]]) %in% names(fc_convert)) return(fcall)

  fcondition <- fc_convert[[as.character(fcall[[1]])]](
    fcall,
    finterface = finterface
  )
  fcondition
}

needs_parenthesis_handling <- function(fcall) {
  if (!is.call(fcall)) return(FALSE)
  if (fcall[[1]] == as.symbol("lp_filter_condition")) {
    return(!is_pipable(fcall))
  } else {
    return(any(sapply(fcall[-1], needs_parenthesis_handling)))
  }
}

is_chainable <- function(
    fcall
) {
  if (!is.null(attr(fcall, "chainable")))
    return(attr(fcall, "chainable"))
  if (length(fcall) == 1) {
    warning("NULL value chainable, defaulting to non-chainable")
    return(FALSE)
  }
  if (is.call(fcall)) return(all(sapply(fcall[-1], is_chainable)))
  all(sapply(fcall, is_chainable))
}
are_chainable <- function(
  left,
  right,
  operation
) {
  if ((!is.null(attr(left, "chainable")) & !is.null(attr(right, "chainable"))) |
      operation == "or_filter_condition") {
    return(is_chainable(left) & is_chainable(right))
  }

  if (1L < length(left)) {
    return(are_chainable(left[[length(left)]], right, operation = operation))
  }
  if (1L < length(right)) {
    return(are_chainable(left, right[[1]], operation = operation))
  }
  return(is_chainable(left) & is_chainable(right))
}

is_pipable <- function(
    fcall
) {
  if (!is.null(attr(fcall, "pipable")))
    return(attr(fcall, "pipable"))
  if (length(fcall) == 1) {
    warning("NULL value pipable, defaulting to non-pipable")
    return(FALSE)
  }
  if (is.call(fcall)) return(all(sapply(fcall[-1], is_pipable)))
  all(sapply(fcall, is_pipable))
}

is_encoded <- function(
  fcall,
  finterface
) {
  if (!is.null(attr(fcall, "encoded")))
    return(attr(fcall, "encoded"))

  if (is.symbol(fcall)) return(as.character(fcall) %in% finterface$column_info$encoded_columns)
  if (!is.call(fcall)) return(FALSE)
  any(sapply(fcall[-1], is_encoded, finterface = finterface))
}

get_decoding_awk <- function(
  fcall,
  finterface
) {
  encoded_symbols <- intersect(as.character(fcall),
                               finterface$column_info$encoded_columns)
  if (length(encoded_symbols) == 0) return()
  finterface$column_info$encoding_columns[
    encoded_symbols,
    paste(pattern, collapse = ""),
    on = "encoded_column"
  ]
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
  fcondition,
  finterface
) {
  with(
    finterface$column_info$bash_index,
    eval(fcondition)
  )
}

as_command_line <- function(
  fcondition,
  finterface,
  ...
) {
  to_awk(
    fcondition,
    finterface = finterface
  ) |>
    flatten_cl_bits() |>
    lapply(wrap_awk, finterface = finterface)
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
  finterface
) {
  if (length(awk_cl) == 1) {
    # TODO Check that single cl is handled correctly
    return(wrap_first_awk(awk_cl, finterface = finterface))
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
  finterface
) {
  if (1L < length(single_awk_cl)) {
    single_awk_cl[[1L]] <- wrap_first_awk(single_awk_cl[[1L]],
                                          finterface = finterface)
    single_awk_cl[-1L] <- wrap_next_awk(single_awk_cl[-1L],
                                        finterface = finterface)
    return(single_awk_cl)
  }
  if (finterface$gzipped) {
    return(
      sprintf("zcat %s | awk%s '%s'",
              finterface$filename,
              ifelse(finterface$sep == ",", " -F','", ""),
              encoded_to_awk(single_awk_cl, finterface))
    )
  }
  sprintf("awk%s '%s' %s",
          ifelse(finterface$sep == ",", " -F','", ""),
          encoded_to_awk(single_awk_cl, decoding_awk = attr(single_awk_cl, "decoding_awk")),
          filename)
}

encoded_to_awk <- function(
  single_awk_cl,
  decoding_awk
) {
  if (is.null(attr(single_awk_cl, "encoded"))) return(single_awk_cl)
  if (is.null(decoding_awk))
    stop("Values are encoded but no pattern is provided")
  sprintf(encoded_column_awk_wrapper, decoding_awk, single_awk_cl)
}

wrap_next_awk <- function(
  single_awk_cl,
  sep = " ",
  decoding_awk = NULL,
  ...
) {
  sprintf("awk%s '%s'",
          ifelse(sep == ",", " -F','", ""),
          encoded_to_awk(single_awk_cl, decoding_awk = decoding_awk))
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


lt_filter_condition <- function(
    column_name,
    value,
    encoded,
    decoding_awk = NULL
) {
  # TODO Check vector length
  structure(sprintf("%s < %s", column_name, value),
            chainable = TRUE,
            encoded   = encoded,
            decoding_awk = decoding_awk)
}
lte_filter_condition <- function(
    column_name,
    value,
    encoded,
    decoding_awk = NULL
) {
  # TODO Check vector length
  structure(sprintf("%s <= %s", column_name, value),
            chainable = TRUE,
            encoded   = encoded,
            decoding_awk = decoding_awk)
}
gt_filter_condition <- function(
    column_name,
    value,
    encoded,
    decoding_awk = NULL
) {
  # TODO Check vector length
  structure(sprintf("%s > %s", column_name, value),
            chainable = TRUE,
            encoded   = encoded,
            decoding_awk = decoding_awk)
}
gte_filter_condition <- function(
    column_name,
    value,
    encoded,
    decoding_awk = NULL
) {
  # TODO Check vector length
  structure(sprintf("%s >= %s", column_name, value),
            chainable = TRUE,
            encoded   = encoded,
            decoding_awk = decoding_awk)
}
eq_filter_condition <- function(
    column_name,
    value,
    encoded = FALSE,
    decoding_awk = NULL
) {
  # TODO Check quoted values are properly handled
  structure(sprintf("%s == %s",
                    column_name,
                    value),
            chainable    = TRUE,
            encoded      = encoded,
            decoding_awk = decoding_awk)
}
in_filter_condition <- function(
  column_name,
  values,
  sep,
  values_need_to_be_quoted = FALSE,
  prefix = NULL
) {
  if (values_need_to_be_quoted) {
    values <- paste0("\\\"", values, "\\\"")
  }
  if (!is.null(prefix)) {
    values <- sapply(values, \(value) {
      if (stringr::str_detect(value, paste0("^", prefix))) return(value)
      paste0(prefix, value)
    })
  }

  structure(
    sprintf(paste("BEGIN {split(\"%s\", vals);",
                  "for (i in vals) arr[vals[i]]}",
                  "{if (%s in arr) print $0}"),
            paste(
              values,
              collapse = sep
            ),
            column_name),
    chainable = FALSE
  )
}

combine_filter_condition <- function(
  condition1,
  condition2,
  operation
) {
  if (operation == "and_filter_condition") {
    operator <- "&&"
  } else {
    operator <- "||"
  }
  # TODO Check vector length
  if (are_chainable(condition1, condition2, operation = operation)) {
    if (1L < length(condition1)) {
      if (1L < length(condition2)) {
        condition1[[length(condition1)]] <- paste(
          condition1[[length(condition1)]], operator, condition2[[1]]
        )
        attr(condition1, "chainable") <- TRUE
        return(get(attr(condition2, "operation"))(
          condition1,
          condition2[[2]],
          encoded = any(attr(condition1, "encoded"), attr(condition2, "encoded")),
          decoding_awk = c(attr(condition1, "decoding_awk"), attr(condition2, "decoding_awk")) |>
            unique() |>
            paste(collapse = "")
        ))
      }
      condition1[[length(condition1)]] <- paste(
        condition1[[length(condition1)]], operator, condition2
      )
      attr(condition1, "chainable") <- TRUE
      return(condition1)
    } else {
      if (length(condition2) > 1L) {
        condition12 <- paste(
          condition1, operator, condition2[[1]]
        )
        attr(condition12, "chainable") <- TRUE
        return(get(attr(condition2, "operation"))(
          condition12,
          condition2[[2]],
          encoded = any(attr(condition1, "encoded"), attr(condition2, "encoded")),
          decoding_awk = c(attr(condition1, "decoding_awk"), attr(condition2, "decoding_awk")) |>
            unique() |>
            paste(collapse = "")
        ))
      }
      conditions12 <- paste(condition1, operator, condition2)
      attr(conditions12, "chainable") <- TRUE
      return(structure(
        paste(condition1, operator, condition2),
        chainable = TRUE,
        encoded = any(attr(condition1, "encoded"), attr(condition2, "encoded")),
        decoding_awk = c(attr(condition1, "decoding_awk"), attr(condition2, "decoding_awk")) |>
          unique() |>
          paste(collapse = "")
      ))
    }
  }
  structure(
    list(condition1, condition2),
    operation    = operation,
    encoded      = any(attr(condition1, "encoded"), attr(condition2, "encoded")),
    decoding_awk = c(attr(condition1, "decoding_awk"), attr(condition2, "decoding_awk")) |>
      unique() |>
      paste(collapse = "")
  )
}

and_filter_condition <- function(
  condition1,
  condition2
) {
  combine_filter_condition(condition1, condition2, "and_filter_condition")
}
or_filter_condition <- function(
  condition1,
  condition2
) {
  combine_filter_condition(condition1, condition2, "or_filter_condition")
}

lp_filter_condition <- function(
  condition
) {
  if (is_chainable(condition)) {
    return(
      structure(sprintf("(%s)", eval(condition)),
                chainable    = TRUE,
                encoded      = attr(condition, "encoded"),
                decoding_awk = attr(condition, "decoding_awk"))
    )
  }
  structure(eval(condition),
            chainable = FALSE,
            encoded      = attr(condition, "encoded"),
            decoding_awk = attr(condition, "decoding_awk"))
}
