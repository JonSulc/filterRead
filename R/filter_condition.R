#' @import data.table
#' @importFrom stringr str_detect str_match str_trim
#' @importFrom rlang enexpr caller_env env
#' @include filter_condition_internal.R

is.filter_condition <- function(x) inherits(x, "filter_condition")

fc_convert <- list(
  "<"      = chainable_to_fc,
  "<="     = chainable_to_fc,
  ">"      = chainable_to_fc,
  ">="     = chainable_to_fc,
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
  structure(
    fcall,
    class = c("filter_condition", class(fcall)) |>
      unique(),
    ...
  )
}


new_filter_condition <- function(
  fcall,
  finterface,
  env = parent.frame()
) {
  if (is.null(fcall)) {
    fcall <- structure(list(), class = c("filter_condition"))
  } else if (!is.call(fcall)) {
    return(fcall)
  }

  fcall <- as_filter_condition(fcall)

  if (is_file_interface(finterface)) {
    attr(fcall, "finterface_env") <- new.env(parent = emptyenv())
    attr(fcall, "finterface_env")$finterface <- finterface
  } else {
    attr(fcall, "finterface_env") <- finterface
  }

  if (length(fcall) == 0) return(fcall)

  if (!as.character(fcall[[1]]) %in% names(fc_convert)) return(fcall)

  fcondition <- fc_convert[[as.character(fcall[[1]])]](
    fcall,
    finterface = attr(fcall, "finterface_env"),
    env = env
  )
  fcondition
}


lt_filter_condition <- function(
    var1,
    var2
) {
  list(condition = sprintf("%s < %s", var1, var2))
}
lte_filter_condition <- function(
    var1,
    var2
) {
  list(condition = sprintf("%s <= %s", var1, var2))
}
gt_filter_condition <- function(
    var1,
    var2
) {
  list(condition = sprintf("%s > %s", var1, var2))
}
gte_filter_condition <- function(
    var1,
    var2
) {
  list(condition = sprintf("%s >= %s", var1, var2))
}
eq_filter_condition <- function(
    var1,
    var2
) {
  list(condition = sprintf("%s == %s", var1, var2))
}
in_filter_condition <- function(
  column_name,
  values,
  filename = tempfile()
) {
  filename_handle <- basename(filename)
  # "var" is pasted instead of subbed to avoid issues if the filename does not
  # contain "file"
  variable_handle <- paste0("var", gsub("file", "", filename_handle))

  list(
    variable_arrays  = setup_variable_array(values,
                                            filename        = filename,
                                            variable_handle = variable_handle),
    condition        = sprintf("(%s in %s)", column_name, variable_handle),
    additional_files = filename
  )
}

combine_filter_condition <- function(
  condition1,
  condition2,
  operation
) {
  list(
    variable_arrays = c(condition1$variable_array, condition2$variable_array),
    condition = paste(condition1$condition, operation, condition2$condition),
    additional_files = c(condition1$additional_files, condition2$additional_files)
  )
}

and_filter_condition <- function(
  condition1,
  condition2
) {
  combine_filter_condition(condition1, condition2, "&&")
}
or_filter_condition <- function(
  condition1,
  condition2
) {
  combine_filter_condition(condition1, condition2, "||")
}

lp_filter_condition <- function(
  condition
) {
  condition$condition <- sprintf("(%s)", condition$condition)
  condition
}

get_used_columns <- function(
  fcondition,
  finterface
){
  if (is.call(fcondition)) {
    return(sapply(fcondition[-1],
                  get_used_columns,
                  get_file_interface(fcondition)) |>
             unlist() |>
             unique())
  }
  intersect(finterface$column_info$name,
            as.character(fcondition))
}

get_file_interface <- function(
  fcondition
) {
  attr(fcondition, "finterface_env")$finterface
}

#' @export
format.filter_condition <- function(
  fcondition
) {
  fcondition_operations <- c(
    lt_filter_condition  = "<",
    lte_filter_condition = "<=",
    gt_filter_condition  = ">",
    gte_filter_condition = ">=",
    eq_filter_condition  = "==",
    in_filter_condition  = "%in%"
  )
  if (as.character(fcondition[[1]]) %in% names(fcondition_operations)) {
    return(paste(format(fcondition[[2]]),
                 fcondition_operations[as.character(fcondition[[1]])],
                 format(fcondition[[3]])))
  }
  if (as.character(fcondition[[1]]) == "lp_filter_condition") {
    return(sprintf("(%s)", format(fcondition[[2]])))
  }
  if (as.character(fcondition[[1]]) == "and_filter_condition") {
    return(paste(format(fcondition[[2]]), "&", format(fcondition[[3]])))
  }
  if (as.character(fcondition[[1]]) == "or_filter_condition") {
    return(paste(format(fcondition[[2]]), "|", format(fcondition[[3]])))
  }
  fcondition
}

#' @export
print.filter_condition <- function(
  fcondition,
  ...,
  quote = FALSE
) {
  format(fcondition) |>
    cat()
}

#' @export
`&.filter_condition` <- function(
  fcondition1,
  fcondition2
) {
  if (is.null(fcondition1)) return(fcondition2)
  if (is.null(fcondition2)) return(fcondition1)
  stopifnot(identical(
    attr(fcondition1, "finterface_env"),
    attr(fcondition2, "finterface_env")
  ))

  if (fcondition1[[1]] == as.symbol("or_filter_condition")) {
    return(fcondition1[[2]] & fcondition2 | fcondition1[[3]] & fcondition2)
  }
  if (fcondition2[[1]] == as.symbol("or_filter_condition")) {
    return(fcondition1 & fcondition2[[2]] | fcondition1 & fcondition2[[3]])
  }

  fcondition <- rlang::expr(and_filter_condition()) |>
    as_filter_condition()
  fcondition[2:3] <- list(fcondition1, fcondition2)
  attr(fcondition, "finterface_env") <- attr(fcondition1, "finterface_env")
  fcondition
}

#' @export
`|.filter_condition` <- function(
  fcondition1,
  fcondition2
) {
  stopifnot(identical(
    attr(fcondition1, "finterface_env"),
    attr(fcondition2, "finterface_env")
  ))
  fcondition <- rlang::expr(or_filter_condition()) |>
    as_filter_condition()
  fcondition[2:3] <- list(fcondition1, fcondition2)
  attr(fcondition, "finterface_env") <- attr(fcondition1, "finterface_env")
  fcondition
}

is_and_block <- function(
  fcondition
) {
  if (!is.call(fcondition)) return(TRUE)

  if (fcondition[[1]] == as.symbol("or_filter_condition")) return(FALSE)

  all(sapply(fcondition[-1], is_and_block))
}

has_chromosome_condition <- function(
  fcondition
) {
  if (is.call(fcondition)) return(any(sapply(fcondition[-1], has_chromosome_condition)))
  fcondition == as.symbol("chr")
}

has_non_genomic_condition <- function(
  fcondition
) {
  if (as.character(fcondition[[1]]) %in% c("and_filter_condition", "or_filter_condition")) {
    return(any(sapply(fcondition[2:3], has_non_genomic_condition)))
  }
  if (as.character(fcondition[[1]]) == "in_filter_condition") {
    return(!is_genomic_symbol(fcondition[[2]]))
  }
  if (as.character(fcondition[[1]]) %in% c("lt_filter_condition", "lte_filter_condition",
                                           "gt_filter_condition", "gte_filter_condition",
                                           "eq_filter_condition")) {
    return(!is_genomic_symbol(fcondition[[2]]) & !is_genomic_symbol(fcondition[[3]]))
  }
  if (fcondition[[1]] == as.symbol("lp_filter_condition"))
    return(has_non_genomic_condition(fcondition[[2]]))
  !is_genomic_symbol(fcondition)
}

is_only_genomic_position_condition <- function(
  fcondition
) {
  has_chromosome_condition(fcondition) & !has_non_genomic_condition(fcondition)
}
is_genomic_symbol <- function(
  symbol
) {
  symbol == as.symbol("chr") | symbol == as.symbol("pos")
}

needs_genomic_condition_isolation <- function(
  fcondition
) {
  has_chromosome_condition(fcondition) & has_non_genomic_condition(fcondition) &
    needs_rsid_matching(get_file_interface(fcondition))
}

split_genomic_conditions <- function(
  fcondition
) {
  if (!has_chromosome_condition(fcondition)) return(fcondition)
  if (!has_non_genomic_condition(fcondition)) {
    to_return <- list()
    attributes(to_return) <- attributes(fcondition)
    attr(to_return, "genomic_condition") <- make_genomic_ranges(fcondition)
    return(to_return)
  }
  # Contains mix of genomic and non-genomic conditions
  if (fcondition[[1]] == as.symbol("and_filter_condition")) {
    return(
      split_genomic_conditions(fcondition[[2]])
      & split_genomic_conditions(fcondition[[3]])
    )
  }
}

make_genomic_ranges <- function(
  fcondition
) {
  stopifnot(!has_non_genomic_condition(fcondition))
  fcondition <- strip_parentheses(fcondition)
  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(
      lapply(fcondition[-1], make_genomic_ranges) |>
        data.table::rbindlist()
    )
  }
  stopifnot(is_and_block(fcondition))
  if (!has_chromosome_condition(fcondition)) {
    return(data.table::data.table(chr = character(), start = numeric(), end = numeric()))
  }

  numeric_operators <- c("lt_filter_condition", "lte_filter_condition",
                         "gt_filter_condition", "gte_filter_condition")
  chr_condition <- get_chr_condition(fcondition)

  if (as.character(chr_condition[[1]]) %in% numeric_operators) {
    as_numeric <- rlang::expr(as.numeric())
    if (chr_condition[[2]] == as.symbol("chr")) {
      as_numeric[[2]] <- chr_condition[[2]]
      chr_condition[[2]] <- as_numeric
    }
    if (chr_condition[[3]] == as.symbol("chr")) {
      as_numeric[[2]] <- chr_condition[[3]]
      chr_condition[[3]] <- as_numeric
    }
  }

  genomic_ranges <- data.table::data.table(
    chr = c(1:22, "X", "Y", "MT")
  )[
    parse(text = format(chr_condition)) |>
      eval() |>
      suppressWarnings()
  ] |>
    cbind(pos_condition_to_genomic_region(fcondition))
  genomic_ranges[!sapply(chr, is.na)]
}

get_chr_condition <- function(
  fcondition
) {
  stopifnot(is_and_block(fcondition))
  fcondition <- strip_parentheses(fcondition)
  if (fcondition[[1]] == as.symbol("and_filter_condition")) {
    return(
      get_chr_condition(fcondition[[2]]) & get_chr_condition(fcondition[[3]])
    )
  }
  if (fcondition[[2]] == as.symbol("chr") | fcondition[[3]] == as.symbol("chr")) {
    return(fcondition)
  }
  NULL
}
get_pos_condition <- function(
  fcondition
) {
  stopifnot(is_and_block(fcondition))
  fcondition <- strip_parentheses(fcondition)
  if (fcondition[[1]] == as.symbol("and_filter_condition")) {
    return(
      get_chr_condition(fcondition[[2]]) & get_chr_condition(fcondition[[3]])
    )
  }
  if (fcondition[[2]] == as.symbol("pos") | fcondition[[3]] == as.symbol("pos")) {
    return(fcondition)
  }
  NULL
}
pos_condition_to_genomic_region <- function(
  fcondition
) {
  fcondition <- strip_parentheses(fcondition)
  if (fcondition[[2]] == as.symbol("pos")) {
    if (fcondition[[1]] == as.symbol("lt_filter_condition"))
      return(data.table::data.table(end = fcondition[[3]]-1))
    if (fcondition[[1]] == as.symbol("lte_filter_condition"))
      return(data.table::data.table(end = fcondition[[3]]))
    if (fcondition[[1]] == as.symbol("gt_filter_condition"))
      return(data.table::data.table(start = fcondition[[3]]+1))
    if (fcondition[[1]] == as.symbol("gte_filter_condition"))
      return(data.table::data.table(start = fcondition[[3]]))
    if (fcondition[[1]] == as.symbol("eq_filter_condition")
        | fcondition[[1]] == as.symbol("in_filter_condition"))
      return(data.table::data.table(start = fcondition[[3]],
                                    end   = fcondition[[3]]))
  }
  if (fcondition[[3]] == as.symbol("pos")) {
    if (fcondition[[1]] == as.symbol("lt_filter_condition"))
      return(data.table::data.table(start = fcondition[[2]]+1))
    if (fcondition[[1]] == as.symbol("lte_filter_condition"))
      return(data.table::data.table(start = fcondition[[2]]))
    if (fcondition[[1]] == as.symbol("gt_filter_condition"))
      return(data.table::data.table(end = fcondition[[2]]-1))
    if (fcondition[[1]] == as.symbol("gte_filter_condition"))
      return(data.table::data.table(end = fcondition[[2]]))
    if (fcondition[[1]] == as.symbol("eq_filter_condition")
        | fcondition[[1]] == as.symbol("in_filter_condition"))
      return(data.table::data.table(start = fcondition[[2]],
                                    end   = fcondition[[2]]))
  }
  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(
      lapply(fcondition[-1], pos_condition_to_genomic_region) |>
        data.table::rbindlist(fill = TRUE, use.names = TRUE)
    )
  }
  if (fcondition[[1]] == as.symbol("and_filter_condition")) {
    to_return <- lapply(fcondition[-1], pos_condition_to_genomic_region) |>
      data.table::rbindlist(fill = TRUE, use.names = TRUE)
    return(
      to_return[
        ,
        .(
          start = {if (any(!is.na(to_return$start))) {
            max(start, na.rm = TRUE)
          } else {
            NA_real_
          }},
          end   = {if (any(!is.na(to_return$end))) {
            min(end, na.rm = TRUE)
          } else {
            NA_real_
          }}
        )
      ]
    )
  }
  NULL
}

needs_and_distributed <- function(
  fcondition
) {
  if (!is.call(fcondition)) return(FALSE)
  if (is_and_block(fcondition)) return(FALSE)
  if (fcondition[[1]] == as.symbol("lp_filter_condition")) return(TRUE)
  any(sapply(fcondition[-1], needs_and_distributed))
}

strip_parentheses <- function(
  fcondition,
  recursive = TRUE
) {
  if (fcondition[[1]] == as.symbol("lp_filter_condition"))
    return(strip_parentheses(fcondition[[2]], recursive = recursive))
  if (recursive) {
    if (fcondition[[1]] == as.symbol("and_filter_condition"))
      return(strip_parentheses(fcondition[[2]], recursive = recursive)
             & strip_parentheses(fcondition[[3]], recursive = recursive))
    if (fcondition[[1]] == as.symbol("or_filter_condition"))
      return(strip_parentheses(fcondition[[2]], recursive = recursive)
             | strip_parentheses(fcondition[[3]], recursive = recursive))
  }
  fcondition
}

distribute_and <- function(
  fcondition1,
  fcondition2
) {
  if (needs_and_distributed(fcondition1))
    fcondition1 <- strip_parentheses(fcondition1)
  if (needs_and_distributed(fcondition2))
    fcondition2 <- strip_parentheses(fcondition2)

  done_distributing <- function(fcondition) {
    !as.character(fcondition[[1]]) %in% c("and_filter_condition", "or_filter_condition")
  }

  if (!done_distributing(fcondition1)) {
    to_return <- fcondition1[1]
    to_return[2:3] <- lapply(fcondition1[2:3], distribute_and, fcondition2)
    return(to_return)
  }
  if (!done_distributing(fcondition2)) {
    to_return <- fcondition2[1]
    to_return[2:3] <- lapply(fcondition2[2:3], distribute_and, fcondition1 = fcondition1)
    return(to_return)
  }

  fcondition1 & fcondition2
}
