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
  env = parent.frame(),
  build = attr(fcall, "build") %||% finterface$build
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

  if (length(fcall) == 0) {
    return(fcall)
  }

  if (!as.character(fcall[[1]]) %in% names(fc_convert)) {
    return(fcall)
  }

  fcondition <- fc_convert[[as.character(fcall[[1]])]](
    fcall,
    finterface = attr(fcall, "finterface_env"),
    env = env,
    build = build
  )

  if ((needs_rsid_matching(get_file_interface(fcondition))) &&
    is.null(attr(fcondition, "genomic_range"))) {
    fcondition <- split_genomic_conditions(fcondition)
  }

  attr(fcondition, "build") <- build

  fcondition
}

combine_filter_condition <- function(
  fcondition1,
  fcondition2,
  operation
) {
  list(
    variable_arrays = c(fcondition1$variable_array, fcondition2$variable_array),
    condition = paste(fcondition1$condition, operation, fcondition2$condition),
    additional_files = c(fcondition1$additional_files, fcondition2$additional_files)
  )
}

get_used_columns <- function(
  fcondition,
  finterface
) {
  if (is.call(fcondition)) {
    return(sapply(
      fcondition[-1],
      get_used_columns,
      get_file_interface(fcondition)
    ) |>
      unlist() |>
      unique())
  }
  intersect(
    finterface$column_info$name,
    as.character(fcondition)
  )
}

get_file_interface <- function(
  fcondition
) {
  attr(fcondition, "finterface_env")$finterface
}

get_build <- function(
  fcondition
) {
  attr(fcondition, "build")
}

#' @export
format.filter_condition <- function(
  x,
  ...
) {
  fcondition_operations <- c(
    lt_filter_condition  = "<",
    lte_filter_condition = "<=",
    gt_filter_condition  = ">",
    gte_filter_condition = ">=",
    eq_filter_condition  = "==",
    in_filter_condition  = "%in%"
  )
  if (length(x) == 0) {
    fcondition_str <- "<Empty fcondition>"
  } else {
    if (as.character(x[[1]]) %in% names(fcondition_operations)) {
      fcondition_str <- paste(
        format(x[[2]]),
        fcondition_operations[as.character(x[[1]])],
        format(x[[3]])
      )
    }
    if (as.character(x[[1]]) == "lp_filter_condition") {
      fcondition_str <- sprintf("(%s)", format(x[[2]]))
    }
    if (as.character(x[[1]]) == "and_filter_condition") {
      fcondition_str <- paste(
        format(x[[2]]),
        "&", format(x[[3]])
      )
    }
    if (as.character(x[[1]]) == "or_filter_condition") {
      fcondition_str <- paste(
        format(x[[2]]),
        "|", format(x[[3]])
      )
    }
  }
  if (!is.null(attr(x, "genomic_range"))) {
    genomic_ranges <- attr(x, "genomic_range")[
      ,
      paste(
        " & rsid %in%",
        sprintf(
          "%s%s",
          chr,
          ifelse(is.na(start) & is.na(end),
            "",
            sprintf(
              ":%s-%s",
              ifelse(is.na(start), "", start),
              ifelse(is.na(end), "", end)
            )
          )
        ) |>
          paste(collapse = " ")
      )
    ]
  } else {
    genomic_ranges <- ""
  }

  paste0(fcondition_str, genomic_ranges)
}

#' @export
print.filter_condition <- function(
  x,
  ...,
  quote = FALSE
) {
  format(x) |>
    cat()
}

#' @export
`&.filter_condition` <- function(
  fcondition1,
  fcondition2
) {
  if (length(fcondition1) == 0) {
    attr(fcondition2, "genomic_range") <- intersect_genomic_ranges(
      attr(fcondition1, "genomic_range"),
      attr(fcondition2, "genomic_range")
    )
    return(fcondition2)
  }
  if (length(fcondition2) == 0) {
    attr(fcondition1, "genomic_range") <- intersect_genomic_ranges(
      attr(fcondition1, "genomic_range"),
      attr(fcondition2, "genomic_range")
    )
    return(fcondition1)
  }

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
  attr(fcondition, "genomic_range") <- intersect_genomic_ranges(
    attr(fcondition1, "genomic_range"),
    attr(fcondition2, "genomic_range")
  )

  if (get_build(fcondition1) != get_build(fcondition2)) {
    warning(
      "filter_conditions being combine do not have the same build, ",
      " using ", get_build(fcondition1)
    )
  }
  attr(fcondition, "build") <- get_build(fcondition1)
  
  fcondition
}

intersect_genomic_ranges <- function(
  genomic_range1,
  genomic_range2
) {
  if (is.null(genomic_range2)) {
    return(genomic_range1)
  }
  if (is.null(genomic_range1)) {
    return(genomic_range2)
  }
  genomic_range1[
    genomic_range2,
    on = "chr",
    nomatch = NULL
  ][
    ,
    .(
      start = ifelse(all(is.na(c(start, i.start))),
        NA_real_,
        max(start, i.start, na.rm = TRUE) |>
          suppressWarnings()
      ),
      end = ifelse(all(is.na(c(end, i.end))),
        NA_real_,
        min(end, i.end, na.rm = TRUE) |>
          suppressWarnings()
      )
    ),
    by = chr
  ]
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
  if (is_single_genomic_range_block(fcondition)) {
    if (length(fcondition1) == 0) {
      fcondition <- fcondition2
    } else if (length(fcondition2) == 0) {
      fcondition <- fcondition1
    }
    attr(fcondition, "genomic_range") <- rbind(
      attr(fcondition1, "genomic_range"),
      attr(fcondition2, "genomic_range")
    )
  } else {
    attr(fcondition, "genomic_range") <- NULL
  }

  if (get_build(fcondition1) != get_build(fcondition2)) {
    warning(
      "filter_conditions being combine do not have the same build, ",
      " using ", get_build(fcondition1)
    )
  }
  attr(fcondition, "build") <- get_build(fcondition1)

  fcondition
}

is_and_block <- function(
  fcondition
) {
  if (!is.call(fcondition)) {
    return(TRUE)
  }

  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(FALSE)
  }

  all(sapply(fcondition[-1], is_and_block))
}

has_genomic_condition <- function(
  fcondition
) {
  has_chromosome_condition(fcondition) | has_position_condition(fcondition)
}

has_chromosome_condition <- function(
  fcondition
) {
  if (!is.null(attr(fcondition, "genomic_range"))) {
    if (any(!is.na(attr(fcondition, "genomic_range")$chr))) {
      return(TRUE)
    }
  }
  if (is.call(fcondition)) {
    while (fcondition[[1]] == as.symbol("lp_filter_condition")) {
      fcondition <- fcondition[[2]]
    }
    return(any(sapply(fcondition[-1], has_chromosome_condition) |> unlist()))
  }
  if (length(fcondition) == 0) {
    return(FALSE)
  }
  fcondition == as.symbol("chr")
}

has_position_condition <- function(
  fcondition
) {
  if (!is.null(attr(fcondition, "genomic_range"))) {
    if (any(attr(fcondition, "genomic_range")[, c(!is.na(start), !is.na(end))])) {
      return(TRUE)
    }
  }
  if (is.call(fcondition)) {
    while (fcondition[[1]] == as.symbol("lp_filter_condition")) {
      fcondition <- fcondition[[2]]
    }
    return(any(sapply(fcondition[-1], has_position_condition) |> unlist()))
  }
  if (length(fcondition) == 0) {
    return(FALSE)
  }
  fcondition == as.symbol("pos")
}

has_non_genomic_condition <- function(
  fcondition
) {
  if (length(fcondition) == 0) {
    return(FALSE)
  }
  if (as.character(fcondition[[1]]) %in% c("and_filter_condition", "or_filter_condition")) {
    return(any(sapply(fcondition[2:3], has_non_genomic_condition)))
  }
  if (as.character(fcondition[[1]]) == "in_filter_condition") {
    return(!is_genomic_symbol(fcondition[[2]]))
  }
  if (as.character(fcondition[[1]]) %in% c(
    "lt_filter_condition", "lte_filter_condition",
    "gt_filter_condition", "gte_filter_condition",
    "eq_filter_condition"
  )) {
    return(!is_genomic_symbol(fcondition[[2]]) & !is_genomic_symbol(fcondition[[3]]))
  }
  if (fcondition[[1]] == as.symbol("lp_filter_condition")) {
    return(has_non_genomic_condition(fcondition[[2]]))
  }
  !is_genomic_symbol(fcondition)
}
is_genomic_symbol <- function(
  symbol
) {
  symbol == as.symbol("chr") | symbol == as.symbol("pos")
}

split_genomic_conditions <- function(
  fcondition
) {
  if (length(fcondition) == 0) {
    return(fcondition)
  }
  fcondition <- strip_parentheses(fcondition)
  if (!has_genomic_condition(fcondition)) {
    return(fcondition)
  }
  if (!has_non_genomic_condition(fcondition)) {
    to_return <- list()
    attributes(to_return) <- attributes(fcondition)
    attr(to_return, "genomic_range") <- make_genomic_ranges(fcondition)
    return(to_return)
  }
  # Contains mix of genomic and non-genomic conditions
  if (fcondition[[1]] == as.symbol("and_filter_condition")) {
    return(
      split_genomic_conditions(fcondition[[2]]) &
        split_genomic_conditions(fcondition[[3]])
    )
  }
  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(
      split_genomic_conditions(fcondition[[2]]) |
        split_genomic_conditions(fcondition[[3]])
    )
  }
  fcondition
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
    genomic_ranges <- data.table::data.table(
      chr = c(1:22, "X", "Y", "MT")
    ) |>
      cbind(pos_condition_to_genomic_region(fcondition))
  } else {
    numeric_operators <- c(
      "lt_filter_condition", "lte_filter_condition",
      "gt_filter_condition", "gte_filter_condition"
    )
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
    genomic_ranges <- genomic_ranges[!sapply(chr, is.na)]
  }

  if (!"end" %in% names(genomic_ranges)) genomic_ranges$end <- NA_real_
  if (!"start" %in% names(genomic_ranges)) {
    genomic_ranges <- genomic_ranges[
      ,
      .(
        chr = chr,
        start = NA_real_,
        end = end
      )
    ]
  }

  # if (get_build(fcondition) %in% c("b36", "b37", "b38")) {
    
  # }

  genomic_ranges
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
    if (fcondition[[1]] == as.symbol("lt_filter_condition")) {
      return(data.table::data.table(end = fcondition[[3]] - 1))
    }
    if (fcondition[[1]] == as.symbol("lte_filter_condition")) {
      return(data.table::data.table(end = fcondition[[3]]))
    }
    if (fcondition[[1]] == as.symbol("gt_filter_condition")) {
      return(data.table::data.table(start = fcondition[[3]] + 1))
    }
    if (fcondition[[1]] == as.symbol("gte_filter_condition")) {
      return(data.table::data.table(start = fcondition[[3]]))
    }
    if (fcondition[[1]] == as.symbol("eq_filter_condition") |
      fcondition[[1]] == as.symbol("in_filter_condition")) {
      return(data.table::data.table(
        start = fcondition[[3]],
        end = fcondition[[3]]
      ))
    }
  }
  if (fcondition[[3]] == as.symbol("pos")) {
    if (fcondition[[1]] == as.symbol("lt_filter_condition")) {
      return(data.table::data.table(start = fcondition[[2]] + 1))
    }
    if (fcondition[[1]] == as.symbol("lte_filter_condition")) {
      return(data.table::data.table(start = fcondition[[2]]))
    }
    if (fcondition[[1]] == as.symbol("gt_filter_condition")) {
      return(data.table::data.table(end = fcondition[[2]] - 1))
    }
    if (fcondition[[1]] == as.symbol("gte_filter_condition")) {
      return(data.table::data.table(end = fcondition[[2]]))
    }
    if (fcondition[[1]] == as.symbol("eq_filter_condition") |
      fcondition[[1]] == as.symbol("in_filter_condition")) {
      return(data.table::data.table(
        start = fcondition[[2]],
        end = fcondition[[2]]
      ))
    }
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
          start = {
            if (any(!is.na(to_return$start))) {
              max(start, na.rm = TRUE)
            } else {
              NA_real_
            }
          },
          end = {
            if (any(!is.na(to_return$end))) {
              min(end, na.rm = TRUE)
            } else {
              NA_real_
            }
          }
        )
      ]
    )
  }
  NULL
}

strip_parentheses <- function(
  fcondition,
  recursive = TRUE
) {
  if (length(fcondition) == 0) {
    return(fcondition)
  }
  if (fcondition[[1]] == as.symbol("lp_filter_condition")) {
    return(strip_parentheses(fcondition[[2]], recursive = recursive))
  }
  if (recursive) {
    if (fcondition[[1]] == as.symbol("and_filter_condition")) {
      return(
        strip_parentheses(fcondition[[2]], recursive = recursive) &
          strip_parentheses(fcondition[[3]], recursive = recursive)
      )
    }
    if (fcondition[[1]] == as.symbol("or_filter_condition")) {
      return(
        strip_parentheses(fcondition[[2]], recursive = recursive) |
          strip_parentheses(fcondition[[3]], recursive = recursive)
      )
    }
  }
  fcondition
}
