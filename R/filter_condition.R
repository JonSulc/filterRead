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
atomic_fc_operators <- list(
  "==" = "eq_filter_condition",
  "<" = "lt_filter_condition",
  "<=" = "lte_filter_condition",
  ">" = "gt_filter_condition",
  ">=" = "gte_filter_condition",
  "%in%" = "in_filter_condition"
)
composite_fc_operators <- list(
  "&" = "and_filter_condition",
  "|" = "or_filter_condition",
  "(" = "lp_filter_condition"
)
composite_op_exceptions <- list(
  "(" = identity
)

empty_filter_condition <- function(
  build = NULL,
  genomic_regions = full_genomic_regions(build = build),
  finterface_env = NULL
) {
  structure(
    list(),
    class = c("filter_condition", "call"),
    build = build,
    genomic_regions = genomic_regions,
    finterface_env = finterface_env
  )
}


as_filter_condition <- function(
  x,
  ...
) {
  structure(
    x,
    class = c("filter_condition", class(x)) |>
      unique(),
    ...
  )
}

is_atomic_filter_condition <- function(
  fcondition,
  finterface = get_file_interface(fcondition),
  atomic_operators = names(atomic_fc_operators) |>
    c(unlist(atomic_fc_operators)) |>
    unname()
) {
  as.character(fcondition[[1]]) %in% atomic_operators &&
    has_finterface_column_names(fcondition, finterface)
}
is_composite_filter_condition <- function(
  fcondition,
  finterface = get_file_interface(fcondition),
  composite_operators = names(composite_fc_operators) |>
    c(unlist(composite_fc_operators)) |>
    unname()
) {
  if (length(fcondition) == 0) {
    return(FALSE)
  }
  as.character(fcondition[[1]]) %in% composite_operators &&
    has_finterface_column_names(fcondition, finterface)
}
has_finterface_column_names <- function(
  fcondition,
  finterface
) {
  if (length(fcondition) == 0) {
    FALSE
  } else if (length(fcondition) == 1) {
    is_column_symbol(fcondition, finterface)
  } else {
    sapply(
      fcondition[-1],
      has_finterface_column_names,
      finterface
    ) |>
      any()
  }
}

#' @export
new_filter_condition <- function(
  x,
  finterface,
  env = parent.frame(),
  build = get_build(x) %||% get_build(finterface)
) {
  UseMethod("new_filter_condition")
}
#' @export
new_filter_condition.default <- function(
  x,
  finterface,
  env = parent.frame(),
  build = get_build(x) %||% get_build(finterface)
) {
  x
}
#' @export
new_filter_condition.call <- function(
  x,
  finterface,
  env = parent.frame(),
  build = get_build(x) %||% get_build(finterface)
) {
  if (is_file_interface(finterface)) {
    finterface_env <- new.env(parent = emptyenv())
    finterface_env$finterface <- finterface
  } else {
    finterface_env <- finterface
  }

  if (is.null(x)) {
    fcondition <- empty_filter_condition(build = build)
  } else if (is_composite_filter_condition(x, finterface_env$finterface)) {
    args <- lapply(
      x[-1],
      new_filter_condition,
      finterface = finterface_env,
      env = env,
      build = build
    )
    fcondition <- switch(as.character(x[[1]]),
      "&" = args[[1]] & args[[2]],
      "|" = args[[1]] | args[[2]],
      "(" = lp_wrap_fcondition(args[[1]]),
      do.call(x[[1]], args)
    )
  } else if (is_atomic_filter_condition(x, finterface_env$finterface)) {
    atomic_fc_name <- atomic_fc_operators[[as.character(x[[1]])]]
    fcondition <- as_filter_condition(x)
    fcondition[[1]] <- atomic_fc_name |>
      as.symbol()
    class(fcondition) <- c(
      atomic_fc_name,
      class(fcondition)
    ) |>
      unique()
    fcondition <- evaluate_non_column_variables(
      fcondition,
      finterface_env$finterface,
      env
    ) |>
      split_genomic_conditions(build = build)
  } else {
    fcondition <- as_filter_condition(x) |>
      split_genomic_conditions(build = build)
  }

  attr(fcondition, "finterface_env") <- finterface_env

  attr(fcondition, "build") <- build

  fcondition
}
#' @export
`new_filter_condition.(` <- new_filter_condition.call

get_used_columns <- function(
  fcondition,
  finterface = get_file_interface(fcondition)
) {
  gregions <- genomic_regions(fcondition)
  if (is.null(gregions)) {
    gregion_columns <- NULL
  } else {
    gregion_columns <- genomic_regions(fcondition)[
      ,
      .(
        chr = chr,
        pos = start,
        pos = end
      )
    ] |>
      sapply(\(x) any(!is.na(x))) |>
      which() |>
      names() |>
      unique()
  }

  if (is.call(fcondition)) {
    return(sapply(
      fcondition[-1],
      get_used_columns,
      get_file_interface(fcondition)
    ) |>
      unlist() |>
      c(gregion_columns) |>
      unique())
  }
  intersect(
    finterface$column_info$name,
    as.character(fcondition)
  ) |>
    c(gregion_columns) |>
    unique()
}

get_file_interface <- function(
  fcondition
) {
  attr(fcondition, "finterface_env")$finterface
}

#' @export
format.filter_condition <- function(
  x,
  append_genomic_regions = TRUE,
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
    } else if (as.character(x[[1]]) == "lp_filter_condition") {
      fcondition_str <- sprintf(
        "(%s)",
        format(x[[2]], append_genomic_regions = append_genomic_regions)
      )
    } else if (as.character(x[[1]]) == "and_filter_condition") {
      fcondition_str <- paste(
        format(x[[2]], append_genomic_regions = FALSE),
        "&", format(x[[3]], append_genomic_regions = FALSE)
      )
    } else if (as.character(x[[1]]) == "or_filter_condition") {
      fcondition_str <- paste(
        format(x[[2]], append_genomic_regions = FALSE),
        "|", format(x[[3]], append_genomic_regions = FALSE)
      )
    } else {
      fcondition_str <- paste(
        format(x[[2]], append_genomic_regions = FALSE),
        as.character(x[[1]]),
        format(x[[3]], append_genomic_regions = FALSE)
      )
    }
  }
  genomic_regions <- ""
  if (all(is.na(genomic_regions))) {
    genomic_regions <- genomic_regions(x)[
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
  }

  paste0(fcondition_str, genomic_regions)
}

#' @export
post_process <- function(x, finterface, env) {
  UseMethod("post_process")
}
#' @export
post_process.lp_filter_condition <- function(
  x,
  finterface = get_file_interface(x),
  env = NULL
) {
  x[[2]] <- post_process(x[[2]], finterface = finterface, env = env)
  x
}
#' @export
post_process.and_filter_condition <- function(
  x,
  finterface = get_file_interface(x),
  env = NULL
) {
  x[-1] <- lapply(
    x[-1],
    post_process,
    finterface = finterface,
    env = env
  )
  x
}
#' @export
post_process.or_filter_condition <- post_process.and_filter_condition
#' @export
post_process.lt_filter_condition <- function(
  x,
  finterface,
  env
) {
  x
}
#' @export
post_process.lte_filter_condition <- function(
  x,
  finterface,
  env
) {
  x
}
#' @export
post_process.gt_filter_condition <- function(
  x,
  finterface,
  env
) {
  x
}
#' @export
post_process.gte_filter_condition <- function(
  x,
  finterface,
  env
) {
  x
}
#' @export
post_process.eq_filter_condition <- function(
  x,
  finterface = get_file_interface(x),
  env = NULL
) {
  is_column_name <- sapply(
    x[-1],
    is_column_symbol,
    finterface = finterface
  )
  if (is_column_name[1] & !is_column_name[2]) {
    x[[3]] <- eval(x[[3]], env) |>
      check_post_processing(x[[2]], finterface)
  } else if (is_column_name[2] & !is_column_name[3]) {
    x[[2]] <- eval(x[[2]], env) |>
      check_post_processing(x[[3]], finterface)
  }
  x
}
#' @export
post_process.in_filter_condition <- function(
  x,
  finterface = get_file_interface(x),
  env = NULL
) {
  x[[3]] <- eval(x[[3]], env) |>
    check_post_processing(x[[2]], finterface, to_write = TRUE)
  x
}
#' @export
post_process.genomic_regions <- function(
  x,
  finterface,
  env = NULL
) {
  copy_genomic_regions(x)[
    !is.na(chr),
    chr := drop_chr_prefix(chr) |>
      check_post_processing("chr", finterface)
  ][] |>
    as_genomic_regions()
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
    genomic_regions(fcondition2) <-
      genomic_regions(fcondition1) & genomic_regions(fcondition2)
    return(fcondition2)
  }
  if (length(fcondition2) == 0) {
    genomic_regions(fcondition1) <-
      genomic_regions(fcondition1) & genomic_regions(fcondition2)
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
  genomic_regions(fcondition) <- intersect_genomic_regions(
    genomic_regions(fcondition1),
    genomic_regions(fcondition2)
  )

  if (!identical(get_build(fcondition1), get_build(fcondition2))) {
    warning(
      "filter_conditions being combine do not have the same build, ",
      " using ", get_build(fcondition1)
    )
  }


  attr(fcondition, "build") <- get_build(fcondition1) %||% get_build(fcondition2)
  class(fcondition) <- c("and_filter_condition", class(fcondition)) |>
    unique()

  fcondition
}

intersect_genomic_regions <- function(
  genomic_regions1,
  genomic_regions2
) {
  if (is.null(genomic_regions2)) {
    return(genomic_regions1)
  }
  if (is.null(genomic_regions1)) {
    return(genomic_regions2)
  }
  genomic_regions1[
    genomic_regions2,
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
  if (is_single_genomic_block(fcondition)) {
    if (length(fcondition1) == 0) {
      fcondition <- fcondition2
    } else if (length(fcondition2) == 0) {
      fcondition <- fcondition1
    }
    genomic_regions(fcondition) <- rbind(
      genomic_regions(fcondition1),
      genomic_regions(fcondition2)
    )
  } else {
    genomic_regions(fcondition) <- full_genomic_regions(
      build = get_build(fcondition1)
    )
  }

  if (!identical(get_build(fcondition1), get_build(fcondition2))) {
    warning(
      "filter_conditions being combine do not have the same build, ",
      " using ", get_build(fcondition1)
    )
  }

  attr(fcondition, "build") <- get_build(fcondition1) %||% get_build(fcondition2)
  class(fcondition) <- c("or_filter_condition", class(fcondition)) |>
    unique()

  fcondition
}

is_and_block <- function(
  fcondition
) {
  if (!inherits(fcondition, "call")) {
    return(TRUE)
  }

  if (!is.null(genomic_regions(fcondition))) {
    if (1 < nrow(genomic_regions(fcondition))) {
      return(FALSE)
    }
  }

  if (length(fcondition) == 0) {
    return(TRUE)
  }

  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(FALSE)
  }

  all(sapply(fcondition[-1], is_and_block))
}

genomic_regions <- function(
  fcondition,
  recursive = FALSE
) {
  if (!recursive || !is_composite_filter_condition(fcondition)) {
    return(attr(fcondition, "genomic_regions"))
  }
  if (fcondition[[1]] == as.symbol("lp_filter_condition")) {
    # fcondition is (
    return(
      attr(fcondition, "genomic_regions") &
        genomic_regions(fcondition[[2]], recursive = TRUE)
    )
  } else if (fcondition[[1]] == as.symbol("and_filter_condition")) {
    # fcondition is & or |
    return(
      attr(fcondition, "genomic_regions") &
        genomic_regions(fcondition[[2]], recursive = TRUE) &
        genomic_regions(fcondition[[3]], recursive = TRUE)
    )
  } else if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(
      attr(fcondition, "genomic_regions") &
        (genomic_regions(fcondition[[2]], recursive = TRUE) |
          genomic_regions(fcondition[[3]], recursive = TRUE))
    )
  }
  stop(
    "Unrecognized composite filter_condition: ",
    as.character(fcondition[[1]])
  )
}
has_no_gregions <- function(
  fcondition
) {
  gregions <- genomic_regions(fcondition, recursive = TRUE)
  is.null(gregions) || is_full_genome(gregions)
}

`genomic_regions<-` <- function(
  fcondition,
  value
) {
  attr(fcondition, "genomic_regions") <- value
  fcondition
}

has_genomic_condition <- function(
  fcondition
) {
  has_chromosome_condition(fcondition) | has_position_condition(fcondition)
}

has_chromosome_condition <- function(
  fcondition
) {
  if (!is.null(genomic_regions(fcondition))) {
    if (any(!is.na(genomic_regions(fcondition)$chr))) {
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
  if (!is.null(genomic_regions(fcondition))) {
    if (any(genomic_regions(fcondition)[, c(!is.na(start), !is.na(end))])) {
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
  fcondition,
  build = get_build(fcondition)
) {
  if (length(fcondition) == 0) {
    return(fcondition)
  }
  fcondition <- strip_parentheses(fcondition)
  if (!has_genomic_condition(fcondition)) {
    genomic_regions(fcondition) <- full_genomic_regions(
      build = build
    )
    return(fcondition)
  }
  if (!has_non_genomic_condition(fcondition)) {
    return(
      empty_filter_condition(
        build = build,
        genomic_regions = as_genomic_regions(fcondition, build = build),
        finterface = attr(fcondition, "finterface_env")
      )
    )
  }
  stop("This should not be occurring")
  # Contains mix of genomic and non-genomic conditions
  if (fcondition[[1]] == as.symbol("and_filter_condition")) {
    return(
      split_genomic_conditions(fcondition[[2]], build = build) &
        split_genomic_conditions(fcondition[[3]], build = build)
    )
  }
  if (fcondition[[1]] == as.symbol("or_filter_condition")) {
    return(
      split_genomic_conditions(fcondition[[2]], build = build) |
        split_genomic_conditions(fcondition[[3]], build = build)
    )
  }
  fcondition
}

is_or_condition <- function(fcondition) {
  inherits(fcondition, "or_filter_condition")
}
is_and_condition <- function(fcondition) {
  inherits(fcondition, "and_filter_condition")
}
is_eq_condition <- function(fcondition) {
  inherits(fcondition, "eq_filter_condition")
}
is_lt_condition <- function(fcondition) {
  inherits(fcondition, "lt_filter_condition")
}
is_lte_condition <- function(fcondition) {
  inherits(fcondition, "lte_filter_condition")
}
is_gt_condition <- function(fcondition) {
  inherits(fcondition, "gt_filter_condition")
}
is_gte_condition <- function(fcondition) {
  inherits(fcondition, "gte_filter_condition")
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
