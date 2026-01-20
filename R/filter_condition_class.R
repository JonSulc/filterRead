#' @include filter_condition_conversion.R

is.filter_condition <- function(x) inherits(x, "filter_condition")

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
    return(FALSE)
  }
  if (length(fcondition) == 1) {
    return(is_column_symbol(fcondition, finterface))
  }
  sapply(
    fcondition[-1],
    has_finterface_column_names,
    finterface
  ) |>
    any()
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
