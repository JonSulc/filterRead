#' @include filter_condition_conversion.R

#' @title Test if an object is a filter_condition
#'
#' @param x Object to test
#' @return Logical indicating whether x inherits from filter_condition
#' @keywords internal
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

#' Create an empty filter condition
#'
#' Creates a filter_condition with no conditions (no filtering, matches all
#' rows).
#'
#' @param build Optional genome build (b36, b37, b38)
#' @param genomic_regions Genomic regions to associate with the condition
#' @param finterface_env Environment containing the file_interface
#' @return An empty filter_condition object
#' @keywords internal
empty_filter_condition <- function(
  build = NULL,
  genomic_regions = full_genomic_regions(build = build),
  finterface_env = NULL
) {
  structure(
    list(),
    class = c("filter_condition", "list"),
    build = build,
    genomic_regions = genomic_regions,
    finterface_env = finterface_env
  )
}


#' Coerce an object to a filter_condition
#'
#' Adds the filter_condition class to an object while preserving existing
#' classes.
#'
#' @param x Object to coerce
#' @param ... Additional attributes to set
#' @return Object with filter_condition class added
#' @rdname as_filter_condition
#' @keywords internal
#' @export
as_filter_condition <- function(
  x,
  ...
) {
  UseMethod("as_filter_condition")
}
#' @rdname as_filter_condition
#' @keywords internal
#' @export
as_filter_condition.default <- function(
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
#' @rdname as_filter_condition
#' @keywords internal
#' @export
as_filter_condition.quosure <- function(
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

#' Check if a filter condition is atomic
#'
#' Atomic conditions are simple comparisons (==, <, <=, >, >=, %in%).
#'
#' @param fcondition A filter_condition object
#' @param finterface file_interface for column name resolution
#' @param atomic_operators Character vector of atomic operator names
#' @return Logical indicating whether the condition is atomic
#' @rdname is_atomic_filter_condition
#' @keywords internal
#' @export
is_atomic_filter_condition <- function(
  fcondition,
  finterface = get_file_interface(fcondition),
  atomic_operators = names(atomic_fc_operators) |>
    c(unlist(atomic_fc_operators)) |>
    unname()
) {
  UseMethod("is_atomic_filter_condition")
}
#' @rdname is_atomic_filter_condition
#' @keywords internal
#' @export
is_atomic_filter_condition.quosure <- function(
  fcondition,
  finterface = get_file_interface(fcondition),
  atomic_operators = names(atomic_fc_operators) |>
    c(unlist(atomic_fc_operators)) |>
    unname()
) {
  fexpr <- rlang::get_expr(fcondition)
  if (is.name(fexpr)) {
    return(FALSE)
  }
  as.character(fexpr[[1]]) %in% atomic_operators &&
    has_finterface_column_names(fexpr, finterface)
}
#' @rdname is_atomic_filter_condition
#' @keywords internal
#' @export
is_atomic_filter_condition.list <- function(
  fcondition,
  ...
) {
  FALSE
}

#' Check if a filter condition is composite
#'
#' Composite conditions combine other conditions with &, |, or parentheses.
#'
#' @param fcondition A filter_condition object
#' @param finterface file_interface for column name resolution
#' @param composite_operators Character vector of composite operator names
#' @return Logical indicating whether the condition is composite
#' @rdname is_composite_filter_condition
#' @keywords internal
#' @export
is_composite_filter_condition <- function(
  fcondition,
  finterface = get_file_interface(fcondition),
  composite_operators = names(composite_fc_operators) |>
    c(unlist(composite_fc_operators)) |>
    unname()
) {
  UseMethod("is_composite_filter_condition")
}
#' @rdname is_composite_filter_condition
#' @keywords internal
#' @export
is_composite_filter_condition.quosure <- function(
  fcondition,
  finterface = get_file_interface(fcondition),
  composite_operators = names(composite_fc_operators) |>
    c(unlist(composite_fc_operators)) |>
    unname()
) {
  fexpr <- rlang::get_expr(fcondition)
  if (length(fexpr) <= 1) {
    return(FALSE)
  }
  as.character(fexpr[[1]]) %in% composite_operators &&
    has_finterface_column_names(fexpr, finterface)
}
#' @rdname is_composite_filter_condition
#' @keywords internal
#' @export
is_composite_filter_condition.list <- function(
  fcondition,
  ...
) {
  FALSE
}

#' Check if a filter condition references file interface columns
#'
#' Recursively checks whether a filter condition contains references to
#' columns defined in finterface.
#'
#' @param fcondition A filter_condition object
#' @param finterface file_interface for column name resolution
#' @return Logical indicating whether any column names are referenced
#' @rdname has_finterface_column_names
#' @keywords internal
#' @export
has_finterface_column_names <- function(
  fcondition,
  finterface
) {
  UseMethod("has_finterface_column_names")
}
#' @rdname has_finterface_column_names
#' @keywords internal
#' @export
has_finterface_column_names.default <- function(
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
#' @rdname has_finterface_column_names
#' @keywords internal
#' @export
has_finterface_column_names.quosure <- function(
  fcondition,
  finterface
) {
  rlang::get_expr(fcondition) |>
    has_finterface_column_names(finterface)
}

#' @export
new_filter_condition <- function(
  x,
  finterface,
  build = NULL
) {
  UseMethod("new_filter_condition")
}
#' @export
new_filter_condition.default <- function(
  x,
  finterface,
  build = NULL
) {
  x
}
#' @export
new_filter_condition.name <- function(
  x,
  finterface,
  env = parent.frame(),
  build = NULL
) {
  x <- eval(x, envir = env)
  if (missing(build) || is.null(build)) {
    build <- build(finterface) %||% build(x)
  }
  new_filter_condition(
    x,
    finterface = finterface,
    build = build
  )
}
#' @export
new_filter_condition.genomic_regions <- function(
  x,
  finterface,
  build = NULL
) {
  if (missing(build) || is.null(build)) {
    build <- build(finterface) %||% build(x)
  }
  if (is_file_interface(finterface)) {
    finterface_env <- new.env(parent = emptyenv())
    finterface_env$finterface <- finterface
  } else {
    finterface_env <- finterface
  }
  empty_filter_condition(
    build = build,
    genomic_regions = liftover(x, build),
    finterface_env = finterface_env
  )
}
#' @export
new_filter_condition.quosure <- function(
  x,
  finterface,
  build = "auto"
) {
  if (!is.null(build) && build == "auto") {
    build <- build(x) %||% build(finterface)
  }
  if (is_file_interface(finterface)) {
    finterface_env <- new.env(parent = emptyenv())
    finterface_env$finterface <- finterface
  } else {
    finterface_env <- finterface
  }

  if (is.null(rlang::get_expr(x))) {
    # TODO Check this actually ever triggers
    fcondition <- empty_filter_condition(build = build)
  } else if (is_composite_filter_condition(x, finterface_env$finterface)) {
    fc_operator <- rlang::get_expr(x)[[1]]
    args <- lapply(
      rlang::get_expr(x)[-1],
      rlang::new_quosure,
      env = rlang::get_env(x)
    ) |>
      lapply(
        new_filter_condition,
        finterface = finterface_env,
        build = build
      )
    fcondition <- switch(as.character(fc_operator),
      "&" = args[[1]] & args[[2]],
      "|" = args[[1]] | args[[2]],
      "(" = lp_wrap_fcondition(args[[1]]),
      do.call(fc_operator, args)
    )
  } else if (is_atomic_filter_condition(x, finterface_env$finterface)) {
    atomic_fc_name <- atomic_fc_operators[[
      as.character(rlang::get_expr(x)[[1]])
    ]]
    fcondition <- as_filter_condition(x)
    fcondition[[1]] <- as.symbol(atomic_fc_name)
    class(fcondition) <- c(
      atomic_fc_name,
      class(fcondition)
    ) |>
      unique()
    fcondition <- evaluate_non_column_variables(
      fcondition,
      finterface_env$finterface
    ) |>
      split_genomic_conditions(build = build)
  } else if (is.name(rlang::get_expr(x))) {
    return(
      new_filter_condition(
        rlang::eval_tidy(x),
        finterface = finterface_env,
        build = build
      )
    )
  } else {
    fcondition <- as_filter_condition(x) |>
      split_genomic_conditions(build = build)
  }

  attr(fcondition, "finterface_env") <- finterface_env

  build(fcondition) <- build

  fcondition
}
#' @export
`new_filter_condition.(` <- new_filter_condition.quosure

#' Get column names used in a filter_condition
#'
#' Recursively extracts all column names referenced in a filter_condition,
#' including those implied by genomic_regions constraints.
#'
#' @param fcondition A filter_condition object
#' @param finterface file_interface for column name resolution
#' @return Character vector of column names used in the condition
#' @keywords internal
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

  if (rlang::is_quosure(fcondition)) {
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

#' Get the file interface from a filter condition
#'
#' Extracts the file interface object stored in the filter condition's
#' finterface_env attribute.
#'
#' @param fcondition A filter_condition object
#' @return The file_interface object, or NULL if not set
#' @keywords internal
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

  paste(
    fcondition_str,
    ifelse(
      is_full_genome(genomic_regions(x)),
      "",
      str(genomic_regions(x))
    )
  )
}

#' @export
`[[.filter_condition` <- function(
  x,
  ...
) {
  rlang::get_expr(x)[[...]]
}

#' @export
`[[<-.filter_condition` <- function(
  x,
  ...,
  value
) {
  fexpr <- rlang::get_expr(x)
  fexpr[[...]] <- value
  rlang::set_expr(x, fexpr)
}

#' @export
`[.filter_condition` <- function(
  x,
  ...
) {
  rlang::get_expr(x)[...]
}

#' @export
`[<-.filter_condition` <- function(
  x,
  ...,
  value
) {
  fexpr <- rlang::get_expr(x)
  fexpr[...] <- value
  rlang::set_expr(x, fexpr)
}

#' @export
post_process <- function(x, finterface, env) {
  UseMethod("post_process")
}
#' @export
post_process.lp_filter_condition <- function(
  x,
  finterface = get_file_interface(x)
) {
  x[[2]] <- post_process(
    x[[2]],
    finterface = finterface
  )
  x
}
#' @export
post_process.and_filter_condition <- function(
  x,
  finterface = get_file_interface(x)
) {
  x[-1] <- lapply(
    x[-1],
    post_process,
    finterface = finterface
  )
  x
}
#' @export
post_process.or_filter_condition <- post_process.and_filter_condition

# Comparison operators don't need post-processing - just return unchanged
#' @export
post_process.filter_condition <- function(x, finterface, env) x
#' @export
post_process.lte_filter_condition <- post_process.filter_condition
#' @export
post_process.gt_filter_condition <- post_process.filter_condition
#' @export
post_process.gte_filter_condition <- post_process.filter_condition
#' @export
post_process.lt_filter_condition <- post_process.filter_condition

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
