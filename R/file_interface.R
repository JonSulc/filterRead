#' @import data.table

new_file_interface <- function(
  filename,
  column_names = NULL
) {
  stopifnot(is.character(filename))
  stopifnot(file.exists(filename))
  finterface <- structure(list(filename = filename,
                               gzipped  = is_gzipped(filename)),
                          class = c("file_interface", "character"))
  finterface$column_info <- get_column_info(
    finterface,
    column_names = column_names
  )

  finterface$sep <- get_file_separator(finterface)
  finterface
}

get_column_info <- function(
  finterface,
  column_names = NULL
) {
  file_colnames <- get_column_names(finterface) |>
    swap_in_column_names(column_names)

  data.frame(
    row.names     = names(file_colnames),
    index         = seq_along(file_colnames),
    column_index  = unlist(file_colnames),
    quoted_values = are_values_quoted(finterface)
  )
}

swap_in_column_names <- function(
  column_indices,
  column_names = NULL
) {
  if (is.null(column_names)) return(column_indices)
  new_names <- flip_column_names(column_names)[names(column_indices)]
  to_replace <- !sapply(new_names, is.null)
  names(column_indices)[to_replace] <- new_names[to_replace]
  column_indices
}

flip_column_names <- function(
  column_names
) {
  lapply(
    names(column_names),
    \(cname) {
      setNames(
        rep(cname, length(column_names[[cname]])),
        column_names[[cname]]
      )
    }
  ) |>
    unlist() |>
    as.list()
}

is_gzipped <- function(
  filename
) {
  stringr::str_detect(filename, "[.]gz$")
}

#' @export
head.file_interface <- function(
  finterface,
  nlines = 1,
  ...
) {
  if (!"column_indices" %in% names(finterface)) {
    return(
      data.table::fread(
        cmd = fhead_cmd(finterface, nlines + 1),
        ...
      )
    )
  }
  data.table::fread(
    cmd = fhead_cmd(finterface, nlines + 1),
    col.names = names(finterface$column_indices),
    ...
  )
}

fhead_cmd <- function(
  finterface,
  nlines
) {
  if (!finterface$gzipped) {
    return(paste("head -n", nlines, finterface$filename))
  }
  paste("zcat", finterface$filename, "| head -n", nlines)
}

are_values_quoted <- function(
  finterface
) {
  quoted_values <- head(finterface, nlines = 1, quote = "") |>
    sapply(stringr::str_detect, "\"")
  names(quoted_values) <- names(quoted_values) |>
    stringr::str_replace_all("\"", "")
  quoted_values
}

check_quotes <- function(
    value,
    value_needs_to_be_quoted
) {
  if (length(value) > 1) {
    # withr::with_environment(
    #   rlang::caller_env(),
    #   sapply(value, check_quotes, value_needs_to_be_quoted)
    # )
    return(sapply(value, check_quotes, value_needs_to_be_quoted))
  }
  if (is_value_numeric(value)) return(value)
  if (!value_needs_to_be_quoted) return(paste0("\"", value, "\""))
  paste0("\"\\\"", value, "\\\"\"")
}

is_value_numeric <- function(
    value
) {
  !is.na(as.numeric(value)) |>
    suppressWarnings()
}

get_file_separator <- function(
  finterface
) {
  dt_output <- head(finterface, nlines = 1, verbose = TRUE) |>
    capture.output() |>
    stringr::str_match("sep='([^']+)'")
  dt_output[!is.na(dt_output[, 1]), 2][1]
}

get_column_names <- function(
  finterface
) {
  column_names <- head(finterface) |>
    names() |>
    get_indices_from_column_names()
}

validate_file_interface <- function(
  finterface
) {
  if (!all(c("gzipped", "values_are_quoted") %chin% attributes(finterface))) {
    stop("finterface is missing attributes ",
         c("gzipped", "values_are_quoted")[
           !c("gzipped", "values_are_quoted") %chin% attributes(finterface)
         ] |>
           paste(collapse(", ")))
  }
  stopifnot(is.logical(attr(finterface, "gzipped")))
  stopifnot(is.logical(attr(finterface, "values_are_quoted")))
  stopifnot(file.exists(finterface))
}

#' @export
`[.file_interface` <- function(
  finterface,
  conditions,
  ...,
  return_only_cmd = FALSE
) {
  conditions <- new_filter_condition(rlang::enexpr(conditions), sep = finterface$sep)
  command_line <- as_command_line(
    conditions,
    finterface$filename,
    setNames(as.list(finterface$column_info$column_index), rownames(column_info)),
    sep = finterface$sep
  )

  # # e <-c(
  # #   list(rlang::caller_env()),
  # #   finterface,
  # #   finterface$column_indices,
  # #   list(`<`    =  lt_filter_condition,
  # #        `<=`   = lte_filter_condition,
  # #        `>`    =  gt_filter_condition,
  # #        `>=`   = gte_filter_condition,
  # #        `==`   =  eq_filter_condition,
  # #        `%in%` =  in_filter_condition,
  # #        `&`    = and_filter_condition,
  # #        `|`    =  or_filter_condition)
  # # )
  #
  # # cmd <- eval(rlang::enexpr(conditions),
  # #      e) |>
  # #   as.list() |>
  # #   purrr::list_c() |>
  # #   as_cmd(finterface = finterface)
  #
  # conditions_sub <- substitute(conditions)
  # print(rlang::enexpr(conditions_sub))
  #
  # print(eval(substitute(
  #   substitute2(.filter_condition, finterface$column_indices),
  #   list(.filter_condition = substitute(conditions))
  # )))
  # print(eval(substitute(
  #   substitute2(.filter_condition, finterface$column_info),
  #   list(.filter_condition = substitute(conditions))
  # )))
  #
  # cmd <- eval(substitute(
  #   substitute2(.filter_condition, finterface$column_info),
  #   list(.filter_condition = substitute(conditions))
  # )) |>
  #   as_cmd(finterface = finterface)

  if (return_only_cmd) return(command_line)
  lapply(
    command_line,
    \(cmd) {
      data.table::fread(
        cmd = cmd,
        ...,
        col.names = names(head(finterface, 0))
      )
    }
  ) |>
    data.table::rbindlist()
}

#' @export
print.file_interface <- function(
  finterface
) {
  cat(sprintf("\"%s\"\n", finterface$filename))
  cat(sprintf("Columns: %s",
              paste(names(finterface$column_indices), collapse = ", ")),
      "\n")
  cat(sprintf("Gzipped: %s, Quoted: %s",
              finterface$gzipped,
              finterface$values_are_quoted),
      "\n")
}

# as_cmd <- function(
#   conditions_list,
#   finterface
# ) {
#   wrap_initial_condition(conditions_list[[1]], finterface) |>
#     list() |>
#     c(lapply(conditions_list[-1], wrap_non_initial_condition, finterface)) |>
#     paste(collapse = " | ")
# }

is_sep_whitespace <- function(
  finterface
) {
  finterface$sep %in% c(" ", "\t")
}

# wrap_initial_condition <- function(
#   single_condition,
#   finterface
# ) {
#   if (finterface$gzipped) {
#     return(
#       sprintf("zcat %s | awk%s '%s%s'",
#               finterface$filename,
#               ifelse(is_sep_whitespace(finterface), "", " -F','"),
#               ifelse(is_chainable_condition(single_condition), "NR == 1 || ", ""),
#               single_condition)
#     )
#   }
#   sprintf("awk%s '%s%s' %s",
#           ifelse(is_sep_whitespace(finterface), "", " -F','"),
#           ifelse(is_chainable_condition(single_condition), "NR == 1 || ", ""),
#           single_condition,
#           finterface$filename)
# }
#
# wrap_non_initial_condition <- function(
#   single_condition,
#   finterface
# ) {
#   sprintf("awk%s '%s'",
#           ifelse(finterface$values_are_comma_separated, " -F','", ""),
#           single_condition)
# }
