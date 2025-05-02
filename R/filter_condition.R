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

supported_fcondition_operations <- c(
  "<", "<=", ">", ">=", "==", "&", "|", "(", "%in%", "%chin%"
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
  finterface,
  env = parent.frame()
) {
  if (!is.call(fcall)) return(fcall)

  fcall <- as_filter_condition(fcall)

  if (!as.character(fcall[[1]]) %in% names(fc_convert)) return(fcall)

  if (is_file_interface(finterface)) {
    attr(fcall, "finterface_env") <- new.env(parent = emptyenv())
    attr(fcall, "finterface_env")$finterface <- finterface
  } else {
    attr(fcall, "finterface_env") <- finterface
  }

  # if (as.character(fcall[[1]]) %in% supported_fcondition_operations) {
  #   fcall[-1] <- lapply(fcall[-1],
  #                       new_filter_condition,
  #                       finterface = finterface,
  #                       env        = env)
  # }
  #
  # eval(fcall)

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
`&.filter_condition` <- function(
  fcondition1,
  fcondition2
) {
  stopifnot(identical(
    attr(fcondition1, "finterface_env"),
    attr(fcondition2, "finterface_env")
  ))
  fcondition <- rlang::expr(and_filter_condition()) |>
    as_filter_condition()
  fcondition[2:3] <- list(fcondition1, fcondition2)
  attr(fcondition, "finterface_env") <- attr(fcondition1, "finterface_env")
  fcondition
}
