#' @import data.table

new_file_interface <- function(
  filename
) {
  stopifnot(is.character(filename))
  stopifnot(file.exists(filename))
  finterface <- structure(list(filename = filename,
                               gzipped  = is_gzipped(filename)),
                          class    = c("file_interface", "character"))
  finterface$quoted_values <- are_values_quoted(finterface)
  if (any(finterface$quoted_values)) {
    warning("Quoted values are not yet implemented")
  }
  finterface$column_indices <- get_column_names(finterface)
  finterface$sep <- get_file_separator(finterface)
  finterface
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
  data.table::fread(cmd = fhead_cmd(finterface, nlines + 1), ...)
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

add_condition <- function(
  finterface,
  condition
) {

}

#' @export
`[.file_interface` <- function(
  finterface,
  conditions,
  ...,
  return_only_cmd = FALSE
) {
  e <-c(
    list(rlang::caller_env()),
    finterface,
    finterface$column_indices,
    list(`<`    =  lt_filter_condition,
         `<=`   = lte_filter_condition,
         `>`    =  gt_filter_condition,
         `>=`   = gte_filter_condition,
         `==`   =  eq_filter_condition,
         `%in%` =  in_filter_condition,
         `&`    = and_filter_condition,
         `|`    =  or_filter_condition)
  )

  cmd <- eval(rlang::enexpr(conditions),
       e) |>
    as.list() |>
    purrr::list_c() |>
    as_cmd(finterface = finterface)

  if (return_only_cmd) return(cmd)
  data.table::fread(
    cmd = cmd,
    ...,
    col.names = names(head(finterface, 0))
  )
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

as_cmd <- function(
  conditions_list,
  finterface
) {
  wrap_initial_condition(conditions_list[[1]], finterface) |>
    list() |>
    c(lapply(conditions_list[-1], wrap_non_initial_condition, finterface)) |>
    paste(collapse = " | ")
}

is_sep_whitespace <- function(
  finterface
) {
  finterface$sep %in% c(" ", "\t")
}

wrap_initial_condition <- function(
  single_condition,
  finterface
) {
  if (finterface$gzipped) {
    return(
      sprintf("zcat %s | awk%s '%s%s'",
              finterface$filename,
              ifelse(is_sep_whitespace(finterface), "", " -F','"),
              ifelse(is_chainable_condition(single_condition), "NR == 1 || ", ""),
              single_condition)
    )
  }
  sprintf("awk%s '%s%s' %s",
          ifelse(is_sep_whitespace(finterface), "", " -F','"),
          ifelse(is_chainable_condition(single_condition), "NR == 1 || ", ""),
          single_condition,
          finterface$filename)
}

wrap_non_initial_condition <- function(
  single_condition,
  finterface
) {
  sprintf("awk%s '%s'",
          ifelse(finterface$values_are_comma_separated, " -F','", ""),
          single_condition)
}
