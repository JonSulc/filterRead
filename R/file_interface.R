#' @import data.table

new_file_interface <- function(
  filename
) {
  stopifnot(is.character(filename))
  stopifnot(file.exists(filename))
  finterface <- structure(list(filename = filename,
                               gzipped  = is_gzipped(filename)),
                          class    = c("file_interface", "character"))
  finterface$values_are_quoted <- are_values_quoted(finterface)
  finterface$column_indices <- get_column_names(finterface)
  finterface$values_are_comma_separated <- is_file_csv(finterface)
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
  head(finterface, nlines = 1, quote = "") |>
    names() |>
    stringr::str_detect("\"") |>
    all()
}

is_file_csv <- function(
  finterface
) {
  dt_output <- head(finterface, nlines = 1, verbose = TRUE) |>
    capture.output() |>
    stringr::str_match("sep='([^']+)'")
  dt_output[!is.na(dt_output[, 1]), 2][1] == ","
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
  conditions
) {
  e <-c(
    list(rlang::caller_env()),
    finterface$column_indices,
    list(`<`  = `<.filter_condition`,
         `<=` = `<=.filter_condition`,
         `>`  = `>.filter_condition`,
         `>=` = `>=.filter_condition`,
         `==` = `==.filter_condition`,
         `%in%` = `%in%.filter_condition`,
         `&`  = `&.filter_condition`)
  )

  eval(rlang::enexpr(conditions),
       e) |>
    purrr::list_flatten() |>
    as_cmd(finterface = finterface)
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

wrap_initial_condition <- function(
  single_condition,
  finterface
) {
  if (finterface$gzipped) {
    return(
      sprintf("zcat %s | awk%s '%s'",
              finterface$filename,
              ifelse(finterface$values_are_comma_separated, " -F',", ""),
              single_condition)
    )
  }
  sprintf("awk%s '%s' %s",
          ifelse(finterface$values_are_comma_separated, " -F',", ""),
          single_condition,
          finterface$filename)
}

wrap_non_initial_condition <- function(
  single_condition,
  finterface
) {
  sprintf("awk%s '%s'",
          ifelse(finterface$values_are_comma_separated, " -F',", ""),
          single_condition)
}
