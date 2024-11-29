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
    purrr::list_flatten()
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
