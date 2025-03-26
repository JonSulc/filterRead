#' @import data.table

#' @export
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

  list(
    index         = setNames(seq_along(file_colnames),
                             names(file_colnames)),
    bash_index    = file_colnames,
    quoted_values = are_values_quoted(finterface) |>
      as.list()
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
    value_needs_to_be_quoted,
    base_enquote = TRUE
) {
  if (is.null(value_needs_to_be_quoted)) {
    value_needs_to_be_quoted <- FALSE
  }

  number_of_quotes <- value_needs_to_be_quoted + (!all(is.numeric(value)) & base_enquote)

  if (number_of_quotes == 0) return(value)
  if (number_of_quotes == 1) return(paste0("\"", value, "\""))
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
  command_line <- new_filter_condition(
    rlang::enexpr(conditions),
    sep = finterface$sep,
    quoted_values = finterface$column_info$quoted_values
  ) |>
    as_command_line(
      finterface$filename,
      column_indices = finterface$column_info$bash_index,
      sep = finterface$sep,
      gzipped = finterface$gzipped
    )

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
              paste(names(finterface$column_info$index), collapse = ", ")),
      "\n")
  cat(sprintf(
    "Gzipped: %s, Quoted: %s",
    finterface$gzipped,
    ifelse(any(unlist(finterface$column_info$quoted_values)),
           paste(names(finterface$column_info$quoted_values)[
             unlist(finterface$column_info$quoted_values)
           ],
                 collapse = ", "),
           FALSE)
  ),
      "\n")
}
