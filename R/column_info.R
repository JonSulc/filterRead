#' @import data.table

get_base_column_info <- function(
  finterface,
  standard_names_dt = summary_stats_standard_names_dt
) {
  column_info <- data.table::data.table(
    input_name = head(finterface) |> names()
  )[
    ,
    c(.SD,
      .(input_index = seq_len(.N),
        bash_index = paste0("$", seq_len(.N))))
  ]
  standard_names_dt[
    column_info,
    on = "input_name"
  ]
}

filter_regex_matches <- function(
  column_info,
  data_to_check
) {
  column_info[
    ,
    {
      if (is.na(pattern[[1]])) {
        .SD
      } else {
        row_match <- .SD[check_single_column_regex(regex, data_to_check[[input_name]])]
        if (nrow(row_match) == 0) {
          row_match <- .SD[1]
          row_match[
            ,
            c("pattern", "regex", "encoded_names", "delimiter") := .(
              NA_character_, NA_character_, list(c()), NA_character_
            )
          ]
        } else if (1 < nrow(row_match)) {
          warning("Column ", input_name, " matches multiple regex patterns:\n",
                  paste(row_match$regex, collapse = "\n"), "\n\n",
                  "Using first.")
          row_match <- row_match[1]
        } else {
          row_match
        }
      }
    },
    by = input_name
  ]
}
check_single_column_regex <- function(
  regexes,
  column_data
) {
  sapply(regexes, \(regex) {
    grepl(regex, column_data) |>
      all()
  })
}

add_quoted_column <- function(
  column_info,
  finterface
) {
  column_info[
    ,
    quoted := are_values_quoted(finterface)[input_name]
  ][] |>
    invisible()
}

add_prefix_column <- function(
  column_info,
  data_to_check
) {
  column_info[
    ,
    prefix := character(0)
  ][
    !is.na(possible_prefixes),
    prefix := check_single_column_prefix(possible_prefixes, data_to_check[[input_name]]),
    by = bash_index
  ][] |>
    invisible()
}
check_single_column_prefix <- function(
  prefixes,
  column_data
) {
  prefixes <- sapply(prefixes, \(prefix) {
    grepl(paste0("^", prefix), column_data) |>
      all()
  })
  if (sum(prefixes) == 0) return()
  names(prefixes)[prefixes][1]
}

add_encoding_columns <- function(
  column_info
) {
  column_info[
    !sapply(encoded_names, is.null),
    encoded_column_index := seq_len(.N)
  ][
    ,
    encoding_column := NA_character_
  ][]
  if (any(!sapply(column_info$delimiter, is.na))) {
    column_info[
      !sapply(delimiter, is.na),
      split_encoding_column := sprintf(
        "split(%s, encoded%i, \"%s\")",
        bash_index,
        encoded_column_index,
        delimiter
      )
    ][
      !sapply(delimiter, is.na),
      recode_columns := sprintf(
        "%s = %s",
        bash_index,
        sprintf("encoded%i[%i]", encoded_column_index, seq_along(encoded_names[[1]])) |>
          paste(collapse = " OFS ")
      ),
      by = bash_index
    ][]
  }
  invisible(column_info)
}
