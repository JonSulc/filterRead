#' @import data.table

get_column_info <- function(
    finterface,
    standard_names_dt = summary_stats_standard_names_dt,
    nrows_to_check = 500) {
  column_info <- get_base_column_info(
    finterface,
    standard_names_dt = standard_names_dt
  )

  data_to_check <- head(finterface, nrows_to_check)
  column_info <- filter_regex_matches(column_info, data_to_check)

  add_quoted_column(column_info, finterface)

  add_prefix_column(column_info, data_to_check)

  add_encoding_columns(column_info)

  add_allele_matching_to_column_info(column_info)

  column_info <- expand_encoded_columns(column_info)

  column_info[
    ,
    name := data.table::fcoalesce(standard_name, input_name)
  ][]
}

get_base_column_info <- function(
    finterface,
    standard_names_dt = summary_stats_standard_names_dt) {
  column_info <- data.table::data.table(
    input_name = head(finterface) |> names()
  )[
    ,
    c(
      .SD,
      .(
        input_index = seq_len(.N),
        bash_index = paste0("$", seq_len(.N))
      )
    )
  ]
  standard_names_dt[
    column_info,
    on = "input_name"
  ]
}

filter_regex_matches <- function(
    column_info,
    data_to_check) {
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
          warning(
            "Column ", input_name, " matches multiple regex patterns:\n",
            paste(row_match$regex, collapse = "\n"), "\n\n",
            "Using first."
          )
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
    column_data) {
  sapply(regexes, \(regex) {
    grepl(regex, column_data) |>
      all()
  })
}

add_quoted_column <- function(
    column_info,
    finterface) {
  column_info[
    ,
    quoted := are_values_quoted(finterface)[input_name]
  ][] |>
    invisible()
}
are_values_quoted <- function(
    finterface) {
  quoted_values <- head(finterface, nlines = 1, quote = "") |>
    sapply(stringr::str_detect, "\"")
  names(quoted_values) <- names(quoted_values) |>
    stringr::str_replace_all("\"", "")
  quoted_values
}

add_prefix_column <- function(
    column_info,
    data_to_check) {
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
    column_data) {
  prefixes <- sapply(prefixes, \(prefix) {
    grepl(paste0("^", prefix), column_data) |>
      all()
  })
  if (sum(prefixes) == 0) {
    return()
  }
  names(prefixes)[prefixes][1]
}

add_encoding_columns <- function(
    column_info) {
  column_info[
    !sapply(encoded_names, is.null),
    encoded_column_index := seq_len(.N)
  ][
    ,
    c("encoding_column", "split_encoding_column", "recode_columns") := NA_character_
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

expand_encoded_columns <- function(
    column_info) {
  # TODO Incomplete if only one of them missing?
  missing_rsid_columns <- c("chr", "pos")[
    !c("chr", "pos") %in% c(column_info$standard_name, unlist(column_info$encoded_names))
  ]

  column_info[
    ,
    {
      if (is.na(encoded_names)) {
        .SD
      } else if (!is.na(regex) & regex == "^(rs[0-9]+)$") {
        list(
          data.table::data.table(
            standard_name         = missing_rsid_columns,
            regex                 = regex,
            delimiter             = NA_character_,
            input_index           = input_index,
            quoted                = FALSE,
            encoding_column       = input_name,
            split_encoding_column = NA_character_,
            recode_columns        = NA_character_
          ),
          .SD
        ) |>
          data.table::rbindlist(fill = TRUE, use.names = TRUE) |>
          data.table::setcolorder(names(column_info))
      } else {
        list(
          .SD,
          expand_single_encoded_row(.SD)
        ) |>
          data.table::rbindlist(fill = TRUE, use.names = TRUE)
      }
    },
    by = seq_len(nrow(column_info))
  ][
    ,
    -"seq_len"
  ]
}

expand_single_encoded_row <- function(
    row_info) {
  stopifnot(nrow(row_info) == 1)
  if (is.null(row_info$encoded_names[[1]])) {
    return(NULL)
  }
  row_info[
    ,
    .(
      standard_name = unlist(encoded_names),
      regex = regex,
      delimiter = delimiter,
      input_index = input_index,
      bash_index = {
        if (is.na(encoded_column_index)) {
          stopifnot(row_info$standard_name == "alt")
          "nea"
        } else {
          sprintf(
            "encoded%i[%i]",
            encoded_column_index,
            seq_along(encoded_names[[1]])
          )
        }
      },
      quoted = FALSE,
      encoding_column = input_name,
      split_encoding_column = split_encoding_column,
      recode_columns = recode_columns
    )
  ]
}

needs_rsid_matching <- function(
    finterface,
    force = FALSE) {
  if (!force & "needs_rsid_matching" %in% names(finterface)) {
    return(finterface$needs_rsid_matching)
  }

  "rsid" %in% column_names(finterface) &
    !all(c("chr", "pos") %in% column_names(finterface, original = TRUE))
}

column_names <- function(
    finterface,
    original = FALSE,
    rsid_parsing = TRUE) {
  if (original) {
    return(finterface$column_info$input_name[!is.na(finterface$column_info$input_name)])
  }

  finterface$column_info[
    sapply(encoded_names, is.null),
    data.table::fcoalesce(standard_name, input_name)
  ]
}

add_allele_matching_to_column_info <- function(
    column_info) {
  if (!needs_a1_a2_to_ref_matching(column_info)) {
    return(column_info)
  }

  column_info[
    standard_name == "alt",
    encoded_names := .(c("ref", "alt"))
  ][
    standard_name == "alt",
    split_encoding_column := match_a1_a2_to_ref(
      a1_bash_index = column_info[standard_name == "allele1", bash_index],
      a2_bash_index = column_info[standard_name == "allele2", bash_index],
      alt_bash_index = column_info[standard_name == "alt", bash_index]
    )
  ][
    standard_name == "alt",
    recode_columns := sprintf(
      "%s = nea OFS %s",
      column_info[standard_name == "alt", bash_index],
      column_info[standard_name == "alt", bash_index]
    )
  ][] |>
    invisible()
}

needs_a1_a2_to_ref_matching <- function(
    column_info) {
  all(c("allele1", "allele2", "alt") %in% column_info$standard_name) &
    !"ref" %in% column_info$standard_name
}

match_a1_a2_to_ref <- function(
    a1_bash_index,
    a2_bash_index,
    alt_bash_index) {
  sprintf(
    "# Deduce NEA based on EA vs Allele1/Allele2
if (%s == %s) {
  nea = %s
} else {
  nea = %s
}",
    a1_bash_index,
    alt_bash_index,
    a2_bash_index,
    a1_bash_index
  )
}
