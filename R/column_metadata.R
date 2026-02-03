# These functions detect and configure column metadata for file interfaces:
# - Matching input column names to standard names (chr, pos, pval, etc.)
# - Detecting quoted values and prefixes (e.g., "chr" prefix)
# - Handling encoded columns (multiple values in one column)
# - Setting up allele matching for genetic data
#
# The column_info is a data.table with columns:
#   - input_name: original column name from file
#   - standard_name: standardized name (chr, pos, pval, rsid, etc.)
#   - bash_index: awk column reference ($1, $2, etc.)
#   - quoted: TRUE if values are quoted
#   - prefix: detected prefix (e.g., "chr")
#   - encoded_names: list of names if column contains multiple encoded values
#   - delimiter: delimiter for encoded columns
#   - etc.

#' Build complete column info for a file interface
#'
#' Main orchestrator that detects and configures all column metadata by
#' calling sub-functions for each type of detection.
#'
#' @param finterface File interface object
#' @param standard_names_dt Data.table mapping input names to standard names
#' @param nrows_to_check Number of rows to sample for pattern detection
#'
#' @return data.table with complete column metadata
#' @keywords internal
get_column_info <- function(
  finterface,
  standard_names_dt = summary_stats_standard_names_dt,
  nrows_to_check = 500
) {
  # Create base column info from headers
  column_info <- get_base_column_info(
    finterface,
    standard_names_dt = standard_names_dt
  )

  # Sample data for pattern detection
  data_to_check <- head(finterface, nrows_to_check)

  # Apply detection functions in sequence
  column_info <- filter_regex_matches(column_info, data_to_check)
  add_quoted_column(column_info, finterface)
  add_prefix_column(column_info, data_to_check)
  add_chr_prefix(column_info, data_to_check)
  add_encoding_columns(column_info)
  add_allele_matching_to_column_info(column_info)
  column_info <- expand_encoded_columns(column_info)

  # Set final column name (prefer standard_name over input_name)
  column_info[
    ,
    name := data.table::fcoalesce(standard_name, input_name)
  ][]
}

#' Create base column info from file headers
#'
#' Initializes column_info data.table with input names, indices, and
#' joins with standard name mappings.
#'
#' @param finterface File interface object
#' @param standard_names_dt Data.table mapping input names to standard names
#'
#' @return data.table with base column metadata
#' @keywords internal
get_base_column_info <- function(
  finterface,
  standard_names_dt = summary_stats_standard_names_dt
) {
  # Create initial column info with input names and indices
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
  # Join with standard name mappings
  standard_names_dt[
    column_info,
    on = "input_name"
  ]
}

#' Filter columns by regex pattern matching
#'
#' For columns with multiple possible patterns (e.g., encoded columns),
#' checks which pattern matches the actual data values.
#'
#' @param column_info data.table with column metadata
#' @param data_to_check Sample data for pattern testing
#'
#' @return Updated column_info with correct pattern matches
#' @keywords internal
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
        # Check which regex patterns match all values in the column
        row_match <- .SD[
          check_single_column_regex(regex, data_to_check[[input_name]])
        ]
        if (nrow(row_match) == 0) {
          # No pattern matches - clear encoding info
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

#' Check if all column values match a regex pattern
#'
#' @param regexes Vector of regex patterns to test
#' @param column_data Vector of values from the column
#'
#' @return Logical vector: TRUE for patterns matching all values
#' @keywords internal
check_single_column_regex <- function(
  regexes,
  column_data
) {
  sapply(regexes, \(regex) {
    grepl(regex, column_data) |>
      all()
  })
}

#' Add quoted column indicator
#'
#' Detects whether column values are quoted (surrounded by double quotes).
#'
#' @param column_info data.table with column metadata (modified in place)
#' @param finterface File interface for reading data
#'
#' @return Invisible column_info (modified by reference)
#' @keywords internal
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

#' Detect which columns have quoted values
#'
#' Reads first row with quote="" to preserve quotes, then checks each
#' column for presence of double quotes.
#'
#' @param finterface File interface object
#'
#' @return Named logical vector: TRUE for quoted columns
#' @keywords internal
are_values_quoted <- function(
  finterface
) {
  quoted_values <- head(finterface, nlines = 1, quote = "") |>
    sapply(stringr::str_detect, "\"")
  # Clean column names (remove any quotes from names)
  names(quoted_values) <- names(quoted_values) |>
    stringr::str_replace_all("\"", "")
  quoted_values
}

#' Add prefix detection to column info
#'
#' For columns with possible_prefixes, checks if all values have the prefix.
#'
#' @param column_info data.table with column metadata (modified in place)
#' @param data_to_check Sample data for prefix detection
#'
#' @return Invisible column_info (modified by reference)
#' @keywords internal
add_prefix_column <- function(
  column_info,
  data_to_check
) {
  column_info[
    ,
    prefix := character(0)
  ][
    !is.na(possible_prefixes),
    prefix := check_single_column_prefix(
      possible_prefixes,
      data_to_check[[input_name]]
    ),
    by = bash_index
  ][] |>
    invisible()
}

#' Check if all column values have a prefix
#'
#' @param prefixes Vector of possible prefixes to check
#' @param column_data Vector of values from the column
#'
#' @return First matching prefix, or NULL if none match
#' @keywords internal
check_single_column_prefix <- function(
  prefixes,
  column_data
) {
  prefixes <- sapply(prefixes, \(prefix) {
    grepl(paste0("^", prefix), column_data) |>
      all()
  })
  if (sum(prefixes) == 0) {
    return()
  }
  names(prefixes)[prefixes][1]
}

#' Add "chr" prefix marker for chromosome column
#'
#' If chromosome column exists but has no detected prefix, marks it to
#' have "chr" added during output (for standardization).
#'
#' @param column_info data.table with column metadata (modified in place)
#' @param data_to_check Sample data (unused, kept for interface consistency)
#'
#' @return column_info (modified by reference)
#' @keywords internal
add_chr_prefix <- function(
  column_info,
  data_to_check
) {
  # This function checks whether the "chr" prefix should be added
  # to the chr column during output
  column_info[
    standard_name == "chr" &
      is.na(prefix),
    add_prefix := "chr",
    by = prefix
  ][]
}


#' Set up encoding columns for split/recode operations
#'
#' For columns containing multiple delimited values, generates awk code
#' to split the column into an array and reassemble for output.
#'
#' @param column_info data.table with column metadata (modified in place)
#'
#' @return Invisible column_info (modified by reference)
#' @keywords internal
add_encoding_columns <- function(
  column_info
) {
  # This function creates the following columns in column_info:
  # - encoding_column: base awk code for encoding
  # - split_encoding_column: awk code to split delimited values
  # - recode_columns: awk code to recode split values
  # - encoded_column_index: unique index for encoded columns
  #
  # These columns are consumed by:
  # - awk_codegen.R: get_awk_column_arrays() uses encoding_column,
  #   split_encoding_column, recode_columns, add_prefix, bash_index,
  #   encoded_names
  # - awk_arrays.R: uses bash_index for column array setup
  #
  # If renaming these columns, update the consuming functions!

  # Assign unique indices to encoded columns
  column_info[
    !sapply(encoded_names, is.null),
    encoded_column_index := seq_len(.N)
  ][
    ,
    c("encoding_column", "split_encoding_column", "recode_columns") :=
      NA_character_
  ][]

  # Generate split/recode awk code for columns with delimiters
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
      # Recombine array elements with OFS
      recode_columns := sprintf(
        "%s = %s",
        bash_index,
        sprintf(
          "encoded%i[%i]",
          encoded_column_index,
          seq_along(encoded_names[[1]])
        ) |>
          paste(collapse = " OFS ")
      ),
      by = bash_index
    ][]
  }
  invisible(column_info)
}

#' Expand encoded columns into virtual columns
#'
#' Creates separate column_info rows for each value encoded in a single
#' column. For example, a column with "chr:pos" becomes two virtual
#' columns: one for chr, one for pos.
#'
#' @param column_info data.table with column metadata
#'
#' @return Expanded column_info with virtual columns added
#' @keywords internal
expand_encoded_columns <- function(
  column_info
) {
  # TODO Incomplete if only one of chr/pos missing?
  # Determine which columns need to be created from RSID encoding
  missing_rsid_columns <- c("chr", "pos")[
    !c("chr", "pos") %in%
      c(column_info$standard_name, unlist(column_info$encoded_names))
  ]

  column_info[
    ,
    {
      if (is.na(encoded_names)) {
        .SD
      } else if (!is.na(regex) & regex == "^(rs[0-9]+)$") {
        # RSID-only column: create virtual chr/pos columns
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
        # Regular encoded column: expand into virtual columns
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

#' Expand a single encoded column row
#'
#' Creates virtual column rows for each value in an encoded column.
#' Uses array syntax (encoded1[1], encoded1[2]) for bash_index.
#'
#' @param row_info Single-row data.table with encoded column info
#'
#' @return data.table with virtual column rows, or NULL if not encoded
#' @keywords internal
expand_single_encoded_row <- function(
  row_info
) {
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
          # Special case: alt column without encoding index
          stopifnot(row_info$standard_name == "alt")
          "nea"
        } else {
          # Normal case: array element references
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


#' Get column names from file interface
#'
#' @param finterface File interface object
#' @param original If TRUE, return original input names; else return
#'   standardized names
#'
#' @return Character vector of column names
#' @keywords internal
column_names <- function(
  finterface,
  original = FALSE
) {
  col_info <- finterface$column_info %||% get_column_info(finterface)
  if (original) {
    return(col_info[!is.na(input_name), input_name])
  }

  # Return standardized names, excluding encoded parent columns
  col_info[
    sapply(encoded_names, is.null),
    data.table::fcoalesce(standard_name, input_name)
  ]
}
