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
#   - dropped_prefix: the prefix a read drops from the values (e.g., "chr")
#   - encoded_names: list of names if column contains multiple encoded values
#   - delimiter: delimiter for encoded columns
#   - etc.

# Allele columns whose values are normalized to upper-case during read.
# Some upstream summary-statistics files store alleles as lowercase, and
# downstream allele harmonization compares case-sensitively, so the
# values are uppered the moment they leave the awk pipeline.
ALLELE_STANDARD_NAMES <- c("ref", "alt", "allele1", "allele2")

#' Test whether a column is a non-allele encoded column
#'
#' Identifies the encoded columns handled by the coordinate/build passthrough
#' and survivor logic: those with decoded names, none of which are alleles
#' (`chr:pos`, build-encoded). Allele encodings (`SNPAlleles`, and the `alt`
#' column repurposed for `allele1`/`allele2` matching) are excluded; they are
#' handled by the effect-allele machinery. This exclusion is provisional — the
#' gate is expected to be removed once allele-encoded redundancy is supported.
#'
#' The predicate keys only on the decoded names. The only encoded column that
#' ever carries its own `standard_name` is the repurposed `alt` (an allele
#' encoding already excluded here), so a `standard_name` test would be
#' redundant.
#'
#' @param encoded_names Character vector of decoded names, or NULL for a
#'   non-encoded column.
#' @return TRUE for a non-allele encoded column.
#' @keywords internal
is_non_allele_encoded_column <- function(encoded_names) {
  !is.null(encoded_names) &&
    !any(encoded_names %in% ALLELE_STANDARD_NAMES)
}

#' Awk references to an encoded column's decoded values
#'
#' A quoted parent is split on its quotes as well as its delimiter, so its
#' decoded values start at the second element. `quoted` shifts the references
#' onto them.
#'
#' @param encoded_column_index The parent's index among the encoded columns.
#' @param positions The decoded values' positions among the decoded names.
#' @param quoted TRUE where the parent's values are quoted in the file.
#'
#' @return Character vector of awk array references.
#' @keywords internal
encoded_array_refs <- function(
  encoded_column_index,
  positions,
  quoted = FALSE
) {
  sprintf(
    "encoded%i[%i]",
    encoded_column_index,
    positions + as.integer(isTRUE(quoted))
  )
}

#' Decoded names of an encoded column that a real column already supplies
#'
#' Returns the decoded names to drop for a non-allele encoded column (those a
#' real column provides); empty for any other column_info row.
#'
#' @param encoded_names Character vector of decoded names, or NULL.
#' @param real_standard_names Standard names supplied by real columns.
#' @return Character vector of redundant decoded names.
#' @keywords internal
redundant_decoded_names <- function(
  encoded_names,
  real_standard_names
) {
  if (is_non_allele_encoded_column(encoded_names)) {
    intersect(encoded_names, real_standard_names)
  } else {
    character(0)
  }
}

#' Build complete column info for a file interface
#'
#' Main orchestrator that detects and configures all column metadata by
#' calling sub-functions for each type of detection.
#'
#' @param finterface File interface object
#' @param standard_names_dt Data.table mapping input names to standard names
#' @param nrecords_to_check Number of records to sample for pattern detection
#'
#' @return data.table with complete column metadata
#' @keywords internal
get_column_info <- function(
  finterface,
  standard_names_dt = summary_stats_standard_names_dt,
  nrecords_to_check = 500
) {
  # Create base column info from headers
  column_info <- get_base_column_info(
    finterface,
    standard_names_dt = standard_names_dt
  )

  # Sample data for pattern detection
  data_to_check <- head(finterface, nrecords_to_check)

  column_info <- filter_regex_matches(column_info, data_to_check)
  add_quoted_column(column_info, finterface, nrecords_to_check)
  add_encoding_columns(column_info)
  add_effect_allele_to_encoded_pair(column_info)
  add_allele_matching_to_column_info(column_info)
  column_info <- expand_encoded_columns(column_info)
  # Prefix detection keys on the standard name, which a decoded column
  # carries only once the expansion above has given it a row.
  add_column_prefixes(
    column_info,
    data_to_check,
    standard_names_dt = standard_names_dt
  )
  add_column_classes(column_info, standard_names_dt = standard_names_dt)

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

#' Drop a column's encoding
#'
#' @param column_rows The `column_info` rows for one input name, one per
#'   candidate encoding pattern.
#' @return A single `column_info` row with the encoding fields
#'   (`pattern`, `regex`, `encoded_names`, `delimiter`) cleared.
#' @keywords internal
drop_encoding <- function(column_rows) {
  column_rows[1][
    ,
    c("pattern", "regex", "encoded_names", "delimiter") := .(
      NA_character_, NA_character_, list(c()), NA_character_
    )
  ][]
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
        if (length(data_to_check[[input_name]]) == 0L) {
          # No values to match a pattern against: decoding would be a
          # guess, so the column is read as the file holds it.
          drop_encoding(.SD)
        } else {
          # Check which regex patterns match all values in the column
          row_match <- .SD[
            resolve_single_column_regex(regex, data_to_check[[input_name]])
          ]
          if (nrow(row_match) == 0) {
            drop_encoding(.SD)
          } else if (1 < nrow(row_match)) {
            warning(
              "Column ", input_name, " matches multiple regex patterns:\n",
              paste(row_match$regex, collapse = "\n"), "\n\n",
              "Using first."
            )
            row_match[1]
          } else {
            row_match
          }
        }
      }
    },
    by = input_name
  ]
}

#' Resolve which regex patterns every column value matches
#'
#' @param regexes Vector of regex patterns to test
#' @param column_data Vector of values from the column
#'
#' @return Logical vector: TRUE for patterns matching all values
#' @keywords internal
resolve_single_column_regex <- function(
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
#' @param column_info data.table with column metadata (modified in place)
#' @param finterface File interface for reading data
#' @param nrecords_to_check Number of records sampled
#'
#' @return Invisible column_info (modified by reference)
#' @keywords internal
add_quoted_column <- function(
  column_info,
  finterface,
  nrecords_to_check = 500
) {
  column_info[
    ,
    quoted := are_values_quoted(finterface, nrecords_to_check)[input_name]
  ][] |>
    invisible()
}

#' Detect which columns have quoted values
#'
#' A column is quoted where every value it carries is. A value a record does
#' not carry is written unquoted, so it is read as evidence of neither.
#'
#' @param finterface File interface object
#' @param nrecords_to_check Number of records sampled
#'
#' @return Named logical vector: TRUE for quoted columns
#' @keywords internal
are_values_quoted <- function(
  finterface,
  nrecords_to_check = 500
) {
  quoted_values <- head(finterface, nlines = nrecords_to_check, quote = "") |>
    sapply(\(values) {
      carried <- values[!is.na(values) & nzchar(values)]
      length(carried) != 0L && all(stringr::str_detect(carried, "\""))
    })
  names(quoted_values) <- names(quoted_values) |>
    stringr::str_replace_all("\"", "")
  quoted_values
}

#' Add each column's prefix to column_info
#'
#' Attaches the prefixes a column's standard name declares its values may
#' carry, and records the one the sampled values carry as `dropped_prefix`. A
#' read drops what `dropped_prefix` names.
#'
#' A column whose values occupy neither a field nor an array element still
#' takes the declaration, but nothing is sampled for it, so it records no
#' dropped prefix.
#'
#' @param column_info data.table with column metadata (modified in place)
#' @param data_to_check Sample data for prefix detection
#' @param standard_names_dt Data.table mapping input names to standard names;
#'   the prefix declarations are read from it by standard name
#'
#' @return Invisible column_info (modified by reference)
#' @keywords internal
add_column_prefixes <- function(
  column_info,
  data_to_check,
  standard_names_dt = summary_stats_standard_names_dt
) {
  declared_prefixes <- unique(
    standard_names_dt[!is.na(standard_name)],
    by = "standard_name"
  )[
    ,
    .(standard_name, possible_prefixes)
  ]
  column_info[
    declared_prefixes,
    possible_prefixes := i.possible_prefixes,
    on = "standard_name"
  ][
    ,
    dropped_prefix := NA_character_
  ][
    # The chr and pos an rsid-indexed file draws from its index occupy no
    # field, so there is nothing to sample them from.
    lengths(possible_prefixes) != 0L & !is.na(bash_index),
    dropped_prefix := resolve_dropped_prefix(
      possible_prefixes[[1]],
      resolve_column_values(
        input_name,
        encoding_column,
        delimiter,
        decoded_index,
        data_to_check
      ),
      column = data.table::fcoalesce(standard_name, input_name),
      allow_inconsistent = isTRUE(standard_name == "chr")
    ),
    by = bash_index
  ][] |>
    invisible()
}

#' Add each column's declared class to column_info
#'
#' Sets `class` on every column whose standard name declares one, a decoded
#' column and a column drawn from an rsid index included.
#'
#' @param column_info data.table with column metadata (modified in place)
#' @param standard_names_dt Data.table mapping input names to standard names;
#'   the class declarations are read from it by standard name
#'
#' @return Invisible column_info (modified by reference)
#' @keywords internal
add_column_classes <- function(
  column_info,
  standard_names_dt = summary_stats_standard_names_dt
) {
  declared_classes <- standard_names_dt[
    ,
    c("standard_name", "class")
  ][
    !is.na(standard_name) & !is.na(class)
  ] |>
    unique(by = "standard_name")
  column_info[
    declared_classes,
    class := i.class,
    on = "standard_name"
  ][] |>
    invisible()
}

#' Resolve a column's values from the sampled records
#'
#' A column with an input name has a field of its own. A column without one is
#' decoded from its parent: the parent's values split on the parent's
#' delimiter, taken at the column's position among the decoded names. The
#' delimiter is a pattern, as awk's `split()` reads it.
#'
#' @param input_name The column's name in the file, or NA for a decoded column
#' @param encoding_column The parent column's input name
#' @param delimiter The parent column's delimiter
#' @param decoded_index The column's position among the decoded names
#' @param data_to_check Sample data, keyed on input name
#'
#' @return Character vector of the column's sampled values
#' @keywords internal
resolve_column_values <- function(
  input_name,
  encoding_column,
  delimiter,
  decoded_index,
  data_to_check
) {
  if (!is.na(input_name)) {
    return(as.character(data_to_check[[input_name]]))
  }
  strsplit(as.character(data_to_check[[encoding_column]]), delimiter) |>
    vapply(`[`, character(1), decoded_index)
}

#' Resolve the prefix a read drops from a column
#'
#' A column carries one of the prefixes its standard name declares, or none.
#' Carrying two of them errors, and so does carrying one inconsistently unless
#' `allow_inconsistent`.
#'
#' @param prefixes The prefixes the column's standard name declares.
#' @param values The column's sampled values.
#' @param column The column's name, for the messages.
#' @param allow_inconsistent TRUE to accept a column carrying the prefix on
#'   some values and not others.
#'
#' @return The prefix the values carry, or NA where they carry none.
#' @keywords internal
resolve_dropped_prefix <- function(
  prefixes,
  values,
  column,
  allow_inconsistent
) {
  carried <- Filter(
    \(prefix) any(startsWith(values, prefix), na.rm = TRUE),
    prefixes
  )
  if (length(carried) == 0L) {
    return(NA_character_)
  }
  if (1L < length(carried)) {
    stop(
      "Column ", column, " carries more than one declared prefix (",
      paste(encodeString(carried, quote = "\""), collapse = " and "),
      "), so a read has no single prefix to drop.",
      call. = FALSE
    )
  }
  if (!allow_inconsistent &&
    !all(startsWith(values, carried), na.rm = TRUE)) {
    stop(
      "Column ", column, " carries the prefix ",
      encodeString(carried, quote = "\""),
      " on some values and not others, so a read would return it in two ",
      "spellings.",
      call. = FALSE
    )
  }
  carried
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
  # - encoded_refs: awk references to each child's decoded value
  #
  # These columns are consumed by:
  # - awk_codegen.R: get_awk_column_arrays() uses encoding_column,
  #   split_encoding_column, recode_columns, dropped_prefix, bash_index,
  #   encoded_names
  # - awk_arrays.R: uses bash_index for column array setup
  # - expand_single_encoded_row(): reads encoded_refs for child bash_index
  #
  # If renaming these columns, update the consuming functions!

  real_standard_names <- column_info[
    !is.na(input_name) & !is.na(standard_name),
    standard_name
  ]

  # Dropping the encoding of a column already covered by real columns lets it
  # pass through plainly, avoiding the per-record split.
  fully_redundant <- sapply(
    column_info$encoded_names,
    function(encoded) {
      is_non_allele_encoded_column(encoded) &&
        all(encoded %in% real_standard_names)
    }
  )
  if (any(fully_redundant)) {
    column_info[fully_redundant, delimiter := NA_character_][
      fully_redundant,
      encoded_names := list(list(NULL))
    ][]
  }

  # Assign unique indices to encoded columns
  column_info[
    !sapply(encoded_names, is.null),
    encoded_column_index := seq_len(.N)
  ][
    ,
    c("encoding_column", "split_encoding_column", "recode_columns") :=
      NA_character_
  ][]
  column_info[, encoded_refs := vector("list", .N)][]

  # Generate split/recode awk code for columns with delimiters
  if (any(!sapply(column_info$delimiter, is.na))) {
    column_info[
      !sapply(delimiter, is.na),
      split_encoding_column := sprintf(
        "split(%s, encoded%i, \"%s\")",
        bash_index,
        encoded_column_index,
        # Adding the column's quotes to the set of delimiters drops them at the
        # same time
        data.table::fifelse(
          !is.na(quoted) & quoted,
          paste0(delimiter, "|\\\""),
          delimiter
        )
      )
    ][
      !sapply(delimiter, is.na),
      # Keep the raw parent and append its decoded values
      recode_columns := {
        positions <- if (is_non_allele_encoded_column(encoded_names[[1]])) {
          which(!encoded_names[[1]] %in% real_standard_names)
        } else {
          seq_along(encoded_names[[1]])
        }
        sprintf(
          "%s = %s OFS %s",
          bash_index,
          bash_index,
          encoded_array_refs(encoded_column_index, positions, quoted) |>
            paste(collapse = " OFS ")
        )
      },
      by = bash_index
    ][
      !sapply(delimiter, is.na),
      encoded_refs := Map(
        encoded_array_refs,
        encoded_column_index,
        lapply(encoded_names, seq_along),
        quoted
      )
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

  real_standard_names <- column_info[
    !is.na(input_name) & !is.na(standard_name),
    standard_name
  ]

  # The chr/pos column_info rows synthesized below are the only output
  # columns a read cannot obtain from the file itself.
  column_info[, from_rsid_index := FALSE][, decoded_index := NA_integer_][]

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
            from_rsid_index       = TRUE,
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
          expand_single_encoded_row(
            .SD,
            exclude = redundant_decoded_names(
              encoded_names[[1]],
              real_standard_names
            )
          )
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
#' Reads each child's awk reference from the parent's `encoded_refs`.
#'
#' @param row_info Single-row data.table with encoded column info
#' @param exclude Decoded names whose virtual child rows to drop (those a real
#'   column already supplies).
#'
#' @return data.table with virtual column rows, or NULL if not encoded
#' @keywords internal
expand_single_encoded_row <- function(
  row_info,
  exclude = character(0)
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
      bash_index = encoded_refs[[1]],
      decoded_index = seq_along(encoded_names[[1]]),
      quoted = FALSE,
      from_rsid_index = FALSE,
      encoding_column = input_name,
      split_encoding_column = split_encoding_column,
      recode_columns = recode_columns
    )
  ][!exclude, on = "standard_name"]
}


#' Get column names from file interface
#'
#' @param finterface File interface object
#' @param raw If TRUE, return the file's header text rather than the
#'   standardized names.
#' @param add_rsid_genomic_columns If FALSE, drop the chr/pos columns an
#'   rsid-indexed file draws from the tabix index, leaving the columns the
#'   file itself carries.
#'
#' @return Character vector of column names
#' @keywords internal
column_names <- function(
  finterface,
  raw = FALSE,
  add_rsid_genomic_columns = TRUE
) {
  col_info <- finterface$column_info %||% get_column_info(finterface)
  if (!add_rsid_genomic_columns) {
    col_info <- col_info[!(from_rsid_index)]
  }
  if (raw) {
    return(col_info[!is.na(input_name), input_name])
  }

  # Return standardized names, preferring standard_name over input_name.
  # Every encoded parent is kept, so every column_info row adds a column.
  col_info[, data.table::fcoalesce(standard_name, input_name)]
}

#' Pull fread `colClasses` overrides from column_info
#'
#' Returns a list keyed by R class, with each entry holding the
#' positional indices of the output columns that should be read as
#' that class (e.g. `chr` and REGENIE `EXTRA` pinned to character). Uses
#' positions because fread parses `colClasses` against the input
#' stream's header, and awk strips the header before fread sees it.
#' Returns NULL when no overrides apply.
#'
#' @param finterface File interface object.
#' @param add_rsid_genomic_columns FALSE where the read leaves out the chr/pos
#'   columns an rsid-indexed file draws from the tabix index, so the positions
#'   count only the columns the file itself carries.
#' @return Named list of integer vectors, or NULL if no overrides
#'   apply.
#' @keywords internal
column_class_overrides <- function(
  finterface,
  add_rsid_genomic_columns = TRUE
) {
  col_info <- finterface$column_info
  if (is.null(col_info)) {
    return(NULL)
  }
  if (!add_rsid_genomic_columns) {
    col_info <- col_info[!(from_rsid_index)]
  }
  output_cols <- col_info[, "class"][, idx := seq_len(.N)][]
  if (all(is.na(output_cols$class))) {
    return(NULL)
  }
  matches <- output_cols[!is.na(class)]
  split(matches$idx, matches$class)
}
