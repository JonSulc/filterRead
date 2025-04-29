#' @import data.table

single_encoded_column_awk <- "split(%s, encoded%s, \"%s\");\n"


encoded_column_awk_wrapper <- "BEGIN{OFS=FS} {
    %s
    if (%s) {
        print $0
    }
}"

awk_wrapper <- function(
  arrays,
  conditions
) {
  sprintf(
    "BEGIN{OFS=FS} {
        %s
        if (%s) {
            print $0
        }
    }",
    paste(arrays, collapse = "\n"),
    conditions
  )
}

awk_array <- function(
  values,
  array_name,
  delimiter = " "
) {
  sprintf(
    "split(%s, %s, \"%s\")",
    paste(values, collapse = delimiter),
    array_name,
    delimiter
  )
}
