dummy_dt <- function(
  values_are_quoted = FALSE,
  prefix            = NULL,
  nrows             = 6,
  ...
) {
  dt <- data.table::data.table(
    char = letters[seq_len(nrows)],
    num  = seq_len(nrows)
  )
  purrr::walk(
    names(prefix),
    \(column_name) {
      dt[
        ,
        (column_name) := paste0(prefix[[column_name]], get(column_name))
      ][]
    }
  )
  if (!values_are_quoted) return(dt)
  dt[
    ,
    char := add_quotes(char)
  ][]
  data.table::setnames(
    dt,
    add_quotes(names(dt))
  )
  dt
}

add_quotes <- function(something) {
  sprintf("\"%s\"", something)
}

local_csv_file <- function(
  filename = "data.csv",
  ...,
  prefix   = NULL,
  nrows    = 6,
  env      = parent.frame()
) {
  if (file.exists(filename)) stop("File ", filename, " already exists.")
  dummy_dt(
    prefix = prefix,
    nrows  = 6
  ) |>
    data.table::fwrite(filename, ...)
  withr::defer(
    {file.remove(filename)},
    envir = env
  )
}

local_file_interface <- function(
  filename = "data.csv",
  ...,
  prefix   = NULL,
  nrows    = 6,
  env      = parent.frame()
) {
  local_csv_file(filename = filename,
                 ...,
                 prefix   = prefix,
                 nrows    = nrows,
                 env      = env)
  new_file_interface(filename)
}
