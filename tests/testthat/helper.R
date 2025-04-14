dummy_dt <- function(
  nrows = 6,
  ...
) {
  data.table::data.table(
    char = letters[seq_len(nrows)],
    num  = seq_len(nrows)
  ) |>
    apply_formatting(...)
}

apply_formatting <- function(
  dt,
  values_are_quoted = FALSE,
  prefix = NULL
) {
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

# If `x` argument of sample is length 1 integer, will sample from 1:x instead
rsample <- function(
    x,
    size,
    replace = TRUE,
    ...
) {
  if (length(x) == 1) return(rep(x, size))
  sample(x, size, replace = replace, ...)
}

dummy_summary_stats <- function(
  nrows  = 500,
  chr    = 1:22,
  start  = 1,
  end    = 12345,
  ref    = c("A", "C", "G", "T"),
  alt    = c("A", "C", "G", "T"),
  effect = NULL,
  pval   = NULL,
  ref_col_names     = summary_stats_column_names,
  random_names      = TRUE,
  values_are_quoted = FALSE,
  prefix = NULL
) {
  if (is.null(pval)) pval <- runif(nrows)
  if (is.null(effect)) effect <- rnorm(nrows)
  dt <- data.table::data.table(
    chr = rsample(chr, nrows),
    pos = rsample(start:end, nrows, replace = FALSE),
    ref = sample(ref, nrows, replace = TRUE),
    pval = pval,
    effect = effect
  )[
    order(chr, pos)
  ]
  dt[
    ,
    alt := sample(setdiff(alt, ref), .N, replace = TRUE),
    by = ref
  ][]

  data.table::setcolorder(
    dt,
    c("chr", "pos", "ref", "alt", "effect", "pval")
  )

  dt <- apply_formatting(dt,
                         values_are_quoted = values_are_quoted,
                         prefix = prefix)

  if (random_names) {
    data.table::setnames(
      dt,
      new = sapply(
        names(dt),
        \(cname) sample(summary_stats_column_names[[cname]], 1)
      )
    )
  }

  dt
}

add_quotes <- function(something) {
  sprintf("\"%s\"", something)
}

local_csv_file <- function(
  filename = "data.csv",
  dt       = dummy_dt(prefix = prefix,
                      nrows  = nrows),
  ...,
  prefix   = NULL,
  nrows    = 6,
  env      = parent.frame()
) {
  if (file.exists(filename)) stop("File ", filename, " already exists.")
  data.table::fwrite(dt, filename, ...)
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

local_summary_stats <- function(
  filename = "data.csv",
  nrows  = 500,
  chr    = 1:22,
  start  = 1,
  end    = 12345,
  ref    = c("A", "C", "G", "T"),
  alt    = c("A", "C", "G", "T"),
  effect = NULL,
  pval   = NULL,
  ref_col_names     = summary_stats_column_names,
  random_names      = TRUE,
  values_are_quoted = FALSE,
  prefix = NULL,
  ...,
  env    = parent.frame()
) {
  dummy_summary_stats(
    nrows = nrows,
    chr = chr,
    start = start,
    end = end,
    ref = ref,
    alt = alt,
    effect = effect,
    pval = pval,
    ref_col_names = ref_col_names,
    random_names = random_names,
    values_are_quoted = values_are_quoted,
    prefix = prefix
  ) |>
    local_csv_file(
      filename = filename,
      dt = _,
      env = env
    )
}

local_summary_stats_interface <- function(
  filename = "data.csv",
  ...,
  column_names = summary_stats_column_names,
  env = parent.frame()
) {
  local_summary_stats(filename = filename, ..., env = env)
  new_file_interface(filename, column_names = column_names)
}
