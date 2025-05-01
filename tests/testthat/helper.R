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
  prefixes = NULL
) {
  add_prefixes_to_dt(dt, prefixes)
  add_quotes_to_dt(dt, values_are_quoted)
  invisible(dt)
}

add_prefixes_to_dt <- function(
  dt,
  prefixes
) {
  purrr::walk(
    names(prefixes),
    \(column_name) {
      dt[
        ,
        (column_name) := paste0(prefixes[[column_name]], get(column_name))
      ][]
    }
  )
  invisible(dt)
}
add_quotes_to_dt <- function(
  dt,
  values_are_quoted
) {
  if (!values_are_quoted) return(dt)
  dt[
    ,
    names(.SD) := lapply(.SD, add_quotes),
    .SDcols = is.character
  ][]
  data.table::setnames(
    dt,
    add_quotes(names(dt))
  )
  invisible(dt)
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
  prefixes          = NULL,
  encode_columns    = FALSE
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

  add_prefixes_to_dt(dt, prefixes)

  if (encode_columns) {
    dt <- encode_column(dt)
  }

  if (random_names) {
    data.table::setnames(
      dt,
      new = sapply(
        names(dt),
        \(cname) {
          if (!cname %in% names(summary_stats_column_names)) return(cname)
          sample(summary_stats_column_names[[cname]], 1)
        }
      )
    )
  }

  add_quotes_to_dt(dt, values_are_quoted)

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
  filename          = "data.csv",
  ...,
  prefix            = NULL,
  nrows             = 6,
  env               = parent.frame(),
  standard_names_dt = summary_stats_standard_names_dt
) {
  local_csv_file(filename = filename,
                 ...,
                 prefix   = prefix,
                 nrows    = nrows,
                 env      = env)

  if (!is.null(prefix)) {
    standard_names_dt <- standard_names_dt[
      data.table::data.table(
        input_name        = names(prefix),
        standard_name     = names(prefix),
        possible_prefixes = unlist(prefix)
      ),
      on = c("input_name", "standard_name", "possible_prefixes")
    ]
  }
  new_file_interface(filename, standard_names_dt = standard_names_dt)
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
  prefixes          = NULL,
  encode_columns    = FALSE,
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
    ref_col_names     = ref_col_names,
    random_names      = random_names,
    values_are_quoted = FALSE,
    prefixes          = prefixes,
    encode_columns    = encode_columns
  ) |>
    local_csv_file(
      filename = filename,
      dt = _,
      quote = values_are_quoted,
      env = env
    )
}

local_summary_stats_interface <- function(
  filename = "data.csv",
  prefixes = NULL,
  ...,
  env = parent.frame()
) {
  local_summary_stats(filename = filename, prefixes = prefixes, ..., env = env)
  new_file_interface(filename)
}

encode_column <- function(
  summary_stats,
  pattern = summary_stats_standard_names_dt[!sapply(pattern, is.na)][sample(.N, 1)],
  drop_columns = pattern$encoded_names[[1]]
) {
  if (pattern[, "build" %in% encoded_names[[1]]] & !"build" %in% names(summary_stats)) {
    summary_stats <- summary_stats[, c(.(build = "b37"), .SD)]
    drop_columns <- c(drop_columns, "build")
  }

  summary_stats[
    ,
    .(do.call(
      sprintf,
      list(pattern$pattern) |>
        c(mget(pattern$encoded_names[[1]]))
    )) |>
      setNames(pattern$input_name) |>
      c(.SD),
    .SDcols = -drop_columns
  ]
}
