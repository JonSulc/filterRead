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
  if (!values_are_quoted) {
    return(dt)
  }
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
  if (length(x) == 1) {
    return(rep(x, size))
  }
  sample(x, size, replace = replace, ...)
}

dummy_summary_stats <- function(
  nrows = 500,
  chr = 1:22,
  start = 1,
  end = 12345,
  ref = c("A", "C", "G", "T"),
  alt = c("A", "C", "G", "T"),
  effect = NULL,
  pval = NULL,
  ref_col_names = summary_stats_column_names,
  random_names = TRUE,
  values_are_quoted = FALSE,
  prefixes = NULL,
  encode_columns = FALSE,
  rsids = FALSE,
  alleles_as_a1_a2_alt = FALSE
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

  if (isTRUE(encode_columns)) {
    dt <- encode_column(dt)
  } else if (!isFALSE(encode_columns)) {
    dt <- encode_column(dt, pattern = encode_columns)
  }

  if (alleles_as_a1_a2_alt) {
    dt <- switch_alt_ref_to_a1_a2(dt)
  }

  if (random_names) {
    set_random_names(dt, ref_col_names = ref_col_names)
  }

  add_quotes_to_dt(dt, values_are_quoted)

  dt
}

set_random_names <- function(
  dt,
  ref_col_names = summary_stats_standard_names_dt
) {
  data.table::setnames(
    dt,
    new = sapply(
      names(dt),
      \(cname) {
        if (!cname %in% ref_col_names$standard_name) {
          return(cname)
        }
        ref_col_names[
          cname,
          sample(input_name, 1),
          on = "standard_name"
        ]
      }
    )
  )
}

add_quotes <- function(something) {
  sprintf("\"%s\"", something)
}

local_csv_file <- function(
  filename = "data.csv",
  dt = dummy_dt(
    prefix = prefix,
    nrows = nrows
  ),
  ...,
  prefix = NULL,
  nrows = 6,
  env = parent.frame()
) {
  if (file.exists(filename)) stop("File ", filename, " already exists.")
  data.table::fwrite(dt, filename, ...)
  withr::defer(
    {
      file.remove(filename)
    },
    envir = env
  )
}

local_file_interface <- function(
  filename = "data.csv",
  ...,
  prefix = NULL,
  nrows = 6,
  env = parent.frame(),
  standard_names_dt = summary_stats_standard_names_dt,
  build = "b38"
) {
  local_csv_file(
    filename = filename,
    ...,
    prefix = prefix,
    nrows = nrows,
    env = env
  )

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
  new_file_interface(
    filename,
    standard_names_dt = standard_names_dt,
    build = build
  )
}

local_summary_stats <- function(
  filename = "data.csv",
  nrows = 500,
  chr = 1:22,
  start = 1,
  end = 12345,
  ref = c("A", "C", "G", "T"),
  alt = c("A", "C", "G", "T"),
  effect = NULL,
  pval = NULL,
  ref_col_names = summary_stats_column_names,
  random_names = TRUE,
  values_are_quoted = FALSE,
  prefixes = NULL,
  encode_columns = FALSE,
  env = parent.frame(),
  rsids = FALSE,
  alleles_as_a1_a2_alt = FALSE
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
    values_are_quoted = FALSE,
    prefixes = prefixes,
    encode_columns = encode_columns,
    rsids = rsids,
    alleles_as_a1_a2_alt = alleles_as_a1_a2_alt
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
  build = NULL,
  ...,
  env = parent.frame()
) {
  local_summary_stats(filename = filename, prefixes = prefixes, ..., env = env)
  new_file_interface(filename, build = build)
}

encode_column <- function(
  summary_stats,
  pattern = summary_stats_standard_names_dt[
    !sapply(delimiter, is.na)
  ][
    sample(.N, 1)
  ],
  drop_columns = pattern$encoded_names[[1]]
) {
  if (
    pattern[, "build" %in% encoded_names[[1]]] &
      !"build" %in% names(summary_stats)) {
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

dummy_rsid_summary_stats <- function(
  nrows = 500,
  # Default arguments provide 1473 SNPs
  chr = 1,
  start = 123,
  end = 1234567,
  columns_to_drop = c("chr", "pos"),
  random_names = FALSE,
  build = c("b37", "b38"),
  ...
) {
  build <- match.arg(build)
  rsids <- get_tabix_process_substitution(
    chr = chr,
    start = start,
    end = end,
    dbsnp_filename = get_dbsnp_filename(build = build, full_path = TRUE)
  ) |>
    sub("^<[(](.*)[)]$", "\\1", x = _) |>
    data.table::fread(cmd = _, select = 3, col.names = "rsid")
  while (nrow(rsids) < nrows) rsids <- rbind(rsids, rsids)

  dt <- dummy_summary_stats(nrows = nrows, random_names = FALSE, ...)[
    ,
    .(rsid = rsids[seq_len(nrows), rsid]) |>
      c(.SD)
  ]

  if (!is.null(columns_to_drop)) {
    dt <- dt[
      ,
      .SD,
      .SDcols = -columns_to_drop
    ]
  }

  if (random_names) {
    set_random_names(dt)
  }

  dt
}

local_rsid_summary_stats <- function(
  filename = "data.csv",
  values_are_quoted = FALSE,
  ...,
  env = parent.frame()
) {
  dummy_rsid_summary_stats(
    ...
  ) |>
    local_csv_file(
      filename = filename,
      dt = _,
      quote = values_are_quoted,
      env = env
    )
}

local_rsid_summary_stats_interface <- function(
  filename = "data.csv",
  build = NULL,
  ...,
  env = parent.frame()
) {
  local_rsid_summary_stats(filename = filename, ..., env = env)
  new_file_interface(filename, build = build)
}

local_file_with_comments <- function(
  filename,
  comment_lines = c("##comment1", "##comment2"),
  header_prefix = "#",
  content_dt = dummy_dt(),
  sep = "\t",
  env = parent.frame()
) {
  header <- paste0(header_prefix, paste(names(content_dt), collapse = sep))
  data_lines <- apply(content_dt, 1, paste, collapse = sep)

  uncompressed <- sub("[.]gz$", "", filename)
  writeLines(c(comment_lines, header, data_lines), uncompressed)

  if (grepl("[.]gz$", filename)) {
    R.utils::gzip(uncompressed, overwrite = TRUE)
  }

  withr::defer(file.remove(filename), envir = env)
}

switch_alt_ref_to_a1_a2 <- function(summary_stats) {
  is_alt_allele1 <- sample(c(TRUE, FALSE), nrow(summary_stats), replace = TRUE)

  summary_stats[, allele1 := data.table::fifelse(is_alt_allele1, alt, ref)]
  summary_stats[, allele2 := data.table::fifelse(is_alt_allele1, ref, alt)]

  summary_stats[, ref := NULL][]

  summary_stats
}

# Used to test %in% conditions
test_in_fc <- function(
  fcall,
  finterface,
  expected_condition,
  file_contents
) {
  awk_condition_list <- new_filter_condition(
    fcall,
    finterface = finterface
  ) |>
    eval_fcondition(finterface = finterface)
  expect_true(
    file.exists(awk_condition_list$additional_files)
  )
  expect_equal(
    readLines(awk_condition_list$additional_files),
    file_contents
  )
  filepath <- awk_condition_list$additional_files
  random_code <- sub(
    "^file",
    "",
    basename(awk_condition_list$additional_files)
  )
  expect_equal(
    awk_condition_list$condition,
    sprintf(
      expected_condition,
      random_code
    )
  )
  expect_equal(
    awk_condition_list$variable_arrays,
    paste0(
      "if (FILENAME == \"%s\") {\n",
      "  var%s[$0] = 1\n",
      "  next\n",
      "}"
    ) |>
      sprintf(filepath, random_code)
  )
}
