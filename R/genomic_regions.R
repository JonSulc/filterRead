#' @import data.table

new_genomic_regions <- function(
  chr = character(),
  start = integer(),
  end = integer(),
  build = NULL,
  merge_contiguous = TRUE
) {
  if (data.table::is.data.table(chr)) {
    return(
      as_genomic_regions(chr, build = build)
    )
  }

  argument_lengths <- list(chr, start, end) |>
    sapply(length)
  nrows <- max(argument_lengths)

  if (!all(argument_lengths %in% c(0, 1, nrows))) {
    stop(
      "Invalid argument lengths. `chr`, `start`, and `end`, ",
      "if provided, must have length 1 or the same length as the longest."
    )
  }
  if (any(argument_lengths == 0) && nrows != 0) {
    if (length(chr) == 0) chr <- NA_character_
    if (length(start) == 0) start <- NA_integer_
    if (length(end) == 0) end <- NA_integer_
  }

  end_before_start <- end < start
  any_end_before_start <- end_before_start |>
    na.omit() |>
    any()
  if (any_end_before_start) {
    stop("start must be less than end.")
  }

  genomic_regions_dt <- data.table::data.table(
    chr = format_chr(chr),
    start = start,
    end = end
  ) |>
    as_genomic_regions(build = build)
  if (!merge_contiguous) {
    return(genomic_regions_dt)
  }
  merge_contiguous_regions(genomic_regions_dt)
}

as_genomic_regions <- function(
  genomic_regions_dt,
  build = attr(genomic_regions_dt, "build")
) {
  stopifnot(is.data.frame(genomic_regions_dt))
  genomic_regions <- data.table::as.data.table(genomic_regions_dt) |>
    data.table::copy()
  validate_genomic_regions_dt(genomic_regions)
  data.table::setattr(genomic_regions, "build", build)
  data.table::setattr(
    genomic_regions,
    "class",
    c("genomic_regions", class(genomic_regions)) |>
      unique()
  )
  data.table::setkey(genomic_regions, chr, start, end)
  genomic_regions
}

is_genomic_regions <- function(
  x
) {
  inherits(x, "genomic_regions")
}

format_chr <- function(
  chr
) {
  chr <- as.character(chr)
  data.table::fifelse(
    grepl("^chr", chr) | is.na(chr),
    chr,
    paste0("chr", chr)
  )
}


#' @export
print.genomic_regions <- function(
  x
) {
  NextMethod()
  if (!is.null(get_build(x))) {
    cat("Build:", get_build(x))
  }
}

validate_genomic_regions_dt <- function(
  genomic_regions_dt
) {
  if (!data.table::is.data.table(genomic_regions_dt)) {
    stop(
      "Object does not inherit the data.table class.\n",
      "Actual class: ", class(genomic_regions_dt)
    )
  }
  invisible(genomic_regions_dt)
}

#' @export
liftover.genomic_regions <- function(
  x,
  target,
  ...
) {
  if (data.table::is.data.table(target)) {
    chain_dt <- target
  } else {
    if (is.null(get_build(x))) {
      warning(
        "No build configured for genomic_regions, ",
        "returning original object"
      )
      return(x)
    }
    if (get_build(x) == target) {
      return(x)
    }
    chain_dt <- get_chain_dt(
      from = get_build(x),
      to = target
    )
  }
  if (nrow(x) == 0) {
    return(x)
  }

  expanded_x <- data.table::copy(x)

  if (!is.character(expanded_x$chr)) {
    expanded_x[
      ,
      chr := as.character(chr)
    ][]
  }

  expanded_x <- expanded_x[
    ,
    c(
      .SD,
      list(chr_is_na = is.na(chr))
    )
  ][
    ,
    {
      if (all(is.na(chr))) {
        expanded_x[
          is.na(chr),
          c(
            .(
              chr = unique(chain_dt$chr)
            ),
            .SD
          ),
          .SD = -"chr"
        ]
      } else {
        .SD
      }
    },
    by = chr_is_na
  ][, -"chr_is_na"]
  expanded_x[
    !grepl("^chr", chr),
    chr := paste0("chr", chr)
  ][]

  data.table::foverlaps(
    replace_na_with_sentinel(
      expanded_x,
      start_sentinel = 1L
    ),
    chain_dt,
    nomatch = 0
  )[
    ,
    .(
      chr = new_chr,
      start = start,
      end = end,
      offset = offset,
      rev = rev,
      i.start = pmax(start, i.start),
      i.end = pmin(end, i.end)
    )
  ][
    ,
    .(
      chr = chr,
      start = data.table::fifelse(
        rev,
        end - offset - (i.end - start),
        i.start - offset
      ),
      end = data.table::fifelse(
        rev,
        end - offset - (i.start - start),
        i.end - offset
      )
    )
  ] |>
    as_genomic_regions(build = get_build(target))
}

#' @export
`[.genomic_regions` <- function(
  gregions,
  ...
) {
  gr_attributes <- attributes(gregions)
  NextMethod() |>
    data.table::setattr("build", gr_attributes$build)
}

#' @export
`+.genomic_regions` <- function(
  e1,
  e2
) {
  rbind(e1, e2) |>
    merge_contiguous_regions()
}

#' @export
rbind.genomic_regions <- function(
  ...
) {
  dots <- list(...)
  # Check if all inputs have the required columns
  # (waldo may pass modified objects without these columns)
  required_cols <- c("chr", "start", "end")
  has_required_cols <- vapply(
    dots,
    function(x) all(required_cols %in% names(x)),
    logical(1)
  )
  if (!all(has_required_cols)) {
    # Fall back to simple rbindlist for waldo (testthat) comparison
    return(data.table::rbindlist(dots))
  }
  dots |>
    data.table::rbindlist() |>
    as_genomic_regions(
      build = get_build(dots[[1]])
    )
}

# If considered as conditions, each row represents a possible region of interest
# OR should combine them to contain all genomic regions
#' @export
`|.genomic_regions` <- `+.genomic_regions`

# AND should find the intersections of all the regions
#' @export
`&.genomic_regions` <- function(
  e1,
  e2
) {
  if (!identical(get_build(e1), get_build(e2))) {
    warning(
      "The genomic_regions do not have the same build, ",
      "converting the second."
    )
    e2 <- liftover(e2, get_build(e1))
  }
  # Intersection with empty set = empty set
  if (nrow(e1) == 0 || nrow(e2) == 0) {
    return(new_genomic_regions(build = get_build(e1)))
  }

  # genomic_regions where is.na(chr) are to be combined with some
  # where !is.na(chr) have to be expanded to match everything instead
  if (chr_has_mixed_na(e1)) {
    return(
      (e1[is.na(chr)] & e2) + (e1[!is.na(chr)] & e2)
    )
  }
  if (chr_has_mixed_na(e2)) {
    return(
      (e1 & e2[is.na(chr)]) + (e1 & e2[!is.na(chr)])
    )
  }

  e1_safe <- data.table::copy(e1)
  e2_safe <- data.table::copy(e2)
  if (is.na(e1_safe$chr[1]) != is.na(e2_safe$chr[1])) {
    if (is.na(e1_safe$chr[1])) {
      e1_safe <- expand_na_chr(e1_safe, e2_safe)
    } else {
      e2_safe <- expand_na_chr(e2_safe, e1_safe)
    }
  }
  # NAs are not handled by foverlaps, need to replace with sentinel values and
  # restore the NAs after foverlaps
  e1_safe <- replace_na_with_sentinel(e1_safe)
  e2_safe <- replace_na_with_sentinel(e2_safe)
  data.table::foverlaps(e1_safe, e2_safe, nomatch = 0)[
    ,
    c(
      .SD,
      list(
        start = pmax(start, i.start, na.rm = TRUE),
        end = pmin(end, i.end, na.rm = TRUE)
      )
    ),
    .SDcols = -c("start", "end", "i.start", "i.end")
  ] |>
    replace_sentinel_with_na() |>
    as_genomic_regions(build = get_build(e1_safe))
}

chr_has_mixed_na <- function(
  gregions
) {
  length(unique(is.na(gregions$chr))) == 2
}

expand_na_chr <- function(
  gregions1,
  gregions2
) {
  stopifnot(get_build(gregions1) == get_build(gregions2))
  stopifnot(all(is.na(gregions1$chr)))
  gregions1[
    ,
    .(
      chr = unique(gregions2$chr)
    ) |>
      c(.SD),
    .SDcols = -"chr",
    by = .N
  ] |>
    as_genomic_regions(build = get_build(gregions1))
}

replace_na_with_sentinel <- function(
  genomic_regions,
  chr_sentinel = "",
  start_sentinel = 0L,
  # Need to subtract 1 because foverlaps adds 1
  end_sentinel = .Machine$integer.max - 1L
) {
  data.table::copy(genomic_regions)[
    is.na(chr),
    chr := chr_sentinel
  ][
    is.na(start),
    start := start_sentinel
  ][
    is.na(end),
    end := end_sentinel
  ][] |>
    data.table::setkey("chr", "start", "end")
}
replace_sentinel_with_na <- function(
  genomic_regions,
  chr_sentinel = "",
  start_sentinel = 0L,
  # Need to subtract 1 because foverlaps adds 1
  end_sentinel = .Machine$integer.max - 1L
) {
  data.table::copy(genomic_regions)[
    chr == chr_sentinel,
    chr := NA_character_
  ][
    start == start_sentinel,
    start := NA_integer_
  ][
    end == end_sentinel,
    end := NA_integer_
  ][] |>
    data.table::setkey("chr", "start", "end")
}

merge_contiguous_regions <- function(
  gregions
) {
  if (nrow(gregions) == 0) {
    return(gregions)
  }
  gregions <- data.table::copy(gregions)
  data.table::setkey(gregions, chr, start, end)
  gregions <- gregions[
    ,
    c(
      .SD,
      # Groups of overlapping regions are obtained by counting (cumsum)
      # regions where the start is less than the end of the previous
      # region + 1 (shift(end + 1))
      .(group = cumsum(
        cummax(data.table::shift(end + 1, fill = 0)) < start
      ))
    ),
    by = chr
  ][
    ,
    .(
      chr = chr,
      start = min(start),
      end = max(end)
    ),
    by = .(chr, group)
  ][
    ,
    .SD,
    .SDcols = -"group"
  ] |>
    data.table::setkey(chr, start, end)
  gregions
}
