#' @import data.table

new_genomic_regions <- function(
  chr = character(),
  start = integer(),
  end = integer(),
  pos = NULL,
  build = NULL,
  merge_contiguous = TRUE
) {
  if (data.table::is.data.table(chr)) {
    return(
      as_genomic_regions(chr, build = build)
    )
  }
  pos_only <- FALSE
  if (!is.null(pos)) {
    if (all(is.na(start)) && all(is.na(end))) {
      start <- end <- pos
      pos_only <- TRUE
    } else {
      warning(
        "`pos` argument is superseded by `start` and/or",
        "`end` arguments, ignoring `pos`"
      )
    }
  }

  argument_lengths <- list(chr, start, end) |>
    sapply(length)
  nrows <- max(argument_lengths)

  if (!all(argument_lengths %in% c(0, 1, nrows))) {
    stop(
      "Invalid argument lengths. `chr`, `start`, `end`, and `pos`, ",
      "if provided, must have length 1 or the same length as the longest."
    )
  }
  if (any(argument_lengths == 0) && nrows != 0) {
    if (length(chr) == 0) chr <- NA_character_
    if (length(start) == 0) start <- NA_integer_
    if (length(end) == 0) end <- NA_integer_
  }

  genomic_regions_dt <- data.table::data.table(
    chr = chr,
    start = start,
    end = end
  ) |>
    as_genomic_regions(pos_only = pos_only, build = build)
  if (!merge_contiguous) {
    return(genomic_regions_dt)
  }
  merge_overlapping_regions(genomic_regions_dt)
}

as_genomic_regions <- function(
  genomic_regions_dt,
  pos_only = NULL,
  build = attr(genomic_regions_dt, "build")
) {
  stopifnot(is.data.frame(genomic_regions_dt))
  genomic_regions <- data.table::as.data.table(genomic_regions_dt) |>
    data.table::copy()
  validate_genomic_regions_dt(genomic_regions)
  set_positions_to_start_end(genomic_regions, pos_only)
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


#' @export
print.genomic_regions <- function(
  x
) {
  if (is.null(attr(x, "pos_only"))) {
    NextMethod()
  } else if (!attr(x, "pos_only")) {
    NextMethod()
  } else {
    x[
      ,
      .(chr = chr, pos = start)
    ] |>
      print()
  }
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

set_positions_to_start_end <- function(
  genomic_regions_dt,
  pos_only = NULL
) {
  if (!"chr" %in% colnames(genomic_regions_dt)) {
    genomic_regions_dt[
      ,
      chr := rep(NA_character_, .N)
    ][]
  }
  if (all(c("start", "end") %in% colnames(genomic_regions_dt))) {
    data.table::setattr(genomic_regions_dt, "pos_only", pos_only %||% FALSE)
    return(genomic_regions_dt)
  }
  if ("pos" %in% colnames(genomic_regions_dt)) {
    pos_only <- pos_only %||% TRUE
    genomic_regions_dt[
      ,
      start := ifelse(
        is.null(start),
        pos,
        data.table::fcoalesce(start, pos)
      )
    ][
      ,
      end := ifelse(
        is.null(end),
        pos,
        data.table::fcoalesce(end, pos)
      )
    ][]
  } else {
    pos_only <- pos_only %||% FALSE
    genomic_regions_dt[
      ,
      start := genomic_regions_dt$start %||% rep(NA_integer_, .N)
    ][
      ,
      end := genomic_regions_dt$end %||% rep(NA_integer_, .N)
    ][]
  }
  data.table::setattr(genomic_regions_dt, "pos_only", pos_only)
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
  if (nrow(x) == 0 || all(x[, is.na(chr)])) {
    return(x)
  }

  if (!is.character(x$chr)) {
    x[
      ,
      chr := as.character(chr)
    ][]
  }
  x[
    !grepl("^chr", chr),
    chr := paste0("chr", chr)
  ][]

  data.table::foverlaps(
    x,
    chain_dt
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
    data.table::setkey(chr, start, end)
}

#' @export
`[.genomic_regions` <- function(
  gregions,
  ...
) {
  gr_attributes <- attributes(gregions)
  NextMethod() |>
    data.table::setattr("pos_only", gr_attributes$pos_only) |>
    data.table::setattr("build", gr_attributes$build)
}

#' @export
`+.genomic_regions` <- function(
  e1,
  e2
) {
  rbind(e1, e2) |>
    merge_overlapping_regions()
}

#' @export
rbind.genomic_regions <- function(
  ...
) {
  dots <- list(...)
  # Check that attributes are the same
  all_pos_only <- dots |>
    lapply(\(gregion) attr(gregion, "pos_only"))
  if (length(unique(all_pos_only)) != 1) {
    warning(
      "genomic_regions have different `pos_only` attributes, ",
      "using first."
    )
  }
  dots |>
    data.table::rbindlist() |>
    as_genomic_regions(
      pos_only = all_pos_only[[1]],
      build = get_build(dots[[1]])
    )
}

# If considered as conditions, each row represents a possible region of interest
#' @export
`|.genomic_regions` <- `+.genomic_regions`

#' @export
`&.genomic_regions` <- function(
  e1,
  e2
) {
  if (1 < length(unique(c(e1$chr, e2$chr))) ||
    min(e1$end, e2$end) < max(e1$start, e2$start)) {
    return(e1[0])
  }
  rbind(e1, e2)[
    ,
    .(
      chr = chr[1],
      start = ifelse(all(is.na(start)),
        NA_real_,
        max(start, na.rm = TRUE) |>
          suppressWarnings()
      ),
      end = ifelse(all(is.na(end)),
        NA_integer_,
        min(end, na.rm = TRUE) |>
          suppressWarnings()
      )
    )
  ]
}

merge_overlapping_regions <- function(
  gregions
) {
  if (nrow(gregions) == 0) {
    return(gregions)
  }
  data.table::setkey(gregions, chr, start, end)
  gregions[
    ,
    c(
      .SD,
      # Groups of overlapping regions are obtained by counting (cumsum)
      # regions where the start is less than the end of the previous
      # region (shift(end))
      .(group = cumsum(
        cummax(shift(end, fill = 0)) < start
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
}
