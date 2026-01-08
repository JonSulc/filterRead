#' @import data.table

new_genomic_regions <- function(
  chr = character(),
  start = integer(),
  end = integer(),
  pos = NULL,
  build = NULL
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
  )
  data.table::setattr(genomic_regions_dt, "pos_only", pos_only)
  data.table::setattr(genomic_regions_dt, "build", build)
  data.table::setattr(
    genomic_regions_dt,
    "class",
    c("genomic_regions", class(genomic_regions_dt))
  )
  genomic_regions_dt
}

as_genomic_regions <- function(
  genomic_regions_dt,
  build = attr(genomic_regions_dt, "build")
) {
  validate_genomic_regions_dt(genomic_regions_dt)
  set_positions_to_start_end(genomic_regions_dt)
  data.table::setattr(genomic_regions_dt, "build", build)
  data.table::setattr(
    genomic_regions_dt,
    "class",
    c("genomic_regions", class(genomic_regions_dt)) |>
      unique()
  )
  genomic_regions_dt
}

is.genomic_regions <- function(
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
  genomic_regions_dt
) {
  if (!"chr" %in% colnames(genomic_regions_dt)) {
    genomic_regions_dt[
      ,
      chr := rep(NA_character_, .N)
    ][]
  }
  if (all(c("start", "end") %in% colnames(genomic_regions_dt))) {
    return(genomic_regions_dt)
  }
  if ("pos" %in% colnames(genomic_regions_dt)) {
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
    genomic_regions_dt[
      ,
      start := genomic_regions_dt$start %||% rep(NA_integer_, .N)
    ][
      ,
      end := genomic_regions_dt$end %||% rep(NA_integer_, .N)
    ][]
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

  x[
    ,
    data.table::foverlaps(
      .SD,
      chain_dt
    )
  ][
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
  ]
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
