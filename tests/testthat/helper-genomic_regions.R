format_sentinel_regions <- function(rows) {
  paste0(
    "(",
    ifelse(is.na(rows$chr), "*", rows$chr), ",",
    ifelse(rows$start == START_SENTINEL, "", rows$start), "-",
    ifelse(rows$end == END_SENTINEL, "", rows$end), ")",
    collapse = " "
  )
}

expect_canonical <- function(gregions) {
  rows <- data.table::data.table(
    chr = gregions$chr,
    start = data.table::fcoalesce(gregions$start, START_SENTINEL),
    end = data.table::fcoalesce(gregions$end, END_SENTINEL)
  )

  sorted <- data.table::copy(rows) |>
    data.table::setorder(chr, start, end)
  expect(
    identical(rows, sorted),
    sprintf(
      "Rows are not sorted.\n  got:    %s\n  sorted: %s",
      format_sentinel_regions(rows),
      format_sentinel_regions(sorted)
    )
  )

  gaps <- rows[
    ,
    .(gap = if (1 < .N) start[-1] - cummax(end)[-.N] else 2L),
    by = chr
  ]
  touching <- gaps[gap <= 1]
  expect(
    nrow(touching) == 0L,
    sprintf(
      "Rows on %s overlap or abut: %s",
      touching$chr[1],
      format_sentinel_regions(rows[chr %in% touching$chr[1]])
    )
  )

  open <- rows[is.na(chr)]
  named <- rows[!is.na(chr)]
  reached <- which(
    outer(named$start, open$end, "<=") & outer(named$end, open$start, ">="),
    arr.ind = TRUE
  )
  expect(
    nrow(reached) == 0L,
    # expect() evaluates the message eagerly, which errors on a 0-row matrix
    if (nrow(reached) != 0L) {
      sprintf(
        "%s reaches the named region %s",
        format_sentinel_regions(open[reached[1, "col"]]),
        format_sentinel_regions(named[reached[1, "row"]])
      )
    } else {
      ""
    }
  )
}
