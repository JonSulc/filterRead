expect_canonical <- function(gregions) {
  rows <- data.table::data.table(
    chr = data.table::fifelse(
      is.na(gregions$chr), CHR_SENTINEL, gregions$chr
    ),
    start = data.table::fcoalesce(gregions$start, START_SENTINEL),
    end = data.table::fcoalesce(gregions$end, END_SENTINEL)
  )
  expect_equal(rows, rows[order(chr, start, end)])

  gaps <- rows[
    ,
    .(gap = if (1 < .N) start[-1] - cummax(end)[-.N] else 2L),
    by = chr
  ]
  expect_true(all(1 < gaps$gap))

  open <- rows[chr == CHR_SENTINEL]
  named <- rows[chr != CHR_SENTINEL]
  expect_false(
    any(
      outer(named$start, open$end, "<=") & outer(named$end, open$start, ">=")
    )
  )
}
