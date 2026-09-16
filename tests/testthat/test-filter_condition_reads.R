test_that("awk returns the records R returns for every filter expression", {
  # Every filter expression is read through awk and evaluated in R over the
  # same records, so a constraint the generator drops shows up as a
  # record-set difference rather than needing an expected awk string per
  # expression.
  condition_fcalls <- list(
    quote(chr == "chr1" & pos <= 200),
    quote(chr == "chr1" | chr == "chr2"),
    quote(pos <= 200 | 400 <= pos),
    quote(ref == "A" & alt == "T"),
    quote(ref == "A" | alt == "G"),
    quote(chr == "chr1" & ref == "A"),
    quote(chr == "chr1" | ref == "A"),
    quote(pos <= 200 & (ref == "A" | alt == "G")),
    quote((ref == "A" | alt == "G") & pos <= 200),
    quote(chr == "chr1" & (ref == "A" | alt == "G")),
    quote(ref == "A" & (pos <= 200 | 400 <= pos)),
    quote((chr == "chr1" | chr == "chr2") & (pos <= 200 | 400 <= pos)),
    quote((chr == "chr1" & pos <= 200) | (chr == "chr2" & 300 <= pos)),
    quote(
      (chr == "chr1" & 150 <= pos & pos <= 350) |
        (chr == "chr2" & 250 <= pos & pos <= 450)
    ),
    quote((chr == "chr1" & ref == "A") | (chr == "chr2" & alt == "G")),
    quote((chr == "chr1") | (pval < 0.5)),
    quote(pval < 0.05 & (chr == "chr1" | chr == "chr2")),
    quote(chr == "chr1" & ref == "A" | chr == "chr2" & alt == "G"),
    quote(
      (chr == "chr1" & (pos <= 200 | 400 <= pos)) | (chr == "chr2" & ref == "C")
    ),
    quote(((chr == "chr1" | chr == "chr2") & pos <= 200) & ref == "A"),
    quote((pval < 0.5 | ref == "A") & chr == "chr1" & pos <= 300),
    quote(chr == "chr1" | chr == "chr1" & pval < 0.5),
    quote(100 <= pos & pos <= 300 | 100 <= pos & pos <= 300 & pval < 0.5)
  )
  finterface <- local_file_interface(dt = dummy_fixed_summary_stats())
  for (fcall in condition_fcalls) {
    expect_read_matches_r(finterface, fcall)
  }
})
