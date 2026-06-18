# tests/testthat/helper-variants.R
# Attach a complete provenance snapshot for a build other than the current one:
# chr/pos/ref/alt plus variant_id, matching what record_build writes when that
# build is current. For mixed-build fixtures a single liftover cannot produce.
record_extra_build <- function(v, build, chr, pos, ref, alt) {
  snapshot <- data.table::data.table(chr = chr, pos = pos, ref = ref, alt = alt)
  add_variant_id(snapshot, build = build)
  for (col in c("chr", "pos", "ref", "alt", "variant_id")) {
    v[, (paste0(col, "_", build)) := snapshot[[col]]]
  }
  v
}
