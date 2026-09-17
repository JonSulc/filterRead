# tests/testthat/helper-variant_set.R
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

make_dummy_variants <- function() {
  new_variant_set(
    data.table::data.table(
      chr = c("chr1", "chr1"), pos = c(100L, 200L),
      ref = c("A", "C"), alt = c("G", "T")
    ),
    build = "b38"
  )
}

keep_coordinates_only <- function(variants) {
  variants[, .SD, .SDcols = coordinate_columns()]
}

drop_build <- function(variants) {
  variants <- data.table::copy(variants)
  data.table::setattr(variants, "build", NULL)
  variants
}
