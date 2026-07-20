# Variant-identity filtering locates rows only; it does not harmonize alleles

`finterface[variants]` matches file rows on `(chr, pos)` and the unordered
allele pair `{ref, alt}`, so a variant matches a file row recorded in either
allele order. When a row matches in swapped order, it is returned in the
file's own allele order, unchanged, with no marker distinguishing it from a
same-order match. Harmonizing the returned rows — flipping effect signs or
allele frequencies for swap-matched rows, or reordering alleles to the
queried orientation — is deliberately left to the caller.

This keeps the filter a pure locator with the same "return file rows
verbatim" contract as positional filtering, and avoids baking one caller's
harmonization convention into the return shape. The cost is that a caller who
needs harmonized output must re-derive which rows were swap-matched by
comparing each returned row back to the queried `variants`.
