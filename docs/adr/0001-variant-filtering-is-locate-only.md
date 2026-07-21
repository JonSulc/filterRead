# Variant-identity filtering locates rows only; it does not harmonize alleles

`finterface[variants]` matches on `(chr, pos)` and the unordered allele pair
`{ref, alt}`, so a row recorded in swapped allele order still matches. Matched
rows are returned in the file's own allele order and unmarked: harmonizing them
— flipping effect signs or frequencies, reordering alleles to the queried
orientation — is left to the caller, which keeps the filter a pure locator
rather than baking one caller's convention into the return shape.

## Consequences

A caller needing harmonized output must re-derive which rows were swap-matched,
by comparing each returned row back to the queried `variants`.
