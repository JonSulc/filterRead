---
status: accepted
---

# Reads return the file's columns; deriving summary statistics is the caller's step

A read returns the file's standardized columns and no computed statistics:
`complete_missing_stats()`, which fills in whichever of `effect`, `effect_se`,
`pval` is missing from the other two, is not invoked by `head.file_interface()`
or `[.file_interface`. Deriving on read made the return shape depend on a
statistical convention rather than on the file, and a derivation that has
already happened cannot be declined — the same reasoning as
[locate-only variant filtering](0001-variant-filtering-is-locate-only.md).

## Consequences

Decoding is unaffected: splitting an encoded `MarkerName` into `build`, `chr`,
`pos` surfaces values already in the file, whereas deriving would add values it
does not contain. A read's schema stops depending on which statistics a file
happens to carry, leaving it fixed by the file interface and the query alone.
Callers wanting completed statistics add an explicit step, as they already do
with `estimate_effect_from_zscore()`.
