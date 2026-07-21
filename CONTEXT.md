# filterRead

filterRead loads and filters large tab/comma-separated genomic files by
translating R-style conditions into awk (with tabix for indexed range
queries). This glossary fixes the vocabulary of the filtering interface.

## Language

**Positional filtering**:
Selecting file rows whose `(chr, pos)` fall within a set of coordinate ranges,
with no allele constraint.
_Avoid_: region filtering, range query (reserve "tabix query" for the indexed mechanism)

**Variant-identity filtering**:
Selecting file rows matching given variants on `(chr, pos)` and the unordered
allele pair `{ref, alt}` — the same variant in either column order. Unlike
positional filtering, it constrains alleles.
_Avoid_: variant filtering (ambiguous — name which)

**Genomic regions**:
A set of `(chr, start, end)` coordinate ranges in one reference build; the unit
of positional filtering.
_Avoid_: intervals, ranges, loci

**Read schema**:
The columns a read returns, fixed by the file interface and the query together
rather than by how many rows matched.
_Avoid_: output columns, result shape

**Encoded column / decoded columns**:
An encoded column packs several values behind a delimiter, as `MarkerName`
holding `b37-c1:12345-A-G` does; its decoded columns are the components a read
splits it into (`build`, `chr`, `pos`).
_Avoid_: composite column, compound column, exploding

**Summary-statistic derivation**:
Computing a missing summary statistic (`effect`, `effect_se`, `pval`) from the
others — an analysis step applied to a loaded table, not part of reading.
_Avoid_: completing stats (name the function), imputation (means genotype
imputation)

**Reference build**:
The coordinate system (`b36`/`b37`/`b38`) a set of coordinates is expressed in.
Coordinates are comparable only within the same build.
_Avoid_: genome version, assembly (in prose)
