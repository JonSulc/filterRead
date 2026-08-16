# filterRead

filterRead loads and filters large tab/comma-separated genomic files by
translating R-style conditions into awk, reading a file by scan or by indexed
read. This glossary fixes the vocabulary of the filtering interface.

## Language

**Positional filtering**:
Selecting file rows whose `(chr, pos)` fall within a set of coordinate ranges,
with no allele constraint.
_Avoid_: region filtering, range query

**Scan**:
Reading every row of a file and deciding each one against the condition. The
way any file can be read, and the only way an unindexed one can.
_Avoid_: full read, sequential read

**Indexed read**:
Reading only the parts of a file its index reports as able to match, in place
of a scan. Returns what a scan returns; it selects no rows itself.
_Avoid_: tabix query, index path, seek

**Source regions**:
The region set covering everything a condition can select, used to narrow what
an indexed read reads. Never decides which rows match — that stays with the
condition — so covering too much only costs speed, while covering too little
loses rows.
_Avoid_: query regions, filter regions

**Reference lookup**:
Querying an auxiliary indexed file, such as dbSNP, for information the file
being read does not carry — the positions of its rsids. Adds information,
where an indexed read only restricts what is read.
_Avoid_: tabix query, dbSNP query

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
