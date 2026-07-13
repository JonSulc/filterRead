# filterRead

filterRead loads and filters large tab/comma-separated genomic files by
translating R-style conditions into awk (with tabix for indexed range
queries). This glossary fixes the vocabulary of the filtering interface.

## Language

**Positional filtering**:
Selecting file rows whose `(chr, pos)` fall within a set of coordinate
ranges — no allele constraint. Served by `finterface[gregions]`.
_Avoid_: region filtering, range query (reserve "tabix query" for the indexed mechanism)

**Variant-identity filtering**:
Selecting file rows matching given variants on `(chr, pos)` and the
unordered allele pair `{ref, alt}` — the same variant in either column
order. Served by `finterface[variants]`. Not interchangeable with
positional filtering: a `variants` constrains alleles, a `genomic_regions`
does not.
_Avoid_: variant filtering (ambiguous — name which)

**Genomic regions**:
A set of `(chr, start, end)` coordinate ranges in one reference build; the
unit of positional filtering. Public constructor `genomic_regions()`. An
empty (0-row) included regions matches nothing.
_Avoid_: intervals, ranges, loci

**Reference build**:
The coordinate system (`b36`/`b37`/`b38`) a set of coordinates is expressed
in. Coordinates are comparable only within the same build; relating two
coordinate sets across builds requires liftover.
_Avoid_: genome version, assembly (in prose)
