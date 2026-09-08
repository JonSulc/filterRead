# filterRead 2.16.0

* `as_genomic_regions()` no longer accepts a `filter_condition`. The regions a
  condition can match within are reported by `source_regions()`: the smallest
  set containing every row the condition can select. Non-genomic constraints
  are ignored, so the result covers more than the condition selects.

* Fixed a chromosome or position constraint being dropped when written beside
  a parenthesized OR. `pos <= 200 & (ref == "A" | alt == "G")` read every row
  matching the OR, ignoring the position. The same defect affected
  `chr == 1 & (...)` and a `genomic_regions` object combined with a
  parenthesized OR.

* Fixed a top-level OR dropping a branch's positional constraint, so that
  `(chr == 1 & pos <= 200 & ref == "A") | (chr == 2 & pos <= 200 & ref == "C")`
  did not read each branch under its own range.

* A parenthesized chromosome or position test no longer exhausts the C stack.
  `finterface[(chr == 1)]` failed with `C stack usage ... is too close to the
  limit`, and so did every condition containing one, among them
  `ref == "A" & (pos <= 200 | 400 <= pos)`, `(chr == 1) | (pval < 0.5)` and
  `pval < 0.05 & (chr == 1 | chr == 2)`. All now read.

* An OR whose branch repeats its partner's genomic constraint no longer fails
  with `no applicable method for 'post_process' applied to an object of class
  "name"`. `chr == 1 | chr == 1 & pval < 0.5` and
  `100 <= pos & pos <= 300 | 100 <= pos & pos <= 300 & pval < 0.5` now read.

* Fixed `source_regions()` reading only the regions recorded at the
  condition's outermost node.
  `(chr == 1 & ref == "A") | (chr == 2 & ref == "C")` reported the whole genome
  instead of chromosomes 1 and 2.

* A blank chromosome name is now read as missing rather than prefixed into
  `"chr"`, so a file whose chromosome field is empty on some rows loads those
  rows with `chr = NA`. Because a missing chromosome means "any chromosome",
  `genomic_regions()` now rejects a blank chromosome instead of silently
  widening the regions to the whole genome.

* Tables returned by `genomic_regions()` and `liftover()` now keep
  data.table's over-allocation, so a `:=` on one no longer warns `Invalid
  .internal.selfref detected and fixed by taking a (shallow) copy` and adds
  its column to a detached copy. Recording the result's build stripped the
  over-allocation, so the first modification by reference a caller made to a
  region set or a lifted table hit the warning.

* `liftover()` of an empty `genomic_regions` with a `NULL` target now returns
  it with its build intact, where it previously came back with the build
  stripped to `NULL`. A non-empty set already warned and kept its build.

* `clear_chain_cache()` now drops the chains held in the session alongside
  the chain RDS files on disk. Each chain is parsed once per session, so a
  chain file replaced on disk out of band is read only once the cache has
  been invalidated, which `clear_chain_cache()`, `set_chain_path()` and
  `setup_chain_files()` each do.

# filterRead 2.15.0

* The `merge_contiguous` argument of `genomic_regions()`,
  `as_genomic_regions()` and `rbind()` is now `normalize_regions`. It gates a
  superset of merging: the rows of a region set are also made pairwise
  disjoint.

* The `include` argument of `genomic_regions()` and `as_genomic_regions()` is
  now `complement`, and its sense is inverted: `include = TRUE` becomes
  `complement = FALSE`. `genomic_regions()` rejects `include` as an unused
  argument, but `as_genomic_regions()` absorbs it into `...` and ignores it,
  so `as_genomic_regions(dt, include = FALSE)` silently returns the regions
  themselves rather than their complement.

* Regions separated by an unconstrained position were merged into one.
  Chromosome 1 up to position 100, unioned with chromosome 1 from position
  200, returned the whole of chromosome 1, covering the positions between
  them that neither region held. Both regions are now kept.

* A region whose chromosome is unconstrained now absorbs the regions it
  covers. `genomic_regions(end = 200) | genomic_regions(chr = 1)` returns the
  two disjoint regions `*:1-200` and `chr1:201-`, where it previously returned
  `*:1-200` alongside the whole of chromosome 1.

* Negating a region whose chromosome is unconstrained enumerated the
  chromosomes, so the complement of the whole genome came back as the 22
  autosomes. It is now the empty set.

* A condition whose positional constraints cover the whole genome, such as
  `chr == 1 | 123 < pos | pos < 456`, read back only the rows on chromosome
  1, dropping rows that satisfied it. It now reads every row.

* Lifting a region set stored as its complement to another genome build
  inverted it, so `!genomic_regions(chr = 1)` in b37 arrived in b38 as
  chromosome 1 itself. It now arrives as the regions it excludes.

* A condition that no row can satisfy, such as `chr == 1 & chr == 2`, read an
  RSID-indexed file as though it carried no positional constraint, and the
  command `return_only_cmd = TRUE` emits for it did the same. Both now match
  no rows.

* A start position of 1 is now treated as unconstrained, so
  `genomic_regions(chr = 1, start = 1, end = 100)` and
  `genomic_regions(chr = 1, end = 100)` are equal. Positions are 1-based, so
  the two always denoted the same set. Negating a region with a lower bound
  now leaves the start open for the same reason, where it previously produced
  a region starting at position 1.

# filterRead 2.14.0

* `new_file_interface()`'s `ieugwas_parsing` argument is now
  `gwas_vcf_parsing`, a breaking change for callers that pass it.

* GWAS-VCF files are recognized by their structure — the nine standard VCF
  columns, a single sample column, and a FORMAT naming what that column
  holds — rather than by a lowercase pattern on the sample column name. A
  file whose study identifier carries an uppercase letter, such as
  `ukb-b-19953.vcf.gz` naming its sample `UKB-b-19953`, now decodes its
  `ES:SE:LP:AF:ID` column to `effect`, `effect_se`, `log10p` and `af` where
  it previously read back as a raw string. The `ID` sub-field decodes to
  `rsid`, which the file's own `ID` column already supplies, so the decoded
  copy is dropped and five keys yield four columns.

* Files whose FORMAT carries an unrecognized key now return the recognized
  statistics plus a column per unrecognized key, where they previously
  returned one packed string. A file whose FORMAT is `ES:SE:XX:LP` reads
  back `effect`, `effect_se`, `XX` and `log10p`.

* Files whose FORMAT carries only shared keys, such as `AF` or `AF:SS`, now
  open as plain VCFs where they previously decoded. A FORMAT must carry at
  least one indicator key — `ES`, `SE`, `LP`, `EZ`, `SI` or `NC` — for its
  sample column to be decoded, and a FORMAT naming `GT` is never decoded,
  which keeps a genotype VCF out even when it names one of those keys for
  an unrelated value.

* A single-sample VCF that is not a GWAS-VCF, such as a genotype VCF's
  `GT:AD:DP:GQ:PL`, opens as a plain VCF without a warning.
