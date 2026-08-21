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
