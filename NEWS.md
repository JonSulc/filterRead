# filterRead 2.13.2

* IEUGWAS files are recognized by their structure — the nine standard VCF
  columns, a single sample column, and a FORMAT string listing IEUGWAS keys —
  rather than by a lowercase pattern on the sample column name. A file whose
  GWAS identifier carries an uppercase letter, such as `ukb-b-19953.vcf.gz`
  naming its sample `UKB-b-19953`, now decodes its `ES:SE:LP:AF:ID` column to
  `effect`, `effect_se`, `log10p` and `af` where it previously read back as a
  raw string.

* A single-sample VCF whose FORMAT is not an IEUGWAS one, such as a genotype
  VCF's `GT:AD:DP:GQ:PL`, opens as a plain VCF without a warning.
