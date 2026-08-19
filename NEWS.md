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
