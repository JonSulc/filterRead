# filterRead

filterRead loads and filters large tab/comma-separated genomic files by
translating R-style conditions into awk, reading a file by scan or by indexed
read. This glossary fixes the vocabulary of the filtering interface. An awk
or shell term it does not list means what POSIX says it means.

Every noun below belongs to one layer. A **file** holds **records**; a
**query** is built from **conditions**; coordinates live in a **space** of
**positions**, **intervals** and **regions**, collected into a **region
set**; and a read **emits** awk **pattern-action pairs** and tabix **region
arguments**.

The coordinate nouns compose: an interval is a pair of positions, a region is
an interval on a chromosome, and a rule is a region whose chromosome may be
absent.

```
position                                one coordinate on a chromosome
interval    (start, end)                a run of positions, on no chromosome
region      (chr, start, end)           an interval on a chromosome
rule        (chr, start, end)           a region a set selects, or — where
                                        the chromosome is absent — one
                                        region per chromosome
```

Only a rule may leave the chromosome out, and one that does names a family of
regions it cannot enumerate. That is where the design lives.

A verb may be shared across layers where the meaning is the same: a condition
selects records, a region set selects positions.

## User vocabulary

What the public API exposes. Changing it is a breaking change.

**Positional filtering**:
Selecting records whose `(chr, pos)` fall within a region set, with no allele
constraint.
_Avoid_: region filtering, range query

**Scan**:
Reading every record of a file and deciding each one against the condition.
The way any file can be read, and the only way an unindexed one can.
_Avoid_: full read, sequential read

**Indexed read**:
Reading only the parts of a file its index reports as able to match, in place
of a scan. Returns what a scan returns; it selects no records itself.
_Avoid_: tabix query (names the mechanism, not the read), index path, seek

**Source regions**:
The region set covering everything a condition can select, used to narrow what
an indexed read reads. Never decides which records match — that stays with the
condition — so covering too much only costs speed, while covering too little
loses records. Follow from the genomic constraints of a condition and of every
condition it combines.
_Avoid_: query regions, filter regions, envelope

**Genomic constraint**:
The coordinate restriction one condition contributes on its own, beyond
whatever the conditions it combines contribute. Every condition carries one,
however deeply combined, and it is a condition's own — never a restatement of
what the conditions beneath it already say. Recording no constraint and
constraining nothing are the same state: neither narrows a read.
_Avoid_: positional constraint, node regions, attached regions

**Pure genomic condition**:
A condition that restricts records by coordinate and nothing else: whatever it
narrows, it narrows through its genomic constraint. `chr == 1` and
`chr == 1 | chr == 2` are pure; `chr == 1 & pval < 0.5` carries a genomic
constraint but is not pure. Purity is a property of what a condition selects,
so how it was written — parenthesized or not — does not bear on it. An
unconstrained condition is the limiting case, narrowing by nothing at all.
_Avoid_: empty condition (empty of expression, never of meaning)

**Unconstrained condition**:
A condition that selects every record: it carries no expression, and its
genomic constraint is a full region set. The limiting case of a pure genomic
condition, and what a read given no condition carries.
_Avoid_: empty condition, null condition, no condition

**Single genomic scope**:
What a condition has when one region set governs every record it can select, so
its source regions apply to the condition as a whole rather than separately to
each condition it combines. Combining with AND intersects constraints to one
scope; combining with OR need not, since each side may reach coordinates the
other excludes.
_Avoid_: AND block (a structural test for the property, not the property)

**Reference lookup**:
Querying an auxiliary indexed file, such as dbSNP, for information the file
being read does not carry — the positions of its rsids. Adds information,
where an indexed read only restricts what is read.
_Avoid_: tabix query (names the mechanism, not the lookup), dbSNP query

**Variant-identity filtering**:
Selecting records matching given variants on `(chr, pos)` and the unordered
allele pair `{ref, alt}` — the same variant in either column order. Unlike
positional filtering, it constrains alleles.
_Avoid_: variant filtering (ambiguous — name which)

**Region**:
A chromosome together with a start and an end. The unit of what a region set
selects. A region always names a chromosome; leaving it out gives a rule that
is not a region.
_Avoid_: interval (that is the chromosome-free pair), rule (that is what a
set stores, and its chromosome may be absent), range, locus

**Region set**:
What a read is narrowed to: the regions it selects, in one reference build. It
stores them as rules — defined under Implementation vocabulary — and a rule
naming no chromosome selects on every chromosome — `str()` prints `*` there —
alongside rules that name one, which state how those chromosomes differ. Its
regions therefore cannot be listed without a list of chromosomes, which is the
point.
_Avoid_: source regions (one particular region set — the one a condition can
select within — not a synonym for the concept)

**Chromosome**:
The value of a region's `chr` axis — the name of a sequence in the reference
build. Usually a chromosome; a reference may also hold scaffolds and alternate
sequences, which the package filters on identically and tells apart only when
spelling them.

The spelling is the package's, not the file's. A region set, a variant set and
a read all name chromosome 1 `chr1`, whatever spelling the file stores, so two
files differing only in how they write it read alike. `chr`-prefixed is the
default everywhere and the only spelling a read produces. The contract holds
over what detection observed — see **Chromosome prefix** — and within that,
where a path lets a file's spelling of a chromosome name reach a caller, that
path is defective.

The prefix reaches a chromosome name and not every value the axis holds. A
scaffold or alternate sequence is returned as the file spells it, because the
conventions naming those differ by more than a prefix and nothing here relates
them. `prefixable_regex` is what draws the line.
_Avoid_: contig (reserved for a reference's own sequence list), sequence,
locus

**Record**:
One row of the file being filtered. What a condition selects and a read
returns.
_Avoid_: row (says which table only by context), line, entry, variant (that is
a record's identity, not the record)

**Variant**:
A `(chr, pos, ref, alt)` identity, independent of any file. A record may carry
one; a record is not one.
_Avoid_: SNP (a variant need not be a substitution), marker, site (that is a
position)

**Variant set**:
The variants a read is matched against, in one reference build. What
`finterface[variants]` takes, and the unit of variant-identity filtering.
_Avoid_: variant list, variant table

**Open end**:
A region that reaches to the end of its chromosome, written `NA` on the end
axis. The package holds no chromosome lengths, so an end is the one bound that
cannot be written as a number; a start always can, the first position being 1.
_Avoid_: missing value (the end is not missing, it is open), blank

**Full region set / empty region set**:
The two extremes — every position of every chromosome, or no position at all.
Each has a spelling in both stored forms, and a full region set has a third — a
condition that records no genomic constraint at all.
_Avoid_: full genome, empty genome (what is full or empty is the set, not the
genome), empty regions, no regions

**Read schema**:
The columns a read returns, fixed by the file interface and the query together
rather than by how many records matched.
_Avoid_: output columns, result shape

**Encoded column / decoded columns**:
An encoded column packs several values behind a delimiter, as `MarkerName`
holding `b37-c1:12345-A-G` does; its decoded columns are the components a read
splits it into (`build`, `chr`, `pos`).
_Avoid_: composite column, compound column, exploding

**GWAS-VCF file**:
A single-sample VCF whose sample column is an encoded column holding one
variant's summary statistics, with the FORMAT field naming the values packed
into it. Recognized by that shape together with a FORMAT carrying at least one
indicator key, never by the sample column's name, which is an arbitrary study
identifier. FORMAT is taken as the file's own account of what its sample column
holds.
_Avoid_: IEUGWAS file, IEU file (the format is not tied to one producer)

**Indicator key**:
A FORMAT key marking the column as holding GWAS summary statistics (`ES`,
`SE`, `LP`, `EZ`, `SI`, `NC`), as against one a genotype VCF may also carry
(`AF`, `SS`, `ID`). One indicator key is what admits a file as a GWAS-VCF,
except that a FORMAT naming `GT` is rejected whatever else it carries: some
genotype callers name a field after an indicator key, as Illumina Pisces does
with `NC` for a no-call fraction and fgbio's `HapCutToVcf` with `SE` for
switch-error confidence. Producers other than IEU emit them, which is the
point — an indicator key reports what the column holds, not who wrote the
file.
_Avoid_: IEUGWAS-only key (they are not tied to one producer)

**Summary-statistic derivation**:
Computing a missing summary statistic (`effect`, `effect_se`, `pval`) from the
others — an analysis step applied to a loaded table, not part of reading.
_Avoid_: completing stats (name the function), imputation (means genotype
imputation)

**Reference build**:
The coordinate system (`b36`/`b37`/`b38`) a set of coordinates is expressed in.
Coordinates are comparable only within the same build.
_Avoid_: genome version, assembly (in prose)

**Chain**:
The block-by-block mapping carrying coordinates from one reference build to
another, in one direction. Directional: the mapping to a build and the mapping
back are different chains, and neither is the other's inverse.
_Avoid_: chain file (names the storage, not the mapping), liftover file

**Chain block**:
One run of positions a chain carries with a single offset and strand, so a
position's target coordinate follows from the block covering it. A block on the
reverse strand inverts the interval it carries, which is what makes an allele's
strand representation change across a lift.
_Avoid_: chain segment, alignment block, interval

**Chain gap**:
A stretch of the origin build no chain block covers. Positions there do not
lift, and are dropped or returned with absent coordinates.
_Avoid_: unmapped region, hole, missing block

**Lift**:
Re-expressing coordinates in another build by carrying each through the chain
block covering it. Not invertible: lifting to a build and back need not return
the coordinate started from.
_Avoid_: convert, translate, remap

**Origin build**:
The build a lift starts from, as against the target build it produces. Unrelated
to source regions, which name a read-narrowing envelope; the `source` argument
of a variants lift selects the origin build.
_Avoid_: source build (collides with source regions), from build

## Implementation vocabulary

What a maintainer meets. Changing it is a refactor.

**Rule**:
One row of a region set: `(chr, start, end)`, the chromosome optionally
absent. A chromosome-specific rule names exactly one region. A general rule
names one region per chromosome — a family it cannot enumerate, which is why
a region set cannot be expanded without a list of chromosomes.
_Avoid_: region (a region always names a chromosome, so no region is a
general rule), row, entry, record (that is a file row)

**General rule**:
A rule naming no chromosome. It applies on every chromosome, including those
that carry rules of their own, which state how they differ rather than what
replaces the general rules.
_Avoid_: default rule, global rule (both suggest the general rules stop where
a chromosome is named), wildcard (`str()` prints `*` for the chromosome, but a
general rule states what specific rules adjust, where a wildcard only
matches), generic (an S3 generic in R)

**Chromosome-specific rule**:
A rule naming one chromosome, stating how that chromosome differs from the
general rules. A chromosome carries one only where it differs, which is what
makes the canonical form minimal.
_Avoid_: override (suggests it replaces the general rules there), exception,
local rule

**Interval**:
A `(start, end)` pair with no chromosome. An **interval set** is the intervals
a set selects on one chromosome, sorted, pairwise disjoint and non-adjacent.
_Avoid_: span, range, region (that carries a chromosome)

**Canonical form**:
The rules a region set is reduced to on construction: within one build and one
stored form, what it selects fixes its rules exactly, so two such sets select
the same positions if and only if their rules are identical. Rules are keyed
on `(chr, start, end)`, pairwise disjoint on a chromosome, and a chromosome
carries a specific rule only where it differs from the general rules.
_Avoid_: normal form, normalized form (a normal form need not be unique to
what it represents, and uniqueness is the whole of this one), minimal form
(that is `compact()`; minimality is a property of the canonical form's rules,
not a name for the form)

**Complement bit**:
Whether a region set stores the positions it selects directly or stores their
complement. These are its two **stored forms**. A representation, not a
meaning: negating a set flips the bit and leaves the rules alone, which is what
keeps the complement of a chromosome independent of which chromosomes exist.
Two sets are comparable by their rules only when the bit agrees. `complement`
is also an argument of the exported `region_set()` and `as_region_set()`, so
renaming it is a breaking change.
_Avoid_: excluded regions, negative regions, blacklist

**End sentinel**:
`END_SENTINEL`, standing in for an open end where an operation needs a
concrete integer — `data.table::foverlaps()` rejects an `NA` in an interval
column. Substituted in before the operation and back out after.

**Canonicalize**:
Reduce a region set to its canonical form. The standing verb for the
operation, in prose and in `canonicalize_region_set()`. `canonicalize` is also
an argument of the exported `region_set()`, `as_region_set()` and
`rbind.region_set()`, so renaming it is a breaking change.

**Chromosome prefix**:
The `chr` a chromosome's name may carry — `chr1` against `1`. Four
`column_info` fields track it, and the first two are declarations keyed on the
standard name, which the package's own table makes for `chr` alone.
`possible_prefixes` is what a standard name's values may carry.
`prefixable_regex` is which of those values the prefix applies to, matched
against a value with any declared prefix removed; it matches values rather
than enumerating names, because the package holds no chromosome universe and
works whatever the organism. `CHR_PREFIXABLE_REGEX` is what `chr` declares
there and what `format_chr()` defaults to, while a declaration naming a prefix
and leaving `prefixable_regex` out is filled with `"."`, every value holding
at least one character. A caller's own standard-names table governs what a
column declares, but not the chromosome an rsid-indexed read draws from its
index: that value occupies no field, so the declaration it takes is never
read, and `CHR_PREFIXABLE_REGEX` is interpolated where the value is built.
`observed_prefixes` is the set the values `prefixable_regex` matches are
observed to carry — one prefix where they agree, `""` where none carries it,
both where they disagree, and empty where there is nothing to observe — and a
literal is rewritten into it on its way to awk, so a condition meets the
file's own spelling. `added_prefix` is what a read prepends where a value
carries none: it is what a read returns, where `observed_prefixes` is what the
file holds. What is observed is what a sample of the file showed, so a file
that changes spelling below the sample is not seen to.
_Avoid_: `header_prefix` and `print_prefix`, which name a header line's own
prefix and awk output text, neither of them a chromosome's spelling

**awk executable**:
The awk a read runs, held as the shell fragment that invokes it: usually a
bare name resolved on `PATH`, quoted on the way in where it holds a space,
and — where `set_awk()` is told not to validate it — anything the shell
accepts, `busybox awk` among them. `get_awk()` answers with the configured
one, else `mawk` where it is installed, else `awk`.
_Avoid_: program (POSIX's word for the instructions the package emits, its
awk program), command (the whole shell string a read runs, which the package
builds as `cmd`)

**Portable subset**:
The part of the awk language the package emits: only constructs that mawk,
gawk and any other awk executable handle the same way. Where the obvious
construct falls outside it and a portable alternative is emitted instead, a
comment in the code that emits it names the construct avoided and why.
_Avoid_: POSIX awk (the subset is set by what the executables actually do, not
by the standard)
