---
status: accepted
---

# The chromosome spelling is bare

A region set, a variant set and a read all name chromosome 1 `1`. A read drops
the `chr` a file's values carry rather than adding the one they lack, and
returns every other value as the file spells it.

Dropping a prefix asks whether a value begins with it, which is always
answerable. Adding one asks whether a value is a chromosome, which is not
without a list of sequence names the package refuses to hold while it works
whatever the organism. It guessed with a pattern — a name of at most four
characters free of `.` and `_` — and where the guess was wrong it was wrong
silently: a scaffold came back prefixed, and nothing in the returned data said
a name had been rewritten. Dropping needs no such judgment, so a scaffold, an
inconsistently prefixed file and a quoted chromosome all fall out correct. It
is also cheaper, because nothing need be emitted at all for the bare files
most summary statistics are.

## Consequences

Every caller reading `chr` sees bare names, and the one shape this can corrupt
is a sequence name beginning with `chr` that does not carry it as a prefix:
`chromosome1` reads back as `omosome1`. `chrUn_gl000220` is not an instance —
it reads back as `Un_gl000220`, the same removal applied consistently.
```
