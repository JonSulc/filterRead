---
status: accepted
---

# Every concept is named for the layer it belongs to

A file holds records, a query is built from conditions, coordinates live in a
space of positions, intervals and regions, and a read emits clauses and region
arguments. Each noun names one thing in one layer, and no function name
carries words from two. `row` is retired as a domain word: it named a file row
in most of the glossary and a coordinate row in the rest, so the term that
should have distinguished them did the opposite. It survives only qualified by
the table it belongs to — a `column_info` row — which is the distinction it
failed to make.

The coordinate class is **`region_set`** — named for what it denotes, as
`data.frame` is, and singular so it reads the same whether one instance is in
play or several. Its rows are **rules**: a rule naming a chromosome selects
one region there, and a rule naming none selects one region on every
chromosome, a family it cannot enumerate.

The glossary in `CONTEXT.md` splits by audience. **User vocabulary** is what
the public API exposes, so changing it is a breaking change; **implementation
vocabulary** is what a maintainer meets, so changing it is a refactor. Mixing
them is how the glossary went stale into a user-facing contradiction.

## Consequences

Renaming a term in the user section is a release note; renaming one in the
implementation section is not. A concept that has no home in either layer has
no name yet, which is the signal to place it before writing the code.
