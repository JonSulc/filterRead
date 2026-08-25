---
status: accepted
---

# An absent genomic constraint means the full genome

A `filter_condition` records the coordinate restriction it contributes as its
`genomic_constraint` attribute, and `NULL` there means the full genome. A
condition that restricts no coordinates therefore stores nothing rather than a
whole-genome region set, and building one allocates nothing: a full-genome
`genomic_regions` costs about 13.5 ms to construct, which an unfiltered read
used to pay for a set that narrowed nothing.

Five functions in `R/filter_condition_utils.R` are the whole of the boundary.
`and_constraints()` and `or_constraints()` combine two possibly-absent
constraints, `eq_constraints()` compares them, and `is_unconstrained()` and
`is_overconstrained()` test the two poles of coverage. Everything else reads a
constraint through the exported `source_regions()`, which is where an absent
constraint becomes a full-genome region set for callers outside the condition
layer.

The region layer is deliberately outside the convention. A `genomic_regions`
object is a set of coordinate ranges and `NULL` is not one, so widening
`is_full_genome()` to accept it would put absence into the wrong type, and
`is_empty_genome()` — whose entire body is `is_full_genome(!gregions)` — would
need a `NULL` case of its own for a value it can never be given.

## Consequences

Code that needs to know what a condition covers takes one of two paths: the
five boundary functions, which answer without allocating, or
`source_regions()`, which allocates and returns a region set that can be
subset and indexed.
