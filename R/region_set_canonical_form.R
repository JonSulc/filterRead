# The canonical form of a region_set
#
# Every object is canonicalized on construction, which fixes three things
# about its rules:
#
# - they are keyed on `(chr, start, end)`, and the rules on one chromosome
#   are pairwise disjoint and non-adjacent;
# - a chromosome carries a specific rule only where it differs from the
#   general rules, so no specific rule lies inside what a general rule
#   already selects;
# - a start is a position, which `validate_region_set()` enforces.
#
# The second is minimality, and with the first it makes the form unique:
# within one build and one stored form, what a set selects fixes its rules
# exactly. How many rules a set holds, and where each begins and ends, are
# therefore facts about the set and how it is stored — which is what lets
# `==` compare rule by rule, `compact()` choose the form with fewer rules,
# and the tabix builder emit one token per rule.

#' Reduce a region_set to its canonical form
#'
#' Merges the rules that overlap or abut, then trims each chromosome-specific
#' rule against the general rules. Two sets in one build and the same stored
#' form select the same positions if and only if their canonical rules are
#' identical.
#'
#' @param regions A region_set object
#' @return A region_set covering the same positions, in canonical form
#' @keywords internal
canonicalize_region_set <- function(
  regions
) {
  merge_rules(regions) |>
    trim_specific_rules()
}

#' Merge the rules that overlap or abut
#'
#' Combines rules on one chromosome whose positions overlap or are adjacent
#' (end + 1 == start) into single rules. A rule with an open end reaches the
#' end of its chromosome, so it merges with whatever it meets there.
#'
#' @param regions A region_set object
#' @return region_set with its overlapping rules merged
#' @keywords internal
merge_rules <- function(
  regions
) {
  if (nrow(regions) == 0) {
    return(regions)
  }
  replace_na_with_sentinel(regions) |>
    # The merging below requires sorted rules
    data.table::setkey(chr, start, end) |>
    _[
      ,
      c(
        .SD,
        # Where the largest end so far + 1 is less than the start, the rules
        # are disjoint and a new group begins; otherwise the rule meets or
        # overlaps that group and joins it, to be merged into one rule
        .(group = cumsum(
          cummax(data.table::shift(end + 1L, fill = 0L)) < start
        ))
      ),
      by = chr
    ][
      ,
      .(
        chr = chr,
        start = min(start),
        end = max(end)
      ),
      by = .(chr, group)
    ][
      ,
      .SD,
      .SDcols = -"group"
    ] |>
    replace_sentinel_with_na() |>
    as_region_set(
      build = build(regions),
      complement = is_complement(regions),
      canonicalize = FALSE
    )
}

#' Trim the chromosome-specific rules against the general rules
#'
#' A rule naming no chromosome applies on every chromosome, so a rule that
#' names one keeps only the positions the general rules leave. The general
#' rules are kept intact and the set selects the same positions; only its
#' rules change. Trimming can split a chromosome-specific rule, so the result
#' may carry more rules than the input.
#'
#' This is the minimality half of the canonical form: a chromosome carries a
#' specific rule only where it differs from the general rules.
#'
#' @param regions A region_set object with merged rules
#' @return A region_set selecting the same positions, with pairwise
#'   disjoint rules
#' @keywords internal
trim_specific_rules <- function(
  regions
) {
  if (!anyNA(regions$chr)) {
    return(regions)
  }
  rows <- data.table::data.table(
    chr = regions$chr,
    start = regions$start,
    end = regions$end
  )
  general_rules <- rows[is.na(chr)]
  list(
    general_rules,
    subtract_rule_set(rows[!is.na(chr)], general_rules)
  ) |>
    data.table::rbindlist() |>
    as_region_set(
      build = build(regions),
      complement = is_complement(regions),
      canonicalize = FALSE
    )
}

#' Subtract one set of rules from another
#'
#' Removes from `rules` the positions `to_remove` selects. A rule is kept
#' when `to_remove` does not reach it, dropped when `to_remove` selects all
#' of it, trimmed when the two overlap at one end, and split into two when a
#' rule of `to_remove` lands inside it. A rule of `to_remove` whose `chr` is
#' NA applies to every rule, and one whose `end` is NA reaches the end of the
#' chromosome. A start is always a position.
#'
#' Both arguments are plain rule tables of `chr`, `start` and `end`, read as
#' the positions they select — there is no complement bit — and the result is
#' one too, with its rules in no particular order.
#'
#' @param rules,to_remove data.tables of rules
#' @return The positions of `rules` that `to_remove` does not select
#' @keywords internal
subtract_rule_set <- function(
  rules,
  to_remove
) {
  if (nrow(rules) == 0L || nrow(to_remove) == 0L) {
    return(rules[, .(chr, start, end)])
  }
  remaining <- data.table::data.table(
    chr = rules$chr,
    start = rules$start,
    end = data.table::fcoalesce(rules$end, END_SENTINEL)
  )
  for (index in seq_len(nrow(to_remove))) {
    remaining <- subtract_rule(remaining, to_remove[index])
  }
  remaining[
    end == END_SENTINEL,
    end := NA_integer_
  ][]
}

#' Subtract one rule from a set of rules
#'
#' Every rule `to_remove` reaches contributes the part of itself before it
#' and the part after, and the empty ones are dropped. Rules `to_remove` does
#' not reach pass through untouched, so subtracting one rule at a time is
#' correct even when they overlap each other.
#'
#' @param rules Rules carrying the end sentinel in place of an NA end
#' @param to_remove A one-row data.table of `chr`, `start` and `end`, whose
#'   start is always a position and whose `chr` may be NA
#' @return The rules with the positions `to_remove` selects removed
#' @keywords internal
subtract_rule <- function(
  rules,
  to_remove
) {
  cut_start <- to_remove$start
  cut_end <- data.table::fcoalesce(to_remove$end, END_SENTINEL)
  reaches <- cut_start <= rules$end & rules$start <= cut_end
  if (is.na(to_remove$chr)) {
    carved <- reaches
  } else {
    if (any(reaches & is.na(rules$chr))) {
      stop(
        "Cannot subtract a named chromosome from a general rule without ",
        "enumerating the chromosomes."
      )
    }
    carved <- reaches & rules$chr == to_remove$chr
  }
  touched <- rules[carved]
  list(
    rules[!carved],
    touched[, .(chr, start, end = pmin(end, cut_start - 1L))][start <= end],
    touched[, .(chr, start = pmax(start, cut_end + 1L), end)][start <= end]
  ) |>
    data.table::rbindlist()
}
