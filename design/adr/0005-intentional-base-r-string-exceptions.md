# ADR 0005: Intentional base-R string exceptions (the `stringi` migration is not total)

- **Status:** Accepted
- **Date:** 2026
- **Tracking:** n/a (recorded during the stringi migration)

## Context

rurl migrated most string manipulation from base R to `stringi` for Unicode
correctness. A blanket "finish the migration for consistency" would be wrong:
some base-R calls are retained because they are *not* behaviorally equivalent to
their `stringi` counterparts, and swapping them would silently change parsing.

## Decision

Keep the following base-R string calls deliberately:

- **`strsplit(..., fixed = TRUE)`** for host-label / subdomain splits on `"."`
  and the path splits in `path-query.R`. It is **not** equivalent to
  `stringi::stri_split_fixed()`: `strsplit("a.b.", ".", fixed = TRUE)` drops the
  trailing empty (`c("a","b")`) and returns `character(0)` for `""`, whereas
  `stri_split_fixed` keeps the trailing `""` and returns `""`. The dot splits
  use `fixed = TRUE` (verified identical to the regex `"\\."` for the
  trailing-empty behavior), which also keeps `fixed_regex_linter` quiet.
- **Genuine ASCII regexes** — `gsub("/+", …)`, `sub("/?[^/]*$", …)`, the
  `regexpr`/`regmatches` in `._remove_dot_segments`, `grepl("^www[0-9]*$", …)`,
  and the status-pattern `grepl` in `canonical_join.R` — operate on ASCII
  delimiters/anchors where `stringi` offers no correctness or Unicode benefit.

## Consequences

- **Standing rule:** do not "finish the migration" on these calls for
  consistency alone. A reviewer seeing a base-R string call in these locations
  should treat it as intentional.
- New string code should still prefer `stringi` unless it hits one of these
  documented equivalence gaps.
