# ADR 0001: Delegate Public Suffix List matching to `pslr`

- **Status:** Accepted
- **Date:** 2026 (rurl 1.3.0)
- **Tracking:** pslr migration epic

## Context

`rurl` originally bundled a copy of the Public Suffix List (PSL) and its own
matcher for domain/TLD extraction. That meant rurl owned the list's freshness,
its parsing edge cases (wildcards, exceptions, IDN A-label vs Unicode), and a
periodic data refresh — none of which is rurl's core concern (URL
normalization). The sibling `pslr` package was built to own exactly this: the
list, its parsing, refresh (`pslr::psl_refresh()`), and a cached query API.

## Decision

Remove the embedded list and matcher. Extract domain/TLD via `pslr` through a
thin seam in `R/domain.R` (`.psl_registered_domain()`, `.psl_public_suffix()`,
`.psl_suffix_extract()`), with a fixed contract: `section` from `source`,
`output = "unicode"` by default, `unknown = "na"`, `invalid = "na"`, and never
`pslr::psl_use()` for per-request behavior. The emitted domain/TLD spelling is
chosen from `host_encoding` so `get_domain()`/`get_tld()` mirror `get_host()`.

## Consequences

- rurl ships no PSL data; freshness and PSL semantics are `pslr`'s
  responsibility.
- **Standing rule:** do not reintroduce an embedded matcher or bundled list;
  query `pslr` through the `R/domain.R` seam.
- Backward compatibility is a hard constraint (rurl and siblings are on CRAN):
  the `output = "unicode"` default preserves rurl's historical decoded-IDN
  domain/TLD output even though pslr itself defaults to ASCII A-labels.
