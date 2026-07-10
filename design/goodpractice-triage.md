# goodpractice triage

This note records the standing triage for `goodpractice::gp()` output so the
same package-check findings do not have to be re-derived on every sweep.

Last reviewed: 2026-07-09, for RURL-xuurxymj under RURL-lyhcyvsa.

## Current decisions

- High cyclomatic complexity: fixed in this slice. `get_query()` was split into
  small internal helpers and now reports `cyclocomp::cyclocomp(get_query) == 3`;
  the `goodpractice` high-complexity bucket is absent.
- Long functions: accepted as scoped refactor debt, not a release blocker.
  Remaining entries are concentrated in parser, diagnostics, canonical join,
  and vectorized punycode/PSL paths where the code is already covered by focused
  regression tests. Split these opportunistically when touching the owning
  behavior; avoid mechanical extraction that hides the data flow.
- Internal functions defined but not called: mixed. Remove entries that are
  genuinely stale when found. Keep scalar compatibility and test seams such as
  the punycode wrappers and `._safe_parse_url_impl()` unless their tests and
  comments are retired at the same time.
- Coverage gaps: accepted as residual branch coverage debt while package
  coverage remains high. Prefer adding tests when a future change touches the
  listed branches, especially fallback/error branches that are currently marked
  as hard-to-trigger.
- Test `:::` usage: intentional for internal parser/phase characterization.
  These tests protect non-exported seams that are part of the package's
  architecture but not public API.
- `expect_equal()` usage: broad cleanup debt. Convert to `expect_identical()`
  only where exact type and attribute matching is intended; do not rewrite all
  cases mechanically.
- Duplicated parameter docs: cleanup debt. Prefer `@inheritParams` for new or
  touched roxygen blocks, but leave stable public docs alone unless regenerating
  documentation for another reason.
- Spelling findings: mostly domain, standards, URL, and changelog vocabulary.
  Review with `spelling::spell_check_package()` before release and add true
  project vocabulary to `inst/WORDLIST`; do not churn historical NEWS text just
  to silence the checker.

