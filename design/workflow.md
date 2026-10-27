# rurl workflow and constraints

Moved from `CONTRIBUTING.md` unchanged (SEOR-ogreubsr).

## Workflow

- Use an FP issue for an independently schedulable outcome, a distinct owner or
  blocker, or a separately deliverable change. Keep intra-slice steps and
  incidental findings in the owning issue unless they meet that threshold.
- Add or update tests for every user-visible change.
- Keep exported function signatures and object shapes stable unless the change
  is explicitly planned as breaking.
- Prefer small, reviewable patches over broad rewrites without coverage.
- Load repository documents selectively: start with the guidance and references
  relevant to the files being changed. Ordinary work does not require reading
  the complete `design/work/url-v3/` workspace.
- Tracker housekeeping, including reorganizing issues or updating their status,
  does not amend the product protocol. Change protocol records only when the
  product contract or its evidence actually changes.
- **Read the governing PRD or ADR before escalating a URL design question, and
  escalate only what they genuinely leave open.** The posture model, the
  scheme-inference seam, the scheme-overlay rule and the credential policy are
  all already settled in writing; re-deriving them from first principles
  manufactures decision forks the records have already closed.
- Escalate **behavior, public API and conformance posture** — nothing else.
  Cosmetic release hygiene, version strings and dev suffixes are decided locally
  and the rationale recorded on the ticket.

## Constraints

- Do not alter the Punycode helpers (`.normalize_and_punycode()`,
  `.punycode_to_unicode()`) or their hardcoded TLD workarounds; see
  [ADR 0002](adr/0002-keep-punycode-helpers.md). [AGENTS.md](../AGENTS.md) lists the
  protected areas. String operations run through the `R/strings-base.R`
  wrappers, or base R with its mode stated, and `stringi` only where
  `tools/stringi-call-allowlist.csv` names it; see
  [ADR 0019](adr/0019-string-operations-run-in-base-r.md), and
  [ADR 0005](adr/0005-intentional-base-r-string-exceptions.md) for what
  it keeps: the split contract, U+FEFF as data with its U+200B twin, the live
  guard table and the explicit-`last` `substring()` rule.
- Public Suffix List data, its parsing, and its refresh (`pslr::psl_refresh()`)
  live in the `pslr` package. `rurl` ships no PSL list of its own and queries
  `pslr` through the `R/domain.R` seam. The delegation contract is in
  [ARCHITECTURE.md](../ARCHITECTURE.md) and [ADR 0001](adr/0001-delegate-psl-to-pslr.md).
- `ACKNOWLEDGMENTS.md` is a **synced canonical file**, byte-identical across
  `rurl`, `pslr`, `punycoder`, `pagerankr` and `sitemapr`, and `.Rbuildignore`d
  in all five. Its only CRAN-visible surface is each package's intro vignette
  `## Acknowledgments` section plus a pkgdown navbar link. Any edit must be
  applied to all five repositories; verify with `md5 -q`.
