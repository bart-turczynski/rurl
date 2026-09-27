# AGENTS.md

`rurl`: R package that parses, normalizes, cleans and joins URLs to WHATWG / RFC 3986 profiles. Domain and public-suffix extraction is delegated to `pslr`.

- Conformant behavior is the default, never an opt-in flag (owner mandate; ADRs 0007, 0016). A shipped behavior that a standard calls invalid is a bug: fix it and cite the standard and clause in `NEWS.md`. CRAN compatibility covers gratuitous API churn only.
- GitLab runs no pipeline for branches or MRs. After each merge, run `tools/local-ci.sh --all origin/main`.
- Git follows the house `agent-workflow` skill. fp status changes stay decoupled from git (the `fp` skill's `references/decoupling.md`).
- Frozen by ADR; read the ADR before editing: punycode helpers (0002) and the PSL seam (0001) in `R/domain.R`, retained base-R string operations (0005), `safe_parse_url()` columns (0006).

Before changing parse behavior or a conformance test, read design/posture-card.md; rulings no gate derives are in design/work/url-v3/registers/rulings.md.
For parser internals, see ARCHITECTURE.md.
For workflow, validation policy and the CRAN release checklist, see CONTRIBUTING.md.
For gates, lint deviations, the backup mirror and a red gate on an untouched tree, see design/verification.md.
For hook setup, reading a gate run, sibling-package tests and known false alarms, see design/agent-workflow.md.
For the design tree, instrument traps, oracle pins and fixtures, and the release order, see design/README.md.
