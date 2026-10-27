# AGENTS.md

`rurl`: R package that parses, normalizes, cleans and joins URLs to WHATWG / RFC 3986 profiles. Domain and public-suffix extraction is delegated to `pslr`.

- rurl executes the standards literally, and the default `url_standard` is `"whatwg"` (owner mandate; ADR 0020). A behavior the selected standard calls invalid, or one that names a different URL than that standard's processing of the same input, is a bug even when an ADR, a ruling or a test pins it: fix it and cite the standard and clause in `NEWS.md`. Conformance is never an opt-in flag. Compatibility covers gratuitous API churn only, never output.
- GitLab runs no pipeline for branches or MRs. After each merge, run `tools/local-ci.sh --all origin/main`.
- Git follows the house `agent-workflow` skill. fp status changes stay decoupled from git (the `fp` skill's `references/decoupling.md`).
- Frozen by ADR; read the ADR before editing: punycode helpers (0002) and the PSL seam (0001) in `R/domain.R`, retained base-R string operations (0005), `safe_parse_url()` columns (0006).

Before changing parse behavior or a conformance test, read design/posture-card.md; rulings no gate derives are in design/work/url-v3/registers/rulings.md.
For parser internals, see ARCHITECTURE.md.
For workflow, see design/workflow.md; validation policy, design/validation.md; the CRAN release deltas, design/release-deltas.md.
For gates, lint deviations, the backup mirror and a red gate on an untouched tree, see design/verification.md.
For hook setup, reading a gate run, sibling-package tests and known false alarms, see design/agent-workflow.md.
For the design tree, instrument traps, oracle pins and fixtures, and the release order, see design/README.md.
