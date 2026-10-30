# AGENTS.md

`rurl`: R package that parses, normalizes, cleans and joins URLs to WHATWG / RFC 3986 profiles. Domain and public-suffix extraction is delegated to `pslr`.

- rurl executes the standards literally, and the default `url_standard` is `"whatwg"` (owner mandate; ADR 0020). A behavior the selected standard calls invalid, or one that names a different URL than that standard's processing of the same input, is a bug even when an ADR, a ruling or a test pins it: fix it, and its `NEWS.md` bullet cites the standard as a short clause, such as "(WHATWG URL §4.4)". Conformance is never an opt-in flag. Compatibility covers gratuitous API churn only, never output.
- `NEWS.md` lists only what a package user acts on or notices, each bullet one terse line (at most 32 words). Cite a governing standard as a short clause, end with at most one issue or MR reference. No housekeeping bullets (CI, gates, lint, agent files, metadata), no Internal section; the `news-terse` hook enforces it.
- Git follows the house `agent-workflow` skill. fp status changes stay decoupled from git (the `fp` skill's `references/decoupling.md`).
- Frozen by ADR; read the ADR before editing: punycode helpers (0002) and the PSL seam (0001) in `R/domain.R`, retained base-R string operations (0005), `safe_parse_url()` columns (0006).
- Push `backup` only as the owner, with `--no-verify` (`tools/mirror-refresh.sh` does, after a merge): the gate refuses a push to `main` that adds more than one merge, and every ref from before the fleet-ci migration. An agent asks first, as for any gate bypass.

## Verification

Gate: local
Verify: .fleet-ci/gate auto

The merge gate is fleet-ci's local gate, run in the fleet image (`fleet-r-idn2`) by the pre-push hook; install it once per clone with `.fleet-ci/pre-push --install`, and its settings are `.fleet-ci.yml`. A diff the classifier calls docs-only gets T0 (spelling, `pkgdown::check_pkgdown()`); any other gets T1: punycoder rebuilt with libidn2, the manifest gates (`tools/verify.R --gates`), docs drift, lintr, the URL check, the locale tests under `LC_ALL=C`, the source-tree tests and one `R CMD check`.
Branch pushes, merge requests and pushes to `main` create no pipeline. CI runs fleet-ci's template: T2 on the weekly `deep-check` schedule, T3 on a `RELEASE=1` web pipeline and on the `v*` tag (seor ADR 0012; design/ci.md).

Before changing parse behavior or a conformance test, read design/posture-card.md; rulings no gate derives are in design/work/url-v3/registers/rulings.md.
For parser internals, see ARCHITECTURE.md.
For workflow, see design/workflow.md; validation policy, design/validation.md; the CRAN release deltas, design/release-deltas.md.
For gates, lint deviations, the backup mirror and a red gate on an untouched tree, see design/verification.md.
For hook setup, reading a gate run, sibling-package tests and known false alarms, see design/agent-workflow.md.
For the design tree, instrument traps, oracle pins and fixtures, and the release order, see design/README.md.
