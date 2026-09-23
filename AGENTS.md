# AGENTS.md

`rurl`: R package that parses, normalizes, cleans and joins URLs to WHATWG / RFC 3986 profiles. Domain and public-suffix extraction is delegated to `pslr`.

- Conformant behavior ships as the default, never as an opt-in compatibility flag. This is a standing owner mandate that ADRs 0007 and 0016 assume. A shipped behavior that a standard calls invalid is a bug: record the break in `NEWS.md` with the standard and clause, and check the reverse-dependency gates before moving a floor. CRAN backward compatibility covers gratuitous API churn only. The `url_standard = NULL` freeze is a compatibility promise, not a conformance claim.
- Delivery bar: `Rscript tools/verify.R` passes. `--fast` does not meet it, and neither does `devtools::test()`, which ignores `Collate:` and runs against the source tree. Judge a run by `0 failed` and the `VERDICT` line, not the step count. A passing step discards its output, so check for warnings with `--verbose`.
- The hooks are installed per clone: `pre-commit install --hook-type pre-push --hook-type post-merge`. The hook skips the gate when pushing to a local-path mirror. That skip is not a way to push unverified work.
- The gate list lives in `tools/verify-manifest.yml`. Moving or deleting that file breaks the gate.
- GitLab creates no pipeline for a branch push or an MR, by design. After every merge, run `tools/local-ci.sh --all origin/main`. Without `--all` there is no `R CMD check --as-cran`.
- A test that reaches into `pslr`, `punycoder` or `raddr` must hold against the released version that the `DESCRIPTION` floor admits. Check both the released export and the released capability. In skip guards, name the dev version, not the next release.
- The `backup` mirror: nothing is ever pruned or force-pushed to it, and `git fetch` is never run inside it.
- Locally, `pkgcheck()` reports "no CI". That is a false negative caused by a missing `GITHUB_PAT`; do not chase it.
- In fp, `done` often means closed as a record, not answered. Read closure comments, not status. An epic whose children are all `done` is a claim to verify.
- These areas are frozen by ADR; read the ADR before editing: the punycode helpers (0002) and the PSL seam (0001) in `R/domain.R`, the retained base-R string operations (0005), and the `safe_parse_url()` columns (0006).
- A red gate on an untouched tree: run `scripts/check-toolchain.R` first. If it passes, report the red gate with evidence instead of assuming your change caused it.
- `lintr::lint_package()` stays clean. Read the `.lintr` header before "fixing" a lint or adding a linter.

Before changing parse behavior or a conformance test, read design/posture-card.md. Rulings that no gate can derive are in design/work/url-v3/registers/rulings.md.
For parser internals, see ARCHITECTURE.md.
For workflow, validation policy and the CRAN release checklist, see CONTRIBUTING.md.
For gates, CI, the mirror and release-time checks, see design/verification.md.
For instrument traps and oracle pins and fixtures, see design/measurement-traps.md, design/oracle-pinning.md and design/oracle-fixtures.md.
For the design tree and the eight-package release order, see design/README.md and design/release-chain.md.
