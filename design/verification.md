# Verification: where each answer lives

This page is an index. The rationale lives in the header of each script, and is
not repeated here. Add a line when a new tool lands; do not summarize its header.
The one exception is the linter set, whose rationale is below because `.lintr`
cannot carry comments.

| Topic | Read |
|---|---|
| Gate modes (`--fast`, `--list`, `--verbose`, `--release`), `watch = <regex>`, and what the gate does not cover | `tools/verify.R` header |
| Which gates exist, and each gate's trigger and self-test | `tools/verify-manifest.yml` header |
| The pre-push skip for local-path mirrors; how pre-commit passes the remote (`PRE_COMMIT_REMOTE_*`, argc=0) | `tools/verify-on-push.sh` header |
| GitLab stages, the `workflow:` block (tighten it if the CI allowance runs out), and the hand-started pipeline hatch | `.gitlab-ci.yml` header |
| Running the CI jobs locally in the CI image; why `--all`; skipping a job whose CI secret is unset | `tools/local-ci.sh` header |
| Whether released dependencies can serve `DESCRIPTION` (needs the network; not in the gate) | `tools/dependency-resolvability-gate.R` header |
| `cran-comments.md`: offline half in the gate, `--online` half at release | `tools/cran-comments-gate.R` header |
| Mirror freshness, refresh, the frozen `refs/remotes/origin/*` | `design/backup-mirror.md`, `tools/mirror-freshness.sh`, `tools/mirror-refresh.sh` |
| A red gate on an untouched tree | `scripts/check-toolchain.R` |
| Generated-docs drift (`man/`, `NAMESPACE` against roxygen), why the `docs` stage checks the pushed commit in a throwaway export, and why CI runs it as its own `docs-drift` job | `scripts/check-docs-drift.R` header (the check), `stage_docs()` in `tools/verify.R` (the export), `docs-drift` in `.gitlab-ci.yml` (the CI job) |
| Lint deviations | [The linter set](#the-linter-set), below (`.lintr` cannot carry a header) |

Facts that no header records:

- The blocking-step count depends on the diff: about 36 on a clean `main`, about
  19 on a typical branch, because `[gate-self-tests]` runs only the self-tests
  for gate implementations the diff touched. A lower count is not evidence that
  a gate was bypassed.
- Before writing another pre-push predicate, probe which mechanism actually
  delivers the remote (positional or environment).
- Run `tools/dependency-resolvability-gate.R --all` and
  `tools/cran-comments-gate.R --online` before every release, and the
  resolvability gate again after changing any floor, `Remotes:` entry, or
  `dep::symbol` call site.
- Records under `design/` that cite `.github/workflows/` are history. The
  determinism cell matrix now lives in `tools/determinism/gha/`.
- rOpenSci pre-submission inquiry software-review #781 (opened 2026-06-26) is
  open. It is a scope-and-fit question, not an active review.

## The linter set

`.lintr` is intentionally aligned with the linter set `goodpractice::gp()` runs
(`goodpractice:::linters_to_lint()`), so a local `lintr::lint_package()`
surfaces the same findings as the goodpractice report used in review and CI.
Regenerate the list after a goodpractice upgrade, comparing against
`names(goodpractice:::linters_to_lint())`.

**Keep `.lintr` free of `#` comments.** It is parsed with `read.dcf()`, which
only learned to skip comment lines in R 4.6. On R 4.5 and older a single comment
makes `lint_package()` abort with `Invalid DCF format`, so this package got no
lint at all there; the rationale lives here instead. Keep it ASCII too: a
non-ASCII byte in `.lintr` comes back `bytes`-encoded on older R and makes any
config error surface as a confusing `sprintf()` failure instead of the real
message.

Documented deviations from the goodpractice set (test-idiom and public-API
reasons):

- `object_name_linter` / `object_usage_linter`: not part of the goodpractice set
  and deliberately NOT added. `canonical_join()`'s `data_A`/`col_A`/`suffix_A`/
  `name_A` (and `_B`) parameters are published API whose mixed-case suffixes
  would trip `object_name_linter`, as would the `._`-prefixed internal helpers
  and the testthat DSL; `object_usage_linter` misreads the `expect_*` DSL.
- `expect_identical_linter`: off. The suite relies on `expect_equal()`'s numeric
  tolerance (`expect_equal(nrow(x), 2)` compares integer vs double) and its
  string-encoding normalization for IDN output, both of which `identical()`
  rejects. A wholesale swap would mean retyping every literal for no behavioral
  gain.
- `implicit_assignment_linter`: off. Tests use the standard
  `expect_warning(res <- f(), "msg")` idiom to capture both the warning and the
  return value (`expect_warning()` returns the condition, not the value).
- `library_require_linter`: off. `tests/testthat.R` and the vignette setup chunk
  legitimately call `library()`.
- `undesirable_operator_linter`: configured to keep flagging `<<-`/`->>` but
  allow `:::`, which tests use to reach internal (unexported) functions.

`strings_as_factors_linter` is off, as in goodpractice, which dropped it in
1.2.0 (ropensci-review-tools/goodpractice#321). It only guarded the pre-R-4.0
`data.frame()` default, and this package Depends on R >= 4.0.0. The existing
`stringsAsFactors = FALSE` arguments stay: harmless here, and load-bearing on
`expand.grid()`.
