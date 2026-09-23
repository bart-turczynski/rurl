# Verification: where each answer lives

This page is an index. The rationale lives in the header of each script, and is
not repeated here. Add a line when a new tool lands; do not summarize its header.

| Topic | Read |
|---|---|
| Gate modes (`--fast`, `--list`, `--verbose`, `--release`), `watch = <regex>`, and what the gate does not cover | `tools/verify.R` header |
| Which gates exist, and each gate's trigger and self-test | `tools/verify-manifest.yml` header |
| The pre-push skip for local-path mirrors; how pre-commit passes the remote (`PRE_COMMIT_REMOTE_*`, argc=0) | `tools/verify-on-push.sh` header |
| GitLab stages, the `workflow:` block (tighten it if the CI allowance runs out), and the hand-started pipeline hatch | `.gitlab-ci.yml` header |
| Running the CI jobs locally in the CI image; why `--all` | `tools/local-ci.sh` |
| Whether released dependencies can serve `DESCRIPTION` (needs the network; not in the gate) | `tools/dependency-resolvability-gate.R` header |
| `cran-comments.md`: offline half in the gate, `--online` half at release | `tools/cran-comments-gate.R` header |
| Mirror freshness, refresh, the frozen `refs/remotes/origin/*` | `design/backup-mirror.md`, `tools/mirror-freshness.sh`, `tools/mirror-refresh.sh` |
| A red gate on an untouched tree | `scripts/check-toolchain.R` |
| Lint deviations | `.lintr` header |

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
