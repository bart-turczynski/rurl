# Contributing

Report bugs and request features in the GitLab issue tracker:
<https://gitlab.com/bart-turczynski/rurl/-/work_items>. Report security issues
privately as described in `SECURITY.md`. Send changes as merge requests on
GitLab; the GitHub repository is a read-only mirror.

New code needs tests, and each user-facing change needs one `NEWS.md` bullet.
A merge request must pass the verification command below.

Run verification (the pre-push chain: the hygiene hooks, the toolchain check,
the URL check, the citation and BugReports checks, then `tools/verify.R` in the
`verify` hook: the manifest gates, lintr, spelling, the roxygen docs-drift
check, `R CMD check --as-cran` and the test suite under `LC_ALL=C`):

```sh
pre-commit run --hook-stage pre-push --all-files
```

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
  [ADR 0002](design/adr/0002-keep-punycode-helpers.md). `AGENTS.md` lists the
  protected areas, and [ADR 0005](design/adr/0005-intentional-base-r-string-exceptions.md)
  covers the intentional base-R string exceptions.
- Public Suffix List data, its parsing, and its refresh (`pslr::psl_refresh()`)
  live in the `pslr` package. `rurl` ships no PSL list of its own and queries
  `pslr` through the `R/domain.R` seam. The delegation contract is in
  [ARCHITECTURE.md](ARCHITECTURE.md) and [ADR 0001](design/adr/0001-delegate-psl-to-pslr.md).
- `ACKNOWLEDGMENTS.md` is a **synced canonical file**, byte-identical across
  `rurl`, `pslr`, `punycoder`, `pagerankr` and `sitemapr`, and `.Rbuildignore`d
  in all five. Its only CRAN-visible surface is each package's intro vignette
  `## Acknowledgments` section plus a pkgdown navbar link. Any edit must be
  applied to all five repositories; verify with `md5 -q`.

## Validation

- **Install the local verify gate once, per clone:**

      pre-commit install --hook-type pre-push

  It is not installed automatically — `.pre-commit-config.yaml` being committed
  does nothing until someone runs that command in their own working copy.
- During iteration, run the targeted tests and checks relevant to the change.
  Run `Rscript tools/verify.R` once at the coherent slice tip, before delivery,
  to reproduce CI's fast gate by hand: the ~20
  gate steps (derived from `tools/verify-manifest.yml`, never transcribed),
  `lintr::lint_package()`, `spelling::spell_check_package()` (real words it
  does not know go in `inst/WORDLIST`), `scripts/check-docs-drift.R` (fails
  when the committed `man/` or `NAMESPACE`, in the commit being pushed or in
  `HEAD` by hand, differ from what the pinned roxygen2 regenerates;
  uncommitted edits do not count, and the fix is `devtools::document()` and a
  commit), `R CMD build` + `R CMD check --as-cran`
  on the built tarball, and the test suite under `LC_ALL=C`. `--fast` runs the
  gates, lint and spelling only and is iteration feedback, never sufficient
  verification for a behavioral slice; `--list` prints the plan; `--release`
  adds the curl clean room. Its header states what it does **not** cover.
- Intermediate local commits may temporarily be red while a slice is being
  assembled. The delivered slice tip and its merged result must pass the
  complete local gate.
- `devtools::test()` can provide targeted feedback during iteration, but it is
  not a substitute for `tools/verify.R`. A green suite says nothing about
  whether the package builds: `devtools::test()` ignores `Collate:` and runs
  against the source tree, not an installed copy.
- **Any path whose bytes a tool owns must be excluded from the whitespace
  fixers**, and belongs in the shared `&byte-pinned` exclude in
  `.pre-commit-config.yaml`. Two kinds qualify: files testthat rewrites on
  passing runs (`tests/testthat/_snaps/*.md`), and the hash-pinned oracle
  fixtures under `inst/bench/` and `tests/testthat/fixtures/`. Left in, a fixer
  mutates the tree during the push and the push then fails a gate the push
  itself just broke — on a run whose verdict line printed `PASS`. Running
  `tools/verify.R` by hand never reproduces this, because only the hook path
  mutates the tree.
- Add a new universal gate only when it has a relevant trigger surface, a named
  owner, and a stated retirement or review condition.
- New prose in `DESCRIPTION`, `.Rd`, README, or vignettes should pass
  `spelling::spell_check_package()`; add genuine terms to `inst/WORDLIST`.

## CRAN release checklist

Follow the fleet checklist,
[seor `design/release-checklist.md`](https://gitlab.com/bart-turczynski/seor/-/blob/main/design/release-checklist.md).
rurl's deltas:

- **Step 1: rurl is one link in the fleet's release chain.**
  [design/release-chain.md](design/release-chain.md) records the order and
  what must already be on CRAN; the live order is seor's SEOR-eqpdrqnl. The
  next rurl update waits a full two months after the previous one (owner,
  2026-10-01, after CRAN questioned pagerankr 0.1.1's spacing).
- **Step 2: check the fleet packages that import rurl, not only CRAN's
  reverse dependencies.** pagerankr is the CRAN reverse dependency today.
  Also check robotstxtr's and sitemapr's latest CRAN release, or `main` if
  they have none: CRAN re-checks every reverse dependency against each later
  rurl, so a break there blocks the next rurl release (owner, 2026-10-01).
- **Step 4: keep the `submission-span` pin.** `cran-comments.md` carries one
  `<!-- submission-span: from=X to=Y -->` line, and
  `tools/cran-comments-gate.R` (in the pre-push gate) checks it against
  `DESCRIPTION`: `to` is the version being submitted, and both versions
  appear in the prose. Leave `cran-comments.md` alone at step 12: while
  `Version:` is `X.Y.Z.9000` the gate checks the pin against `X.Y.Z`
  (RURL-efbcrhjc).
- **Step 5: run punycoder built with libidn2, and run the CI jobs locally.**
  punycoder's `configure` uses libidn2 when `pkg-config` finds it, as on
  CRAN's Debian flavors, and rurl's tests can behave differently there. rurl
  3.1.0's first upload failed CRAN's pre-test on exactly that
  (RURL-woljcpfu). Install the current CRAN punycoder from source with
  libidn2 and run the suite against it, besides the binary build. Then run
  `tools/local-ci.sh --all origin/main`, which reproduces the CI jobs in the
  CI image against a clean clone.
- **Step 14: the concept DOI is `10.5281/zenodo.20972584`.** Its
  `<concept-recid>` in the Zenodo API query is `20972584`.
