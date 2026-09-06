# Contributing

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
  `lintr::lint_package()`, `R CMD build` + `R CMD check --as-cran` on the built
  tarball, and the test suite under `LC_ALL=C`. `--fast` runs the gates and lint
  only and is iteration feedback, never sufficient verification for a
  behavioral slice; `--list` prints the plan; `--release` adds the curl clean
  room. Its header states what it does **not** cover.
- Intermediate local commits may temporarily be red while a slice is being
  assembled. The delivered slice tip and its squash-merged result must pass the
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

Follow these steps in order for every CRAN release. The first three and the
fast-forward are the easiest to miss — skipping them leaves `NEWS.md`, the
published version, and `main` out of sync. `rurl` is one link in an eight-package
chain; [design/release-chain.md](design/release-chain.md) records the order and
what must already be on CRAN before this checklist starts.

1. **Update the NEWS heading.** Ensure the top `NEWS.md` heading matches the
   release version (e.g. `## rurl 1.2.0`) and fold any unreleased items into
   that section.

   **Nothing checks this any more, and the freeze is why.** `rurl` ships as
   3.0.0 with no further version bumps, so the `news-version` GitHub workflow —
   which asserted the top NEWS heading is either `(development version)` or
   the `DESCRIPTION` Version — compared two constants and could never fail; it
   was deleted with RURL-vunvxusf. It was never in `tools/verify-manifest.yml`
   either, so `tools/verify.R` does not derive it. With the version bump gone as a
   checkpoint, `## rurl 3.0.0` accumulates months of work with nothing asserting
   that a user-visible change added an entry. The gate a freeze needs is NEWS
   **completeness**, not NEWS/version **consistency**; until one exists, this
   step is entirely manual.
2. **Set the release version** in `DESCRIPTION`.
3. Update `cran-comments.md` for this submission.
4. Run `R CMD build . && R CMD check --as-cran rurl_*.tar.gz` clean. There is no
   platform CI to confirm — the `R-CMD-check` and R-hub workflows ran on GitHub
   Actions and cannot fire. Run `tools/local-ci.sh --all origin/main` instead,
   which reproduces the CI jobs in the CI image against a clean clone.
5. Submit to CRAN. Once accepted, **tag the released commit**
   (`git tag -a vX.Y.Z`) and push the tag.
6. **Fast-forward `main` to the released/tagged commit** so the default branch
   always reflects what shipped (`git merge --ff-only vX.Y.Z && git push`).
   Verify: `git merge-base vX.Y.Z main` equals the tag.
7. Create the GitLab release from the tag
   (`glab release create vX.Y.Z`).
8. Open a post-release PR that (a) bumps `DESCRIPTION` to the next development
   version, (b) adds a fresh `## rurl (development version)` NEWS heading, and
   (c) adds the CRAN canonical URL to the `DESCRIPTION` `URL:` field
   (`https://CRAN.R-project.org/package=rurl`), which only exists once accepted.
9. Sanity check: diff the published CRAN tarball
   (`cran.r-project.org/src/contrib/rurl_X.Y.Z.tar.gz`) against the tag — only
   CRAN's auto-added `DESCRIPTION` fields should differ.
