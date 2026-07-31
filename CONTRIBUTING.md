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

## Constraints

- Do not alter the Punycode helpers (`.normalize_and_punycode()`,
  `.punycode_to_unicode()`) or their hardcoded TLD workarounds. See `CLAUDE.md`
  for the full list of protected areas and the intentional base-R string
  exceptions.
- Public Suffix List data, its parsing, and its refresh (`pslr::psl_refresh()`)
  live in the `pslr` package. `rurl` ships no PSL list of its own and queries
  `pslr` through the `R/domain.R` seam. See `CLAUDE.md` for the delegation
  contract.

## Validation

- **Install the local verify gate once, per clone:**

      pre-commit install --hook-type pre-push

  It is not installed automatically — `.pre-commit-config.yaml` being committed
  does nothing until someone runs that command in their own working copy.
- During iteration, run the targeted tests and checks relevant to the change.
  Run `Rscript tools/verify.R` once at the coherent slice tip, before delivery,
  to reproduce CI's fast gate by hand: the ~20
  gate steps (derived from `.github/workflows/verify.yml`, never transcribed),
  `lintr::lint_package()`, `R CMD build` + `R CMD check --as-cran` on the built
  tarball, and the test suite under `LC_ALL=C`. `--fast` runs the gates and lint
  only; `--list` prints the plan; `--release` adds the curl clean room. Its
  header states what it does **not** cover.
- Intermediate local commits may temporarily be red while a slice is being
  assembled. The delivered slice tip and its squash-merged result must pass the
  complete local gate.
- `devtools::test()` can provide targeted feedback during iteration, but it is
  not a substitute for `tools/verify.R`. A green suite says nothing about
  whether the package builds: `devtools::test()` ignores `Collate:` and runs
  against the source tree, not an installed copy.
- Add a new universal gate only when it has a relevant trigger surface, a named
  owner, and a stated retirement or review condition.
- New prose in `DESCRIPTION`, `.Rd`, README, or vignettes should pass
  `spelling::spell_check_package()`; add genuine terms to `inst/WORDLIST`.

## CRAN release checklist

Follow these steps in order for every CRAN release. The first three and the
fast-forward are the easiest to miss — skipping them leaves `NEWS.md`, the
published version, and `main` out of sync.

1. **Update the NEWS heading.** Ensure the top `NEWS.md` heading matches the
   release version (e.g. `## rurl 1.2.0`) and fold any unreleased items into
   that section. The `news-version` CI check enforces that the top NEWS heading
   is either `(development version)` or the `DESCRIPTION` Version.
2. **Set the release version** in `DESCRIPTION`.
3. Update `cran-comments.md` for this submission.
4. Run `R CMD build . && R CMD check --as-cran rurl_*.tar.gz` clean; confirm the
   platform CI (`R-CMD-check`, R-hub) is green.
5. Submit to CRAN. Once accepted, **tag the released commit**
   (`git tag -a vX.Y.Z`) and push the tag.
6. **Fast-forward `main` to the released/tagged commit** so the default branch
   always reflects what shipped (`git merge --ff-only vX.Y.Z && git push`).
   Verify: `git merge-base vX.Y.Z main` equals the tag.
7. Create the GitHub Release from the tag.
8. Open a post-release PR that (a) bumps `DESCRIPTION` to the next development
   version, (b) adds a fresh `## rurl (development version)` NEWS heading, and
   (c) adds the CRAN canonical URL to the `DESCRIPTION` `URL:` field
   (`https://CRAN.R-project.org/package=rurl`), which only exists once accepted.
9. Sanity check: diff the published CRAN tarball
   (`cran.r-project.org/src/contrib/rurl_X.Y.Z.tar.gz`) against the tag — only
   CRAN's auto-added `DESCRIPTION` fields should differ.
