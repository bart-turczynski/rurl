# Contributing

## Workflow

- Open an issue or discussion before large behavioral changes.
- Add or update tests for every user-visible change.
- Keep exported function signatures and object shapes stable unless the
  change is explicitly planned as breaking.
- Prefer small, reviewable patches over broad rewrites without coverage.

## Constraints

- Do not alter the Punycode helpers (`.normalize_and_punycode()`,
  `.punycode_to_unicode()`) or their hardcoded TLD workarounds. See
  `CLAUDE.md` for the full list of protected areas and the intentional
  base-R string exceptions.
- Public Suffix List data, its parsing, and its refresh
  ([`pslr::psl_refresh()`](https://bart-turczynski.github.io/pslr/reference/psl_refresh.html))
  live in the `pslr` package. `rurl` ships no PSL list of its own and
  queries `pslr` through the `R/domain.R` seam. See `CLAUDE.md` for the
  delegation contract.

## Validation

- Run `devtools::test()` locally for every change.
- For packaging changes, run `R CMD check --as-cran` before release
  work.
- New prose in `DESCRIPTION`, `.Rd`, README, or vignettes should pass
  `spelling::spell_check_package()`; add genuine terms to
  `inst/WORDLIST`.

## CRAN release checklist

Follow these steps in order for every CRAN release. The first three and
the fast-forward are the easiest to miss — skipping them leaves
`NEWS.md`, the published version, and `main` out of sync.

1.  **Update the NEWS heading.** Ensure the top `NEWS.md` heading
    matches the release version (e.g. `## rurl 1.2.0`) and fold any
    unreleased items into that section. The `news-version` CI check
    enforces that the top NEWS heading is either `(development version)`
    or the `DESCRIPTION` Version.
2.  **Set the release version** in `DESCRIPTION`.
3.  Update `cran-comments.md` for this submission.
4.  Run `R CMD build . && R CMD check --as-cran rurl_*.tar.gz` clean;
    confirm the platform CI (`R-CMD-check`, R-hub) is green.
5.  Submit to CRAN. Once accepted, **tag the released commit**
    (`git tag -a vX.Y.Z`) and push the tag.
6.  **Fast-forward `main` to the released/tagged commit** so the default
    branch always reflects what shipped
    (`git merge --ff-only vX.Y.Z && git push`). Verify:
    `git merge-base vX.Y.Z main` equals the tag.
7.  Create the GitHub Release from the tag.
8.  Open a post-release PR that (a) bumps `DESCRIPTION` to the next
    development version, (b) adds a fresh
    `## rurl (development version)` NEWS heading, and
    3.  adds the CRAN canonical URL to the `DESCRIPTION` `URL:` field
        (`https://CRAN.R-project.org/package=rurl`), which only exists
        once accepted.
9.  Sanity check: diff the published CRAN tarball
    (`cran.r-project.org/src/contrib/rurl_X.Y.Z.tar.gz`) against the tag
    — only CRAN’s auto-added `DESCRIPTION` fields should differ.
