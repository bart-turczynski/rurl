# rurl validation policy

Moved from `CONTRIBUTING.md` unchanged (SEOR-ogreubsr).

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
