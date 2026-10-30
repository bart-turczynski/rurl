# rurl validation policy

Moved from `CONTRIBUTING.md` unchanged (SEOR-ogreubsr).

- **Install the local gate once, per clone:**

      pre-commit install
      .fleet-ci/pre-push --install

  It is not installed automatically — the committed hook configuration does
  nothing until someone runs those commands in their own working copy.
- During iteration, run the targeted tests and checks relevant to the change.
  Run `.fleet-ci/gate auto` once at the coherent slice tip, before delivery
  (the push runs it too). It checks the committed tree in the fleet image:
  for a diff that is not docs-only, the ~20 manifest gates and their
  self-tests (`tools/verify.R --gates`, derived from
  `tools/verify-manifest.yml`, never transcribed), `lintr::lint_package()`,
  `spelling::spell_check_package()` (real words it does not know go in
  `inst/WORDLIST`), `scripts/check-docs-drift.R` (fails when the committed
  `man/` or `NAMESPACE` differ from what the pinned roxygen2 regenerates;
  uncommitted edits do not count, and the fix is `devtools::document()` and a
  commit), the NEWS heading, the URL check, the libidn2 assertion, the
  locale-relevant tests under `LC_ALL=C` (`tools/check-locale-tests.R`) and
  `R CMD build` + `R CMD check --as-cran` on the built tarball.
  `Rscript tools/verify.R --gates` runs the manifest gates alone, as
  iteration feedback; `--release` adds the curl clean room.
- Intermediate local commits may temporarily be red while a slice is being
  assembled. The delivered slice tip and its merged result must pass the
  complete local gate.
- `devtools::test()` can provide targeted feedback during iteration, but it is
  not a substitute for the gate. A green suite says nothing about
  whether the package builds: `devtools::test()` ignores `Collate:` and runs
  against the source tree, not an installed copy.
- **Any path whose bytes a tool owns must be excluded from the whitespace
  fixers**, and belongs in the shared `&byte-pinned` exclude in
  `.pre-commit-config.yaml`. Two kinds qualify: files testthat rewrites on
  passing runs (`tests/testthat/_snaps/*.md`), and the hash-pinned oracle
  fixtures under `inst/bench/` and `tests/testthat/fixtures/`. Left in, a fixer
  mutates the tree during the push and the push then fails a gate the push
  itself just broke — on a run whose verdict line printed `PASS`. Running
  the gate by hand never reproduces this, because only the hook path
  mutates the tree.
- Add a new universal gate only when it has a relevant trigger surface, a named
  owner, and a stated retirement or review condition.
- New prose in `DESCRIPTION`, `.Rd`, README, or vignettes should pass
  `spelling::spell_check_package()`; add genuine terms to `inst/WORDLIST`.
