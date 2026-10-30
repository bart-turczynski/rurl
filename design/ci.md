# Continuous integration

rurl runs fleet-ci's tiered CI (seor ADR 0012). `.gitlab-ci.yml` includes
fleet-ci's `templates/r-package.yml`, pinned by commit SHA, and sets only
package variables plus two `x-*` jobs; `.fleet-ci.yml` holds the local gate's
settings, and `.fleet-ci/` is fleet-ci's vendored gate (re-run fleet-ci's
`bin/vendor`, never edit it). The job names below are the template's.

| Tier | Where | What runs |
| --- | --- | --- |
| hygiene hooks | pre-commit, at commit and at push | `.pre-commit-config.yaml`. At commit and again at push: the hygiene fixers (end of file, trailing whitespace, line endings), the large-file, merge-conflict, YAML and case-conflict checks, and codespell. At push only: the citation, BugReports, NEWS and toolchain checks, the `check-news.py` and `bestpractices-url.py` self-tests when those scripts change, and the warn-only `fleet-ci-hook-installed` check. After a merge: the `backup` mirror refresh |
| T0 | every push whose diff against `main` is docs-only (`design/**/*.md` and the like, minus `.fleet-ci.yml`'s `docs_exclude`), locally, the pre-push hook | the gate's docs tier, `.fleet-ci/gate` in the fleet image (`fleet-r-idn2`): spelling and `pkgdown::check_pkgdown()` |
| T1 | every other push, locally, the pre-push hook | the gate's full tier: T0's checks, then punycoder rebuilt from source with libidn2 and asserted (`scripts/punycoder-backend.R`), the manifest gates and their self-tests (`tools/verify.R --gates`), docs drift, lintr, the NEWS heading, the URL check, the locale-relevant tests under `LC_ALL=C` (`tools/check-locale-tests.R`), the tests that need the source tree, from it (`tools/check-source-tests.R`), and one `R CMD check --as-cran` (incoming checks off) against the rebuilt punycoder |
| T2 | the weekly `deep-check` schedule; every T3 run | `t2-check-release`, `t2-check-oldrel`, `t2-check-devel`, `t2-floor` (R 4.1.3), `t2-locale` (the whole suite under `LC_ALL=C`), `t2-ascran`, `coverage` (fails below 95%), `osv-audit`, `security-audit`, `citation-version`, and rurl's `x-check-no-idn2` (punycoder rebuilt without libidn2, asserted) and `x-fossa` (`fossa analyze`) |
| T3 | a `RELEASE=1` web pipeline on `main`; a `v*` tag | all of T2 plus `t3-manual`; the tag, or `RELEASE=1` on a tag, adds `pages` |

Pushes to `main` or a branch, merge requests and API pipelines (`glab ci run`)
create no pipeline at all. An empty pipeline list is not a pass: the merge
gate is the local gate (T0 or T1), run before the branch leaves the machine.

Before a CRAN submission, start T3 on `main` from **Build > Pipelines > Run
pipeline**, ref `main`, variable `RELEASE=1` (see
[release-deltas.md](release-deltas.md)). `PAGES=1` on a `v*` tag runs `pages`
alone.

## The hook, once per clone

```sh
pre-commit install
.fleet-ci/pre-push --install
```

It runs the gate on each pushed ref's tip, then the pre-commit pre-push
stage. Two commands drop it: `pre-commit install -f`, which installs every
hook type (`.pre-commit-config.yaml`'s `default_install_hook_types`), and
`pre-commit install -f --hook-type pre-push`; re-run the install after
either. Without `-f`, `pre-commit install` (with or without
`--hook-type pre-push`) moves the hook to `hooks/pre-push.legacy`
(pre-commit's migration mode), which still runs the gate first.
`.fleet-ci/gate auto` runs the gate by hand on `HEAD`. A pass is stamped by
tree, tier, classifier version, image and (full tier) resolved dependency
lock, and a stamped commit is skipped.

The gate runs for every remote, `backup` included, and refuses a push to
`main` that adds more than one merge and every ref from before the fleet-ci
migration. `tools/mirror-refresh.sh`, the owner's post-merge refresh of the
`backup` mirror, therefore pushes with `--no-verify`
([backup-mirror.md](backup-mirror.md)).

## Project settings CI depends on, which are not in this repository

These live in GitLab's project settings, so a clone does not carry them:

- **Pipeline schedule 4472809** (Build > Pipeline schedules), "Weekly deep
  check (fleet standard)", `0 21 * * 5` Europe/Warsaw on `main`, carrying
  `SCHEDULE_KIND=deep-check`. Required: it is T2's only unattended run, and
  every pipeline on `main` carries `coverage`, so the coverage badge reads
  it.
- **Pipeline schedule 4467472**, `SCHEDULE_KIND=dependency-audit`. The
  template starts no pipeline for it any more (the audits run in T2); its
  removal is owner-gated.
- **`OSSINDEX_USER` / `OSSINDEX_TOKEN`** (Settings > CI/CD > Variables;
  `OSSINDEX_TOKEN` masked; the user is the literal `x`). `security-audit`
  sets `OSSINDEX_AUDIT_REQUIRED=true`, so without them it fails rather than
  passing having audited nothing.
- **`FOSSA_API_KEY`** (masked), for `x-fossa`.
- **Pages**: the site lives at `https://bart-turczynski.gitlab.io/rurl/`,
  which `_pkgdown.yml` and `DESCRIPTION` name. `pages` publishes only the
  top-level `.md` files on `.fleet-ci.yml`'s `pages_keep`.

## Gaps accepted in the fleet-ci migration

- **The template's release legs check the fallback build of punycoder.**
  `t2-check-release`, `t2-ascran` and `coverage` run on `fleet-r-idn2`
  (`FLEET_RELEASE_VARIANT`, which must equal `.fleet-ci.yml`'s `variant`),
  but they install punycoder as Posit Package Manager's binary, which is
  built without libidn2. The libidn2 cell is the local gate, which rebuilds
  punycoder from source in `fleet-r-idn2` and asserts the backend; the
  fallback cell is asserted in `x-check-no-idn2`. Coverage measures the
  fallback build.
- **`t2-locale`'s `LC_ALL=C` leg may run the tests under a UTF-8
  `LC_CTYPE`.** `tests/testthat.R` selects `en_US.UTF-8` when that locale
  exists, and `t2-locale` builds it from `.fleet-ci.yml`'s `locales`. The
  former locale cell ran `testthat::test_local()`, which skips that file.
  The local gate's `tools/check-locale-tests.R` also uses `test_local()` and
  asserts `LC_CTYPE` is `C`, so the locale-relevant tests do run under C
  before every push.
- **The floor leg is unproven.** `t2-floor` installs every dependency,
  `Suggests` included, from one dated snapshot (`FLEET_FLOOR_SNAPSHOT`), and
  rurl's floors (`pslr >= 1.2.2`, on CRAN 2026-10-24; `raddr >= 0.1.2`) put
  that snapshot in late 2026, after R 4.6.0. The former `floor-check` took a
  2025 snapshot and installed the five fleet packages from CRAN source; the
  template has no such mix. If the first T2 run shows a dependency that no
  longer supports R 4.1, move the date or record `FLEET_FLOOR: "false"`
  here with the reason.
- **`pages` deploys from a `v*` tag only**, not on every push to `main`, so
  the site documents the release CRAN serves.
- **`fossa analyze` runs weekly and at each release** (`x-fossa`), not on
  every push to `main`.
- **No sanitizer leg** (`FLEET_SANITIZERS: "false"`): rurl has no compiled
  code of its own, and never had one.
- **`NOT_CRAN=true` in T1 and the T2 check legs.** `skip_on_cran()` tests run
  there; the T2 check legs and `t2-locale` also run `test-osv.R` (network)
  and `test-security.R` (OSS Index credentials); T1 excludes both
  (`.fleet-ci.yml`'s `exclude_tests`).
- **Comments in `tests/` still name the former locale cell**
  (`test-locale-invariance.R`, `test-latin1-input.R`, `test-security.R`); a
  CI migration does not edit `tests/`.
- **Pandoc** comes from the fleet image (`.fleet-ci/toolchain`, 3.10).
  `scripts/check-toolchain.R` checks a local pandoc against it, so a local
  `build_readme()` writes what the image reproduces. No gate re-knits
  `README.md`, as before.
