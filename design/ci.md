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
| T1 | every other push, locally, the pre-push hook | the gate's full tier: T0's checks, then punycoder rebuilt from source with libidn2 and asserted (`scripts/punycoder-backend.R`), the manifest gates and their self-tests (`tools/verify.R --gates`), docs drift, lintr, the NEWS heading, the URL check, the test suite under `LC_ALL=C` (`tools/check-locale-tests.R`), the tests that need the source tree, from it (`tools/check-source-tests.R`), and one `R CMD check --as-cran` (incoming checks off) against the rebuilt punycoder |
| T2 | the weekly `deep-check` schedule; every T3 run | `t2-check-release`, `t2-check-oldrel`, `t2-check-devel`, `t2-floor` (R 4.1.3), `t2-locale` (`R CMD check` under `LC_ALL=C`; its tests run under `en_US.UTF-8`, see the gaps), `t2-ascran`, `coverage` (fails below 95%), `osv-audit`, `security-audit`, `citation-version`, and rurl's `x-check-idn2` (punycoder rebuilt from source with libidn2, asserted) and `x-fossa` (`fossa analyze`, allowed to fail) |
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

The gate skips a push to a local mirror: when the push URL is local by
git's rule (a path such as `backup`'s, or `file:///` with an empty host) it
prints `gate: skipped, <remote> is a local mirror` and exits 0. So
`tools/mirror-refresh.sh`, the post-merge refresh of the `backup` mirror,
pushes without `--no-verify`, and neither the owner nor an agent needs it
there. Every other remote is gated, where the gate refuses a push to `main`
that adds more than one merge and every ref from before the fleet-ci
migration. Under pre-commit's migration mode the skip ends only the gate's
hook, and `.pre-commit-config.yaml`'s pre-push stage still runs
([backup-mirror.md](backup-mirror.md)).

NOTEs from `R CMD check` don't fail the gate, as before: `.fleet-ci.yml`
sets no `allowed_notes`.

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
  built without libidn2, as do the oldrel, devel, floor and locale legs.
  Those legs are the fallback cell; nothing asserts their backend. The
  libidn2 cell is `x-check-idn2` in T2 and the local gate before every
  push: each rebuilds punycoder from source in `fleet-r-idn2` and asserts
  the libidn2 backend. Coverage measures the fallback build.
- **`t2-locale`'s `LC_ALL=C` leg runs the tests under a UTF-8
  `LC_CTYPE`.** `tests/testthat.R` selects `en_US.UTF-8` when that locale
  exists, and `t2-locale` builds it from `.fleet-ci.yml`'s `locales`, so
  the leg checks the build and the examples under C but not the tests. The
  former locale cell ran `testthat::test_local()`, which skips that file.
  The local gate's `tools/check-locale-tests.R` does the same: it asserts
  `LC_CTYPE` is `C` and runs the whole suite (bar the two audits) with
  `test_local()` before every push, so the non-UTF-8 run is T1's.
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
  every push to `main`, and is allowed to fail, so a FOSSA outage cannot
  block `pages` on a tag.
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
