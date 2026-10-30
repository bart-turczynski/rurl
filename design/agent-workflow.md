# Agent workflow notes

Rules that apply to one kind of task. `AGENTS.md` points here; the house
`agent-workflow` skill's `preflight` reads this file for repository rules.

## Hook setup, per clone

- `pre-commit install` (the pre-commit, pre-push and post-merge hooks), then
  `.fleet-ci/pre-push --install`, which puts fleet-ci's local gate in front
  of pre-commit's pre-push stage. post-merge refreshes the `backup` mirror.
- The gate skips a push to a local mirror (a push URL that is a path, or
  `file:///`), so `backup` needs no `--no-verify`, from the owner or an
  agent; every other remote is gated. Under pre-commit's migration mode the
  pre-commit pre-push stage still runs for it (design/backup-mirror.md).
- `tools/verify-manifest.yml` holds the manifest gate list, which the gate
  runs through `tools/verify.R --gates`. Moving or deleting it breaks the
  gate.

## Reading a gate run

- The delivery bar is `.fleet-ci/gate auto` (or the push itself) passing.
  `Rscript tools/verify.R --gates` runs only the manifest gates, and
  `devtools::test()` ignores `Collate:` and runs against the source tree.
- The gate prints each gate's exit, the R CMD check summary and the test
  skip count; any skip not on `.fleet-ci.yml`'s `allowed_skips` fails it.
  Inside the manifest-gate step, judge by `0 failed` and the `VERDICT` line;
  a passing step discards its output, so check for warnings with
  `Rscript tools/verify.R --gates --verbose`.

## Tests that reach into pslr, punycoder or raddr

- The test holds against the released version that the `DESCRIPTION` floor
  admits. Check both the released export and the released capability.
- Skip guards name the dev version, not the next release.
- Before moving a floor, check the reverse-dependency gates.

## Known false alarms

- Locally, `pkgcheck()` reports "no CI". A missing `GITHUB_PAT` causes it;
  do not chase it.
- A 404 or 403 that GitLab serves a logged-out client says nothing about this
  project until a known-good peer, measured in the same run, says otherwise.
  The `/-/issues` 404 was first written up as a project setting, then as
  anti-scraping; both were wrong (RURL-ladruqhn). `cran-comments.md` keeps the
  control table, and a control set shows how far a behavior reaches, not what
  causes it.
