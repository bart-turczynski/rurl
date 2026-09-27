# Agent workflow notes

Rules that apply to one kind of task. `AGENTS.md` points here; the house
`agent-workflow` skill's `preflight` reads this file for repository rules.

## Hook setup, per clone

- `pre-commit install --hook-type pre-push --hook-type post-merge`. The
  pre-push stage runs the verify gate; post-merge refreshes the `backup`
  mirror.
- The pre-push hook skips the gate when pushing to a local-path mirror
  (`tools/verify-on-push.sh` header). The skip is not a way to push
  unverified work.
- `tools/verify-manifest.yml` holds the gate list. Moving or deleting it
  breaks the gate.

## Reading a gate run

- The delivery bar is `Rscript tools/verify.R`. `--fast` does not meet it,
  and neither does `devtools::test()`, which ignores `Collate:` and runs
  against the source tree.
- Judge a run by `0 failed` and the `VERDICT` line, not the step count. A
  passing step discards its output, so check for warnings with `--verbose`.

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
