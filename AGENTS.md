# AGENTS.md

`rurl` is an R package for parsing, normalizing, cleaning, and joining URLs —
standards-conformant parsing (WHATWG / RFC 3986 profiles), with domain and
public-suffix extraction delegated to `pslr`.

## Verifying a change

```sh
Rscript tools/verify.R      # full gate; --fast = gates + lint, --list = plan
```

`tools/verify.R` reproduces CI's fast gate locally and is the bar a delivered
slice has to clear. `--fast` is iteration feedback, never sufficient on its own.
`devtools::test()` is not a substitute: it ignores `Collate:` and runs against
the source tree, so a green suite can hide a package that does not build. The
gate runs as a pre-push hook once you run `pre-commit install --hook-type
pre-push` in your clone — committing the config does not install it.

The hook's `entry` is `tools/verify-on-push.sh`, not `tools/verify.R`. That
wrapper looks at the destination remote and **skips the gate when the
destination is a directory on this filesystem** — the local archival mirror
`backup`, whose history already passed the gate on its way to `origin`, and
where a run would only burn minutes and rewrite `_snaps/` and fixture bytes in
your tree (RURL-qkowfsdt). A push to `origin` — or to any remote the wrapper
cannot positively identify as a local mirror — runs the full gate unchanged.
The skip is scoped to mirrors; it is not a way to push unverified work.

**GitLab CI is paused** — the free-tier compute allowance is exhausted, so
`.gitlab-ci.yml` creates a pipeline only for a hand-triggered web run
(RURL-psqmlgjf). Nothing verifies a push server-side. `tools/local-ci.sh` runs
the same CI jobs in the same image against a clean clone of a commit, which is
where environment-shaped defects live that a fully-populated local library hides:

```sh
tools/local-ci.sh --list          # which jobs apply to this ref, and why
tools/local-ci.sh                 # run them (needs Docker)
git fetch origin main && tools/local-ci.sh --all origin/main   # after a merge
```

Run it against `origin/main` after merging: a squash-merge produces a commit
that has never existed on any machine, and the pipeline that used to check it is
the one that is off. **`--all` is required** — the `check` job's rules select a
tag or a hand-started pipeline only, so without it you get the cheap half and no
`R CMD check --as-cran`. The runner is pull-based, so it does not restore the
property that mattered most about a real runner — that verification was not
opt-in.

The expensive `check` job runs on the forge only at release time: on a tag, or
on a pipeline started by hand as the finishing touch after `/cran` has been
worked through. It is deliberately not part of `/cran`, which stays local and
cheap.

### Which checks run where (RURL-sgkplmot)

The forge moved to GitLab, so the answer is explicit rather than inherited:

| Check | Where it runs | When |
|---|---|---|
| Gate list (lint, build, tests, `R CMD check`, record structure) | **Local**, `tools/verify.R` | Every push — it is a pre-push hook |
| The same jobs in the CI image, clean clone | **Local**, `tools/local-ci.sh` | On demand; after every merge to `main` |
| `gates` (~40s) | GitLab | **Paused.** Resumes on quota reset — RURL-utsbwfvc |
| `check` (`R CMD check --as-cran`) | GitLab | Release time only: a tag, or a hand-started pipeline |
| Everything in `.github/workflows/` | **Nowhere** | The account is suspended; none of it can fire |

**Nothing verifies a push server-side today.** `tools/verify.R` as a pre-push
hook is the only non-optional gate, and it is only installed if you ran
`pre-commit install --hook-type pre-push` in your clone.

### Dependency resolvability — a release check, across all seven repos

```sh
Rscript tools/dependency-resolvability-gate.R          # this repo
Rscript tools/dependency-resolvability-gate.R --all    # every sibling package
Rscript tools/dependency-resolvability-gate.R --self-test
```

Every other gate here asks whether this tree is internally consistent. This one
asks whether the *rest of the world* can serve what `DESCRIPTION` promises: that
each version floor is satisfiable by a release that exists, that each
`dep::symbol` is exported by the **released** NAMESPACE rather than by the
development checkout sitting in your library, and that each `Remotes:` entry is
pinned and reachable. It reads CRAN tarballs and sibling git tags; it never
reads the local library, which is the whole point — `R CMD check` and
`devtools::test()` are both green on defects this catches, because locally the
dependency *is* the development tree.

Measured 2026-08-12 it fails in four of the seven on four different defects; see
the script header for the list and RURL-ovgasmea for rurl's.

**It is deliberately NOT in `tools/verify.R`'s gate list.** It needs the network
by construction, and wiring a network dependency into the only non-optional
pre-push gate would make every push fail on a train. Run it before a release,
and after changing any floor, `Remotes:` entry, or `dep::symbol` call site.
`--offline` scores from the artifact cache and aborts on a miss rather than
guessing.

### The archival mirror, and checking it is not stale (RURL-eqgqbeti)

With no server-side gate and a suspended GitHub account, the bare mirror at
`~/Projects/_backups/rurl.git` (the `backup` remote) is the only copy of the
history that is not your working clone. It was wired once and pushed once, and
by 2026-08-08 it was six commits behind with nothing anywhere saying so — which
is the failure being fixed: not staleness, *silent* staleness in something still
trusted.

```sh
tools/mirror-freshness.sh    # exit 0 = mirror carries origin/main, exit 1 = drifted
tools/mirror-refresh.sh      # refresh every local-path mirror, then assert it took
```

`tools/mirror-freshness.sh` **exits non-zero** on drift; that is the whole point,
and it is why it is not merely a print. The refresh runs automatically as a
pre-commit **`post-merge`** hook, installed alongside the gate:

```sh
pre-commit install --hook-type pre-push --hook-type post-merge
```

Be honest about what that buys: it is per-clone like every hook here, it fires
on `git merge`/`git pull` but not on `git pull --rebase` or `git fetch` + reset,
and git ignores a `post-merge` hook's exit status, so a failure is loud but not
blocking. The non-optional half is the check — run it by hand whenever you are
about to rely on the backup. Both scripts find mirrors by the same
URL-is-a-local-directory predicate `tools/verify-on-push.sh` uses, and the
refresh pushes through that wrapper rather than around it with `--no-verify`.

**Nothing is ever pruned or force-pushed to the mirror**, and its
`refs/remotes/origin/*` namespace is a **frozen pre-migration GitHub snapshot**,
not current forge state — `refs/remotes/origin/main` sits 186 commits back at
what GitHub last held. See [design/backup-mirror.md](design/backup-mirror.md)
for that namespace, the "never `git fetch` inside the mirror" hazard, and the
triage of the eight mirror-only branch tips.

`.github/workflows/` is **kept on purpose**, even though not one of those
workflows can run. The files are in-repo configuration that local tooling and
the test suite read as data, independently of GitHub:

- `tools/verify.R` derives its entire gate list from `verify.yml` at run time,
  precisely so the list is never transcribed twice. Deleting it breaks the gate.
- `tools/determinism/compare-gate.R` reads `determinism-probe.yml` and
  `_determinism-cells.yml`; `tools/oracle/check-uts46-mapping-pin.R` reads
  `verify.yml`.
- `tests/testthat/test-osv.R`, `test-security.R` and `test-locale-invariance.R`
  assert against their contents.

26 tracked files reference `.github/workflows/` paths. Untangling that — moving
the gate manifest somewhere forge-neutral so the workflow files can go — is a
refactor in its own right (RURL-vunvxusf), not part of a URL migration. Until
then, read those files as manifests, not as promises that anything runs.

`lintr::lint_package()` must stay clean. `.lintr` mirrors the linter set
`goodpractice::gp()` runs, and its header documents every intentional
deviation — read that header before "fixing" a lint or adding a linter.

## Protected code

Four areas are frozen by ADR. Read the ADR before editing:

- Punycode helpers in `R/domain.R` — [ADR 0002](design/adr/0002-keep-punycode-helpers.md)
- PSL delegation seam in `R/domain.R` — [ADR 0001](design/adr/0001-delegate-psl-to-pslr.md)
- The retained base-R string operations — [ADR 0005](design/adr/0005-intentional-base-r-string-exceptions.md)
- `safe_parse_url()` columns; diagnostics stay companion helpers — [ADR 0006](design/adr/0006-diagnostics-companion-helpers-only.md)

## Where things live

- [ARCHITECTURE.md](ARCHITECTURE.md) — load order, file map, parse data flow,
  the Stage-A/Stage-B split, seams, caches. Read before working on the parser.
- [CONTRIBUTING.md](CONTRIBUTING.md) — issue and PR workflow, validation policy,
  CRAN release checklist.
- [design/](design/) — ADRs for *why* a decision holds, accepted PRDs for *what*
  was specified.
