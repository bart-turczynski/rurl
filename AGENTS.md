# AGENTS.md

`rurl` is an R package for parsing, normalizing, cleaning, and joining URLs —
standards-conformant parsing (WHATWG / RFC 3986 profiles), with domain and
public-suffix extraction delegated to `pslr`.

**Before touching parse behavior or a conformance test, read
[design/posture-card.md](design/posture-card.md)** — one page: the exact
argument recipe for each posture, where posture gets lost, and the definition
of done for a conformance change. Decisions no gate can derive live in
[design/work/url-v3/registers/rulings.md](design/work/url-v3/registers/rulings.md);
its standing rule says which ones an agent may take by citing a clause.

## Verifying a change

```sh
Rscript tools/verify.R      # full gate; --fast = gates + lint, --list = plan
Rscript tools/verify.R --verbose   # …and print every step's log, passing included
```

`tools/verify.R` reproduces CI's fast gate locally and is the bar a delivered
slice has to clear. `--fast` is iteration feedback, never sufficient on its own.
`devtools::test()` is not a substitute: it ignores `Collate:` and runs against
the source tree, so a green suite can hide a package that does not build. The
gate runs as a pre-push hook once you run `pre-commit install --hook-type
pre-push` in your clone — committing the config does not install it.

**The blocking-step count is diff-dependent, and a lower one is not evidence
that a gate was bypassed.** `tools/verify.R` reports around 36 blocking steps on
a clean `main` and around 19 on a typical feature branch, because the
`[gate-self-tests]` stage runs a self-test only for the gate *implementations*
the diff touched. A doc-only branch legitimately shows the lower number; a
branch editing one gate shows one more. Read `0 failed` and the `VERDICT` line,
never the step count.

**A step's output is printed only when it fails** — a passing gate that dumps
40 lines is how a real failure scrolls past. The cost is that anything real
which does not change an exit status is invisible: a testthat `WARN` reports
`PASS` and its output is discarded (`RURL-aajradge` survived a green 40-step run
that way). Two escape hatches, both proven by `Rscript tools/verify.R
--self-test`:

- `--verbose` prints every step's log **in full**, pass or fail. Reach for it
  whenever the question is "did anything warn", not "did anything fail".
- a step may declare `watch = <regex>`; on a PASS whose log matches, the matched
  block prints under a `!` marker. The `LC_ALL=C` test step carries one for
  testthat's warnings section, so that step is honest by default.

The hook's `entry` is `tools/verify-on-push.sh`, not `tools/verify.R`. That
wrapper looks at the destination remote and **skips the gate when the
destination is a directory on this filesystem** — the local archival mirror
`backup`, whose history already passed the gate on its way to `origin`, and
where a run would only burn minutes and rewrite `_snaps/` and fixture bytes in
your tree (RURL-qkowfsdt). A push to `origin` — or to any remote the wrapper
cannot positively identify as a local mirror — runs the full gate unchanged.
The skip is scoped to mirrors; it is not a way to push unverified work.

**How that wrapper learns the destination is not how git documents it.** Git
hands a `pre-push` hook `$1` = remote name and `$2` = remote URL, but
`pre-commit` consumes both itself and re-exports them as
`PRE_COMMIT_REMOTE_NAME` / `PRE_COMMIT_REMOTE_URL`, invoking the `entry` with
**argc=0**. A wrapper written to the documented `$1`/`$2` contract alone would
never fire — it would always run the gate and never skip, silently.
`tools/verify-on-push.sh` reads positional first, environment second, and
**fails open** — running the gate — when neither is present. Verify with a probe
hook which mechanism actually delivers the values before writing another
pre-push predicate.

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

An rOpenSci **pre-submission inquiry** (software-review #781, opened
2026-06-26) is still **open** — a scope-and-fit question, not an active review;
siblings `punycoder` #779 and `pslr` #780 are in the same state. Run locally,
`pkgcheck()` reports **"no CI"**, which is a false negative from a missing
`GITHUB_PAT`: its badge probe cannot reach the Actions API, while `has_ci` is
`TRUE`. Do not chase it.

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

**When a test reaches into `pslr`, `punycoder` or `raddr`, check two things, not
one.** First that the export exists in the **released** version the
`DESCRIPTION` floor admits — `getNamespaceExports()` on your machine proves
nothing, because locally the dependency *is* the development tree. Then that the
**capability** exists there: read the released function's formals and its C++
source. A skip guard that looks like it leaves a coverage gap may leave none at
all, because the property is undefined on what actually ships — released
`punycoder` compiles one Unicode table, so "invariant across offered Unicode
versions" is not untested there, it is *empty*. Name the **dev** version in the
guard, not the next release: the next release skips on every dev checkout, which
is the only machine where the assertion can run.

The submission order these floors sit inside is
[design/release-chain.md](design/release-chain.md).

### `cran-comments.md`, and the half of its gate that needs the network

```sh
Rscript tools/cran-comments-gate.R            # in the gate list, network-free
Rscript tools/cran-comments-gate.R --online   # release time only
```

`cran-comments.md` was untracked until 2026-08-16 (`e029b11` removed it and
`.gitignore` kept it out), so nothing could see it and it drifted **eleven
releases** — announcing a "1.4.0 -> 2.2.0" release when CRAN held 1.2.0 and
`DESCRIPTION` said 3.0.0, instructing removal of a `Remotes:` field that was
already gone, and quoting a stale `pslr` floor. It is now tracked *and* gated
(RURL-ladruqhn).

The offline half rides `tools/verify.R` like every other gate. The **`--online`
half does not**, for the same reason the resolvability gate does not: `from=` is
a claim about what CRAN publishes, and only the network can settle it. Run
`--online` before a release. The span itself lives in one machine-readable pin
in the file, `<!-- submission-span: from=X to=Y -->`, and the gate additionally
requires both versions to appear in the visible prose — so the pin and the
sentences a reviewer actually reads cannot drift apart.

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

## Design posture

Standing owner mandate. ADRs 0007 and 0016 assume it; none of them states it.

**On standards-conformance work, the conformant behavior ships as the DEFAULT,
never as an opt-in compatibility flag.** A behavior a standard calls invalid is
a bug even when it has shipped, and preserving it trades away the one thing this
package is for. Record the break in `NEWS.md` citing the standard and the
clause, and check the reverse-dependency gates before moving a floor. "Keep the
old output for compatibility" is not a serious option to put on the table.

The CRAN backward-compatibility constraint governs **gratuitous API churn** and
nothing else. In particular it does not reach `url_standard = NULL`, whose
freeze is a compatibility *promise* rather than a conformance *claim* — the NULL
arm names no standard, so there is nothing for it to be wrong against
([ADR 0007](design/adr/0007-url-standard-selector.md), with the causation
boundary in [ADR 0016](design/adr/0016-null-freeze-binds-selector-caused-drift.md)).

## Reading the tracker

`done` on this repository's fp tracker often means **closed as a record**, not
"question answered". The 2026-07-31 backlog migration closed issues with
verbatim boilerplate as the live questions moved to an outcome carrier: only
three of RURL-unroqtac's nine closed children are actually answered.

So read **closure comments**, never status fields, and treat an epic whose
children are all `done` as a claim to verify rather than a signal to close.
Skipping this produced a wrong claim in two consecutive handovers. The cheapest
disproof is usually a date on the closure comment.

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
  was specified. Its [README](design/README.md) lists the whole tree; four
  standing method notes are worth naming here:
  - [design/measurement-traps.md](design/measurement-traps.md) — how a green
    test, gate or harness returns a plausible wrong number. Read before building
    or extending any instrument.
  - [design/oracle-pinning.md](design/oracle-pinning.md) — what a `verified` pin
    asserts, and what has to happen before the string is written.
  - [design/oracle-fixtures.md](design/oracle-fixtures.md) — which columns of
    `external-url-vectors.csv` are posture-scoped claims and must not be
    "corrected".
  - [design/release-chain.md](design/release-chain.md) — the seven-package CRAN
    submission order and the sibling-pinning rule.
