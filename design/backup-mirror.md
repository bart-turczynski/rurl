# The local archival mirror

Standing record for the bare mirror at `~/Projects/_backups/rurl.git`, wired as
the `backup` remote. Filed under RURL-ivsyshdj, repaired under RURL-eqgqbeti.

`rurl` has **no server-side gate**. GitLab CI is paused for quota
(RURL-utsbwfvc) and the `github` remote points at a suspended account whose
fetch returns 403, so this mirror is the only copy of the history that is not
the working clone. That makes its freshness load-bearing — and makes a mirror
that is stale *without saying so* worse than no mirror at all, because absence
is at least honest.

## How it stays fresh

| | |
|---|---|
| Refresh | `tools/mirror-refresh.sh`, wired as a pre-commit **`post-merge`** hook (`.pre-commit-config.yaml`, id `mirror-refresh`) |
| Check | `tools/mirror-freshness.sh` — **exits non-zero** when a mirror's `refs/heads/main` does not contain the forge's `main` |
| Install | `pre-commit install --hook-type post-merge` (alongside `--hook-type pre-push` for the verify gate) |

By hand, any time:

```sh
tools/mirror-freshness.sh            # exit 0 = fresh, exit 1 = drifted
tools/mirror-refresh.sh              # refresh, then assert it took
```

Both find the mirror by the same predicate `tools/verify-on-push.sh` uses — a
remote whose **URL is a directory on this filesystem** — not by the name
`backup`, which is a local alias anyone can rename.

**What the hook does not guarantee.** It is installed per clone, so a fresh
clone has it only once someone runs `pre-commit install`; it fires on `git
merge`/`git pull` but not on `git pull --rebase`, `git fetch` + `git reset`, or
`git checkout`; and git ignores a `post-merge` hook's exit status by design, so
a failure is loud but not blocking. The freshness check is therefore the
load-bearing half — it is the thing that turns "stale" from silence into a
non-zero exit. Read the header of `tools/mirror-refresh.sh` for why `post-merge`
was chosen over `post-commit` and over a cron job.

## Two hazards

**Never `git fetch` inside the mirror.** It was made with `git clone --mirror`
from the working clone, so its `remote.origin.fetch` is `+refs/*:refs/*` and
`remote.origin.mirror` is `true`. A fetch there would force-overwrite every ref
from whatever the working clone currently holds — including the branch tips
below, which exist nowhere else. Refresh it only by **pushing to it**, which is
what `tools/mirror-refresh.sh` does.

**Nothing is ever deleted, pruned or forced.** `tools/mirror-refresh.sh` uses
plain `git push` with no `--prune` and no `--force`: a local branch that was
rebased since its last archival push is reported as a warning and the mirror
keeps its copy. Holding refs found nowhere else is the entire point of this
repository.

## `refs/remotes/origin/*` is a pre-migration GitHub snapshot — not current forge state

The mirror is a `--mirror` clone, so it carries the working clone's
remote-tracking refs as well as its branches. Those refs are **frozen at clone
time and are never refreshed by anything**, and — this is the part that misleads
— they were captured while `origin` still meant **GitHub**.

Measured 2026-08-09, the mirror's `refs/remotes/origin/*` is byte-identical to
the working clone's `refs/remotes/github/*` (same eight refs, same SHAs), which
is the remote that RURL-sgkplmot renamed `origin` → `github` when rurl's public
identity moved to GitLab (`0df6cce`, 2026-08-08).

| Mirror ref | Object | Reality |
|---|---|---|
| `refs/remotes/origin/main` | `c0c8dca` (2026-07-26) | **186 commits behind** the real `origin/main`; it is GitHub's last-seen `main` |
| `refs/remotes/origin/HEAD` | `c0c8dca` | same |
| `refs/remotes/origin/gh-pages` | `b16b091` (2026-07-03) | a GitHub Pages branch; GitLab has no such branch |
| `refs/remotes/origin/chore/cran-2.7.0` | `9d67d0e` | closed GitHub topic branch |
| `refs/remotes/origin/chore/g3-closure-roster-split` | `f544978` | closed GitHub topic branch |
| `refs/remotes/origin/chore/seal-g3-acceptance-2` | `07737cb` | closed GitHub topic branch |
| `refs/remotes/origin/chore/seal-p0.6-g3-acceptance-3` | `584d800` | closed GitHub topic branch |
| `refs/remotes/origin/docs/de4-migration-table` | `ed8d439` | still open, but on GitLab now |

**Read this namespace as an archive of the old forge, never as the state of the
current one.** It is kept, not refreshed: refreshing it would mean fetching into
the mirror, which the hazard above rules out, and the snapshot has archival
value of its own — it is the only record of what GitHub held before the account
was suspended. The current forge's state is `refs/heads/*` on the mirror, which
`tools/mirror-refresh.sh` pushes.

## Branch-tip triage

Eight branch tips exist on the mirror and not on `origin`. Evidence is
`git cherry -v origin/main <tip>`: a `-` line means that patch already has an
equivalent upstream, a `+` line means it does not. **`-` proves redundancy; `+`
proves nothing** — a commit that landed via squash or rebase gets a different
patch-id and shows `+` even though its content is on `main`. So the two verdicts
are not symmetric: a `+` row cannot be settled by `git cherry` at all, and is
settled below by comparing the tip's actual content against `main`.

Measured 2026-08-09 against `origin/main` = `62c9fda`.

| Mirror branch | Tip | Date | `git cherry` | Verdict |
|---|---|---|---|---|
| `chore/lean-rurl-operating-model` | `6bc058d` | 2026-08-01 | 81 `-`, 0 `+` | **Abandoned.** Every patch is already upstream; the branch landed and only the tip was left behind. |
| `feat/v3-protocol-hardening` | `fa5355f` | 2026-08-01 | 107 `-`, 0 `+` | **Abandoned.** Same — fully upstream. |
| `fix/oracle-spec-pin` | `fa5355f` | 2026-08-01 | 107 `-`, 0 `+` | **Abandoned.** A second name for the same commit as the row above. |
| `fix/verification-feedback-time` | `a8d109d` | 2026-08-01 | 83 `-`, 0 `+` | **Abandoned.** Same — fully upstream. |
| `chore/seal-p0.6-g3-acceptance-3` | `584d800` | 2026-07-26 | 0 `-`, 1 `+` | **Abandoned, already declared.** `origin` carries the tag `abandoned/seal-p0.6-g3-acceptance-3` at this exact commit, so the ruling exists on the forge and the object is not mirror-only. It is a phase-3 *seal*, and [ADR 0014](adr/0014-retire-the-v3-control-plane.md) retired seals entirely. |
| `chore/drop-remotes` | `de68e2d` | 2026-07-27 | 0 `-`, 1 `+` | **Abandoned** (owner ruling, 2026-08-09). All three files it touched are accounted for on `main`: `DESCRIPTION` has no `Remotes:` block; the `NEWS.md` bullet it added is present verbatim (`NEWS.md:1243`); and the one `CLAUDE.md` line it edited no longer exists anywhere, that file having since become a thin `@AGENTS.md` import — so that edit is moot rather than lost. |
| `docs/p0.11-forge-binding` | `85d4b38` | 2026-08-01 | 0 `-`, 2 `+` | **Abandoned** (owner ruling, 2026-08-09). `design/work/url-v3/decisions/P0.11-forge-binding-ratifying-identity.md` at this tip is **byte-identical** to the copy on `origin/main`, so the content landed. |
| `fix/error-clean-url-invariant` | `70e1eab` | 2026-08-02 | 0 `-`, 2 `+` | **Abandoned** (owner ruling, 2026-08-09). The `clean_url[parse_status == "error"] <- NA_character_` seam and its comment are present verbatim in `R/parse.R` on `origin/main` (line 2548), so the fix landed. |

Two further mirror branches, `chore/sibling-durability` (`66978d4`) and
`docs/de4-migration-table` (`ed8d439`), are **not** mirror-only — both are live
open branches on `origin`.

Nothing above was deleted, and nothing is to be. Every tip is now dispositioned,
but "abandoned" here means *nothing on it needs resurrecting onto `origin`* — it
is not an instruction to prune. The mirror keeps all of them, which is the point
of a mirror: the cost of holding a redundant tip is a few kilobytes, and the cost
of dropping the one that was not redundant is unrecoverable.

The three rows settled by content rather than by `git cherry` are the standing
example of why the asymmetry above matters. Each showed `+` — the signal that
normally means "carries unmerged work" — purely because the branch reached `main`
through a squash, which rewrites the patch-id. Read a `+` as *unproven*, never as
*unmerged*, and check the content before acting on it.

### Other refs

- The three `abandoned/*` tags (`gitlab-ci-probe`, `seal-p0.11`,
  `seal-p0.6-g3-acceptance-3`) all exist **on `origin` too**, at the same
  objects. Their status is carried in the ref name and is not re-triaged here.
- `refs/cmux/last-turn/13f17fc…` is an agent-harness ref that got picked up by
  the `--mirror` clone. It is not project history.

Ref census, 2026-08-09: **39 refs** — 11 `refs/heads`, 19 `refs/tags`,
8 `refs/remotes`, 1 `refs/cmux`.
