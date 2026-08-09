#!/usr/bin/env bash
#
# Refresh the local archival mirror(s), then assert it took (RURL-eqgqbeti).
#
# WHY THIS EXISTS, AND WHY IT IS A post-merge HOOK.
#
# RURL-ivsyshdj wired the `backup` remote with a one-time push and left the
# refresh as something a person remembers. It was remembered once. The drift
# that followed was not carelessness: refreshing the mirror used to fire the
# full ~36-step verify gate, because the pre-push hook had no destination
# predicate, so an archival push cost minutes AND rewrote `tests/testthat/_snaps/`
# and byte-pinned fixtures in the working tree. A refresh that expensive is a
# refresh you postpone. RURL-qkowfsdt removed that cost.
#
# So this script pushes with a PLAIN `git push` -- no `--no-verify`. That is
# deliberate. `tools/verify-on-push.sh` already recognizes a local-path
# destination and skips the gate for it, and letting the push go through the
# hook is what keeps that predicate exercised. `--no-verify` here would work by
# defeating the gate rather than composing with it, and would go on "working"
# unchanged on the day the predicate breaks.
#
# THE MECHANISM CHOSEN, of the three the issue allowed (post-merge/post-commit
# hook, scheduled job, documented manual step):
#
#   a pre-commit `post-merge` hook -- `.pre-commit-config.yaml`, id `mirror-refresh`.
#
# WHY post-merge. The event that makes the mirror stale is history arriving,
# and on this repo history arrives exactly one way: merges happen on the forge,
# and they reach this machine through `git pull` / `git merge`. `post-merge` is
# the hook git runs at that moment. post-commit would fire on every local commit,
# most of which are not yet anywhere worth archiving; a cron job would need to
# be installed outside the repository, where nothing in a clone can describe or
# review it, and would run against whatever branch happened to be checked out.
#
# WHY pre-commit RATHER THAN A HAND-DROPPED .git/hooks/post-merge. This repo
# already routes its hooks through pre-commit, so the config is committed,
# reviewed and diffable, and one `pre-commit install` line covers it. A file
# copied into `.git/hooks/` by hand is invisible to every other clone.
#
# WHAT THIS DOES NOT GUARANTEE -- stated plainly, because the issue it closes is
# about false confidence:
#
#   * IT IS NOT AUTOMATIC IN A FRESH CLONE. It is installed per clone, with
#     `pre-commit install --hook-type post-merge`, exactly like the verify gate
#     is installed with `--hook-type pre-push`. Committing the config does not
#     install anything. This is the same opt-in weakness `tools/local-ci.sh`
#     names in its own header, and it is not fixable from inside a repository.
#   * IT ONLY FIRES ON A MERGE. `git pull --rebase` runs post-rewrite, not
#     post-merge; `git fetch` followed by `git reset --hard`, and a plain
#     `git checkout`, run neither.
#   * A post-merge hook CANNOT FAIL THE MERGE -- git ignores its exit status by
#     design. A failure here is loud (pre-commit prints it), not blocking.
#
# Which is why the loud check is the load-bearing half: `tools/mirror-freshness.sh`
# exits non-zero on drift, is run at the end of every refresh, and is the thing
# to run by hand when you want to know rather than assume.
#
# NOTHING IS EVER DELETED. No `--prune`, no `--mirror`, no force. The mirror
# holds branch tips and `abandoned/*` tags found nowhere else -- see
# design/backup-mirror.md -- and that is its whole value.
#
# Usage:
#   tools/mirror-refresh.sh          # fetch upstream, push every local-path mirror
#   tools/mirror-refresh.sh --check  # check freshness only, push nothing
#
# Exit status:
#   0  every mirror now contains the forge's main
#   1  a mirror could not be brought up to date
#   2  bad usage
#
# Environment:
#   MIRROR_UPSTREAM_REMOTE   remote treated as the forge (default: origin)

set -euo pipefail

usage() { sed -n '/^# Usage:/,/^#$/p' "$0" | sed 's/^# \{0,1\}//'; }

HERE="$(CDPATH='' cd -- "$(dirname -- "$0")" && pwd)"

CHECK_ONLY=0
while [ $# -gt 0 ]; do
  case "$1" in
    --check) CHECK_ONLY=1 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

UPSTREAM="${MIRROR_UPSTREAM_REMOTE:-origin}"

if [ "$CHECK_ONLY" -eq 1 ]; then
  exec "$HERE/mirror-freshness.sh"
fi

# Collect the mirrors first, by the same URL-is-a-local-directory predicate
# tools/verify-on-push.sh and tools/mirror-freshness.sh use. No mirror is not an
# error: a fresh clone has none.
MIRRORS=()
for name in $(git remote); do
  [ "$name" = "$UPSTREAM" ] && continue
  url="$(git remote get-url --push "$name" 2>/dev/null || true)"
  [ -n "$url" ] && [ -d "$url" ] || continue
  MIRRORS+=("$name")
done

if [ "${#MIRRORS[@]}" -eq 0 ]; then
  echo "mirror-refresh: no mirror configured in this clone -- nothing to refresh."
  exit 0
fi

# The mirror is measured against the forge, so learn the forge's state before
# pushing. A fetch failure is survivable (the objects may already be local);
# a stale idea of main is not silently accepted -- mirror-freshness.sh below
# goes to the wire itself and warns if it cannot.
if ! git fetch --quiet "$UPSTREAM" 2>/dev/null; then
  echo "mirror-refresh: WARNING could not fetch '$UPSTREAM'; refreshing from whatever this clone already has" >&2
fi

UPSTREAM_REF="refs/remotes/$UPSTREAM/main"
UPSTREAM_SHA="$(git rev-parse --verify --quiet "$UPSTREAM_REF" || true)"
if [ -z "$UPSTREAM_SHA" ]; then
  echo "mirror-refresh: FAIL no $UPSTREAM_REF in this clone -- cannot establish what main is" >&2
  exit 1
fi

FAILED=0

for name in "${MIRRORS[@]}"; do
  url="$(git remote get-url --push "$name")"
  echo "mirror-refresh: refreshing '$name' ($url)"

  # Everything this clone holds, as a best effort. A local branch that was
  # rebased since its last archival push is a non-fast-forward here and is
  # REPORTED, not forced: the mirror's copy of that tip may be the only one
  # left, and overwriting it would destroy the thing being backed up.
  if ! git push "$name" --all; then
    echo "mirror-refresh: WARNING pushing all local heads to '$name' did not fully succeed (a rebased branch is the usual cause). Nothing was forced; the mirror keeps its copy." >&2
  fi
  if ! git push "$name" --tags; then
    echo "mirror-refresh: WARNING pushing tags to '$name' did not fully succeed." >&2
  fi

  # main is the one ref that is not best-effort: it is what the mirror exists
  # to hold. Pushed from the remote-tracking ref rather than the local branch,
  # so a clone whose `main` is behind still archives what the forge has.
  mirror_main="$(git ls-remote "$url" refs/heads/main 2>/dev/null |
    awk 'NR == 1 { print $1 }')"
  if [ -n "$mirror_main" ] &&
    git cat-file -e "$mirror_main^{commit}" 2>/dev/null &&
    git merge-base --is-ancestor "$UPSTREAM_SHA" "$mirror_main"; then
    echo "mirror-refresh: '$name' refs/heads/main already contains $UPSTREAM/main"
  elif ! git push "$name" "$UPSTREAM_REF:refs/heads/main"; then
    echo "mirror-refresh: FAIL could not advance '$name' refs/heads/main to $UPSTREAM/main (${UPSTREAM_SHA:0:8}). Not forcing -- resolve by hand." >&2
    FAILED=1
  fi
done

# The refresh is not done until something independent says so. This is the loud
# half: it exits non-zero on drift, and that status is this script's status.
echo "mirror-refresh: verifying"
if ! "$HERE/mirror-freshness.sh"; then
  FAILED=1
fi

exit "$FAILED"
