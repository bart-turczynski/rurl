#!/usr/bin/env bash
#
# Loud freshness check for the local archival mirror (RURL-eqgqbeti).
#
# WHY THIS EXISTS. The mirror at ~/Projects/_backups/rurl.git is the durability
# net for a repository with no server-side gate: GitLab CI is paused for quota
# (RURL-utsbwfvc) and the old GitHub remote points at a suspended account whose
# fetch returns 403. RURL-ivsyshdj wired the remote with a ONE-TIME
# `push backup --all && push backup --tags`, and nothing ever refreshed it. By
# 2026-08-08 it was six commits behind and no output anywhere said so.
#
# That is the defect: not that a backup went stale -- backups do -- but that it
# went stale SILENTLY, while still being counted on. A stale backup nobody knows
# about is worse than an absent one, because absence is at least honest. So this
# check EXITS NON-ZERO on drift. A version that only printed would reproduce the
# failure mode it exists to remove.
#
# WHAT COUNTS AS A MIRROR: the same predicate `tools/verify-on-push.sh` uses --
# a remote whose URL is a DIRECTORY ON THIS FILESYSTEM. Not a remote named
# `backup`; a name is a local alias anyone can `git remote rename`, and keying
# on the string would silently detach this check from the thing it checks. The
# URL is what says where objects land, so that is what gets tested, and the
# check picks up a second mirror without an edit here.
#
# The one remote excluded from that sweep is the UPSTREAM itself (`origin`, or
# $MIRROR_UPSTREAM_REMOTE). It is not a mirror candidate by definition: it is
# the reference point the mirrors are measured against, and in a test rig or a
# local-path clone its URL is a directory too.
#
# WHAT "FRESH" MEANS. The mirror's `refs/heads/main` must CONTAIN the forge's
# `main`. Equal is the normal case. Ahead also passes -- a mirror carrying local
# commits not yet on the forge is a superset, which is exactly what a durability
# net is for, and failing there would train people to ignore this. Behind is the
# drift being fixed, and diverged means someone rewrote history; both fail.
#
# THE ONE CARVE-OUT THAT EXITS 0 WITHOUT CHECKING ANYTHING: a clone that
# configures no local-path remote at all. A fresh clone and a CI checkout
# legitimately have none, and there is nothing there to be stale. That is stated
# on stdout rather than passed over in silence, because "no mirror configured"
# and "mirror is current" are different facts and must not read the same.
#
# Deliberately NOT part of `tools/verify.R`. The gate runs pre-push, i.e. before
# the commit under test exists on the forge, so a mirror that correctly does not
# yet have it would fail every push. This is a post-merge property, checked by
# `tools/mirror-refresh.sh` and by hand.
#
# Usage:
#   tools/mirror-freshness.sh            # ask the forge for main, then compare
#   tools/mirror-freshness.sh --offline  # compare against the local origin/main
#
# Exit status:
#   0  every mirror contains the forge's main -- or this clone has no mirror
#   1  a mirror is behind, diverged, unreadable, or has no refs/heads/main
#   2  bad usage
#
# Environment:
#   MIRROR_UPSTREAM_REMOTE   remote treated as the forge (default: origin)

set -euo pipefail

usage() { sed -n '/^# Usage:/,/^#$/p' "$0" | sed 's/^# \{0,1\}//'; }

OFFLINE=0
while [ $# -gt 0 ]; do
  case "$1" in
    --offline) OFFLINE=1 ;;
    -h|--help) usage; exit 0 ;;
    *) echo "unknown argument: $1" >&2; usage >&2; exit 2 ;;
  esac
  shift
done

UPSTREAM="${MIRROR_UPSTREAM_REMOTE:-origin}"

# The forge's main. Asked over the wire, because the whole point is to not
# trust a local copy that may itself be days old. When the wire is unavailable
# the local remote-tracking ref is used instead -- but SAID SO on stderr, so a
# green line is never read as stronger evidence than it is.
UPSTREAM_SHA=""
if [ "$OFFLINE" -eq 0 ]; then
  UPSTREAM_SHA="$(git ls-remote "$UPSTREAM" refs/heads/main 2>/dev/null |
    awk 'NR == 1 { print $1 }')"
  if [ -z "$UPSTREAM_SHA" ]; then
    echo "mirror-freshness: WARNING could not read refs/heads/main from '$UPSTREAM'; falling back to the local refs/remotes/$UPSTREAM/main, which may itself be stale" >&2
  fi
fi
if [ -z "$UPSTREAM_SHA" ]; then
  UPSTREAM_SHA="$(git rev-parse --verify --quiet "refs/remotes/$UPSTREAM/main" || true)"
fi
if [ -z "$UPSTREAM_SHA" ]; then
  echo "mirror-freshness: FAIL cannot resolve '$UPSTREAM' main, neither over the wire nor as refs/remotes/$UPSTREAM/main -- nothing to compare against" >&2
  exit 1
fi

echo "mirror-freshness: $UPSTREAM main = ${UPSTREAM_SHA:0:8}"

FOUND=0
STALE=0

for name in $(git remote); do
  [ "$name" = "$UPSTREAM" ] && continue
  url="$(git remote get-url --push "$name" 2>/dev/null || true)"
  [ -n "$url" ] || continue
  [ -d "$url" ] || continue

  FOUND=$((FOUND + 1))

  have="$(git ls-remote "$url" refs/heads/main 2>/dev/null |
    awk 'NR == 1 { print $1 }')"
  if [ -z "$have" ]; then
    echo "mirror-freshness: FAIL '$name' ($url) has no refs/heads/main -- unreadable, or never received one" >&2
    STALE=1
    continue
  fi

  if [ "$have" = "$UPSTREAM_SHA" ]; then
    echo "mirror-freshness: ok   '$name' refs/heads/main = ${have:0:8} (identical to $UPSTREAM/main)"
    continue
  fi

  # Beyond equality the verdict needs real ancestry, which needs BOTH commits
  # as objects in THIS repository -- the mirror's tip and the forge's. The
  # forge tip is routinely missing, because the wire query above reads a commit
  # this clone may never have fetched; one fetch fixes that and touches only
  # remote-tracking refs. If either is still absent, `--is-ancestor` cannot
  # answer and would fail its way into a bogus "diverged", so say what is
  # actually true: unknown. Unknown fails -- an unverifiable backup is the
  # condition this script exists to stop treating as fine.
  if ! git cat-file -e "$UPSTREAM_SHA^{commit}" 2>/dev/null && [ "$OFFLINE" -eq 0 ]; then
    git fetch --quiet "$UPSTREAM" 2>/dev/null || true
  fi
  for missing in "$UPSTREAM_SHA" "$have"; do
    if ! git cat-file -e "$missing^{commit}" 2>/dev/null; then
      echo "mirror-freshness: FAIL '$name' refs/heads/main = ${have:0:8} differs from $UPSTREAM/main (${UPSTREAM_SHA:0:8}), and ${missing:0:8} is not an object in this clone, so the direction cannot be determined -- run 'git fetch $UPSTREAM' and re-check" >&2
      STALE=1
      continue 2
    fi
  done

  if git merge-base --is-ancestor "$UPSTREAM_SHA" "$have"; then
    ahead="$(git rev-list --count "$UPSTREAM_SHA..$have")"
    echo "mirror-freshness: ok   '$name' refs/heads/main = ${have:0:8}, $ahead ahead of $UPSTREAM/main -- it contains the forge's main, which is what the mirror is for"
  elif git merge-base --is-ancestor "$have" "$UPSTREAM_SHA"; then
    behind="$(git rev-list --count "$have..$UPSTREAM_SHA")"
    echo "mirror-freshness: FAIL '$name' refs/heads/main = ${have:0:8} is $behind commit(s) BEHIND $UPSTREAM/main (${UPSTREAM_SHA:0:8}). Refresh it: tools/mirror-refresh.sh" >&2
    STALE=1
  else
    echo "mirror-freshness: FAIL '$name' refs/heads/main = ${have:0:8} has DIVERGED from $UPSTREAM/main (${UPSTREAM_SHA:0:8}) -- neither contains the other. Someone rewrote history; resolve by hand, do not force-push the mirror." >&2
    STALE=1
  fi
done

if [ "$FOUND" -eq 0 ]; then
  echo "mirror-freshness: no mirror configured in this clone -- no remote's URL is a directory on this filesystem, so there is nothing here to be stale. Wire one with: git remote add backup /path/to/rurl.git"
  exit 0
fi

exit "$STALE"
