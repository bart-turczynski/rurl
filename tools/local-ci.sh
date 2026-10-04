#!/usr/bin/env bash
#
# Local stand-in for the GitLab runner (RURL-psqmlgjf).
#
# WHY THIS EXISTS. `.gitlab-ci.yml` is paused: the free-tier compute allowance
# ran out mid-slice and every pipeline after it failed for a reason that had
# nothing to do with the code. That left the pre-push hook as the only gate --
# which is the exact situation the CI config's own header calls out as
# insufficient, and for two reasons that are still true with the runner off:
#
#   1. A HOOK IS OPT-IN PER CLONE. `pre-commit install --hook-type pre-push`
#      is a thing a person does, not a property of the repository, so "the
#      history is green" states nothing about whether anything checked it.
#   2. A HOOK VERIFIES THE BRANCH TIP, NOT WHAT LANDS. Merges happen on the
#      forge. A squash-merge produces a commit that has never existed on any
#      machine, and the pipeline that used to check it was the one on `main`.
#
# So this is not a second copy of `tools/verify.R` -- the hook already runs that
# against the working tree, on this machine, in the ambient library. This runs
# the CI JOBS: the same image, the same dependency install, against a CLEAN
# CLONE OF A COMMIT rather than a working tree. That difference is where the
# environment-shaped defects live, and they are not hypothetical here: a missing
# Suggests, a `Collate:` that only a real build reads, an `install.packages()`
# that reports failure as a warning and exits 0. None of them are visible to a
# green suite in a developer's fully-populated library.
#
# WHAT IT STILL DOES NOT COVER, so a green run is not read as more than it is:
# it is one machine, one architecture, one R. The cross-platform matrix, rhub,
# coverage and the README re-render run nowhere: their GitHub workflows were
# deleted (RURL-vunvxusf), and the determinism matrix survives only as the
# record under `tools/determinism/gha/` (pkgdown now publishes from the
# `pages` job in `.gitlab-ci.yml`, release-time only). And it is pull-based --
# nothing makes it run, so it carries the same "someone has to do it"
# weakness as the hook. It narrows the gap the paused runner opened; it does
# not close it.
#
# Usage:
#   tools/local-ci.sh                 # jobs for HEAD, as GitLab would pick them
#   tools/local-ci.sh main            # ... for a named branch, tag or SHA
#   tools/local-ci.sh --list [ref]    # print the plan and exit, run nothing
#   tools/local-ci.sh --all [ref]     # every job, ignoring `rules:`
#   tools/local-ci.sh --keep [ref]    # keep the work tree even on success
#
# AFTER A MERGE, run it against what actually landed:
#   git fetch origin main && tools/local-ci.sh --all origin/main
#
# `--all` IS LOAD-BEARING THERE, not belt-and-braces. The `check` job's rules
# select a tag or a hand-started pipeline and nothing else, so honoring them on
# `main` would run the cheap half and silently skip the `R CMD check --as-cran`
# that is the whole reason to look at a merge commit. Those rules exist to
# ration billed minutes on the forge; locally the minutes are free, so the
# rationing is exactly what you want to override.
#
# A JOB THAT NEEDS A CI SECRET IS SKIPPED, NOT FAILED, WHEN THE SECRET IS UNSET
# HERE (RURL-hlcpduoq). Such a secret lives only as a GitLab CI/CD variable, so
# locally the job fails on every run whatever the commit holds, and a verdict
# that is always red says nothing. `SECRET_JOBS` below declares each such job as
# `job:VARIABLE`; a job may have several entries. When one of its variables is
# unset or empty, the job prints as SKIPPED, names the variable, and stays out
# of the verdict, which the other jobs decide (the rule seor's
# `scripts/check-fleet-standard.py` follows: not judged, not a gap). When all
# are set, the job runs with them passed into its container and can still fail.
# The next secret-gated job needs one entry there, nothing else.

set -euo pipefail

SECRET_JOBS="fossa:FOSSA_API_KEY"

usage() { sed -n '/^# Usage:/,/^#$/p' "$0" | sed 's/^# \{0,1\}//'; }

REF=""
LIST_ONLY=0
IGNORE_RULES=0
KEEP=0

while [ $# -gt 0 ]; do
  case "$1" in
    --list) LIST_ONLY=1 ;;
    --all) IGNORE_RULES=1 ;;
    --keep) KEEP=1 ;;
    -h|--help) usage; exit 0 ;;
    -*) echo "unknown flag: $1" >&2; usage >&2; exit 2 ;;
    *)
      if [ -n "$REF" ]; then
        echo "at most one ref, got '$REF' and '$1'" >&2
        exit 2
      fi
      REF="$1"
      ;;
  esac
  shift
done
REF="${REF:-HEAD}"

ROOT="$(git rev-parse --show-toplevel)"
cd "$ROOT"

command -v docker >/dev/null 2>&1 || {
  echo "docker is required: the point is to run the job in CI's image" >&2
  exit 1
}
docker info >/dev/null 2>&1 || {
  echo "the docker daemon is not reachable -- start Docker and retry" >&2
  exit 1
}

SHA="$(git rev-parse --verify "${REF}^{commit}")"

# GitLab sets exactly one of $CI_COMMIT_BRANCH / $CI_COMMIT_TAG, and the `check`
# job's rules read both. Getting this wrong would silently run the cheap half of
# the pipeline on a tag, so resolve it from the ref rather than assuming.
TAG="$(git describe --exact-match --tags "$SHA" 2>/dev/null || true)"
BRANCH=""
if [ -z "$TAG" ]; then
  case "$REF" in
    HEAD) BRANCH="$(git symbolic-ref --short -q HEAD || true)" ;;
    origin/*) BRANCH="${REF#origin/}" ;;
    *)
      if git show-ref --verify -q "refs/heads/$REF" ||
         git show-ref --verify -q "refs/remotes/origin/$REF"; then
        BRANCH="$REF"
      fi
      ;;
  esac
fi

PLAN_ARGS=(--branch "$BRANCH" --tag "$TAG" --source push)
[ "$IGNORE_RULES" -eq 1 ] && PLAN_ARGS+=(--all)

echo "rurl local CI runner -- $(date '+%Y-%m-%d %H:%M:%S')"
echo "ref: ${REF} -> ${SHA}  branch='${BRANCH}' tag='${TAG}'"
echo

if [ "$LIST_ONLY" -eq 1 ]; then
  exec Rscript tools/local-ci-plan.R --list "${PLAN_ARGS[@]}"
fi

JOBS="$(Rscript tools/local-ci-plan.R --jobs "${PLAN_ARGS[@]}")"
if [ -z "$JOBS" ]; then
  echo "no job applies to this ref -- nothing to run"
  exit 0
fi

WORK="$(mktemp -d "${TMPDIR:-/tmp}/rurl-local-ci.XXXXXX")"
FAILED=""
SKIPPED=""
RAN=0

cleanup() {
  if [ "$KEEP" -eq 1 ] || [ -n "$FAILED" ]; then
    echo "work tree kept: $WORK"
  else
    rm -rf "$WORK"
  fi
}
trap cleanup EXIT

# A CLONE, NOT A WORKTREE, and not the checkout you are sitting in. Three
# reasons, each of which has a matching defect class:
#   * untracked and ignored files (`_scratch/`, a stray fixture) are invisible
#     to the forge but visible to every gate that scans the tree;
#   * `git worktree` writes `.git` as a FILE, and `tools/verify.R` requires a
#     `.git` DIRECTORY -- it would refuse to start;
#   * a clone keeps `origin/main`, which is what verify.R diffs against to
#     select gate self-tests. This matters for the `check` job only: its apt
#     line transitively installs git, while the gates job's does not, so the
#     gates job cannot diff at all and runs every self-test. That asymmetry is
#     real CI behavior, not an artifact of running locally -- reproducing it is
#     the point.
git clone --quiet --no-hardlinks "$ROOT" "$WORK/repo"
git -C "$WORK/repo" checkout --quiet --detach "$SHA"

for JOB in $JOBS; do
  # The job's declared secrets (header): pass the set ones into the container,
  # skip the job if any is missing. `-e NAME` copies the value from this
  # environment, so it never appears on a command line.
  MISSING=""
  SECRET_ARGS=()
  for ENTRY in $SECRET_JOBS; do
    if [ "${ENTRY%%:*}" = "$JOB" ]; then
      VAR="${ENTRY#*:}"
      if [ -n "${!VAR:-}" ]; then
        SECRET_ARGS+=(-e "$VAR")
      else
        MISSING="$MISSING $VAR"
      fi
    fi
  done
  if [ -n "$MISSING" ]; then
    echo "=== job: $JOB ==============================="
    echo "--- $JOB SKIPPED, not judged: CI secret unset locally:${MISSING}"
    echo
    SKIPPED="$SKIPPED $JOB"
    continue
  fi
  RAN=$((RAN + 1))

  IMAGE="$(Rscript tools/local-ci-plan.R --image "$JOB")"
  {
    echo "set -ex"
    Rscript tools/local-ci-plan.R --script "$JOB"
  } > "$WORK/$JOB.sh"

  echo "=== job: $JOB (image: $IMAGE) ==============================="
  START=$(date +%s)
  # /ci is read-only so a job cannot rewrite its own script mid-run; /repo is
  # the throwaway clone, so a job that dirties the tree costs nothing.
  # bash when the image has it, POSIX sh otherwise -- as GitLab's own shell
  # detection does. `python:3.13-alpine` (citation-version) ships only busybox
  # sh, and a bare `bash` there fails before the job starts (RURL-dswufxky).
  # `exec` keeps the job's exit status as the container's. The `+` expansion
  # keeps an empty SECRET_ARGS legal under `set -u` on macOS's bash 3.2.
  if docker run --rm \
      ${SECRET_ARGS[@]+"${SECRET_ARGS[@]}"} \
      -v "$WORK/repo:/repo" \
      -v "$WORK:/ci:ro" \
      -w /repo \
      "$IMAGE" sh -c \
        'if command -v bash >/dev/null 2>&1; then exec bash "$1"; fi; exec sh "$1"' \
        sh "/ci/$JOB.sh"; then
    echo "--- $JOB PASS ($(( $(date +%s) - START ))s)"
  else
    echo "--- $JOB FAIL ($(( $(date +%s) - START ))s)"
    FAILED="$FAILED $JOB"
  fi
  echo
done

[ -n "$SKIPPED" ] && echo "not judged (CI secret unset locally):${SKIPPED}"
if [ "$RAN" -eq 0 ]; then
  echo "VERDICT: NONE -- every job was skipped, nothing was judged"
  exit 0
fi
if [ -n "$FAILED" ]; then
  echo "VERDICT: FAIL --${FAILED}"
  exit 1
fi
echo "VERDICT: PASS (one machine, one architecture, one R -- see the header)"
