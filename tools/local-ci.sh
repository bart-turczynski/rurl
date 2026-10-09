#!/usr/bin/env bash
#
# Local stand-in for the GitLab runner (RURL-psqmlgjf).
#
# WHY THIS EXISTS. It was written while `.gitlab-ci.yml` was paused (2026-08-08
# to 2026-09-03, re-enabled after the monthly minute reset under RURL-utsbwfvc;
# jobs have been pinned to self-hosted runners since 2026-09-30): the
# free-tier compute allowance ran out mid-slice and every pipeline after it
# failed for a reason that had nothing to do with the code. That left the
# pre-push hook as the only gate, which is not enough, for two reasons that
# hold whether or not the runner is on:
#
#   1. A HOOK IS OPT-IN PER CLONE. `pre-commit install --hook-type pre-push`
#      is a thing a person does, not a property of the repository, so "the
#      history is green" states nothing about whether anything checked it.
#   2. A HOOK VERIFIES THE BRANCH TIP, NOT WHAT LANDS. Merges happen on the
#      forge. A squash-merge produces a commit that has never existed on any
#      machine, and the pipeline on `main` is what checks it.
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
# it is one machine, one architecture, one R. The cross-platform matrix, rhub
# and the README re-render run nowhere: their GitHub workflows were deleted
# (RURL-vunvxusf), and the determinism matrix survives only as the record
# under `tools/determinism/gha/`. And it is pull-based -- nothing makes it
# run, so it carries the same "someone has to do it" weakness as the hook.
#
# Usage:
#   tools/local-ci.sh                 # jobs for HEAD, as GitLab would pick them
#   tools/local-ci.sh main            # ... for a named branch, tag or SHA
#   tools/local-ci.sh --list [ref]    # print the plan and exit, run nothing
#   tools/local-ci.sh --all [ref]     # every job, ignoring `rules:`
#   tools/local-ci.sh --keep [ref]    # keep the work tree even on success
#
# Exit status: 0 PASS, or no job applies to the ref; 1 FAIL, or the runner
# could not start; 2 a bad argument, a ref that lacks tools/local-ci-plan.R or
# .gitlab-ci.yml or whose planner is too old for this runner, or a
# `SECRET_JOBS` entry in tools/local-ci-plan.R that is malformed or names no
# job; 3 NONE, every job that applies was skipped for an unset secret, so
# nothing was judged.
#
# THE PLAN COMES FROM THE REF UNDER TEST, NOT THIS CHECKOUT (RURL-ecwpdtci):
# the job list, images, scripts and the `SECRET_JOBS` gate are those of the
# ref's own tools/local-ci-plan.R reading the ref's own .gitlab-ci.yml, so they
# match the code the jobs run against. An uncommitted edit to either file is
# therefore invisible to every mode, `--list` included; the header's `plan:`
# line names the revision planned from. A ref whose planner predates
# `--not-judged` is refused with exit 2. The ref's planner runs on this host,
# outside Docker, with your environment (the secret gate reads it), so run
# only refs whose code you trust.
#
# AFTER A MERGE, run it against what actually landed:
#   git fetch origin main && tools/local-ci.sh --all origin/main
#
# `--all` ignores the jobs' rules. On `main` it now selects the same jobs as a
# plain run: `check`, `coverage` and `pages` run on every push to `main`
# (ffd9bb3), where `check` once waited for a tag or a hand-started pipeline to
# ration billed minutes. It still matters on any other ref, where `pages`'
# rules skip it. Jobs that only a schedule starts (`full-check`, `floor-check`
# and the dependency audits) stay out either way: `schedule_only()` in
# tools/local-ci-plan.R.
#
# A JOB THAT NEEDS A CI SECRET IS SKIPPED, NOT FAILED, WHEN THE SECRET IS UNSET
# HERE (RURL-hlcpduoq). The planner owns that gate and its one list of
# job-to-secret entries, `SECRET_JOBS` in tools/local-ci-plan.R, which says why
# (RURL-bsfwpfil): `--list` shows such a job as NOT JUDGED with the variable
# named, and a run prints it as SKIPPED, keeps it out of the judgment the other
# jobs make, and names it on the VERDICT line. A declared secret is never
# forwarded into the container. The gate runs before the clone, so a run in
# which every job is skipped costs nothing and ends NONE.

set -euo pipefail

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

PLAN_DIR=""
WORK=""
FAILED=""

cleanup() {
  if [ -n "$PLAN_DIR" ]; then
    rm -rf "$PLAN_DIR"
  fi
  if [ -z "$WORK" ]; then
    return 0
  fi
  if [ "$KEEP" -eq 1 ] || [ -n "$FAILED" ]; then
    echo "work tree kept: $WORK"
  else
    rm -rf "$WORK"
  fi
}
trap cleanup EXIT

# THE PLAN COMES FROM THE REF UNDER TEST, NOT FROM THIS CHECKOUT
# (RURL-ecwpdtci). The jobs run in a clone at $SHA, so the job list, images,
# scripts and the secret gate must come from $SHA's planner reading $SHA's CI
# config; planning from the working tree mixed two revisions whenever the ref
# was not the checkout (`--all origin/main` from a feature branch) or the tree
# was dirty. The planner reads `.gitlab-ci.yml` from its working directory and
# nothing else from the tree, so the two files are copied out of $SHA into a
# scratch folder with the same layout and every planner call runs there. A ref
# that lacks either file stops here: falling back to the working tree's copy is
# the defect this replaces. Still no clone and no Docker, so `--list` stays
# cheap.
PLAN_DIR="$(mktemp -d "${TMPDIR:-/tmp}/rurl-local-ci-plan.XXXXXX")"
mkdir -p "$PLAN_DIR/tools"
for PLAN_FILE in tools/local-ci-plan.R .gitlab-ci.yml; do
  if [ "$(git cat-file -t "${SHA}:${PLAN_FILE}" 2>/dev/null)" != "blob" ]; then
    echo "local-ci: ${REF} (${SHA}) has no ${PLAN_FILE} -- the plan comes" \
      "from the ref under test, never from the working tree" >&2
    exit 2
  fi
  git show "${SHA}:${PLAN_FILE}" > "$PLAN_DIR/$PLAN_FILE"
done

# This runner speaks to the ref's planner, so the ref's planner must know every
# mode the runner asks for. One answers wrong rather than failing: a planner
# from before RURL-bsfwpfil has no `--not-judged` and falls through to its
# default, the selected job list, which would be reported as not judged while
# those same jobs ran -- and that planner judged no secrets at all. The other
# modes predate this runner's history.
if ! grep -qF '"--not-judged"' "$PLAN_DIR/tools/local-ci-plan.R"; then
  echo "local-ci: ${REF} (${SHA})'s tools/local-ci-plan.R predates" \
    "--not-judged (RURL-bsfwpfil), which this runner needs -- run that ref's" \
    "own tools/local-ci.sh from a checkout of it" >&2
  exit 2
fi

plan() { (cd "$PLAN_DIR" && Rscript tools/local-ci-plan.R "$@"); }

echo "rurl local CI runner -- $(date '+%Y-%m-%d %H:%M:%S')"
echo "ref: ${REF} -> ${SHA}  branch='${BRANCH}' tag='${TAG}'"
echo "plan: tools/local-ci-plan.R and .gitlab-ci.yml as of ${SHA}"
echo

if [ "$LIST_ONLY" -eq 1 ]; then
  plan --list "${PLAN_ARGS[@]}"
  exit 0
fi

# `--jobs` lists the jobs to run, after the secret gate, and exits 3 when every
# job that applies is not judged; `--not-judged` names those it left out. The
# planner reads this process's environment, which holds only what the caller
# exported, as a real job's would.
PLAN_STATUS=0
JOBS="$(plan --jobs "${PLAN_ARGS[@]}")" || PLAN_STATUS=$?
case "$PLAN_STATUS" in
  0|3) ;;
  *) exit "$PLAN_STATUS" ;;
esac
SKIPPED="$(plan --not-judged "${PLAN_ARGS[@]}")"
if [ -n "$SKIPPED" ]; then
  echo "--- SKIPPED, not judged: $SKIPPED"
  echo
fi
if [ "$PLAN_STATUS" -eq 3 ]; then
  echo "VERDICT: NONE -- not judged: $SKIPPED"
  exit 3
fi
if [ -z "$JOBS" ]; then
  echo "no job applies to this ref -- nothing to run"
  exit 0
fi
NOT_JUDGED="${SKIPPED:+ -- not judged: $SKIPPED}"

# Docker only from here: planning, `--list`, a bad SECRET_JOBS entry (exit 2)
# and a NONE run all answer without it.
command -v docker >/dev/null 2>&1 || {
  echo "docker is required: the point is to run the job in CI's image" >&2
  exit 1
}
docker info >/dev/null 2>&1 || {
  echo "the docker daemon is not reachable -- start Docker and retry" >&2
  exit 1
}

WORK="$(mktemp -d "${TMPDIR:-/tmp}/rurl-local-ci.XXXXXX")"

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
  IMAGE="$(plan --image "$JOB")"
  {
    echo "set -ex"
    plan --script "$JOB"
  } > "$WORK/$JOB.sh"

  echo "=== job: $JOB (image: $IMAGE) ==============================="
  START=$(date +%s)
  # /ci is read-only so a job cannot rewrite its own script mid-run; /repo is
  # the throwaway clone, so a job that dirties the tree costs nothing.
  # bash when the image has it, POSIX sh otherwise -- as GitLab's own shell
  # detection does. `python:3.13-alpine` (citation-version) ships only busybox
  # sh, and a bare `bash` there fails before the job starts (RURL-dswufxky).
  # `exec` keeps the job's exit status as the container's.
  if docker run --rm \
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

if [ -n "$FAILED" ]; then
  echo "VERDICT: FAIL --${FAILED}${NOT_JUDGED}"
  exit 1
fi
echo "VERDICT: PASS${NOT_JUDGED} (one machine, one architecture, one R -- see the header)"
