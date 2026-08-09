#!/usr/bin/env bash
#
# pre-push entry point for the verify gate (RURL-qkowfsdt).
#
# WHY THIS EXISTS. `.pre-commit-config.yaml` used to spell the pre-push hook as
# `entry: Rscript tools/verify.R` with `always_run: true` and no predicate, so
# the full ~36-step gate fired for EVERY destination -- including
# `git push backup`, whose target is the local bare mirror at
# ~/Projects/_backups/rurl.git. Running the gate there verifies nothing (the
# mirror only ever receives history that already passed on its way to `origin`),
# costs minutes, and MUTATES THE WORKING TREE on the way: a gate run rewrites
# `tests/testthat/_snaps/` and can touch byte-pinned fixtures. An archival push
# should not be able to dirty the tree, and a gate that fires where it cannot
# help is a gate people learn to `--no-verify` past -- including where it can.
#
# So: skip for a mirror, run unchanged for a forge. `always_run` stays -- the
# missing predicate was the remote, not the file set.
#
# THE PREDICATE: is the destination a directory on THIS filesystem?
#
# Not the remote NAME. A name is a local alias; `backup` is what this clone
# happens to call the mirror today and anyone can rename it, so keying on the
# string would leave the skip silently attached to the wrong remote after a
# `git remote rename`. The URL is the thing that says where the objects land,
# so test that instead:
#
#   -d "$url"  ->  a path on this machine  ->  a mirror  ->  skip
#   otherwise  ->  a transport (ssh/https/git)  ->  a forge  ->  run the gate
#
# `git@gitlab.com:bart-turczynski/rurl.git` and `https://github.com/...` are not
# directories; `/Users/.../_backups/rurl.git` is. One readable test, and it
# generalizes to a second mirror without editing this file.
#
# WHY `github` IS GATED, not skipped. That remote points at an account that is
# currently suspended, so in practice a push there fails at the transport. It is
# still a NETWORK remote -- somewhere other people would consume from if it came
# back -- and account status is a forge-side fact that can flip without anyone
# touching this repo. Encoding "github is dead" here would leave a real publish
# path ungated the day it is un-suspended. It costs nothing to gate a remote
# nobody pushes to, so it is gated by simply not being special-cased.
#
# FAILING OPEN MEANS RUNNING. Every path that cannot positively identify a local
# mirror runs the full gate: no arguments (a hand-run
# `pre-commit run --hook-stage pre-push` passes none), an empty URL, an
# unrecognized shape. A missed skip costs three minutes; a missed gate ships
# unverified work.
#
# Usage:
#   tools/verify-on-push.sh                  # no remote known -> run the gate
#   tools/verify-on-push.sh <name> <url>     # as git calls a pre-push hook
#
# Arguments are optional because both callers exist. Git hands a pre-push hook
# `$1`=remote name and `$2`=remote URL, but pre-commit does NOT forward them to
# a hook `entry` -- it consumes them itself and re-exports them as
# `PRE_COMMIT_REMOTE_NAME` / `PRE_COMMIT_REMOTE_URL`. The environment is
# therefore the live path; the positional form is what makes the predicate
# testable by hand, without pushing anything.

set -euo pipefail

REMOTE_NAME="${1-${PRE_COMMIT_REMOTE_NAME-}}"
REMOTE_URL="${2-${PRE_COMMIT_REMOTE_URL-}}"

if [ -n "$REMOTE_URL" ] && [ -d "$REMOTE_URL" ]; then
  echo "verify: skipped -- '${REMOTE_NAME:-?}' is a local path ($REMOTE_URL), i.e. an archival mirror, not a forge; the gate runs on the push to origin. Force it with: Rscript tools/verify.R"
  exit 0
fi

exec Rscript tools/verify.R
