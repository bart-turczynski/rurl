#!/bin/sh
# Fail when this clone's git pre-push hook is not fleet-ci's (the
# `fleet-ci-hook-installed` hook in .pre-commit-config.yaml).
#
#   sh scripts/check-fleet-hook.sh
#
# The containerized gate (.fleet-ci/gate) runs only from the hook that
# `.fleet-ci/pre-push --install` writes. Without it, a push runs pre-commit's
# hygiene hooks alone. fleet-ci decides whether its hook is installed, by the
# rules `--install` uses (pre-commit's migration mode, an older fleet-ci
# shim and a push to a local mirror included): `.fleet-ci/pre-push
# --check-installed`, given pre-commit's PRE_COMMIT_REMOTE_URL (fleet-ci
# SEOR-zxfijefd). So a re-vendored fleet-ci needs no change here. It never
# runs the gate: the fleet standard forbids a pre-commit hook that does.
#
# It was warn-only until the hook was installed fleet-wide (seor
# SEOR-cjxarlcq); it fails since (SEOR-gksaxuoz).
set -u

top=$(git rev-parse --show-toplevel 2>/dev/null) || top="."
vendored="$top/.fleet-ci/pre-push"
if [ ! -f "$vendored" ]; then
  cat >&2 <<EOF
!! ERROR: push refused: $vendored is missing, so nothing can tell whether
!! fleet-ci's pre-push hook is installed. Re-run fleet-ci's bin/vendor.
EOF
  exit 1
fi
exec python3 "$vendored" --check-installed "${PRE_COMMIT_REMOTE_URL:-}"
