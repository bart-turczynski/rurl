#!/bin/sh
# Fail when this clone's git pre-push hook is not fleet-ci's (the
# `fleet-ci-hook-installed` hook in .pre-commit-config.yaml).
#
#   sh scripts/check-fleet-hook.sh
#
# The containerized gate (.fleet-ci/gate) runs only from the hook that
# `.fleet-ci/pre-push --install` writes. Without it, a push runs pre-commit's
# hygiene hooks alone. This looks for the marker line that hook carries in
# `hooks/pre-push`, or in `hooks/pre-push.legacy` under pre-commit's migration
# mode (fleet-ci README, "The local gate"). The marker is read from the
# `MARK =` line of the repository's .fleet-ci/pre-push, so a re-vendored
# marker is followed. It never runs the gate: the fleet standard forbids a
# pre-commit hook that does.
#
# It was warn-only until the hook was installed fleet-wide (seor
# SEOR-cjxarlcq); it fails since (SEOR-gksaxuoz).
set -u

top=$(git rev-parse --show-toplevel 2>/dev/null) || top="."
vendored="$top/.fleet-ci/pre-push"

marker=""
if [ -f "$vendored" ]; then
  marker=$(sed -n 's/^MARK[[:space:]]*=[[:space:]]*"\(.*\)"[[:space:]]*$/\1/p' "$vendored" | head -n 1)
fi
if [ -z "$marker" ]; then
  cat >&2 <<EOF
!! ERROR: push refused: cannot tell whether fleet-ci's pre-push hook is
!! installed: $vendored is missing or has no MARK = "..." line.
!! Re-run fleet-ci's bin/vendor to restore it.
EOF
  exit 1
fi

hooks=$(git rev-parse --git-path hooks 2>/dev/null) || hooks=""
if [ -n "$hooks" ]; then
  for f in "$hooks/pre-push" "$hooks/pre-push.legacy"; do
    if [ -f "$f" ] && grep -qF -- "$marker" "$f"; then
      exit 0
    fi
  done
fi

cat >&2 <<'EOF'
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! ERROR: push refused: the containerized gate is not installed in this
!! clone, so this push would run only the hygiene hooks: no R CMD check,
!! lintr, docs drift or spelling. Install it once, from the package root:
!!
!!     .fleet-ci/pre-push --install
!!
!! then push again.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
EOF
exit 1
