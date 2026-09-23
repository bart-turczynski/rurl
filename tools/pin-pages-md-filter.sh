#!/usr/bin/env bash
#
# Pin for the `pages` job's agent-md filter in .gitlab-ci.yml (SEOR-wqxhftpv).
#
# WHAT THIS PINS. Not R behavior -- the CI FILE'S TEXT. pkgdown renders every
# top-level .md file except the handful pkgdown:::package_mds() hardcodes
# (README/NEWS/LICENSE/cran-comments), and _pkgdown.yml has no setting that
# can exclude one. The `pages` job therefore moves unwanted files out of the
# source tree before build_site() runs. This script extracts the actual shell
# text between the `agent-md-filter:start`/`:end` markers in that job -- not a
# re-implementation of it -- RUNS it against a scratch copy of every top-level
# .md file this repository currently tracks, and asserts which files survive
# (are NOT moved to /tmp/agent-md/) against PIN_EXPECTED_SURVIVORS below. A
# change to the filter logic, or to the repo's top-level .md file set, that
# changes the survivor set without that expectation also changing is exactly
# the drift this exists to catch.
#
# Usage: tools/pin-pages-md-filter.sh
# Exit 0 and print OK on a match; exit 1 with a diff on a mismatch.

set -euo pipefail

repo_root=$(git rev-parse --show-toplevel)
cd "$repo_root"

ci_file=.gitlab-ci.yml
mark_start='# --- agent-md-filter:start ---'
mark_end='# --- agent-md-filter:end ---'

filter_script=$(sed -n "/${mark_start}/,/${mark_end}/p" "$ci_file")
if [ -z "$filter_script" ]; then
  echo "pin: could not find agent-md-filter markers in $ci_file" >&2
  exit 1
fi

# EXPECTED SURVIVORS: the top-level .md files the filter logic currently in
# .gitlab-ci.yml is claimed to leave in place for pkgdown to render (plus
# whatever pkgdown itself already skips, since this script does not simulate
# pkgdown -- it only simulates the mv/case-statement step). Update this list
# in the SAME commit that changes the filter's keep/deny logic, or the repo's
# top-level .md files: that discipline is what makes this a pin rather than a
# tautology that always agrees with whatever the file currently says.
default_expected='ACKNOWLEDGMENTS.md
ARCHITECTURE.md
CODE_OF_CONDUCT.md
CONTRIBUTING.md
LICENSE.md
NEWS.md
README.md
RELEASE_NOTES_v1.md
SECURITY.md
cran-comments.md'
expected="${PIN_EXPECTED_SURVIVORS:-$default_expected}"

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

# Seed the scratch dir with every top-level .md file this repo actually
# tracks -- the real input the job runs against, not a hand-picked sample.
( cd "$repo_root" && git ls-files -- '*.md' ) | grep -v '/' | while IFS= read -r f; do
  : > "$work/$f"
done

scratch_target=/tmp/agent-md-pin-scratch
rm -rf "$scratch_target"

# Run the extracted script text verbatim, redirecting its /tmp/agent-md
# target to a scratch dir so this pin never touches a real pipeline's /tmp.
( cd "$work" && echo "$filter_script" | sed "s#/tmp/agent-md#${scratch_target}#g" | sh )

actual=$(cd "$work" && ls -1 *.md 2>/dev/null | sort)
expected_sorted=$(printf '%s\n' "$expected" | sort)

if [ "$actual" != "$expected_sorted" ]; then
  echo "pin FAILED: the filter's survivor set does not match expectation" >&2
  echo "--- expected ---" >&2
  echo "$expected_sorted" >&2
  echo "--- actual ---" >&2
  echo "$actual" >&2
  exit 1
fi

count=$(printf '%s\n' "$actual" | grep -c . || true)
echo "OK: $count top-level .md file(s) survive the filter, matching expectation."
