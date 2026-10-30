#!/bin/sh
# news-version, a cheap gate of the fleet-ci local gate (.fleet-ci.yml): the
# top NEWS.md heading is either "(development version)" or exactly
# DESCRIPTION's Version, which catches a stale heading left in at release.
#
#   sh scripts/check-news-version.sh      # from the package root
#
# The body of the former `gates` CI job's news-version step, unchanged:
# seor's `news-version` job with the heading level rurl's NEWS.md uses
# (`## rurl ...`). Pure shell.
set -eu

version="$(grep -E '^Version:' DESCRIPTION | head -n1 | sed -E 's/^Version:[[:space:]]*//')"
heading="$(grep -m1 -E '^##? ' NEWS.md | sed -E 's/^##?[[:space:]]+(rurl[[:space:]]+)?//')"
echo "DESCRIPTION Version : '$version'"
echo "Top NEWS heading    : '$heading'"
if [ "$heading" = "(development version)" ] || [ "$heading" = "$version" ]; then
  echo "OK: NEWS heading and version are consistent."
else
  echo "Top NEWS.md heading ('$heading') is neither '(development version)' nor the DESCRIPTION Version ('$version'). Update NEWS.md before release."
  exit 1
fi
