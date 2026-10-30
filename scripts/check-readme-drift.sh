#!/bin/sh
# README drift gate, a cheap gate of the fleet-ci local gate (.fleet-ci.yml).
#
#   sh scripts/check-readme-drift.sh      # from the package root
#
# Fails when README.md is not what a fresh knit of README.Rmd writes. It is
# what devtools::build_readme() does, without devtools (the fleet image's
# tooling library has none): install the package into a throwaway library,
# render README.Rmd with it on the library path, compare. It does what the
# `readme` job of tools/verify-manifest.yml did, which tools/verify.R never
# ran: that job's step is `devtools::build_readme()`, not an
# `Rscript <script>` line, so it derives no gate (SEOR-qaulrrja). Copied from
# pslr's scripts/check-readme-drift.sh.
#
# It runs on the gate's export of the pushed commit, which has no .git, so the
# comparison is `git diff --no-index` against a copy of the committed README.md
# taken before the knit. The knit overwrites README.md in place; the export is
# thrown away after the gate.
#
# Blank-line-only differences don't count: pandoc versions disagree about the
# blank line after `<!-- badges: start -->` (SEOR-kaqtnovh). The test is on the
# diff's output, not its exit status: git 2.43 exits 1 on a blank-only diff
# under --ignore-blank-lines. Any exit status above 1 is a git error and fails.
#
# The knit is byte-stable only under the pinned pandoc, which the fleet image
# carries (.fleet-ci/toolchain).
set -eu

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

cp README.md "$work/README.committed.md"
mkdir "$work/lib"
if ! R CMD INSTALL --no-test-load --library="$work/lib" . >"$work/install.log" 2>&1; then
  cat "$work/install.log"
  echo "ERROR: could not install the package to knit README.Rmd." >&2
  exit 1
fi
R_LIBS="$work/lib${R_LIBS:+:$R_LIBS}" Rscript -e 'rmarkdown::render("README.Rmd", output_options = list(html_preview = FALSE), quiet = TRUE, envir = new.env(parent = globalenv()))'
rm -f README.html

# Exit 0 (same) and 1 (differs) are answers; anything else is a git error.
diff_status=0
git diff --no-index --ignore-blank-lines -- "$work/README.committed.md" README.md >"$work/readme.diff" || diff_status=$?
if [ "$diff_status" -gt 1 ]; then
  echo "ERROR: git diff --no-index failed (exit $diff_status)." >&2
  exit 1
fi
readme_diff=$(cat "$work/readme.diff")
if [ -n "$readme_diff" ]; then
  printf '%s\n' "$readme_diff"
  echo "ERROR: README.md is out of sync with README.Rmd."
  echo "Run devtools::build_readme() under pandoc $(sed -n 's/^PANDOC_VERSION=//p' .fleet-ci/toolchain 2>/dev/null || echo '(the pin)') and commit the result."
  exit 1
fi
echo "readme: README.md matches a fresh knit of README.Rmd"
