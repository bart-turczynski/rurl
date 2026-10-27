# rurl's CRAN release deltas

Follow the fleet checklist,
[seor `design/release-checklist.md`](https://gitlab.com/bart-turczynski/seor/-/blob/main/design/release-checklist.md).
rurl's deltas:

- **Step 1: rurl is one link in the fleet's release chain.**
  [design/release-chain.md](release-chain.md) records the order and
  what must already be on CRAN; the live order is seor's SEOR-eqpdrqnl. The
  next rurl update waits a full two months after the previous one (owner,
  2026-10-01, after CRAN questioned pagerankr 0.1.1's spacing).
- **Step 2: check the fleet packages that import rurl, not only CRAN's
  reverse dependencies.** pagerankr is the CRAN reverse dependency today.
  Also check robotstxtr's and sitemapr's latest CRAN release, or `main` if
  they have none: CRAN re-checks every reverse dependency against each later
  rurl, so a break there blocks the next rurl release (owner, 2026-10-01).
- **Step 4: keep the `submission-span` pin.** `cran-comments.md` carries one
  `<!-- submission-span: from=X to=Y -->` line, and
  `tools/cran-comments-gate.R` (in the pre-push gate) checks it against
  `DESCRIPTION`: `to` is the version being submitted, and both versions
  appear in the prose. Leave `cran-comments.md` alone at step 12: while
  `Version:` is `X.Y.Z.9000` the gate checks the pin against `X.Y.Z`
  (RURL-efbcrhjc).
- **Step 5: run punycoder built with libidn2, and run the CI jobs locally.**
  punycoder's `configure` uses libidn2 when `pkg-config` finds it, as on
  CRAN's Debian flavors, and rurl's tests can behave differently there. rurl
  3.1.0's first upload failed CRAN's pre-test on exactly that
  (RURL-woljcpfu). Install the current CRAN punycoder from source with
  libidn2 and run the suite against it, besides the binary build. Then run
  `tools/local-ci.sh --all origin/main`, which reproduces the CI jobs in the
  CI image against a clean clone.
- **Step 14: the concept DOI is `10.5281/zenodo.20972584`.** Its
  `<concept-recid>` in the Zenodo API query is `20972584`.
