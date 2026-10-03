## Submission

This is an update. CRAN serves `rurl` 3.0.1 (published 2026-09-09); this is
3.1.0.

This is a resubmission. The 2026-10-01 upload of 3.1.0 failed the incoming
pre-test on Debian r-devel with one test failure. The failing test describes
`punycoder`, not `rurl`, and assumed that `punycoder::host_normalize()` and
`punycoder::puny_decode()` agree on one label. They do not when `punycoder`
1.3.0 is built with libidn2, as on CRAN's Debian checks. The test now accepts
every answer `punycoder` gives across its versions and builds. Nothing else
changed: no code in `R/` and no other test.

This is the `rurl` update announced in the `pagerankr` 0.1.1 submission
(accepted 2026-10-01). `pagerankr` 0.1.0's tests failed against 3.1.0, so
`pagerankr` was fixed first, and this update follows it, less than two months
after 3.0.1.

## R CMD check results

Checked with `R CMD check --as-cran` on a tarball built from a clean export
of the submitted commit (`git archive`, then `R CMD build`), so nothing
untracked in a working clone can reach the check:

- macOS aarch64, R 4.6.0, local, 2026-09-29: **0 errors | 0 warnings |
  2 notes**. One is the `BugReports:` note explained below. The other,
  `checking HTML version of manual` ("Skipping checking math rendering:
  package 'V8' unavailable"), is local-only: the check library lacks `V8`.
- Ubuntu, R-release (`r-base:latest` container), through
  `tools/local-ci.sh --all`, which reproduces the package's CI jobs against a
  clean clone
- Windows, win-builder, on a clean export of commit 761f17f, 2026-09-30:
  - R-oldrelease 4.5.3: **1 NOTE**, tests OK. The NOTE is the incoming
    check's: `IDNA` and `Punycode` as possibly misspelled words in
    `DESCRIPTION` (both are the standard terms), and the `BugReports:` URL
    explained below.
  - R-release 4.6.1 and R-devel: not completed. Both built the package and
    its binary, then stopped at `checking CRAN incoming feasibility ...`
    with no result, as they did on two earlier uploads each. The incoming
    checks are covered by the R-oldrelease run above and the local
    `--as-cran` run.

The test suite is additionally run under `LC_ALL=C` on Linux. For this
resubmission it was also run on macOS against CRAN's `punycoder` 1.3.0 built
from source both with and without libidn2: 0 failures in each.

### Expected NOTE: CRAN incoming feasibility, `BugReports:`

As in 3.0.1, the incoming check reports
`https://gitlab.com/bart-turczynski/rurl/-/issues` as `Status: 404`, from
`DESCRIPTION` and from the `NEWS.md` entry that records the `BugReports:`
choice. That is GitLab's behavior for every project, not this one's: GitLab
has migrated issues to work items and serves 404 to logged-out clients on the
legacy `/-/issues` path, while a browser is redirected to `/-/work_items`, which
returns 200. The 3.0.1 submission note documents the measurement against
GitLab's own tracker and other public projects. `BugReports:` keeps the
`/-/issues` form because that is the form R's incoming check accepts for a
gitlab.com tracker; the project and its tracker are public at
<https://gitlab.com/bart-turczynski/rurl>.

## Changes in this version

<!-- submission-span: from=3.0.1 to=3.1.0 -->
<!-- Checked by tools/cran-comments-gate.R. `to` must equal DESCRIPTION's
     Version, and both versions must appear in the prose below, so this pin and
     the sentences a reviewer reads cannot drift apart. `--online` additionally
     checks `from` against what CRAN publishes. -->

3.1.0 follows 3.0.1 with fixes that a downstream security package (`ssrfr`,
not yet on CRAN) reported, plus small additive features. `NEWS.md` has the
full list.

* Under `url_standard = "whatwg"`, a host whose UTS #46 mapping produces a
  forbidden domain code point (fullwidth `#`, `/`, `?`, `:`, `%`, and the
  no-break and ideographic spaces) now fails the parse, as the WHATWG URL
  Standard's host parser requires. Previously such a host serialized into a
  string that re-parses to a different host.
* Long URLs no longer fail with "variable names are limited to 10000 bytes":
  a cache key past R's limit now skips the cache.
* `get_url_diagnostics()` reports a new host fact, `domain-invalid-ace-label`,
  for an `xn--` label that is not a genuine A-label. The parse itself does not
  change, as the WHATWG URL Standard requires.
* `check_schemes()` reports a new reasons token, `no-authority`.
* With `host_encoding = "unicode"`, an A-label whose ASCII part holds a code
  point other than a letter, digit or hyphen (`xn--a_-wia`) keeps rendering
  in Unicode under punycoder 1.3.0 (on CRAN since 2026-09-30), whose decoder
  rejects such labels: rurl decodes those labels itself (RFC 3492 section
  6.2).
* British spellings of the exported names are accepted as aliases
  (`serialise_url()`, `path_normalisation`).

The default `url_standard = NULL` profile is unchanged.

## Dependencies

`rurl` depends on R (>= 4.1.0). It imports `utils`, `stringi`,
`punycoder (>= 1.2.1)` and `pslr (>= 1.1.1)`, both floors already on CRAN.
There is no `Remotes:` field.

## Downstream dependencies

One reverse dependency is on CRAN, `pagerankr` (same maintainer). Its 0.1.0
tests failed against this version: a deliberate guard in them fails whenever
`get_clean_url()` gains an argument, and 3.1.0 adds the `path_normalisation`
alias. `pagerankr` 0.1.1, published on CRAN on 2026-10-01, accepts the alias.

`R CMD check` of the CRAN `pagerankr` 0.1.1 tarball against this version
(macOS aarch64, R 4.6.0, with `punycoder` 1.3.0 and `pslr` 1.2.1 from CRAN),
2026-10-01: Status OK, tests 0 failures. The result is the same against
`rurl` 3.0.1.

## Submission history

### 3.1.0, first upload (failed the incoming pre-test)

* Submitted 2026-10-01 21:43:49 UTC with `devtools::submit_cran()`, from a
  clean clone of `main` at `fe09814db53ed20a56b910d9a572a4111a34e2de`
  (recorded in `CRAN-SUBMISSION`).
* Submitted tarball `rurl_3.1.0.tar.gz`, 1013424 bytes, SHA-256
  `222cf2ab7f534d3f6ee7c5908bd906acbee916469296bb5ea5d12b0cf3a78f3e`.
  Its `R/`, `man/`, `tests/`, `NAMESPACE` and `NEWS.md` are identical to a
  `git archive` of that commit.
* The incoming pre-test failed on Debian r-devel: one test failure in
  `test-punycoder-host-probe-characterization.R`, which assumed two
  `punycoder` calls agree. They do not when `punycoder` 1.3.0 is built with
  libidn2. Windows r-devel passed with the expected `BugReports:` NOTE.

### 3.1.0, resubmission

* Submitted 2026-10-01 23:04:28 UTC with `devtools::submit_cran()`, from a
  clean clone of `main` at `a7fafd66fa06e8741ba2ba73c1a0442dfa054cdc`
  (recorded in `CRAN-SUBMISSION`). It differs from the first upload only in
  the fixed test and this file.
* Submitted tarball `rurl_3.1.0.tar.gz`, 1013562 bytes, SHA-256
  `fab81cf10a04d78c9e3a8980c56470bbe514c71a6530921e04575054dc6512ae`, the
  same file CRAN's incoming queue serves. Its `R/`, `man/`, `tests/`,
  `NAMESPACE` and `NEWS.md` are identical to a `git archive` of that commit.
