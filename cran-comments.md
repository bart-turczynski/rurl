## Submission

This is an update. CRAN serves `rurl` 3.0.1 (published 2026-09-09); this is
3.1.0.

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

The test suite is additionally run under `LC_ALL=C` on Linux.

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

`rurl` depends on R (>= 4.0.0). It imports `utils`, `stringi`,
`punycoder (>= 1.2.1)` and `pslr (>= 1.1.1)`, both floors already on CRAN.
There is no `Remotes:` field.

## Downstream dependencies

One reverse dependency is on CRAN, `pagerankr` 0.1.0. REVDEP-STATUS: its
tests fail against this version (4 failures in `test-canonicalization.R`; 0
against 3.0.1). The test is a deliberate drift guard that fails whenever
`get_clean_url()` gains an argument, and 3.1.0 adds the `path_normalisation`
alias. This must be resolved before submission; see RURL-woljcpfu.
