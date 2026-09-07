> **DO NOT SUBMIT — this document is stale as of 2026-09-06.**
>
> The claim below that "The ERROR is fixed" is **false**. `rurl_3.0.1.tar.gz`
> was checked on both win-builder queues on 2026-09-06 and returned
> `1 ERROR, 1 NOTE` on each — R-devel (2026-09-04 r90492 ucrt) and R-release
> 4.6.1. The same six `test-external-url-vectors.R` failures 3.0.0 reported are
> still present, plus five more from the corpus-shape pin this version added.
> The corpus reads 389 rows on Windows where the fixture has 396. The
> `LC_CTYPE = "C"` pin changed nothing about the parse.
>
> Full transcript and the preserved logs: `design/win-builder.md`. Owner:
> RURL-gxgxyzpk, reopened. This file's Resubmission section must be rewritten
> against whatever actually fixes the parse before anything is uploaded.

## Resubmission

This is a resubmission. Version 3.0.0 did not pass the incoming pre-tests on
2026-09-06 (`rurl_3.0.0_20260906_162044`): `1 ERROR, 1 NOTE` on
r-devel-windows-x86_64, `1 NOTE` on r-devel-linux-x86_64-debian-gcc.

**The ERROR is fixed.** Six `testthat` failures on Windows only, all in
`tests/testthat/test-external-url-vectors.R`. The package's own code was not
implicated: the failures were in a *test fixture reader*. That file's fixture
is a corpus of adversarial URL vectors imported from the WHATWG
web-platform-tests, so by construction its cells carry raw C0 control
characters, embedded newlines, astral-plane code points and U+FFFF. Reading it
with `utils::read.csv()` sends those bytes through `scan()`'s `mbrtowc()` path
whenever `mbcslocale` is TRUE, and on Windows one row parsed a field out of
register.

The fixture is now read with `LC_CTYPE` pinned to `"C"` and restored on exit,
which takes `scan()` down its single-byte path. That is the correct reader
rather than a workaround: UTF-8 is ASCII-transparent, so no multibyte sequence
can contain a `,` or `"` byte and a bytewise parse of a UTF-8 CSV is exact —
and exact identically on every platform. The parse is `identical()` to the
previous one, encoding marks included, on platforms where the previous one was
already correct.

A second test now pins the corpus's shape (row count, runnable count, class
counts). Every conformance test in that file filters on `runnable == "yes"`, so
a mis-parsed row previously left the corpus silently rather than failing; the
pin makes that condition loud.

No user-facing behavior changed. `NEWS.md` records both under `## rurl 3.0.1
### Internal`.

**The NOTE is unchanged and is addressed below.** Its two components — the
maintainer address and the `BugReports:` URL — are explained in the following
section. The pre-test also reported `IDNA` and `Punycode` as possibly
misspelled words in `DESCRIPTION`; both are correct spellings of the standards
named (`IDNA` is RFC 5890's Internationalized Domain Names in Applications;
`Punycode` is RFC 3492's encoding), and both are already recorded in
`inst/WORDLIST`, which CRAN's incoming `aspell` run does not consult.

---

## R CMD check results

Checked with `R CMD check --as-cran` on a tarball built from a clean export
of the submitted commit (`git archive`, then `R CMD build`), so nothing
untracked in a working clone can reach the check:

- macOS aarch64 (`aarch64-apple-darwin23`), R 4.6.0 (2026-04-24), local,
  2026-09-04
- Ubuntu, R-release (`r-base:latest` container) is run through
  `tools/local-ci.sh --all` before the tarball is submitted

The test suite is additionally run under `LC_ALL=C` on Linux, since several of
this release's fixes concern non-UTF-8 sessions.

Result on 2026-09-04: **0 errors | 0 warnings | 3 notes**, of which one is
expected on CRAN and explained below, and two are local-only and will not
appear in the submitted tarball's check:

| NOTE | on CRAN? |
|---|---|
| CRAN incoming feasibility — maintainer address, and the `BugReports:` 404 | yes; explained below |
| `checking top-level files` — `Non-standard file/directory found at top level: 'tmp'` | no — excluded by `.Rbuildignore` (`^tmp$`) since 2026-09-04; see below |
| `checking HTML version of manual` — `Skipping checking math rendering: package 'V8' unavailable` | no; the local library lacks `V8` |

The `tmp` NOTE was a repository-hygiene defect, not a package one: four gate
logs under `tmp/orchestrate/` are tracked in git and `.Rbuildignore` excluded
only `^\.tmp$`, so the directory rode into a clean-export tarball. `^tmp$` is
now excluded as well (RURL-ladruqhn, 2026-09-04); re-check the tarball listing
(`tar tzf rurl_*.tar.gz | grep '^rurl/tmp/'` must print nothing) when the
submission tarball is built.

---

### Note

**CRAN incoming feasibility**

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Bart Turczynski <bartek@turczynski.pl>'

New maintainer:
  Bart Turczynski <bartek@turczynski.pl>
Old maintainer(s):
  Bart Turczynski <bartek+rurl@turczynski.pl>
```

The maintainer has not changed. `bartek+rurl@turczynski.pl` and
`bartek@turczynski.pl` are the same person and the same mailbox — the address
on CRAN's `rurl` 1.2.0 used a plus-tag subaddress, which was simplified to the
bare address. Both deliver to me, and the ORCID in `Authors@R`
(0000-0002-8788-7980) is unchanged from the published version. I can confirm
the change from either address if that would help.

The same note also reports:

```
Found the following (possibly) invalid URLs:
  URL: https://gitlab.com/bart-turczynski/rurl/-/issues
    From: DESCRIPTION
    Status: 404
    Message: Not Found
```

This is a false positive, and not one the package can fix. The `BugReports:`
address is correct: the page opens normally in a browser session and accepts
bug reports from anyone with a GitLab account. GitLab.com serves an HTTP 404 to
any logged-out client requesting an issue-list path, on every project on the
platform, as an anti-scraping measure. It is not a setting of this project —
the project is `visibility: public` with `issues_access_level: enabled` (read
back from the GitLab API), and the response is 404 whether the client identifies
as `curl`, as a browser, or as R's URL checker.

The behaviour was confirmed to be site-wide rather than a misconfiguration by
measuring, in the same unauthenticated run (last repeated 2026-09-04), control
projects whose trackers are unquestionably public:

| URL | anonymous status |
|---|---|
| `gitlab.com/gitlab-org/gitlab/-/issues` (GitLab's own tracker) | 404 |
| `gitlab.com/gitlab-org/gitlab-runner/-/issues` | 404 |
| `gitlab.com/inkscape/inkscape/-/issues` | 404 |
| `gitlab.com/bart-turczynski/pslr/-/issues` (sibling package, already on CRAN) | 404 |
| `gitlab.com/bart-turczynski/rurl/-/issues` | 404 |
| `gitlab.com/bart-turczynski/rurl/-/issues/new` | 302 to sign-in |
| `gitlab.com/bart-turczynski/rurl` (repository root) | **200** |
| `gitlab.com/api/v4/projects/bart-turczynski%2Frurl/issues` (anonymous API) | **200** |

The issue tracker of GitLab itself returning 404 to the same request is the
clearest demonstration that this reflects the platform rather than the package.
The repository root and every file path under it return 200 to the same client,
and the issue list is served to the anonymous API and to a logged-in browser.
The same NOTE is declared for the sibling `pslr`, whose `BugReports:` points at
the same host. No change to `DESCRIPTION` is warranted; repointing
`BugReports:` to work around a platform-wide behaviour would make it less
accurate, not more.

The project is public and the tracker is open: <https://gitlab.com/bart-turczynski/rurl>.

---

## Changes in this version

<!-- submission-span: from=1.2.0 to=3.0.1 -->
<!-- Checked by tools/cran-comments-gate.R. `to` must equal DESCRIPTION's
     Version, and both versions must appear in the prose below, so this pin and
     the sentences a reviewer reads cannot drift apart. `--online` additionally
     checks `from` against what CRAN publishes. -->

This release consolidates all development since the version currently on CRAN.
It is a large span: CRAN holds **1.2.0**, and this is **3.0.1**, covering
eleven intermediate releases that were made on the development branch and never
submitted, plus the test-only portability fix described under Resubmission
above. Version **3.0.0** was submitted on 2026-09-06 and did not clear the
incoming pre-tests, so it was never published.

**1.3.0 – 1.4.1 — dependency extraction and correctness**

* Public Suffix List matching delegated to the `pslr` package; `rurl` no longer
  ships its own processed copy of the list or its embedded matcher. This fixed
  real matcher defects: wildcard (`*.`) and exception (`!`) rules are now
  honored by TLD extraction, and IDN hosts resolve a registered domain in every
  section.
* Accessor arguments brought into line with `safe_parse_url()`'s options;
  the domain-family accessors now follow `host_encoding`.
* IPv6 literals with an embedded dotted-quad IPv4 tail (RFC 4291 §2.2 form 3)
  are recognized rather than falling through to the registered-name path.

**2.0.0 – 2.2.2 — query handling, identity columns, and the standard selector**

* Opt-in query-string handling for `clean_url` (`query_handling` =
  `"drop"`/`"filter"`/`"allow"`/`"keep"` with a built-in tracker denylist and
  `params_keep`/`params_drop`/`sort_params`/`empty_param_handling`/
  `params_case_sensitive`/`decode_plus`), threaded through `get_clean_url()`,
  `get_query()` and `canonical_join()`; new `query_param_summary()`.
* Four additive parse columns (`domain_ascii`, `domain_unicode`, `tld_ascii`,
  `tld_unicode`) exposing the registrable domain and public suffix in both
  canonical spellings as encoding-independent identity keys.
* New `url_standard` selector (`NULL` default / `"rfc3986"` / `"whatwg"`)
  choosing a coherent set of standard-conformant behaviors; a standalone
  `port_handling` option; WHATWG backslash-as-separator handling; `resolve_url()`
  (RFC 3986 §5 reference resolution); and companion classification/diagnostics
  helpers (`get_host_type()`, `get_scheme_class()`, `get_url_diagnostics()`).

**2.3.0 – 2.5.0 — conformance and orthogonality**

* WHATWG tab/LF/CR stripping; RFC 3986 `reg-name` sub-delimiters accepted under
  `"rfc3986"`; WPT-valid IPv4 hosts with empty hex zero parts; UTS-46
  ignored-code-point mappings under `host_encoding = "idna"`.
* New `scheme_policy` argument (`"infer"` / `"require"`) controlling whether
  scheme-less host-shaped input is accepted at all — an axis orthogonal to
  `protocol_handling` and `url_standard`.
* `path_encoding` became an orthogonal presentation knob layering on any
  `url_standard` profile, mirroring `host_encoding`.

**2.6.0 – 2.7.0 — general URL parsing, and locale determinism**

* New `scheme_acceptance` argument (`"web"` / `"general"`). The default `"web"`
  is byte-for-byte unchanged; `"general"` turns `rurl` into a general URL parser
  in which opaque, non-special, RFC 3986-generic and `file:` URLs parse and
  round-trip.
* New `profile` argument (`"browser"` / `"whatwg"` / `"rfc-syntax"` / `"seo"`)
  bundling the acceptance, interpretation, leniency and canonicalization knobs
  under one inspectable name, with a companion `url_profile()` inspector.
* Per-request Public Suffix List selection via a new `engine` argument taking a
  `pslr::psl_engine()` snapshot, with no global state mutated.
* `get_mailto_recipients()`, and `mailto:` recipient decomposition through the
  standard accessors under `"general"`.
* Practical host-validation policy helpers `is_valid_host()` / `check_hosts()`.
* **`rurl`'s output no longer depends on the R session's character set.** In a
  non-UTF-8 session — `LC_ALL=C`, or the non-UTF-8 Windows locale many CRAN
  Windows checks run in — several results were different and in some cases
  simply wrong: IDN hosts returned `NA` for `domain`/`tld`, a non-ASCII path was
  percent-encoded from the session locale rather than from its octets, and
  `utf8towcs` errors were reachable. Encoding is now declared at the points
  where a string enters the pipeline. Over the full suite, `LC_ALL=C` moves from
  99 failures and 348 warnings to 0 failures and 1 warning. **A UTF-8 session is
  byte-for-byte unaffected.** This change also removed 347 "unable to translate
  ... to native encoding" warnings from a non-UTF-8 check run.
* Scheme and host case normalization no longer inherits ICU's locale-tailored
  case mapping, which in a Turkish or Azeri session mapped `I` to `ı` and so
  returned a *different domain* than the one requested.

**3.0.0 / 3.0.1 — identity surfaces, and the removal of the `curl` dependency**

* **`rurl` no longer depends on `curl`.** URL parsing is entirely in-tree, so
  installing `rurl` no longer pulls in `curl` or requires the system `libcurl`
  it links against. This was verified as a behavior-preserving engine swap
  before the dependency was dropped: 106,898 inputs were compared field by field
  against `curl_parse_url()` — a structural grid, a per-octet sweep of every URL
  position, an IPv6/IPv4/percent-escape fuzz corpus, the WHATWG
  web-platform-tests `urltestdata` corpus, and every URL literal in the
  package's own tests — including encoding marks, at zero differences. It is
  flagged as breaking only because a declared dependency disappears.
* New identity and rendering surfaces: `get_url_key()`, `serialize_url()`,
  `format_url()` (safe rendering of a URL for a human to read),
  `resolve_url(output = "serialized")`, and `get_parse_verdicts()`.
  `get_url_key()` projects a URL onto a versioned comparison key derived from
  its canonical state, which no cleaning dial can reach; `url_key_policy()`
  carries the two identity dials — `standard`, and an opt-in `scheme_equality`
  treating `http` and `https` as one class.
* Six identity-keyed joins — `url_inner_join()`, `url_left_join()`,
  `url_right_join()`, `url_full_join()`, `url_semi_join()` and
  `url_anti_join()` — join two data frames on that key rather than on a
  cleaned display string. Duplicate keys are kept as a multiplicity fact;
  unkeyable rows and rows that parsed with a warning are governed by two
  independent arguments (`invalid`, `warnings`); and failures raise typed
  conditions that report row positions and truncated keys, never URL content.
  `canonical_join()` is unchanged and still matches on `clean_url`.
* New `check_schemes()`, a scheme-axis policy companion to `check_hosts()`. It
  reports each URL's scheme, scheme class and membership in a caller-supplied
  `allowed_schemes`, filling the gap between `scheme_acceptance = "web"`'s
  fixed five-scheme allowlist and `"general"`'s accept-everything. It is a
  policy layer and never changes how a URL parses.
* `resolve_url()` reference resolution is now standard-aware, and a colon in a
  relative path's first segment is no longer mistaken for a scheme (RFC 3986
  §3.1's real scheme production replaces Appendix B's non-validating group).
* `profile = "seo"` now resolves dot segments and renders the host in Unicode,
  so the Punycode and Unicode spellings of one host clean to identical bytes.
* `canonical_join()` warns, via the classed condition
  `"rurl_legacy_join_dial_warning"`, when a presentation dial forwarded through
  `...` would silently move the comparison key. Results are unchanged.

Throughout this span, `url_standard = NULL` — the default — is frozen
byte-for-byte and was verified unchanged across the full fixture corpus. Every
breaking change above is reachable only by passing a non-default argument, with
the single exception of the `curl` dependency removal, which changes no output.

## Dependencies

`rurl` depends on R (>= 4.0.0). It imports `utils`, `stringi`,
`punycoder (>= 1.2.1)` and `pslr (>= 1.1.1)`.

Both floors name the versions already on CRAN — `punycoder` 1.2.1 and
`pslr` 1.1.1 — so this submission resolves against current CRAN with no
coordinated ordering required. There is no `Remotes:` field.

`curl` was removed from `Imports` in this release; see above.

## Downstream dependencies

None on CRAN.
