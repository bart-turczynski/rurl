## R CMD check results

Checked with `R CMD check --as-cran` on a clean clone at the submitted commit:

- macOS aarch64 (R 4.6.0, local)
- Ubuntu, R-release (`r-base:latest` container, `tools/local-ci.sh`)

The test suite is additionally run under `LC_ALL=C` on Linux, since several of
this release's fixes concern non-UTF-8 sessions.

Result: **0 errors | 0 warnings | 1 note**

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
```

The `BugReports:` address is correct and the issue tracker is public. GitLab
serves 404 for the **HTML** `/-/issues` page to any logged-out client, on every
project on the platform, as anti-scraping behavior — it is not specific to this
project and not a visibility setting. The project is `visibility: public` with
`issues_access_level: enabled`.

Verified against three control projects whose trackers are unquestionably
public, measured unauthenticated on 2026-08-16:

| URL | anonymous status |
|---|---|
| `gitlab.com/gitlab-org/gitlab/-/issues` | 404 |
| `gitlab.com/gitlab-org/gitlab-runner/-/issues` | 404 |
| `gitlab.com/inkscape/inkscape/-/issues` | 404 |
| `gitlab.com/bart-turczynski/rurl/-/issues` | 404 |
| `gitlab.com/api/v4/projects/bart-turczynski%2Frurl/issues` | **200** |

GitLab's own issue tracker returning 404 to the same check is the clearest
demonstration that this reflects the platform rather than the package. The
tracker is reachable in a browser session and through the anonymous API. No
change to `DESCRIPTION` is warranted, and repointing `BugReports:` to work
around a platform-wide behavior would make it less accurate, not more.

---

## Changes in this version

This release consolidates all development since the version currently on CRAN.
It is a large span: CRAN holds **1.2.0**, and this is **3.0.0**, covering
eleven intermediate releases that were made on the development branch and never
submitted.

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

**3.0.0 — identity surfaces, and the removal of the `curl` dependency**

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
`punycoder (>= 1.2.0)` and `pslr (>= 1.1.0)`.

Both floors are satisfied by versions already on CRAN — `punycoder` 1.2.1 and
`pslr` 1.1.1 — so this submission resolves against current CRAN with no
coordinated ordering required. There is no `Remotes:` field.

`curl` was removed from `Imports` in this release; see above.

## Downstream dependencies

None on CRAN.
