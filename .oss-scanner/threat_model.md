# Threat model

## What this project does and where untrusted input enters

rurl is a pure-R package (no `src/`, no native code) that parses, normalizes, cleans,
serializes, resolves and joins URLs under three postures: `url_standard = "whatwg"`
(the WHATWG URL Standard), `"rfc3986"` (RFC 3986 generic syntax) and `NULL` (the
byte-frozen legacy profile, ADR 0007, whose web route reproduces libcurl's URL parse).
It makes no network connection and resolves no names. Domains and public suffixes come
from `pslr`, IDNA from `punycoder`, IP-literal facts from `raddr`.

It sits under security decisions in other packages: the SSRF guard `ssrfr` reads its
host and diagnostics, and the `sitemapr` / `robotstxtr` fetchers resolve and fetch the
URLs it returns. A host, port or authority that rurl reads differently from the client
that later connects is therefore a security bug, not a cosmetic one.

Untrusted: every URL string (character vector elements, any length, any encoding mark,
invalid UTF-8 included) passed to the exports in `NAMESPACE`:
- parsing: `safe_parse_url()`, `safe_parse_urls()`, `get_parse_status()`,
  `get_parse_verdicts()`;
- component accessors: `get_scheme()`, `get_host()`, `get_port()`, `get_user()`,
  `get_password()`, `get_userinfo()`, `get_path()`, `get_query()`, `get_fragment()`,
  `get_domain()`, `get_tld()`, `get_subdomain()`, `get_clean_url()`;
- facts and policy: `get_host_type()`, `get_url_diagnostics()`, `get_scheme_class()`,
  `check_hosts()`, `is_valid_host()`, `check_schemes()`, `get_mailto_recipients()`,
  `query_param_summary()`;
- output: `serialize_url()` / `serialise_url()`, `format_url()`, `get_url_key()`;
- `resolve_url()` (both the reference and the base are untrusted);
- the URL columns of data frames given to `url_*_join()` and the deprecated
  `canonical_join()`.

Trusted: the calling R code and every option it passes (`profile`, `url_standard`,
`scheme_policy`, `scheme_acceptance`, the presentation dials, `allowed_schemes`, a
caller-built `pslr::psl_engine()`), the cache settings (`rurl_cache_config()`), and the
data bundled in the package (`R/query-denylist.R`) or in its dependencies (the PSL).

## Components that matter most / least

Most important, in order:
- `R/parse-web.R`: the in-tree parser for special schemes. Authority split (where the
  last `@` ends userinfo), host parser `.web_parse_host()`, the IPv4/IPv6 canonicalizers,
  the WHATWG IPv4 reading `.whatwg_ipv4_read()` over `raddr::addr_whatwg()`, and the
  five acceptance dials (`host_charset`, `host_pct`, `last_at_userinfo`, `pqf_bytes`,
  `host_ipv4`).
- `R/parse-phases.R`: the WHATWG pre-parse rewrites (backslash to slash, C0/space strip,
  domain-separator mapping), the host model `.apply_host_standard_model_vec()`, the
  host/address boundary `.host_ends_in_number_vec()` (ADR 0018 D2), and the two
  spec serializers `.serialize_whatwg_full_vec()` / `.serialize_rfc_full_vec()`.
- `R/parse.R`: option and profile resolution, the Stage A cache key
  `.parse_cache_keys()` (two inputs or two postures that share a key would return each
  other's parse), and the Stage A / Stage B split.
- `R/parse-state.R`: the non-special and opaque routes, the RFC 3986 grammar gate and
  the general-acceptance router.
- `R/resolve.R`: reference resolution (WHATWG reference parsing, RFC 3986 section 5).
- `R/serialize.R`, `R/url-key.R`, `R/url-join.R`: a serialized URL must re-parse as the
  same URL; the key must be injective (two URLs with different hosts must never share a
  key, or a join merges them).
- `R/domain.R`, `R/diagnostics.R`, `R/host-policy.R`, `R/scheme-policy.R`: the
  facts consumers gate on. `domain-invalid-ace-label` carries a completeness guarantee
  that `ssrfr` relies on (ruling RUL-023): it must fire on every host meeting its
  predicate.
- `R/format.R`: display escaping. A bidi or other hazard code point that survives, or a
  credential that is not redacted, defeats its purpose.
- `R/utils.R` (byte-position cutting helpers), `R/percent-coding.R`, `R/path-query.R`.

Less important but in scope: `get_clean_url()` (a lossy projection, ADR 0017),
`R/email-diagnostics.R`, `R/canonical_join.R` (deprecated), the cache API in `R/zzz.R`.

Out of scope: `pslr`, `punycoder`, `raddr`, `stringi`/ICU and base R, unless rurl calls
them unsafely (feeds them a host other than the one it parsed, or trusts output they do
not promise). Also out of scope: dev tooling not shipped in the package (`tools/`,
`scripts/`, `inst/bench/`, `analysis/`, `design/`, `.gitlab/`).

## How to exercise it

- The image: source at `/src`, rurl installed from it, every Import and Suggest
  installed (testthat, jsonlite, withr, pkgload), no network. No native code, so gdb,
  valgrind and sanitizers have nothing to instrument.
- Full suite: `Rscript -e 'testthat::test_local("/src")'`. One area:
  `Rscript -e 'testthat::test_local("/src", filter = "url-standard-host")'`.
  The dependency audits `test-osv.R` and `test-security.R` skip offline.
- Ad-hoc calls (name the posture; `safe_parse_urls()` defaults to `NULL`):
  `Rscript -e 'library(rurl); print(safe_parse_urls("http://a\\b@c/", profile = "whatwg")[, c("scheme", "host", "port", "user", "path")])'`
  `Rscript -e 'library(rurl); serialize_url("HTTP://0x7f.1/", standard = "whatwg")'`
  `Rscript -e 'library(rurl); get_url_diagnostics("http://xn--a.example/", url_standard = "whatwg")'`
  `Rscript -e 'library(rurl); resolve_url("//evil/x", "http://good/", url_standard = "whatwg", output = "serialized")'`
- One URL under all four postures, beside the WPT oracle row if one exists:
  `Rscript /src/tools/posture-probe.R 'http://example.com/'` (it loads the `/src` tree
  with pkgload).
- Corpora worth reusing: `inst/bench/wpt-url-cases.json` (WPT `urltestdata`, the WHATWG
  oracle), `tests/testthat/fixtures/wpt-url-base-relative.json`,
  `tests/testthat/fixtures/external-url-vectors.csv` (vectors from ada, curl and other
  parsers, with per-standard expected values), `url-standard-conformance.csv`,
  `rfc3986-abnf-fixtures.csv`, `encoding-fixtures.csv`, `parse-corpus.csv`; and the
  hostile-input classes in `tests/testthat/helper-hostile-inputs.R` (leading U+FEFF,
  invalid UTF-8, latin1 and bytes marks, NA, long components).
- No browser or Node is installed; use the WPT rows as the WHATWG reference.

## How you rate severity

- **Critical**: a parse differential that lets a URL through `ssrfr`'s default policy to
  loopback or cloud-metadata addresses: under `whatwg`, rurl reports a host or IP that is
  neither while a WHATWG client connects to `127.0.0.0/8`, `::1` or `169.254.169.254`
  (for example an IPv4 number form, a mapped separator or a backslash read differently).
- **High**: any other host, port, scheme or userinfo boundary that differs from the
  posture's reference reading in a way that changes a caller's allow/deny decision.
  The reference is the WHATWG URL Standard under `whatwg`, RFC 3986 under `rfc3986`, and
  libcurl's URL parse under `NULL`. This covers allowlist bypass through `get_host()`,
  `get_domain()` or `check_hosts()`; a `serialize_url()` or `resolve_url(output =
  "serialized")` result that re-parses with a different authority; a `get_url_key()`
  collision between URLs with different hosts; a miss of `domain-invalid-ace-label`;
  and an `ipv4-*` / `ipv6-*` token missing on a literal the standard parses as an
  address.
- **Medium**: denial of service from small input: superlinear CPU or memory on a short
  string, catastrophic regex backtracking, a hang; one row's R error aborting a whole
  vectorized call, so every other row is lost, for an input class not already listed
  (see below). Credentials leaking into `clean_url`, `get_url_key()`, `format_url()`
  output or a condition message.
- **Low**: wrong results with no security consequence, including standards
  nonconformance. The owner treats nonconformance as a bug to fix (ADRs 0007, 0016):
  report it, rated Low unless it changes a security decision, then High. Also Low:
  issues that need the caller to misuse a documented option or pass hostile options.

## Anything to leave alone

- R-level `stop()` on an invalid argument, including `url_standard = NULL` on
  `get_host_type()`, `get_url_diagnostics()`, `get_scheme_class()` (ADR 0015) and
  `serialize_url(standard = NULL)`.
- rurl is not an SSRF guard and holds no address ranges (ADR 0018, `SECURITY.md`): do
  not report that it does not classify loopback, private or link-local addresses.
- Documented opt-outs and facts: `scheme_acceptance = "general"` accepts `javascript:`,
  `file:` and any other scheme; `file://server/share` is a UNC path; `get_query()` and
  `path_encoding = "decode"` return decoded bytes, CR and LF included (RUL-004);
  `get_user()`, `get_password()` and `serialize_url()` return credentials by design.
- `get_clean_url()` is a lossy policy projection (ADR 0017) and never a conformance
  oracle; judge conformance on `serialize_url()` and `safe_parse_urls()` components.
- Under `NULL`, a reading that differs from WHATWG or RFC 3986 but matches libcurl is the
  frozen profile, not a bug (ADR 0007), including the loose Appendix B scheme group in
  `.split_uri_ref()`.
- Settled rulings in `design/work/url-v3/registers/rulings.md`, for example: an `xn--`
  label that is not a genuine A-label still parses under `whatwg` (RUL-023, reported by
  the diagnostic); `get_host("mailto:a@b")` returns the recipient domain as metadata
  (ADR 0012 D7); a raw space in a `mailto:` opaque path is conformant.
- Cases listed in `known_failures` in `tests/testthat/test-hostile-input-contracts.R`
  (bytes-marked elements, invalid UTF-8 in `resolve_url()`), and the long-input limits
  the latest `NEWS.md` entry for RURL-vewhvxnb names: both are tracked.
- Bugs inside `pslr`, `punycoder` or `raddr`: report them against that package.

Patches:
- Minimal R in the package's style: `stringi` for strings, except where ADR 0005 keeps
  base R; cut strings by byte position with the `R/utils.R` helpers; no new dependency.
- A conformance fix changes the default; never put it behind an opt-in flag (ADRs 0007,
  0016). Respect the frozen areas or say why the patch cannot: the punycode helpers
  (ADR 0002) and the PSL seam (ADR 0001) in `R/domain.R`, the retained base-R string
  operations (ADR 0005), the `safe_parse_url()` columns (ADR 0006: new facts go to the
  companion helpers, never to a new column).
- A `testthat` regression test next to the existing tests for that area (for example
  `test-url-standard-authority.R`, `test-url-standard-host.R`,
  `test-url-standard-whatwg-ipv4.R`, `test-resolve-url.R`, `test-url-key.R`), naming the
  posture it runs under and failing before the fix.
- A `NEWS.md` entry under `## rurl (development version)` that cites the standard and
  clause when the fix is a conformance fix. Fixtures under `tests/testthat/fixtures/`
  are sha256-pinned in `oracle-provenance.json`: a fix that moves a row says so.
