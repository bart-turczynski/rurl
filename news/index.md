# Changelog

## rurl (development version)

### Performance

- The parse pipeline is split into an option-independent core (Stage A:
  curl components, IP detection, the post-www host, and the PSL
  domain/TLD decomposition) and a presentation stage (Stage B: path
  handling, case, host-encoding spelling, subdomain trimming, clean-URL
  assembly, status). The `full_parse` cache now stores Stage A, keyed
  only by URL, protocol/scheme handling, `www_handling`, and
  `tld_source`. Calling several accessors with different presentation
  profiles on the same URLs
  (e.g. [`get_host()`](https://bart-turczynski.github.io/rurl/reference/get_host.md),
  [`get_domain()`](https://bart-turczynski.github.io/rurl/reference/get_domain.md),
  [`get_tld()`](https://bart-turczynski.github.io/rurl/reference/get_tld.md),
  [`get_clean_url()`](https://bart-turczynski.github.io/rurl/reference/get_clean_url.md),
  [`get_subdomain()`](https://bart-turczynski.github.io/rurl/reference/get_subdomain.md))
  now shares one cache entry per URL and re-runs only the cheap Stage B,
  so the expensive curl + PSL work happens once instead of once per
  profile. Output is unchanged. Cache memory per URL also drops to a
  single (option-independent) entry. (RURL-dkwrebdt)

- [`safe_parse_urls()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md)
  now de-duplicates its input, parsing each unique URL only once (with
  cross-call reuse via the `full_parse` cache) and expanding the results
  back with [`match()`](https://rdrr.io/r/base/match.html). Repeated /
  duplicate URLs cost only the match, so warm and duplicate-heavy inputs
  are dramatically faster.
  [`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md)
  (scalar) shares the same cached code path. (RURL-ohepgzyf)

- Query-string parsing (`get_query(format = "list")`) is now linear in
  the number of key/value pairs (previously quadratic from incremental
  list growth), so URLs with very long query strings parse faster.
  Output is unchanged. (RURL-actrnerd)

### Behavior changes

- Accessor results (`get_*()`) are no longer named by the input URLs.
  The `get_*()` functions now parse their input in a single vectorized
  pass and return plain unnamed vectors (or lists), instead of vectors
  carrying a `names` attribute of the input URLs. Wrap in
  `stats::setNames(x, url)` if you relied on the old names.
  (RURL-zpatukuq)

- The `full_parse` memoization cache is now bounded by default at 100000
  unique url × option combinations (previously `Inf`), so parsing
  millions of unique URLs can no longer grow the cache without limit.
  Override with `rurl_cache_config(max_full_parse = Inf)` to restore the
  previous unbounded behavior; the reset-watermark semantics are
  unchanged. (RURL-ohepgzyf)

- A present-but-empty `query`, `fragment`, `user`, or `password`
  component (e.g. the query of `"https://example.com/?"`) is now
  reported as `NA` consistently.
  [`curl::curl_parse_url()`](https://jeroen.r-universe.dev/curl/reference/curl_parse_url.html)
  returns such components as `NULL` on some libcurl versions and `""` on
  others; both now normalize to `NA`, so output no longer depends on the
  installed libcurl version. This matches the behavior already produced
  on platforms where curl returned `NULL`. (RURL-dkwrebdt)

### Behavior changes

- [`safe_parse_urls()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md)
  now accepts a factor input, coercing it to its character labels up
  front (matching
  [`canonical_join()`](https://bart-turczynski.github.io/rurl/reference/canonical_join.md)),
  instead of returning an all-`error` row for every element.
  (RURL-actrnerd)

### Bug fixes

- path/fragment/userinfo are no longer percent-decoded during parsing;
  `path_encoding = 'keep'` now honors its contract (leaves the path
  byte-for-byte); the raw query is preserved (`?flag` stays `flag`, not
  `flag=`). NOTE: `clean_url` values change for URLs containing
  percent-encoded path bytes — since `clean_url` is a `canonical_join`
  key, `/a%2Fb` and `/a/b` no longer collide. (RURL-yuozrhop)

## rurl 1.4.1

### Bug fixes

- [`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md)/[`safe_parse_urls()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md)
  now recognize IPv6 address literals that carry an embedded dotted-quad
  IPv4 tail (RFC 4291 §2.2 form 3 / §2.5.5, e.g. `[::ffff:127.0.0.1]`,
  `[64:ff9b::8.8.8.8]`). Previously these fell through to the
  registered-name path, returning `is_ip_host = FALSE` and a spurious
  `warning-invalid-tld` status; they now report `is_ip_host = TRUE` and
  `parse_status = "ok"`. Both the dotted and hex-hextet spellings of the
  same address (`[::ffff:0808:0808]` vs `[::ffff:7f00:1]`) now classify
  identically. A malformed embedded tail (octet out of range) is still
  rejected. (RURL-tvfpeocg)

### Infrastructure

- Added a dependency vulnerability audit against the Sonatype OSS Index
  via `oysteR` (new `Suggests`). `tests/testthat/test-security.R` runs
  `oysteR::expect_secure("rurl")` and a dedicated `security-audit.yml`
  workflow (weekly + on demand) executes it with OSS Index credentials;
  the test skips cleanly without credentials, offline, or on CRAN.
  (RURL-tyfshnat)
- Added a second, token-free dependency vulnerability audit against the
  OSV database (<https://osv.dev>) via `rosv` (new `Suggests`).
  `tests/testthat/test-osv.R` checks the runtime dependency closure of
  rurl (recursive `Depends` + `Imports`) at installed versions, and a
  dedicated `osv-audit.yml` workflow (weekly + on demand) executes it
  with no secrets; the test skips cleanly offline or on CRAN.
  (RURL-ttkwljva)

## rurl 1.4.0

### Dependencies

- The `pslr` dependency floor is now `>= 1.0.2` and the `punycoder`
  floor is `>= 1.2.0`. Those releases form the coordinated
  `punycoder 1.2.0` host-normalization API pair, so a fresh install
  pulls a compatible set; `rurl` should be submitted after both
  dependency updates are on CRAN.

### Accessor improvements

- [`get_path()`](https://bart-turczynski.github.io/rurl/reference/get_path.md)
  gains `path_normalization`, `index_page_handling`,
  `trailing_slash_handling`, and `path_encoding` arguments, matching the
  corresponding options of
  [`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md).
- [`get_scheme()`](https://bart-turczynski.github.io/rurl/reference/get_scheme.md)
  gains `scheme_relative_handling`.
- [`get_parse_status()`](https://bart-turczynski.github.io/rurl/reference/get_parse_status.md)
  gains `source` (mapped to `tld_source`) so warning statuses can be
  queried under a specific PSL section.
- [`get_clean_url()`](https://bart-turczynski.github.io/rurl/reference/get_clean_url.md)
  and
  [`get_host()`](https://bart-turczynski.github.io/rurl/reference/get_host.md)
  gain `source` (mapped to `tld_source`).
- [`get_host()`](https://bart-turczynski.github.io/rurl/reference/get_host.md)
  gains `host_encoding`.
- [`get_domain()`](https://bart-turczynski.github.io/rurl/reference/get_domain.md),
  [`get_tld()`](https://bart-turczynski.github.io/rurl/reference/get_tld.md),
  and
  [`get_subdomain()`](https://bart-turczynski.github.io/rurl/reference/get_subdomain.md)
  gain `host_encoding`, mirroring
  [`get_host()`](https://bart-turczynski.github.io/rurl/reference/get_host.md).

All new arguments default to the same values as
[`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md),
so existing calls are unaffected.

### Behavior change

- The domain-family accessors
  ([`get_domain()`](https://bart-turczynski.github.io/rurl/reference/get_domain.md),
  [`get_tld()`](https://bart-turczynski.github.io/rurl/reference/get_tld.md),
  [`get_subdomain()`](https://bart-turczynski.github.io/rurl/reference/get_subdomain.md))
  now follow `host_encoding` (default `"keep"`) instead of always
  returning Unicode. Under `"keep"` the emitted domain/TLD/ subdomain
  mirrors the input host’s own spelling: an A-label (`xn--…`) host
  yields A-label parts, a Unicode host yields Unicode parts. Pass
  `host_encoding = "unicode"` for the previous always-decoded output, or
  `"idna"` to force A-labels. This makes the domain accessors consistent
  with
  [`get_host()`](https://bart-turczynski.github.io/rurl/reference/get_host.md),
  whose `host_encoding` already defaulted to `"keep"`.

### Internal

- Parse-status string literals replaced by named constants
  (`R/status-constants.R`) and predicates (`.is_ok_status()`,
  `.is_warning_status()`, `.is_joinable_status()`).
- Cache touchpoints in `R/zzz.R` now driven from a single
  `.CACHE_REGISTRY` instead of repeating cache names by hand.
- Cleared the `lintr`/`goodpractice` findings across `R/` and the tests
  (e.g. `fixed = TRUE` dot splits, condition-message construction,
  dropped unnecessary lambdas) with no behavior change.
- `.lintr` now mirrors `goodpractice`’s linter set, so a local
  [`lintr::lint_package()`](https://lintr.r-lib.org/reference/lint.html)
  matches the `goodpractice` report; intentional test-idiom deviations
  are documented in the config header.
- Restored 100% line coverage: added targeted tests for the
  `.punycode_to_unicode("")`, `.host_is_ace()`, and `.cache_enabled()`
  guard branches and the `derive_parse_status()` NA-host-dot fallback
  (and fixed an over-escaped regex literal that left the scheme-slash NA
  guard untested). The two genuinely unreachable `www`-prefix
  regex-capture fallbacks are now marked `# nocov` with justification.
- Reduced the cyclomatic complexity of
  [`canonical_join()`](https://bart-turczynski.github.io/rurl/reference/canonical_join.md)
  (47→7),
  [`get_subdomain()`](https://bart-turczynski.github.io/rurl/reference/get_subdomain.md)
  (26→6),
  [`rurl_cache_config()`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_config.md)
  (23→5), and
  [`safe_parse_urls()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md)
  (19→3) by extracting named sub-helpers (e.g.
  `.cj_validate_inputs()`/`.cj_resolve_sides()`/`.cj_build_join_df()`,
  `.subdomain_labels()`, `.validate_max_full_parse()`,
  `.spu_coerce_original()`). No behavior change; no function in the
  package now exceeds the `goodpractice` cyclocomp threshold of 15.

### Documentation & metadata

- Added package-level documentation
  ([`?rurl`](https://bart-turczynski.github.io/rurl/reference/rurl-package.md)
  / `man/rurl-package.Rd`) via a `"_PACKAGE"` sentinel, so the
  maintainer ORCID, package URLs, and the cross-promotion of
  `pslr`/`punycoder` now render on a help/landing page.
- Enabled roxygen2 markdown (`Roxygen: list(markdown = TRUE)`),
  regenerating all `man/*.Rd` (inline backticks now render as
  `\code{}`).
- Fixed the stale `inst/CITATION`: it now reads the version from package
  metadata (was hardcoded `0.2.0`), uses the correct title, and carries
  the maintainer ORCID. Added a root `CITATION.cff`.
- Added `X-schema.org-keywords`, the r-universe URL, and a
  `codemeta.json` for discoverability.
- Maintainer email simplified to `bartek@turczynski.pl`.

## rurl 1.3.0

### Dependencies

- Public Suffix List matching is now delegated to the `pslr` package
  (`Imports: pslr (>= 1.0.1)`). `rurl` no longer ships its own processed
  copy of the list (`R/sysdata.rda`) or its embedded matcher, and
  `data-raw/update_psl.R` has been removed. `punycoder` is now required
  at `>= 1.1.0`.

### Behavior changes (PSL correctness)

The embedded matcher used through 1.2.0 was not fully spec-correct.
Delegating to `pslr` fixes the following; outputs change accordingly:

- **Wildcard rules (`*.`)** are now honored by TLD extraction. For
  example `get_tld("a.b.kobe.jp")` is now `"b.kobe.jp"` (was
  `"kobe.jp"`).
- **Exception rules (`!`)** are now honored by TLD extraction. For
  example `get_tld("www.ck")` is now `"ck"` (was `"www.ck"`), and
  `get_tld("foo.ck")` is now `"foo.ck"` (was `"ck"`).
- **IDN hosts** now resolve a registered domain in every section. For
  example `get_domain("example.рф")` is now `"example.рф"` (was `NA`).
- [`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md)
  /
  [`safe_parse_urls()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md)
  now derive the `domain` field using the requested `tld_source` rather
  than always using the combined list, so `domain` and `tld` are
  consistent within a parse. Under `tld_source = "private"` (or
  `"icann"`), a host with no suffix in that section now has
  `domain = NA`; consequently `subdomain_levels_to_keep` is a no-op for
  such hosts (there is no registered domain to trim toward). The default
  `tld_source = "all"` is unaffected.
- Hosts under an unknown TLD continue to return `NA` for both domain and
  TLD (`rurl` queries `pslr` with `unknown = "na"`), rather than
  treating an unknown single label as a public suffix.

### Cache changes

- The per-host `domain` and `tld` memoization caches have been removed;
  `pslr` caches its own query results.
  [`rurl_cache_config()`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_config.md)
  and
  [`rurl_cache_info()`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_info.md)
  now cover only `full_parse`, `puny_encode`, and `puny_decode`, and the
  `domain` / `tld` arguments to
  [`rurl_cache_config()`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_config.md)
  no longer exist.

## rurl 1.2.0

CRAN release: 2026-06-19

### Dependencies

- `punycoder` (used for IDNA/Punycode encoding and decoding) is now on
  CRAN. `DESCRIPTION` requires `punycoder (>= 1.0.0)`.

### Behavior changes

- The package-wide default for `case_handling` is now `"lower_host"`
  (was `"keep"` for
  [`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md),
  [`safe_parse_urls()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md),
  [`get_clean_url()`](https://bart-turczynski.github.io/rurl/reference/get_clean_url.md),
  and the `get_*()` accessors, and `"lower"` for
  [`get_path()`](https://bart-turczynski.github.io/rurl/reference/get_path.md)).
  This is the RFC 3986 §6.2.2.1 normalization: the case-insensitive
  scheme and host fold to lowercase while the case-sensitive path is
  preserved. With the previous defaults, hosts such as `WWW.Example.COM`
  and `www.example.com` did not fold to one identity, and
  [`get_path()`](https://bart-turczynski.github.io/rurl/reference/get_path.md)
  silently lowercased paths (two pages that differ only by path casing
  collapsed to one). Pass `case_handling = "keep"` to restore the
  previous reconstruction, or `"lower"` to lowercase the whole URL
  including the path. (RURL-lzepdnmm)

## rurl 1.1.0

### New features

- [`canonical_join()`](https://bart-turczynski.github.io/rurl/reference/canonical_join.md)
  gains `name_A` / `name_B` arguments to set the output original-URL
  column names explicitly. They default to `NULL`, preserving the
  previous `deparse(substitute())` behavior; supply them for stable
  names when piping or passing anonymous inputs
  (e.g. `canonical_join(df[df$x > 1, ], get_b())`), which otherwise
  produced unstable column names. (RURL-fsygrelr)

- [`canonical_join()`](https://bart-turczynski.github.io/rurl/reference/canonical_join.md)
  gains a `join_parse_status` argument controlling which parse statuses
  yield joinable keys. The default `"ok"` preserves the previous
  behavior (only `ok*` statuses join); `"ok_or_warning"` additionally
  treats the parseable-but-suspicious `warning-*` statuses
  (`warning-no-tld`, `warning-invalid-tld`, `warning-public-suffix`) as
  joinable, at the cost of more potential false-positive matches.
  (RURL-edqdrvfu)

- Cache introspection and configuration.
  [`rurl_cache_info()`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_info.md)
  reports the entry count, enabled state, and any bound for each
  memoization cache (`full_parse`, `domain`, `tld`).
  [`rurl_cache_config()`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_config.md)
  enables or disables individual caches and sets an optional
  `max_full_parse` bound on the full-parse cache (default `Inf`,
  preserving the previous unbounded behavior); when the bound is reached
  the cache is reset so peak memory stays bounded. The `domain` and
  `tld` caches remain unbounded by design — they grow with the number of
  unique hosts, not with URL/option combinations — and can be disabled
  for workloads with very many unique hosts. (RURL-iuotpaqs)

### Bug fixes

- [`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md)
  now returns `port` as an integer (or `NA_integer_`), and
  [`safe_parse_urls()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md)
  no longer errors on URLs that contain an explicit port
  (e.g. `http://example.com:8080/path`). Previously the scalar parser
  returned the port as a character string and the vectorized parser
  aborted. (RURL-fxyzanfg)
- Bracketed IPv6 hosts (e.g. `http://[2001:db8::1]/`) are now correctly
  detected as IP hosts: `is_ip_host` is `TRUE`, `parse_status` is
  `"ok"`, and no TLD/domain derivation is attempted — matching how IPv4
  hosts were already handled. An over-escaped detection pattern
  previously prevented this. (RURL-jpqjndld)

### Behavior changes (potentially breaking)

- `subdomain_levels_to_keep = N` (for `N > 0`) now keeps the `N`
  rightmost subdomain labels as documented, instead of silently
  retaining all subdomains. For example,
  `safe_parse_url("http://deep.sub.domain.example.com", subdomain_levels_to_keep = 1)`
  now returns host `domain.example.com` (was
  `deep.sub.domain.example.com`). `N = 0` (strip all) is unchanged. Code
  that relied on the previous no-op behavior for `N > 0` will see
  different output. (RURL-szumhumv)

### Documentation

- Documented `clean_url` composition: it is a normalized canonical key
  built from scheme, host, and path only. Port, query, fragment, and
  userinfo are intentionally excluded, and with
  `path_encoding = "decode"` the path is shown decoded (human-readable,
  not guaranteed URL-safe). This matches the existing behavior and the
  key used by
  [`canonical_join()`](https://bart-turczynski.github.io/rurl/reference/canonical_join.md)
  — no behavior change. Corrected a `lower_host` description that
  implied userinfo could be retained in `clean_url`, and fixed a README
  example whose input contained a literal space (now percent-encoded) so
  it parses as documented. (RURL-jnboujtd)

------------------------------------------------------------------------

## rurl 0.3.0

This release adds powerful capabilities for URL normalization and
canonical dataset joining. It significantly improves robustness in
handling malformed or inconsistent URLs.

### Highlights

- New `case_handling` and `trailing_slash_handling` parameters in
  [`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md)
  and
  [`get_clean_url()`](https://bart-turczynski.github.io/rurl/reference/get_clean_url.md)
  provide greater control over URL formatting.
- Introduced
  [`canonical_join()`](https://bart-turczynski.github.io/rurl/reference/canonical_join.md)
  for joining datasets on normalized URL keys.
- Improved handling of non-standard or malformed schemes like `htp://`.
- Fixed parsing for schemeless URLs with ports (e.g.,
  `example.com:8080/path`).
- More reliable fallback when
  [`curl::curl_parse_url()`](https://jeroen.r-universe.dev/curl/reference/curl_parse_url.html)
  fails internally.
- Corrected regular expressions for IPv6 parsing.

------------------------------------------------------------------------

## rurl 0.2.0

- First version for a potential CRAN submission.
- Fully tested across macOS, Windows, and Linux.
- Achieved 100% unit test coverage.
- Improved README and documentation.

This release adds robust support for internationalized domain names
(IDNs), improves punycode handling, and ensures accurate extraction of
TLDs and registered domains.

### Highlights

- Accurate TLD extraction for both ASCII and Unicode domains
- Graceful fallback when `urltools` is unavailable
- NFC normalization with `stringi`
- 100% test coverage with edge cases and punycode validation
- Improved internal helpers and clearer test diagnostics

## rurl 0.1.3

### Improvements

- Removed the dependency on the `psl` package.
- Implemented an internal registered domain extraction using the Public
  Suffix List.
- Added internal `update_psl.R` script to fetch and process the PSL
  during development.
- Improved test coverage to 100%.
- Cleaned up exports and internal helpers.
- Updated ignores.
- Tested on macOS, Windows, and Linux via rhub and win-builder.  
- CRAN checks pass with 0 errors/warnings and only standard notes.

### Documentation

- README updated to reflect the use of the PSL and internal domain
  logic.
- LICENSE and attribution clarified for MIT + Mozilla Public Suffix
  List.

## rurl 0.1.2

### Stabilization & Coverage

- Achieved **100% test coverage**.
- Added examples to all exported functions.
- Improved documentation (`@param`, `@return`, etc.) for CRAN
  compliance.
- Cleaned up `NAMESPACE` and removed unnecessary functions like
  `hello()`.
- Refined URL parsing logic and improved output consistency.

## rurl 0.1.0

- All `get_*()` functions are now vectorized and work on character
  vectors.
- Deprecated scalar-only behavior.
- Internal parsing made more robust using `curl` and `psl`.
- Ready for use in `mutate()` and other tidy workflows.
