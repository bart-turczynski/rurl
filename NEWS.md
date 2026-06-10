## rurl 1.2.0

### Behavior changes

- The package-wide default for `case_handling` is now `"lower_host"` (was
  `"keep"` for `safe_parse_url()`, `safe_parse_urls()`, `get_clean_url()`, and
  the `get_*()` accessors, and `"lower"` for `get_path()`). This is the
  RFC 3986 §6.2.2.1 normalization: the case-insensitive scheme and host fold to
  lowercase while the case-sensitive path is preserved. With the previous
  defaults, hosts such as `WWW.Example.COM` and `www.example.com` did not fold
  to one identity, and `get_path()` silently lowercased paths (two pages that
  differ only by path casing collapsed to one). Pass `case_handling = "keep"`
  to restore the previous reconstruction, or `"lower"` to lowercase the whole
  URL including the path. (RURL-lzepdnmm)

## rurl 1.1.0

### New features

- `canonical_join()` gains `name_A` / `name_B` arguments to set the output
  original-URL column names explicitly. They default to `NULL`, preserving the
  previous `deparse(substitute())` behavior; supply them for stable names when
  piping or passing anonymous inputs (e.g. `canonical_join(df[df$x > 1, ],
  get_b())`), which otherwise produced unstable column names. (RURL-fsygrelr)
- `canonical_join()` gains a `join_parse_status` argument controlling which
  parse statuses yield joinable keys. The default `"ok"` preserves the previous
  behavior (only `ok*` statuses join); `"ok_or_warning"` additionally treats
  the parseable-but-suspicious `warning-*` statuses (`warning-no-tld`,
  `warning-invalid-tld`, `warning-public-suffix`) as joinable, at the cost of
  more potential false-positive matches. (RURL-edqdrvfu)

- Cache introspection and configuration. `rurl_cache_info()` reports the entry
  count, enabled state, and any bound for each memoization cache
  (`full_parse`, `domain`, `tld`). `rurl_cache_config()` enables or disables
  individual caches and sets an optional `max_full_parse` bound on the
  full-parse cache (default `Inf`, preserving the previous unbounded
  behavior); when the bound is reached the cache is reset so peak memory stays
  bounded. The `domain` and `tld` caches remain unbounded by design — they
  grow with the number of unique hosts, not with URL/option combinations — and
  can be disabled for workloads with very many unique hosts. (RURL-iuotpaqs)

### Bug fixes

- `safe_parse_url()` now returns `port` as an integer (or `NA_integer_`), and
  `safe_parse_urls()` no longer errors on URLs that contain an explicit port
  (e.g. `http://example.com:8080/path`). Previously the scalar parser returned
  the port as a character string and the vectorized parser aborted.
  (RURL-fxyzanfg)
- Bracketed IPv6 hosts (e.g. `http://[2001:db8::1]/`) are now correctly detected
  as IP hosts: `is_ip_host` is `TRUE`, `parse_status` is `"ok"`, and no
  TLD/domain derivation is attempted — matching how IPv4 hosts were already
  handled. An over-escaped detection pattern previously prevented this.
  (RURL-jpqjndld)

### Behavior changes (potentially breaking)

- `subdomain_levels_to_keep = N` (for `N > 0`) now keeps the `N` rightmost
  subdomain labels as documented, instead of silently retaining all subdomains.
  For example, `safe_parse_url("http://deep.sub.domain.example.com",
  subdomain_levels_to_keep = 1)` now returns host `domain.example.com` (was
  `deep.sub.domain.example.com`). `N = 0` (strip all) is unchanged. Code that
  relied on the previous no-op behavior for `N > 0` will see different output.
  (RURL-szumhumv)

### Documentation

- Documented `clean_url` composition: it is a normalized canonical key built
  from scheme, host, and path only. Port, query, fragment, and userinfo are
  intentionally excluded, and with `path_encoding = "decode"` the path is shown
  decoded (human-readable, not guaranteed URL-safe). This matches the existing
  behavior and the key used by `canonical_join()` — no behavior change.
  Corrected a `lower_host` description that implied userinfo could be retained
  in `clean_url`, and fixed a README example whose input contained a literal
  space (now percent-encoded) so it parses as documented. (RURL-jnboujtd)

---

## rurl v1 (GitHub Release) - 2026-02-16

- Published first stable GitHub release tag: `v1`.
- Release notes added in `RELEASE_NOTES_v1.md`.
- GitHub release page: <https://github.com/bart-turczynski/rurl/releases/tag/v1>
- Package version for this release is `1.0.0` (see `DESCRIPTION`).

---

## rurl 0.3.0

This release adds powerful capabilities for URL normalization and canonical dataset joining. It significantly improves robustness in handling malformed or inconsistent URLs.

### Highlights

- New `case_handling` and `trailing_slash_handling` parameters in `safe_parse_url()` and `get_clean_url()` provide greater control over URL formatting.
- Introduced `canonical_join()` for joining datasets on normalized URL keys.
- Improved handling of non-standard or malformed schemes like `htp://`.
- Fixed parsing for schemeless URLs with ports (e.g., `example.com:8080/path`).
- More reliable fallback when `curl::curl_parse_url()` fails internally.
- Corrected regular expressions for IPv6 parsing.

---

## rurl 0.2.0

* First version for a potential CRAN submission.
* Fully tested across macOS, Windows, and Linux.
* Achieved 100% unit test coverage.
* Improved README and documentation.

This release adds robust support for internationalized domain names (IDNs),
improves punycode handling, and ensures accurate extraction of TLDs and
registered domains.

### Highlights
- Accurate TLD extraction for both ASCII and Unicode domains
- Graceful fallback when `urltools` is unavailable
- NFC normalization with `stringi`
- 100% test coverage with edge cases and punycode validation
- Improved internal helpers and clearer test diagnostics

## rurl 0.1.3

### Improvements

- Removed the dependency on the `psl` package.
- Implemented an internal registered domain extraction using the Public Suffix List.
- Added internal `update_psl.R` script to fetch and process the PSL during development.
- Improved test coverage to 100%.
- Cleaned up exports and internal helpers.
- Updated ignores.
- Tested on macOS, Windows, and Linux via rhub and win-builder.  
- CRAN checks pass with 0 errors/warnings and only standard notes.

### Documentation

- README updated to reflect the use of the PSL and internal domain logic.
- LICENSE and attribution clarified for MIT + Mozilla Public Suffix List.

## rurl 0.1.2

### Stabilization & Coverage

- Achieved **100% test coverage**.
- Added examples to all exported functions.
- Improved documentation (`@param`, `@return`, etc.) for CRAN compliance.
- Cleaned up `NAMESPACE` and removed unnecessary functions like `hello()`.
- Refined URL parsing logic and improved output consistency.

## rurl 0.1.0

- All `get_*()` functions are now vectorized and work on character vectors.
- Deprecated scalar-only behavior.
- Internal parsing made more robust using `curl` and `psl`.
- Ready for use in `mutate()` and other tidy workflows.
