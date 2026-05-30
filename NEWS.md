## rurl 1.0.0.9000 (development)

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
