# Package index

## Parsing

Parse one or many URLs into structured components.

- [`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md)
  : Parse a URL comprehensively, extracting and deriving all relevant
  components.
- [`safe_parse_urls()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md)
  : Parse multiple URLs and return a data.frame of components

## Accessors

Extract individual URL components from character vectors.

- [`get_clean_url()`](https://bart-turczynski.github.io/rurl/reference/get_clean_url.md)
  : Get cleaned URLs
- [`get_parse_status()`](https://bart-turczynski.github.io/rurl/reference/get_parse_status.md)
  : Get the parse status of URLs
- [`get_scheme()`](https://bart-turczynski.github.io/rurl/reference/get_scheme.md)
  : Get URL schemes
- [`get_host()`](https://bart-turczynski.github.io/rurl/reference/get_host.md)
  : Get URL hosts
- [`get_domain()`](https://bart-turczynski.github.io/rurl/reference/get_domain.md)
  : Get domain names
- [`get_tld()`](https://bart-turczynski.github.io/rurl/reference/get_tld.md)
  : Extract the top-level domain (TLD) from a URL
- [`get_subdomain()`](https://bart-turczynski.github.io/rurl/reference/get_subdomain.md)
  : Get URL subdomains
- [`get_path()`](https://bart-turczynski.github.io/rurl/reference/get_path.md)
  : Get URL paths
- [`get_query()`](https://bart-turczynski.github.io/rurl/reference/get_query.md)
  : Get URL query strings
- [`get_fragment()`](https://bart-turczynski.github.io/rurl/reference/get_fragment.md)
  : Get URL fragments
- [`get_port()`](https://bart-turczynski.github.io/rurl/reference/get_port.md)
  : Get URL ports
- [`get_user()`](https://bart-turczynski.github.io/rurl/reference/get_user.md)
  : Get URL user names
- [`get_password()`](https://bart-turczynski.github.io/rurl/reference/get_password.md)
  : Get URL passwords
- [`get_userinfo()`](https://bart-turczynski.github.io/rurl/reference/get_userinfo.md)
  : Get URL userinfo

## Query introspection

Audit query parameters across a set of URLs.

- [`query_param_summary()`](https://bart-turczynski.github.io/rurl/reference/query_param_summary.md)
  : Summarize query parameters across a set of URLs

## Joining

Join datasets on canonicalized URL keys.

- [`canonical_join()`](https://bart-turczynski.github.io/rurl/reference/canonical_join.md)
  : Canonical Join of Two URL Sets (Base R Version)

## Cache management

Inspect and control the memoization caches.

- [`rurl_cache_info()`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_info.md)
  : Inspect the rurl memoization caches
- [`rurl_cache_config()`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_config.md)
  : Configure the rurl memoization caches
- [`rurl_clear_caches()`](https://bart-turczynski.github.io/rurl/reference/rurl_clear_caches.md)
  : Clear all rurl caches
