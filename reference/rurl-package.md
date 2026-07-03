# rurl: Parse, Clean, and Normalize URLs

A lightweight toolkit for extracting structured information from URLs.
Includes functions for parsing, normalizing protocols, extracting
domains, and constructing clean URLs. Domain and public-suffix
extraction is delegated to the 'pslr' package, which implements the
Public Suffix List from <https://publicsuffix.org>. Punycode and IDNA
encoding is handled by the 'punycoder' package.

## See also

Parsing:
[`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md),
[`safe_parse_urls()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md).
Accessors:
[`get_host()`](https://bart-turczynski.github.io/rurl/reference/get_host.md),
[`get_domain()`](https://bart-turczynski.github.io/rurl/reference/get_domain.md),
[`get_tld()`](https://bart-turczynski.github.io/rurl/reference/get_tld.md),
[`get_subdomain()`](https://bart-turczynski.github.io/rurl/reference/get_subdomain.md),
[`get_path()`](https://bart-turczynski.github.io/rurl/reference/get_path.md),
[`get_query()`](https://bart-turczynski.github.io/rurl/reference/get_query.md).
Cleaning and joining:
[`get_clean_url()`](https://bart-turczynski.github.io/rurl/reference/get_clean_url.md),
[`canonical_join()`](https://bart-turczynski.github.io/rurl/reference/canonical_join.md).
Query introspection:
[`query_param_summary()`](https://bart-turczynski.github.io/rurl/reference/query_param_summary.md).
Cache management:
[`rurl_clear_caches()`](https://bart-turczynski.github.io/rurl/reference/rurl_clear_caches.md),
[`rurl_cache_info()`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_info.md),
[`rurl_cache_config()`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_config.md).

Domain and public-suffix extraction is delegated to the pslr package;
Punycode/IDNA encoding is handled by the punycoder package.

## Author

**Maintainer**: Bart Turczynski <bartek@turczynski.pl>
([ORCID](https://orcid.org/0000-0002-8788-7980))

Authors:

- Bart Turczynski <bartek@turczynski.pl>
  ([ORCID](https://orcid.org/0000-0002-8788-7980))
