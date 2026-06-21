# rurl v1 Release Notes

This is the first stable GitHub release of `rurl`, published as `v1`. It
reflects package version `1.0.0` as declared in `DESCRIPTION`.

## What `rurl` Provides

`rurl` is a vectorized R toolkit for URL parsing, normalization,
extraction, and matching.

Core capabilities:

- Safe parsing with
  [`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md)
  and
  [`safe_parse_urls()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md)
- URL normalization with configurable handling for:
  - protocol
  - `www`
  - case
  - trailing slash
  - index pages
  - path normalization
  - scheme-relative URLs
  - host encoding (IDNA/Unicode)
  - path encoding
- URL component accessors (`get_*` helpers)
- Dataset joins based on canonical URLs with
  [`canonical_join()`](https://bart-turczynski.github.io/rurl/reference/canonical_join.md)
- Built-in memoization caches with
  [`rurl_clear_caches()`](https://bart-turczynski.github.io/rurl/reference/rurl_clear_caches.md)

## Included in This Release

This `v1` release includes the current `1.0.0` functionality, including:

- Flexible normalization controls (`case_handling`,
  `trailing_slash_handling`, and related options)
- Canonical URL joins
- Improved handling of malformed schemes and schemeless URLs with ports
- Safer parsing fallbacks and improved IPv6 parsing reliability

## Installation

``` r

# install.packages("remotes")
remotes::install_github("bart-turczynski/rurl")
```

## Notes

- This release tag is `v1`.
- The R package version for this release is `1.0.0`.
- For full historical package changes, see `NEWS.md`.
