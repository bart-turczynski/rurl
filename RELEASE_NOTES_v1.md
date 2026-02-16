# rurl v1 Release Notes

This is the first stable GitHub release of `rurl`, published as `v1`.
It reflects the current package implementation (version `0.3.0` in `DESCRIPTION`).

## What `rurl` Provides

`rurl` is a vectorized R toolkit for URL parsing, normalization, extraction, permutation, and matching.

Core capabilities:

- Safe parsing with `safe_parse_url()` and `safe_parse_urls()`
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
- URL permutation with `permute_url()`
- Dataset joins based on canonical or permuted URLs:
  - `canonical_join()`
  - `permutation_join()`
- Built-in memoization caches with `rurl_clear_caches()`

## Included in This Release

This `v1` release includes all current functionality from package release `0.3.0`, including:

- Flexible normalization controls (`case_handling`, `trailing_slash_handling`, and related options)
- URL permutation generation for robust matching workflows
- Canonical and permutation-based URL joins
- Improved handling of malformed schemes and schemeless URLs with ports
- Safer parsing fallbacks and improved IPv6 parsing reliability

## Installation

```r
# install.packages("remotes")
remotes::install_github("bart-turczynski/rurl")
```

## Notes

- This release tag is `v1`.
- The R package version remains `0.3.0` as declared in `DESCRIPTION`.
- For full historical package changes, see `NEWS.md`.
