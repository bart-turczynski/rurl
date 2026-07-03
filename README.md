rurl
================

<!-- badges: start -->

[![R-CMD-check](https://github.com/bart-turczynski/rurl/actions/workflows/verify.yml/badge.svg)](https://github.com/bart-turczynski/rurl/actions/workflows/verify.yml)
[![CRAN
status](https://www.r-pkg.org/badges/version/rurl)](https://CRAN.R-project.org/package=rurl)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/rurl)](https://CRAN.R-project.org/package=rurl)
[![Codecov
coverage](https://codecov.io/gh/bart-turczynski/rurl/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bart-turczynski/rurl)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.20972584.svg)](https://doi.org/10.5281/zenodo.20972584)
[![Zenodo](https://img.shields.io/badge/Zenodo-all_software-1682D4?logo=zenodo&logoColor=white)](https://zenodo.org/search?q=metadata.creators.person_or_org.identifiers.identifier:0000-0002-8788-7980)
[![FOSSA
Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Fbart-turczynski%2Frurl.svg?type=shield&issueType=security)](https://app.fossa.com/projects/git%2Bgithub.com%2Fbart-turczynski%2Frurl?ref=badge_shield&issueType=security)
[![FOSSA
Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Fbart-turczynski%2Frurl.svg?type=shield&issueType=license)](https://app.fossa.com/projects/git%2Bgithub.com%2Fbart-turczynski%2Frurl?ref=badge_shield&issueType=license)
[![OpenSSF Best
Practices](https://www.bestpractices.dev/projects/13394/badge)](https://www.bestpractices.dev/projects/13394)
[![Security
audit](https://github.com/bart-turczynski/rurl/actions/workflows/security-audit.yml/badge.svg)](https://github.com/bart-turczynski/rurl/actions/workflows/security-audit.yml)
[![OSV
audit](https://github.com/bart-turczynski/rurl/actions/workflows/osv-audit.yml/badge.svg)](https://github.com/bart-turczynski/rurl/actions/workflows/osv-audit.yml)
<!-- badges: end -->

`rurl` is a lightweight, vectorized toolkit for URL parsing,
normalization, extraction, and matching in R.

Current package capabilities include:

- Robust parsing via `safe_parse_url()` and `safe_parse_urls()`
- URL normalization with fine-grained controls for protocol, `www`,
  case, trailing slashes, index pages, path normalization,
  scheme-relative URLs, host encoding, and path encoding
- URL component extractors (`get_*` helpers)
- URL-based joins with `canonical_join()`
- Built-in memoization caches with introspection and configuration
  (`rurl_cache_info()`, `rurl_cache_config()`, `rurl_clear_caches()`)

## Installation

``` r
# From CRAN
install.packages("rurl")

# Development version from GitHub
# install.packages("remotes")
remotes::install_github("bart-turczynski/rurl")
```

## Function Overview

- Parsing and normalization: `safe_parse_url()`, `safe_parse_urls()`,
  `get_clean_url()`
- Accessors: `get_scheme()`, `get_host()`, `get_subdomain()`,
  `get_domain()`, `get_tld()`, `get_path()`, `get_query()`,
  `get_fragment()`, `get_port()`, `get_user()`, `get_password()`,
  `get_userinfo()`, `get_parse_status()`
- Matching/joining: `canonical_join()` for deterministic canonical-key
  joins
- Cache control: `rurl_cache_info()`, `rurl_cache_config()`,
  `rurl_clear_caches()`

## Quick Start

`safe_parse_url()` is the core workhorse. It returns parsed components
and a normalized `clean_url`.

``` r
library(rurl)

parsed <- safe_parse_url(
  "HTTP://www.Example.com/a//b/../index.html?x=1#frag",
  protocol_handling = "https",
  www_handling = "strip",
  case_handling = "lower_host",
  trailing_slash_handling = "strip",
  index_page_handling = "strip",
  path_normalization = "both",
  host_encoding = "idna",
  path_encoding = "encode"
)

parsed$clean_url
#> [1] "https://example.com/a"
parsed$parse_status
#> [1] "ok"
```

`clean_url` is a normalized canonical key built from **scheme, host, and
path** by default. Port, fragment, and userinfo are always excluded — read
them from the dedicated components (`get_port()`, `get_fragment()`,
`get_userinfo()`) instead. The query is dropped by default too, but you can
opt in to a shaped query on the cleaned URL by passing `query_handling` (and
its `params_*` / `sort_params` / `empty_param_handling` / `decode_plus`
companions) to either `safe_parse_url()` or `get_clean_url()`:

```r
get_clean_url("http://example.com/p?utm_source=nl&id=42") # query dropped
#> [1] "http://example.com/p"
get_clean_url(
  "http://example.com/p?utm_source=nl&id=42",
  query_handling = "filter" # strip trackers, keep contentful params
)
#> [1] "http://example.com/p?id=42"
```

With `path_encoding = "decode"` the path is shown decoded, so `clean_url` is
human-readable rather than guaranteed URL-safe.

Scheme-relative URL handling is configurable:

``` r
safe_parse_url("//example.com/path", scheme_relative_handling = "keep")$parse_status
#> [1] "ok-scheme-relative"
safe_parse_url("//example.com/path", scheme_relative_handling = "https")$clean_url
#> [1] "https://example.com/path"
```

For vectors, use `safe_parse_urls()`:

``` r
safe_parse_urls(c("example.com", "https://www.example.com/path"))[, c("original_url", "clean_url", "parse_status")]
```

## Normalization Controls

`safe_parse_url()` and `get_clean_url()` support these controls:

- `protocol_handling`: `keep`, `none`, `strip`, `http`, `https`
- `www_handling`: `none`, `strip`, `keep`, `if_no_subdomain`
- `case_handling`: `lower_host` (default), `keep`, `lower`, `upper`
- `trailing_slash_handling`: `none`, `keep`, `strip`
- `index_page_handling`: `keep`, `strip`
- `path_normalization`: `none`, `collapse_slashes`, `dot_segments`,
  `both`
- `scheme_relative_handling`: `keep`, `http`, `https`, `error`
- `host_encoding`: `keep`, `idna`, `unicode`
- `path_encoding`: `keep`, `encode`, `decode`
- `subdomain_levels_to_keep`: `NULL`, `0`, or `N > 0`

Subdomain retention is applied after `www_handling`:

``` r
get_host("http://www.three.two.one.example.com", www_handling = "strip", subdomain_levels_to_keep = 1)
#> [1] "one.example.com"
get_clean_url("http://www.deep.sub.example.com/path", subdomain_levels_to_keep = 0)
#> [1] "http://www.example.com/path"
```

Host and path encoding controls:

``` r
get_clean_url("http://münich.com/a%20b",
              host_encoding = "idna",
              path_encoding = "encode",
              case_handling = "lower_host")
#> [1] "http://xn--mnich-kva.com/a%20b"

get_clean_url("http://xn--mnich-kva.com/a%20b",
              host_encoding = "unicode",
              path_encoding = "decode",
              case_handling = "keep")
#> [1] "http://münich.com/a b"
```

## Accessors

``` r
u <- "https://user:pass@www.blog.example.co.uk/path/to/page?a=1&b=2#frag"

get_scheme(u)
get_host(u)
get_subdomain(u)
get_domain(u)
get_tld(u)
get_path(u)
get_query(u)
get_query(u, format = "list")
get_fragment(u)
get_port(u)
get_user(u)
get_password(u)
get_userinfo(u)
get_parse_status(c(u, "mailto:test@example.com"))
```

## URL Joins

`canonical_join()` matches on one canonicalized key per URL and is the
preferred option for large datasets:

``` r
A <- data.frame(URL = c("http://Example.com/Page", "http://example.com/Other"),
                ValA = 1:2, stringsAsFactors = FALSE)
B <- data.frame(URL = c("https://www.example.com/Page/", "http://example.com/Miss"),
                ValB = c("x", "y"), stringsAsFactors = FALSE)

canonical_join(
  A, B,
  protocol_handling = "strip",
  www_handling = "strip",
  case_handling = "lower_host",
  trailing_slash_handling = "strip"
)
```

## Caching

`rurl` memoizes URL parsing and punycode round-trips to speed repeated
operations over large URL vectors; PSL query caching lives in `pslr`.
Inspect, clear, and configure the caches:

``` r
rurl_cache_info()                          # entries / enabled / max per cache
rurl_clear_caches()                        # free memory in a long-running session
rurl_cache_config(max_full_parse = 1e5)    # bound the full-parse cache
rurl_cache_config(puny_encode = FALSE)     # disable a cache entirely
```

`rurl_cache_config()` covers three caches: `full_parse`, `puny_encode`,
and `puny_decode`. The `full_parse` cache is unbounded by default
(`max_full_parse = Inf`); set a bound to cap its peak memory. The
`puny_encode` and `puny_decode` caches are unbounded by design and can
be disabled for workloads with very many unique hosts.

## Public Suffix List

Domain and TLD extraction is delegated to the
[`pslr`](https://github.com/bart-turczynski/pslr) package, which owns
the Public Suffix List and its refresh cycle. `rurl` ships no embedded
copy of the list. To update the PSL, call `pslr::psl_refresh()` (see the
`pslr` documentation for details).

## Acknowledgments

These packages build on data, libraries, and prior work from many
others. See [ACKNOWLEDGMENTS.md](ACKNOWLEDGMENTS.md) for the full list
of thanks.

## Related packages

`rurl` is part of a small ecosystem of R packages by the same author:

- **[pslr](https://bart-turczynski.github.io/pslr/)** — the Public
  Suffix List engine that powers domain and TLD extraction in `rurl`.
  Use it directly when you need raw eTLD / registrable-domain queries
  without full URL parsing.
- **[punycoder](https://bart-turczynski.github.io/punycoder/)** — the
  Punycode and IDNA codec that `rurl` uses for internationalized host
  handling. Useful on its own for host normalization and Unicode ↔ ACE
  round-trips.

## Citation

If you use `rurl` in your work, please cite it. Run `citation("rurl")`
for the current citation, or see [`CITATION.cff`](CITATION.cff).

Each release is archived on Zenodo. Cite the concept DOI
[10.5281/zenodo.20972584](https://doi.org/10.5281/zenodo.20972584) to
refer to the software in general (it always resolves to the latest
version), or the version-specific DOI shown on the [Zenodo
record](https://doi.org/10.5281/zenodo.20972584) for a particular
release.

## Code of Conduct

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.

## License

MIT © 2026 Bart Turczynski
