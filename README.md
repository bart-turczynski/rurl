rurl
================

`rurl` is a lightweight, vectorized toolkit for URL parsing,
normalization, extraction, permutation, and matching in R.

Current package capabilities include:

- Robust parsing via `safe_parse_url()` and `safe_parse_urls()`
- URL normalization with fine-grained controls for protocol, `www`,
  case, trailing slashes, index pages, path normalization,
  scheme-relative URLs, host encoding, and path encoding
- URL component extractors (`get_*` helpers)
- Variant generation with `permute_url()`
- URL-based joins with `canonical_join()` and `permutation_join()`
- Built-in memoization caches with `rurl_clear_caches()`

## Installation

``` r
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
- Permutations: `permute_url()`
- Matching/joining:
  - `canonical_join()` for deterministic canonical-key joins
  - `permutation_join()` for broader matching across generated variants
- Cache control: `rurl_clear_caches()`

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
- `case_handling`: `keep`, `lower`, `upper`, `lower_host`
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
get_clean_url("http://münich.com/a b",
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

## Permutations and URL Joins

Generate variants with `permute_url()`:

``` r
permute_url(
  "Example.com/path",
  protocol_handling = c("http", "https"),
  www_handling = c("none", "keep"),
  trailing_slash_handling = c("none", "keep"),
  include_rank = TRUE
)
```

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

`permutation_join()` matches rows when any generated permutation
overlaps:

``` r
permutation_join(
  A, B,
  keep = "best",
  include_join_rank = TRUE,
  protocol_handling = c("http", "https"),
  www_handling = c("none", "keep"),
  trailing_slash_handling = c("none", "keep")
)
```

## Caching

`rurl` memoizes parse/domain/TLD work to speed repeated operations over
large URL vectors. To clear caches in a long-running session:

``` r
rurl_clear_caches()
```

## Public Suffix List Data

`rurl` ships with a processed copy of the [Public Suffix List
(PSL)](https://publicsuffix.org/) and never downloads it at runtime.

To refresh it:

``` r
source("data-raw/update_psl.R")
```

This regenerates internal data used for domain and TLD extraction.

## License

MIT © 2025 Bart Turczynski
