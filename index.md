# rurl

`rurl` is a lightweight, vectorized toolkit for URL parsing,
normalization, extraction, and matching in R.

Current package capabilities include:

- Robust parsing via
  [`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md)
  and
  [`safe_parse_urls()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md)
- URL normalization with fine-grained controls for protocol, `www`,
  case, trailing slashes, index pages, path normalization,
  scheme-relative URLs, host encoding, and path encoding
- Opt-in query-string handling: drop trackers, keep contentful params,
  and audit params across a URL set with
  [`query_param_summary()`](https://bart-turczynski.github.io/rurl/reference/query_param_summary.md)
- URL component extractors (`get_*` helpers)
- URL-based joins with
  [`canonical_join()`](https://bart-turczynski.github.io/rurl/reference/canonical_join.md)
- Built-in memoization caches with introspection and configuration
  ([`rurl_cache_info()`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_info.md),
  [`rurl_cache_config()`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_config.md),
  [`rurl_clear_caches()`](https://bart-turczynski.github.io/rurl/reference/rurl_clear_caches.md))

## Installation

``` r

# From CRAN
install.packages("rurl")

# Development version from GitHub
# install.packages("remotes")
remotes::install_github("bart-turczynski/rurl")
```

## Function Overview

- Parsing and normalization:
  [`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md),
  [`safe_parse_urls()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md),
  [`get_clean_url()`](https://bart-turczynski.github.io/rurl/reference/get_clean_url.md)
- Accessors:
  [`get_scheme()`](https://bart-turczynski.github.io/rurl/reference/get_scheme.md),
  [`get_host()`](https://bart-turczynski.github.io/rurl/reference/get_host.md),
  [`get_subdomain()`](https://bart-turczynski.github.io/rurl/reference/get_subdomain.md),
  [`get_domain()`](https://bart-turczynski.github.io/rurl/reference/get_domain.md),
  [`get_tld()`](https://bart-turczynski.github.io/rurl/reference/get_tld.md),
  [`get_path()`](https://bart-turczynski.github.io/rurl/reference/get_path.md),
  [`get_query()`](https://bart-turczynski.github.io/rurl/reference/get_query.md),
  [`get_fragment()`](https://bart-turczynski.github.io/rurl/reference/get_fragment.md),
  [`get_port()`](https://bart-turczynski.github.io/rurl/reference/get_port.md),
  [`get_user()`](https://bart-turczynski.github.io/rurl/reference/get_user.md),
  [`get_password()`](https://bart-turczynski.github.io/rurl/reference/get_password.md),
  [`get_userinfo()`](https://bart-turczynski.github.io/rurl/reference/get_userinfo.md),
  [`get_parse_status()`](https://bart-turczynski.github.io/rurl/reference/get_parse_status.md)
- Query introspection:
  [`query_param_summary()`](https://bart-turczynski.github.io/rurl/reference/query_param_summary.md)
- Matching/joining:
  [`canonical_join()`](https://bart-turczynski.github.io/rurl/reference/canonical_join.md)
  for deterministic canonical-key joins
- Cache control:
  [`rurl_cache_info()`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_info.md),
  [`rurl_cache_config()`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_config.md),
  [`rurl_clear_caches()`](https://bart-turczynski.github.io/rurl/reference/rurl_clear_caches.md)

## Quick Start

[`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md)
is the core workhorse. It returns parsed components and a normalized
`clean_url`.

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
path**. By default the query is dropped and port, fragment, and userinfo
are always excluded — read them from the dedicated components
([`get_port()`](https://bart-turczynski.github.io/rurl/reference/get_port.md),
[`get_query()`](https://bart-turczynski.github.io/rurl/reference/get_query.md),
[`get_fragment()`](https://bart-turczynski.github.io/rurl/reference/get_fragment.md),
[`get_userinfo()`](https://bart-turczynski.github.io/rurl/reference/get_userinfo.md))
instead. The query can be selectively kept on `clean_url` via
`query_handling` on either
[`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md)
or
[`get_clean_url()`](https://bart-turczynski.github.io/rurl/reference/get_clean_url.md)
(see [Query handling](#query-handling)). With `path_encoding = "decode"`
the path is shown decoded, so `clean_url` is human-readable rather than
guaranteed URL-safe.

Scheme-relative URL handling is configurable:

``` r

safe_parse_url("//example.com/path", scheme_relative_handling = "keep")$parse_status
#> [1] "ok-scheme-relative"
safe_parse_url("//example.com/path", scheme_relative_handling = "https")$clean_url
#> [1] "https://example.com/path"
```

For vectors, use
[`safe_parse_urls()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md):

``` r

safe_parse_urls(c("example.com", "https://www.example.com/path"))[, c("original_url", "clean_url", "parse_status")]
```

## Normalization Controls

[`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md)
and
[`get_clean_url()`](https://bart-turczynski.github.io/rurl/reference/get_clean_url.md)
support these controls:

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
- `query_handling`: `drop` (default), `filter`, `allow`, `keep` (see
  [Query handling](#query-handling))

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

## Query handling

By default `clean_url` is query-free (`query_handling = "drop"`), so a
tracker like `?utm_source=` never changes the canonical key. Opt in to
keep contentful params while stripping known trackers (`utm_*`,
`fbclid`, `gclid`, …) via a built-in denylist:

``` r

# "filter": keep contentful params, drop known trackers
get_clean_url("https://youtube.com/watch?v=dQw4w9&utm_source=nl",
              query_handling = "filter")
#> [1] "https://youtube.com/watch?v=dQw4w9"

# "allow": keep ONLY the names you list (params_keep is the allowlist)
get_clean_url("https://shop.com/p?id=42&ref=abc&fbclid=xyz",
              query_handling = "allow", params_keep = "id")
#> [1] "https://shop.com/p?id=42"

# "keep": keep every param, canonicalized (optionally sorted)
get_clean_url("https://ex.com/?b=2&a=1",
              query_handling = "keep", sort_params = TRUE)
#> [1] "https://ex.com/?a=1&b=2"
```

The same engine arguments flow through
[`safe_parse_urls()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md)
(and therefore
[`canonical_join()`](https://bart-turczynski.github.io/rurl/reference/canonical_join.md)),
so filtering also shapes the join key — `?v=1` and `?v=2` stay distinct
while `utm`-only differences still collapse under `"filter"`:

``` r

safe_parse_urls(c("https://ex.com/?v=1&utm_source=x", "https://ex.com/?v=2"),
                query_handling = "filter")[, c("original_url", "clean_url")]
#>                       original_url           clean_url
#> 1 https://ex.com/?v=1&utm_source=x https://ex.com/?v=1
#> 2              https://ex.com/?v=2 https://ex.com/?v=2
```

[`get_query()`](https://bart-turczynski.github.io/rurl/reference/get_query.md)
takes the same arguments (defaulting to `query_handling = "keep"`) to
pull a cleaned query directly:

``` r

get_query("https://youtube.com/watch?v=dQw4w9&utm_source=nl",
          query_handling = "filter")
#> [1] "v=dQw4w9"
```

Audit a URL set before choosing a policy with
[`query_param_summary()`](https://bart-turczynski.github.io/rurl/reference/query_param_summary.md),
whose `would_drop` column previews what `"filter"` would remove:

``` r

urls <- c(
  "https://ex.com/?utm_source=nl&id=42",
  "https://ex.com/watch?v=abc&utm_source=x",
  "https://ex.com/?id=99"
)
query_param_summary(urls)
#>        param n n_urls example_value                             example_url would_drop
#> 1 utm_source 2      2            nl     https://ex.com/?utm_source=nl&id=42       TRUE
#> 2         id 2      2            42     https://ex.com/?utm_source=nl&id=42      FALSE
#> 3          v 1      1           abc https://ex.com/watch?v=abc&utm_source=x      FALSE
```

## URL Joins

[`canonical_join()`](https://bart-turczynski.github.io/rurl/reference/canonical_join.md)
matches on one canonicalized key per URL and is the preferred option for
large datasets:

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

[`rurl_cache_config()`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_config.md)
covers three caches: `full_parse`, `puny_encode`, and `puny_decode`. The
`full_parse` cache is unbounded by default (`max_full_parse = Inf`); set
a bound to cap its peak memory. The `puny_encode` and `puny_decode`
caches are unbounded by design and can be disabled for workloads with
very many unique hosts.

## Public Suffix List

Domain and TLD extraction is delegated to the
[`pslr`](https://github.com/bart-turczynski/pslr) package, which owns
the Public Suffix List and its refresh cycle. `rurl` ships no embedded
copy of the list. To update the PSL, call
[`pslr::psl_refresh()`](https://bart-turczynski.github.io/pslr/reference/psl_refresh.html)
(see the `pslr` documentation for details).

## Acknowledgments

These packages build on data, libraries, and prior work from many
others. See
[ACKNOWLEDGMENTS.md](https://bart-turczynski.github.io/rurl/ACKNOWLEDGMENTS.md)
for the full list of thanks.

## Related packages

`rurl` is part of a small ecosystem of R packages by the same author:

- **[pslr](https://bart-turczynski.github.io/pslr/)** — the Public
  Suffix List engine that powers domain and TLD extraction in `rurl`.
  Use it directly when you need raw eTLD / registrable-domain queries
  without full URL parsing.
- **[punycoder](https://bart-turczynski.github.io/punycoder/)** — the
  Punycode and IDNA codec that `rurl` uses for internationalized host
  handling. Useful on its own for host normalization and Unicode ↔︎ ACE
  round-trips.

## Citation

If you use `rurl` in your work, please cite it. Run `citation("rurl")`
for the current citation, or see
[`CITATION.cff`](https://bart-turczynski.github.io/rurl/CITATION.cff).

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
