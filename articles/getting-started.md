# Getting Started with rurl

## Introduction

The `rurl` package provides tools to parse, normalize, and extract
information from URLs using a consistent and safe API.  
It is fully vectorized and delegates domain handling to the `pslr`
package, which implements the [Public Suffix
List](https://publicsuffix.org) for accurate domain and TLD extraction.

## Safe URL Parsing

Use
[`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md)
to parse URLs robustly:

``` r

safe_parse_url("https://sub.example.co.uk/path?q=1")
#> $original_url
#> [1] "https://sub.example.co.uk/path?q=1"
#> 
#> $scheme
#> [1] "https"
#> 
#> $host
#> [1] "sub.example.co.uk"
#> 
#> $port
#> [1] NA
#> 
#> $path
#> [1] "/path"
#> 
#> $query
#> [1] "q=1"
#> 
#> $fragment
#> [1] NA
#> 
#> $user
#> [1] NA
#> 
#> $password
#> [1] NA
#> 
#> $domain
#> [1] "example.co.uk"
#> 
#> $tld
#> [1] "co.uk"
#> 
#> $is_ip_host
#> [1] FALSE
#> 
#> $clean_url
#> [1] "https://sub.example.co.uk/path"
#> 
#> $parse_status
#> [1] "ok"
```

The `protocol_handling` argument controls how schemes are handled:

- `"keep"` (default; keeps the current protocol or prepends `http://` if
  missing)
- `"none"` (doesn’t add, remove, or change protocols)
- `"strip"` (removes protocols)
- `"http"` (changes protocols to `http://` or adds it if missing)
- `"https"` (changes protocols to `https://` or adds it if missing)

## Extracting URL Components

``` r

get_scheme("https://sub.example.com")
#> https://sub.example.com 
#>                 "https"
get_host("https://sub.example.com")
#> https://sub.example.com 
#>       "sub.example.com"
get_path("https://sub.example.com/path/to/page")
#> https://sub.example.com/path/to/page 
#>                      "/path/to/page"
```

Each function works on vectors of URLs and gracefully handles `NA`.

## Domain and TLD Parsing

These functions rely on the Public Suffix List:

``` r

get_domain("https://a.b.example.co.uk")
#> https://a.b.example.co.uk 
#>           "example.co.uk"
```

Extracting TLDs from different sources:

``` r

get_tld("https://foo.blogspot.com")
#> https://foo.blogspot.com 
#>           "blogspot.com"
```

Sources include: - `"all"` (default; will match to the longest available
TLD) - `"private"` (only extract private TLDs) - `"icann"` (only extract
ICANN TLDs)

## Vectorization and Edge Cases

All core functions support vectors and handle malformed inputs safely:

``` r

urls <- c("example.com", "http://example.com", NA)
get_clean_url(urls)
#>           example.com    http://example.com                  <NA> 
#> "http://example.com/" "http://example.com/"                    NA
```

## Advanced Host Manipulation with `subdomain_levels_to_keep`

Several functions, including
[`safe_parse_url()`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md),
[`get_host()`](https://bart-turczynski.github.io/rurl/reference/get_host.md),
and
[`get_clean_url()`](https://bart-turczynski.github.io/rurl/reference/get_clean_url.md),
support the `subdomain_levels_to_keep` argument. This allows for
fine-grained control over how many subdomain levels are preserved in the
host component of a URL, *after* initial `www_handling` has been
applied.

- `NULL` (Default): No specific subdomain stripping is performed beyond
  `www_handling`.
- `0`: All subdomains are stripped. If `www_handling` preserved or added
  ‘www.’, it remains (e.g., ‘www.sub.example.com’ becomes
  ‘www.example.com’; ‘sub.example.com’ becomes ‘example.com’).
- `N > 0`: Keeps up to N levels of subdomains, counted from
  right-to-left (closest to the registered domain), in addition to any
  ‘www.’ prefix.

Here are some examples demonstrating its effect on
[`get_host()`](https://bart-turczynski.github.io/rurl/reference/get_host.md):

``` r

get_host(
  "http://www.three.two.one.example.com",
  subdomain_levels_to_keep = 0
) # www_handling default is "none"
#> http://www.three.two.one.example.com 
#>                    "www.example.com"
# Expected: "www.example.com"

get_host(
  "http://three.two.one.example.com",
  www_handling = "strip",
  subdomain_levels_to_keep = 0
)
#> http://three.two.one.example.com 
#>                    "example.com"
# Expected: "example.com"

get_host("http://www.three.two.one.example.com", subdomain_levels_to_keep = 1)
#> http://www.three.two.one.example.com 
#>                "www.one.example.com"
# Expected: "www.one.example.com"

get_host(
  "http://three.two.one.example.com",
  www_handling = "strip",
  subdomain_levels_to_keep = 1
)
#> http://three.two.one.example.com 
#>                "one.example.com"
# Expected: "one.example.com"

get_host(
  "http://www.three.two.one.example.com",
  www_handling = "keep",
  subdomain_levels_to_keep = 2
)
#> http://www.three.two.one.example.com 
#>            "www.two.one.example.com"
# Expected: "www.two.one.example.com"
```

And its effect on
[`get_clean_url()`](https://bart-turczynski.github.io/rurl/reference/get_clean_url.md):

``` r

get_clean_url(
  "http://www.deep.sub.example.com/some/path",
  subdomain_levels_to_keep = 0,
  www_handling = "keep"
)
#> http://www.deep.sub.example.com/some/path 
#>        "http://www.example.com/some/path"
# yields http://www.example.com/some/path

get_clean_url(
  "http://deep.sub.example.com/some/path",
  subdomain_levels_to_keep = 1
)
#> http://deep.sub.example.com/some/path 
#>    "http://sub.example.com/some/path"
# yields http://sub.example.com/some/path
```

Note that
[`get_domain()`](https://bart-turczynski.github.io/rurl/reference/get_domain.md)
also accepts `subdomain_levels_to_keep`, but it does not change the
*returned domain value*. The domain is derived from the host *before*
this specific host modification occurs. The parameter influences the
host component that might be used in other parts of the `safe_parse_url`
output, such as the `clean_url`.

## Summary

- Vectorized functions for parsing and cleaning URLs
- Uses the Public Suffix List for domain logic
- Unicode/punycode support
