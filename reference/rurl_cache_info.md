# Inspect the rurl memoization caches

Reports the number of entries currently held in each memoization cache,
along with whether the cache is enabled and any configured entry bound.

## Usage

``` r
rurl_cache_info()
```

## Value

A data.frame with one row per cache (`full_parse`, `puny_encode`,
`puny_decode`) and columns `entries`, `enabled`, and `max_entries`.

## See also

[`rurl_cache_config`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_config.md),
[`rurl_clear_caches`](https://bart-turczynski.github.io/rurl/reference/rurl_clear_caches.md)

## Examples

``` r
get_domain("https://www.example.com")
#> https://www.example.com 
#>           "example.com" 
rurl_cache_info()
#>         cache entries enabled max_entries
#> 1  full_parse      34    TRUE       10000
#> 2 puny_encode       0   FALSE         Inf
#> 3 puny_decode       0    TRUE         Inf
```
