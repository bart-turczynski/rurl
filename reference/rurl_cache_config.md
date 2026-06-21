# Configure the rurl memoization caches

Enables or disables individual caches and sets an optional bound on the
`full_parse` cache. Called with no arguments, it leaves the
configuration unchanged and returns the current state.

## Usage

``` r
rurl_cache_config(
  full_parse = NULL,
  puny_encode = NULL,
  puny_decode = NULL,
  max_full_parse = NULL
)
```

## Arguments

- full_parse:

  Logical; enable/disable the full URL parse cache.

- puny_encode:

  Logical; enable/disable the IDNA/Punycode encode cache.

- puny_decode:

  Logical; enable/disable the Punycode decode cache.

- max_full_parse:

  A single number (\\\ge 1\\) or `Inf` bounding the `full_parse` cache.

## Value

Invisibly, the updated
[`rurl_cache_info`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_info.md)
data.frame.

## Details

Disabling a cache stops new writes to it (existing entries are left in
place until
[`rurl_clear_caches`](https://bart-turczynski.github.io/rurl/reference/rurl_clear_caches.md)
is called). When `full_parse` reaches `max_full_parse` entries, the
*entire* cache is cleared before the next new entry is stored, so its
peak size never exceeds the bound. This is a hard reset-watermark, not
an LRU or FIFO eviction policy: `max_full_parse` caps peak memory, but
is *not* a working-set size — once the bound is hit the cache empties
completely and rebuilds from scratch. The default of `Inf` preserves the
historical unbounded behavior. The `puny_encode` and `puny_decode`
caches are unbounded by design (each stays small — bounded by the number
of unique hosts/labels seen, not URL+option combinations).

## See also

[`rurl_cache_info`](https://bart-turczynski.github.io/rurl/reference/rurl_cache_info.md),
[`rurl_clear_caches`](https://bart-turczynski.github.io/rurl/reference/rurl_clear_caches.md)

## Examples

``` r
rurl_cache_config(max_full_parse = 10000)
rurl_cache_config(puny_encode = FALSE)
rurl_cache_config() # inspect current configuration
```
