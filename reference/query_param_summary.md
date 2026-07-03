# Summarize query parameters across a set of URLs

Tabulates which query parameters appear across a vector of URLs and what
values they take, with a `would_drop` column previewing what
`query_handling = "filter"` would remove. Useful for auditing a URL set
before choosing a cleaning policy: see the trackers before you strip
them.

## Usage

``` r
query_param_summary(
  urls,
  level = c("param", "value"),
  params_keep = NULL,
  params_drop = NULL,
  params_case_sensitive = FALSE,
  empty_param_handling = c("keep", "drop"),
  decode_plus = FALSE
)
```

## Arguments

- urls:

  A character vector of URLs.

- level:

  One of "param" (default) for one row per distinct parameter name, or
  "value" for one row per distinct (parameter, value) pair.

- params_keep:

  Character vector of parameter-name globs (only `*` is special), or
  `NULL` (default). In "filter" mode this is the rescue list; in "allow"
  mode it is the allowlist. Ignored in "drop"/"keep".

- params_drop:

  Character vector of parameter-name globs to add to the built-in
  denylist in "filter" mode, or `NULL` (default). Ignored in
  "drop"/"allow"/"keep".

- params_case_sensitive:

  Logical (default `FALSE`). Controls whether the denylist and
  `params_keep`/`params_drop` matching is case-sensitive.

- empty_param_handling:

  One of "keep" (default) or "drop". "drop" removes empty-valued params
  (e.g. `?ref=`), except those rescued by `params_keep` in "filter"
  mode.

- decode_plus:

  Logical (default `FALSE`). When `TRUE`, `+` in query values is treated
  as a space (HTML-form decoding) before percent-decoding. `FALSE` keeps
  `+` literal (RFC 3986 generic behavior).

## Value

A flat (long) `data.frame`. For `level = "param"`: `param`, `n` (total
occurrences), `n_urls` (distinct URLs containing the param),
`example_value`, `example_url`, `would_drop`. For `level = "value"`:
`param`, `value`, `n`, `n_urls`, `example_url`, `would_drop`. The
`example_*` columns and the param-level `would_drop` reflect the
first-seen occurrence (deterministic given input order). Returns a
zero-row `data.frame` with the level's columns when no URL carries a
query.

## Details

Parameter names are grouped *faithfully* (case-sensitively and by their
decoded spelling), so `utm_source` and `UTM_SOURCE` are reported as
separate rows. The `would_drop` preview, by contrast, honours
`params_case_sensitive`: with the default
`params_case_sensitive = FALSE`, `UTM_SOURCE` matches the built-in
denylist and shows `would_drop = TRUE`; set it to `TRUE` and the
upper-case spelling no longer matches. The raw `query` field is only
read, never mutated.

## Examples

``` r
urls <- c(
  "http://example.com/?utm_source=nl&id=42",
  "http://example.com/watch?v=abc&utm_source=x",
  "http://example.com/?id=99"
)
query_param_summary(urls)
#>        param n n_urls example_value                                 example_url
#> 1 utm_source 2      2            nl     http://example.com/?utm_source=nl&id=42
#> 2         id 2      2            42     http://example.com/?utm_source=nl&id=42
#> 3          v 1      1           abc http://example.com/watch?v=abc&utm_source=x
#>   would_drop
#> 1       TRUE
#> 2      FALSE
#> 3      FALSE
query_param_summary(urls, level = "value")
#>        param value n n_urls                                 example_url
#> 1 utm_source    nl 1      1     http://example.com/?utm_source=nl&id=42
#> 2         id    42 1      1     http://example.com/?utm_source=nl&id=42
#> 3          v   abc 1      1 http://example.com/watch?v=abc&utm_source=x
#> 4 utm_source     x 1      1 http://example.com/watch?v=abc&utm_source=x
#> 5         id    99 1      1                   http://example.com/?id=99
#>   would_drop
#> 1       TRUE
#> 2      FALSE
#> 3      FALSE
#> 4       TRUE
#> 5      FALSE
# Preview a custom policy:
query_param_summary(urls, params_drop = "id")
#>        param n n_urls example_value                                 example_url
#> 1 utm_source 2      2            nl     http://example.com/?utm_source=nl&id=42
#> 2         id 2      2            42     http://example.com/?utm_source=nl&id=42
#> 3          v 1      1           abc http://example.com/watch?v=abc&utm_source=x
#>   would_drop
#> 1       TRUE
#> 2       TRUE
#> 3      FALSE
```
