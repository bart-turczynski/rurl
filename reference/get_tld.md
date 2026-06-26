# Extract the top-level domain (TLD) from a URL

Uses safe_parse_url internally to extract the TLD, benefiting from all
memoization layers for improved performance.

## Usage

``` r
get_tld(
  url,
  source = c("all", "private", "icann"),
  host_encoding = c("keep", "idna", "unicode")
)
```

## Arguments

- url:

  A character vector of URLs.

- source:

  Which TLD source to use: "all", "icann", or "private".

- host_encoding:

  How to present the host in `clean_url`. Defaults to "keep".

  - "keep": Leave host as parsed by curl (may preserve original case).

  - "idna": Convert Unicode host labels to Punycode (IDNA) for the
    cleaned URL.

  - "unicode": Decode Punycode labels to Unicode for the cleaned URL.

## Value

A character vector of TLDs.

## Examples

``` r
get_tld("example.com")
#> example.com 
#>       "com" 
```
