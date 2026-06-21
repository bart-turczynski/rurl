# Get URL fragments

Extracts the fragment component of a URL.

## Usage

``` r
get_fragment(url, protocol_handling = "keep")
```

## Arguments

- url:

  A character vector of URLs.

- protocol_handling:

  A character string specifying how to handle protocols. Defaults to
  "keep".

  - "keep": If a scheme exists (http, https, ftp, ftps), it's used. If
    no scheme, "http://" is added.

  - "none": If a scheme exists, it's used. If no scheme, then no scheme
    is used (scheme component will be NA).

  - "strip": Any existing scheme is removed (scheme component will be
    NA).

  - "http": The scheme is forced to be "http".

  - "https": The scheme is forced to be "https".

## Value

A character vector of fragments.

## Examples

``` r
get_fragment("http://example.com/path#section")
#> http://example.com/path#section 
#>                       "section" 
```
