# Get URL query strings

Extracts the query component of a URL, optionally parsing it into a
list.

## Usage

``` r
get_query(
  url,
  protocol_handling = "keep",
  format = c("string", "list"),
  decode = TRUE
)
```

## Arguments

- url:

  A character vector of URLs.

- protocol_handling:

  A character string specifying how to handle protocols. Defaults to
  "keep". Regardless of this option, rurl only processes authority-based
  URLs whose scheme is one of http, https, ftp, or ftps; a
  scheme-bearing input with any other scheme (e.g. `mailto:`, `tel:`,
  `ws:`) yields `parse_status = "error"`. Scheme inference (below) also
  requires the input to be host-shaped: a scheme-less string that is not
  a host (e.g. `"asdfghjkl"`, `"12345"`, `"/path"`) or is a
  non-canonical IP literal (integer/hex/octal/short forms, or
  leading-zero octets like `"192.168.010.1"`) is rejected as `"error"`
  rather than having a scheme fabricated for it.

  - "keep": If a supported scheme exists (http, https, ftp, ftps), it's
    used. If no scheme and the input is host-shaped, "http://" is added;
    otherwise the input is not a URL and yields `"error"`.

  - "none": If a supported scheme exists, it's used. If no scheme, then
    no scheme is used (scheme component will be NA).

  - "strip": Any existing scheme is removed (scheme component will be
    NA).

  - "http": The scheme is forced to be "http".

  - "https": The scheme is forced to be "https".

- format:

  Return format: "string" (default) or "list" for parsed elements.

- decode:

  Logical; if TRUE (default), percent-decodes the query (the whole
  string for format="string", keys/values for format="list"). Set FALSE
  to return the raw query as written in the URL.

## Value

A character vector (format="string") or list (format="list").

## Details

The underlying parse preserves the raw query string byte-for-byte (a
bare key such as `?flag` stays `flag`, not `flag=`). By default this
accessor still percent-decodes for readability (`decode = TRUE`); pass
`decode = FALSE` to obtain the raw query exactly as written in the URL.

## Examples

``` r
get_query("http://example.com/path?a=1&b=2")
#> [1] "a=1&b=2"
get_query("http://example.com/path?a=1&b=2", format = "list")
#> [[1]]
#> [[1]]$a
#> [1] "1"
#> 
#> [[1]]$b
#> [1] "2"
#> 
#> 
```
