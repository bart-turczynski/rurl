# Get URL schemes

Extracts the scheme (protocol) of a URL.

## Usage

``` r
get_scheme(url, protocol_handling = "keep", scheme_relative_handling = "keep")
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

- scheme_relative_handling:

  How to handle URLs starting with "//". Defaults to "keep".

  - "keep": Parse using http but return scheme as NA and set status to
    "ok-scheme-relative".

  - "http": Assume http for parsing and output.

  - "https": Assume https for parsing and output.

  - "error": Treat scheme-relative URLs as invalid.

## Value

A character vector of URL schemes.

## Examples

``` r
get_scheme("https://example.com")
#> [1] "https"
```
