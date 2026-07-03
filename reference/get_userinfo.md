# Get URL userinfo

Extracts the userinfo component of a URL (user or user:password).

## Usage

``` r
get_userinfo(url, protocol_handling = "keep")
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

## Value

A character vector of userinfo values.

## Examples

``` r
get_userinfo("ftp://alice:secret@ftp.example.com/file.txt")
#> [1] "alice:secret"
get_userinfo("ftp://alice@ftp.example.com/file.txt")
#> [1] "alice"
```
