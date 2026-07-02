# Get URL user names

Extracts the user component of a URL. The value is returned raw, exactly
as written in the URL (not percent-decoded).

## Usage

``` r
get_user(url, protocol_handling = "keep")
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

A character vector of user names.

## Examples

``` r
get_user("ftp://alice:secret@ftp.example.com/file.txt")
#> [1] "alice"
```
