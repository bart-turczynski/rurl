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

A character vector of userinfo values.

## Examples

``` r
get_userinfo("ftp://alice:secret@ftp.example.com/file.txt")
#> ftp://alice:secret@ftp.example.com/file.txt 
#>                              "alice:secret" 
get_userinfo("ftp://alice@ftp.example.com/file.txt")
#> ftp://alice@ftp.example.com/file.txt 
#>                              "alice" 
```
