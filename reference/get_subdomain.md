# Get URL subdomains

Extracts the subdomain component of a URL.

## Usage

``` r
get_subdomain(
  url,
  protocol_handling = "keep",
  www_handling = "none",
  source = c("all", "private", "icann"),
  include_www = FALSE,
  format = c("string", "labels"),
  host_encoding = c("keep", "idna", "unicode")
)
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

- www_handling:

  A character string specifying how to handle "www" and `www[number]`
  prefixes in the host. Defaults to "none".

  - "none": (Default) Leaves the host's www prefix (or lack thereof)
    untouched.

  - "strip": Removes any "www." or `www[number].` prefix.

  - "keep": Ensures the host starts with "www.". If it has
    `www[number].`, it's normalized to "www.". If no www prefix, "www."
    is added. An empty input host remains empty.

  - "if_no_subdomain": If the host is a bare registered domain (e.g.,
    "example.com"), "www." is added. If the host already has a "www." or
    `www[number].` prefix, it is normalized to "www." (e.g.,
    "www1.example.com" becomes "www.example.com"; "www1.sub.example.com"
    becomes "www.sub.example.com"). If a non-www subdomain exists (e.g.,
    "sub.example.com" or the normalized "www.sub.example.com"), the host
    is not further altered. An empty input host remains empty.

- source:

  Which PSL source to use: "all", "private", or "icann".

- include_www:

  Logical; if FALSE (default), removes a leading `www`/`www[0-9]*` label
  only when it is the sole subdomain label.

- format:

  Return format: "string" (default) or "labels" for a character vector
  of labels.

- host_encoding:

  How to present the host in `clean_url`. Defaults to "keep".

  - "keep": Leave host as parsed by curl (may preserve original case).

  - "idna": Convert Unicode host labels to Punycode (IDNA) for the
    cleaned URL.

  - "unicode": Decode Punycode labels to Unicode for the cleaned URL.

## Value

A character vector (format="string") or list of label vectors
(format="labels").

## Examples

``` r
get_subdomain("http://www.blog.example.co.uk")
#> [1] "www.blog"
get_subdomain("http://www.blog.example.co.uk", format = "labels")
#> [[1]]
#> [1] "www"  "blog"
#> 
```
