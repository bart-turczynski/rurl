# Get URL hosts

Extracts the host component of a URL.

## Usage

``` r
get_host(
  url,
  protocol_handling = "keep",
  www_handling = "none",
  source = c("all", "private", "icann"),
  subdomain_levels_to_keep = NULL,
  case_handling = c("lower", "keep", "upper", "lower_host"),
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

  Which PSL source to use: "all", "private", or "icann". Subdomain
  trimming depends on which section is consulted, so pass
  `source = "icann"` to exclude private suffixes (e.g. github.io).

- subdomain_levels_to_keep:

  An integer or NULL. Determines how many levels of subdomains are kept,
  in addition to any 'www.' prefix handled by `www_handling`.

  - `NULL`: (Default) No specific subdomain stripping is performed
    beyond `www_handling`.

  - `0`: All subdomains are stripped. If `www_handling` preserved or
    added 'www.', it remains (e.g., 'www.sub.example.com' becomes
    'www.example.com'; 'sub.example.com' becomes 'example.com').

  - `N > 0`: Keeps up to N levels of subdomains, counted from
    right-to-left (closest to the registered domain), in addition to any
    'www.' prefix. E.g., if N=1, 'three.two.one.example.com' becomes
    'one.example.com'; 'www.three.two.one.example.com' (post
    www_handling) becomes 'www.one.example.com'.

- case_handling:

  How to handle casing of the returned host. Defaults to "lower".

- host_encoding:

  How to present the host in `clean_url`. Defaults to "keep".

  - "keep": Leave host as parsed by curl (may preserve original case).

  - "idna": Convert Unicode host labels to Punycode (IDNA) for the
    cleaned URL.

  - "unicode": Decode Punycode labels to Unicode for the cleaned URL.

## Value

A character vector of URL hosts.

## Examples

``` r
get_host("http://sub.example.com:8080")
#> http://sub.example.com:8080 
#>           "sub.example.com" 
get_host(
  "http://www.two.one.example.com",
  subdomain_levels_to_keep = 1
) # Result: "www.one.example.com"
#> http://www.two.one.example.com 
#>          "www.one.example.com" 
get_host(
  "http://www.two.one.example.com",
  www_handling = "strip",
  subdomain_levels_to_keep = 1
) # Result: "one.example.com"
#> http://www.two.one.example.com 
#>              "one.example.com" 
get_host(
  "http://www.two.one.example.com",
  www_handling = "keep",
  subdomain_levels_to_keep = 1
) # Result: "www.one.example.com"
#> http://www.two.one.example.com 
#>          "www.one.example.com" 
get_host(
  "http://three.two.one.example.com",
  subdomain_levels_to_keep = 0
) # Result: "example.com"
#> http://three.two.one.example.com 
#>                    "example.com" 
get_host(
  "http://www.three.two.one.example.com",
  subdomain_levels_to_keep = 0
) # Result: "www.example.com"
#> http://www.three.two.one.example.com 
#>                    "www.example.com" 
```
