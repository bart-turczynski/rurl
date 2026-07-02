# Get the parse status of URLs

Get the parse status of URLs

## Usage

``` r
get_parse_status(
  url,
  protocol_handling = "keep",
  www_handling = "none",
  subdomain_levels_to_keep = NULL,
  source = c("all", "private", "icann")
)
```

## Arguments

- url:

  A character vector of URLs to be parsed.

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

- source:

  Which PSL source to use: "all", "private", or "icann". Warning
  statuses such as `warning-no-tld`, `warning-invalid-tld`, and
  `warning-public-suffix` depend on which PSL section is consulted, so
  pass `source = "icann"` to use only ICANN-managed TLDs.

## Value

A character vector with the parse status of each URL.

## Examples

``` r
get_parse_status(
  c("http://example.com", "ftp://example.com", "mailto:user@example.com")
)
#> [1] "ok"     "ok-ftp" "error" 
get_parse_status(c("http://example.com", "not-a-url"))
#> [1] "ok"             "warning-no-tld"
get_parse_status("http://example.com", source = "icann")
#> [1] "ok"
```
