# Parse multiple URLs and return a data.frame of components

Vectorized wrapper around
[`safe_parse_url`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md)
that returns a data.frame with one row per input URL.

## Usage

``` r
safe_parse_urls(
  url,
  protocol_handling = c("keep", "none", "strip", "http", "https"),
  www_handling = c("none", "strip", "keep", "if_no_subdomain"),
  tld_source = c("all", "private", "icann"),
  case_handling = c("lower_host", "keep", "lower", "upper"),
  trailing_slash_handling = c("none", "keep", "strip"),
  index_page_handling = c("keep", "strip"),
  path_normalization = c("none", "collapse_slashes", "dot_segments", "both"),
  scheme_relative_handling = c("keep", "http", "https", "error"),
  subdomain_levels_to_keep = NULL,
  host_encoding = c("keep", "idna", "unicode"),
  path_encoding = c("keep", "encode", "decode")
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

- tld_source:

  Which TLD source to use for TLD extraction: "all", "icann", or
  "private". Defaults to "all".

- case_handling:

  A character string specifying how to handle the case of the cleaned
  URL. Defaults to "lower_host", the RFC 3986 §6.2.2.1 normalization
  (scheme and host are case-insensitive and folded to lowercase; the
  path is case-sensitive and preserved).

  - "lower_host": (Default) Lowercases scheme and host only; the path
    keeps its original casing.

  - "keep": Preserves casing of the reconstructed URL.

  - "lower": Converts the cleaned URL to lowercase.

  - "upper": Converts the cleaned URL to uppercase.

- trailing_slash_handling:

  A character string specifying how to handle trailing slashes in the
  path component of the cleaned URL. Defaults to "none".

  - "none": (Default) No specific handling is applied. Path remains as
    is after initial parsing.

  - "keep": Ensures a trailing slash. If a path exists and doesn't end
    with one, it's added. If path is just "/", it's kept.

  - "strip": Removes a trailing slash if present, unless the path is
    solely "/".

- index_page_handling:

  A character string specifying how to handle index/default pages.
  Defaults to "keep".

  - "keep": (Default) Leave index/default page segments untouched.

  - "strip": Remove a trailing index.\* or default.\* segment
    (case-insensitive).

- path_normalization:

  How to normalize path structure. Defaults to "none".

  - "none": (Default) No normalization.

  - "collapse_slashes": Collapse duplicate slashes in the path.

  - "dot_segments": Resolve . and .. segments per RFC 3986.

  - "both": Apply both collapse_slashes and dot_segments.

- scheme_relative_handling:

  How to handle URLs starting with "//". Defaults to "keep".

  - "keep": Parse using http but return scheme as NA and set status to
    "ok-scheme-relative".

  - "http": Assume http for parsing and output.

  - "https": Assume https for parsing and output.

  - "error": Treat scheme-relative URLs as invalid.

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

- host_encoding:

  How to present the host in `clean_url`. Defaults to "keep".

  - "keep": Leave host as parsed by curl (may preserve original case).

  - "idna": Convert Unicode host labels to Punycode (IDNA) for the
    cleaned URL.

  - "unicode": Decode Punycode labels to Unicode for the cleaned URL.

- path_encoding:

  How to handle percent-encoding in the path for `clean_url`. Defaults
  to "keep".

  - "keep": Leave the path percent-encoding untouched.

  - "encode": Normalize by decoding first, then percent-encoding each
    segment (slashes preserved).

  - "decode": Percent-decode UTF-8 sequences in the path.

## Value

A data.frame with one row per URL and the same fields returned by
[`safe_parse_url`](https://bart-turczynski.github.io/rurl/reference/safe_parse_url.md).
Invalid inputs return NA fields with `parse_status = "error"`.

## Examples

``` r
safe_parse_urls(c("example.com", "https://www.example.com/path"))
#>                   original_url scheme            host port  path query fragment
#> 1                  example.com   http     example.com   NA     /  <NA>     <NA>
#> 2 https://www.example.com/path  https www.example.com   NA /path  <NA>     <NA>
#>   user password      domain tld is_ip_host                    clean_url
#> 1 <NA>     <NA> example.com com      FALSE          http://example.com/
#> 2 <NA>     <NA> example.com com      FALSE https://www.example.com/path
#>   parse_status
#> 1           ok
#> 2           ok
```
