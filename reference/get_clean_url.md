# Get cleaned URLs

This function returns the cleaned version of the URLs after applying
protocol, www, case, and trailing slash handling rules. The result is a
normalized canonical key composed of scheme, host, and path only; port,
query, fragment, and userinfo are intentionally excluded (use
[`get_port`](https://bart-turczynski.github.io/rurl/reference/get_port.md),
[`get_query`](https://bart-turczynski.github.io/rurl/reference/get_query.md),
[`get_fragment`](https://bart-turczynski.github.io/rurl/reference/get_fragment.md),
or
[`get_userinfo`](https://bart-turczynski.github.io/rurl/reference/get_userinfo.md)
for those).

## Usage

``` r
get_clean_url(
  url,
  protocol_handling = "keep",
  www_handling = "none",
  source = c("all", "private", "icann"),
  case_handling = "lower_host",
  trailing_slash_handling = "none",
  index_page_handling = "keep",
  path_normalization = "none",
  scheme_relative_handling = "keep",
  subdomain_levels_to_keep = NULL,
  host_encoding = "keep",
  path_encoding = "keep"
)
```

## Arguments

- url:

  A character vector containing URLs to be parsed.

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

  - "keep": Leave the path percent-encoding untouched (the path is
    preserved byte-for-byte as written in the URL, so `%2F` stays `%2F`
    rather than decoding into a path-separating `/`).

  - "encode": Normalize by decoding first, then percent-encoding each
    segment (slashes preserved).

  - "decode": Percent-decode UTF-8 sequences in the path.

## Value

A character vector of cleaned URLs.

## Examples

``` r
get_clean_url("Example.COM/Path") # Default lower_host: host folds, path kept
#> [1] "http://example.com/Path"
get_clean_url(
  "Example.COM/Path",
  case_handling = "keep",
  trailing_slash_handling = "keep"
)
#> [1] "http://Example.COM/Path/"
get_clean_url(
  "Example.COM/Path/",
  case_handling = "upper",
  trailing_slash_handling = "strip"
)
#> [1] "HTTP://EXAMPLE.COM/PATH"
get_clean_url("http://example.com", www_handling = "strip")
#> [1] "http://example.com/"
get_clean_url(
  "http://deep.sub.domain.example.com/path",
  subdomain_levels_to_keep = 0
)
#> [1] "http://example.com/path"
# -> "http://example.com/path"
get_clean_url(
  "http://www.deep.sub.domain.example.com/path",
  subdomain_levels_to_keep = 1,
  www_handling = "strip"
)
#> [1] "http://domain.example.com/path"
# -> "http://domain.example.com/path"
get_clean_url(
  "http://www.deep.sub.domain.example.com/path",
  subdomain_levels_to_keep = 1,
  www_handling = "keep"
)
#> [1] "http://www.domain.example.com/path"
# -> "http://www.domain.example.com/path"
```
