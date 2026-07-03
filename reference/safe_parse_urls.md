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
  path_encoding = c("keep", "encode", "decode"),
  query_handling = c("drop", "filter", "allow", "keep"),
  params_keep = NULL,
  params_drop = NULL,
  sort_params = FALSE,
  empty_param_handling = c("keep", "drop"),
  params_case_sensitive = FALSE,
  decode_plus = FALSE
)
```

## Arguments

- url:

  A character vector of URLs to be parsed.

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

  How to normalize path structure. Defaults to "none". rurl owns
  dot-segment resolution: the path is read from the input verbatim (not
  from libcurl's pre-normalized path), so `"none"` preserves `.` / `..`
  segments (`/a/../b` stays `/a/../b`) and only the settings below
  change them. Resolution follows RFC 3986 section 5.2.4 and acts on
  *literal* `.`/`..` segments only — a percent-encoded `%2e` is a normal
  path byte, never a dot segment, so it is never treated as traversal.

  - "none": (Default) No normalization; dot and slash structure is
    preserved exactly as written.

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
    preserved as written in the URL, so `%2F` stays `%2F` rather than
    decoding into a path-separating `/`). The one normalization applied
    is percent-hex case: the two hex digits of each `%XX` triplet are
    uppercased, so `%2f` becomes `%2F`. This is an RFC 3986 (section
    6.2.2.1) canonicalization — the two forms are equivalent — and it
    makes such paths compare equal in
    [`canonical_join`](https://bart-turczynski.github.io/rurl/reference/canonical_join.md).
    Use "encode" to additionally normalize which bytes are encoded.

  - "encode": Normalize by decoding first, then percent-encoding each
    segment (slashes preserved).

  - "decode": Percent-decode UTF-8 sequences in the path.

- query_handling:

  A character string controlling whether (and how) the query string is
  included in `clean_url`. Defaults to "drop", which preserves the
  historical query-free `clean_url`. The raw `query` result field is
  never affected by this option — it always reports the faithful
  original query.

  - "drop": (Default) `clean_url` carries no query, exactly as before.

  - "filter": Keep contentful params, dropping known trackers via a
    built-in denylist (e.g. `utm_*`, `fbclid`, `gclid`). `params_drop`
    extends the denylist; `params_keep` rescues names (winning over both
    the denylist and empty-dropping).

  - "allow": Keep only params whose names match `params_keep`; all
    others are dropped. Here `params_keep` is an inclusion criterion
    only, not an empty-rescue.

  - "keep": Keep every param, re-encoded into canonical form (not the
    verbatim original — that stays on the `query` field).

  In every non-"drop" mode the surviving query is re-encoded canonically
  (uppercase percent-hex, spaces as `%20`) and appended after the path.
  The query is intentionally EXEMPT from `case_handling` (query values
  are case-sensitive — tokens, IDs, signatures), so under
  `case_handling = "lower"` or `"upper"` the `clean_url` is no longer
  uniformly cased: scheme/host/path fold but the query keeps its
  original case. Because `clean_url` is the
  [`canonical_join`](https://bart-turczynski.github.io/rurl/reference/canonical_join.md)
  key, any non-"drop" mode also brings the query into that join key (so
  `?id=1` and `?id=2` stop collapsing, while `utm`-only differences
  still collapse under "filter").

- params_keep:

  Character vector of parameter-name globs (only `*` is special), or
  `NULL` (default). In "filter" mode this is the rescue list; in "allow"
  mode it is the allowlist. Ignored in "drop"/"keep".

- params_drop:

  Character vector of parameter-name globs to add to the built-in
  denylist in "filter" mode, or `NULL` (default). Ignored in
  "drop"/"allow"/"keep".

- sort_params:

  Logical (default `FALSE`). When `TRUE`, surviving params are stably
  sorted by decoded key. Active in "filter"/"allow"/"keep".

- empty_param_handling:

  One of "keep" (default) or "drop". "drop" removes empty-valued params
  (e.g. `?ref=`), except those rescued by `params_keep` in "filter"
  mode.

- params_case_sensitive:

  Logical (default `FALSE`). Controls whether the denylist and
  `params_keep`/`params_drop` matching is case-sensitive.

- decode_plus:

  Logical (default `FALSE`). When `TRUE`, `+` in query values is treated
  as a space (HTML-form decoding) before percent-decoding. `FALSE` keeps
  `+` literal (RFC 3986 generic behavior).

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
