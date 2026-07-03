# Get URL paths

Extracts the path component of a URL.

## Usage

``` r
get_path(
  url,
  protocol_handling = "keep",
  case_handling = c("lower_host", "keep", "lower", "upper"),
  trailing_slash_handling = c("none", "keep", "strip"),
  index_page_handling = c("keep", "strip"),
  path_normalization = c("none", "collapse_slashes", "dot_segments", "both"),
  path_encoding = c("keep", "encode", "decode")
)
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

- case_handling:

  How to handle casing of the returned path. Defaults to "lower_host",
  which preserves the path's original casing (paths are case-sensitive
  per RFC 3986 §6.2.2.1). Use "lower"/"upper" to force a case.

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

A character vector of URL paths.

## Examples

``` r
get_path("http://example.com/some/path?query=1")
#> [1] "/some/path"
```
