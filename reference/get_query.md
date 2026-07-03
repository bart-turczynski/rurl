# Get URL query strings

Extracts the query component of a URL, optionally parsing it into a
list.

## Usage

``` r
get_query(
  url,
  protocol_handling = "keep",
  format = c("string", "list"),
  decode = TRUE,
  query_handling = c("keep", "drop", "filter", "allow"),
  params_keep = NULL,
  params_drop = NULL,
  params_case_sensitive = FALSE,
  sort_params = FALSE,
  empty_param_handling = c("keep", "drop"),
  decode_plus = FALSE
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

- format:

  Return format: "string" (default) or "list" for parsed elements.

- decode:

  Logical; if TRUE (default), percent-decodes the query (the whole
  string for format="string", keys/values for format="list"). Set FALSE
  to obtain the query as written: the raw query for the default
  `query_handling = "keep"`, or the canonical re-encoded form (uppercase
  hex, `%20`, `%26`/`%3D`) once any filtering is requested.

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

- params_case_sensitive:

  Logical (default `FALSE`). Controls whether the denylist and
  `params_keep`/`params_drop` matching is case-sensitive.

- sort_params:

  Logical (default `FALSE`). When `TRUE`, surviving params are stably
  sorted by decoded key. Active in "filter"/"allow"/"keep".

- empty_param_handling:

  One of "keep" (default) or "drop". "drop" removes empty-valued params
  (e.g. `?ref=`), except those rescued by `params_keep` in "filter"
  mode.

- decode_plus:

  Logical (default `FALSE`). When `TRUE`, `+` in query values is treated
  as a space (HTML-form decoding) before percent-decoding. `FALSE` keeps
  `+` literal (RFC 3986 generic behavior).

## Value

A character vector (format="string") or list (format="list").

## Details

The underlying parse preserves the raw query string byte-for-byte (a
bare key such as `?flag` stays `flag`, not `flag=`). By default this
accessor still percent-decodes for readability (`decode = TRUE`); pass
`decode = FALSE` to obtain the raw query exactly as written in the URL.

The filtering arguments (`query_handling`, `params_keep`, `params_drop`,
`params_case_sensitive`, `sort_params`, `empty_param_handling`,
`decode_plus`) share the engine used by
[`get_clean_url`](https://bart-turczynski.github.io/rurl/reference/get_clean_url.md),
but default to `query_handling = "keep"` here: an accessor returns the
query as found unless you ask it to filter. When no filtering or
reordering is requested (the default profile), the output is
byte-for-byte identical to earlier releases; once you opt in, the
surviving params are selected first and only then rendered per
`format`/`decode`.

## Examples

``` r
get_query("http://example.com/path?a=1&b=2")
#> [1] "a=1&b=2"
get_query("http://example.com/path?a=1&b=2", format = "list")
#> [[1]]
#> [[1]]$a
#> [1] "1"
#> 
#> [[1]]$b
#> [1] "2"
#> 
#> 
# Drop trackers, keep contentful params:
get_query(
  "http://example.com/?utm_source=nl&id=42",
  query_handling = "filter"
)
#> [1] "id=42"
# Canonical (re-encoded) form:
get_query(
  "http://example.com/?a=1%262",
  query_handling = "keep", decode = FALSE
)
#> [1] "a=1%262"
```
