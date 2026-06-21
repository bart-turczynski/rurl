# Canonical Join of Two URL Sets (Base R Version)

Performs a join between two data frames by canonicalizing URLs to a
shared "clean" format using
[`safe_parse_urls`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md)
and then matching on that key. This is suitable for large crawl exports.

## Usage

``` r
canonical_join(
  data_A,
  data_B,
  col_A = "URL",
  col_B = "URL",
  suffix_A = "_A",
  suffix_B = "_B",
  name_A = NULL,
  name_B = NULL,
  join = c("inner", "left", "right", "full"),
  collision = c("first", "all", "error"),
  on_parse_error = c("keep", "drop", "error"),
  join_parse_status = c("ok", "ok_or_warning"),
  ...
)
```

## Arguments

- data_A:

  A data frame containing URLs for the left side of the join.

- data_B:

  A data frame containing URLs for the right side of the join.

- col_A:

  Character string, the name of the column in `data_A` that contains
  URLs. Defaults to "URL".

- col_B:

  Character string, the name of the column in `data_B` that contains
  URLs. Defaults to "URL".

- suffix_A:

  Character string, suffix to append to `data_A` columns (excluding the
  URL column) in the output. Defaults to "\_A".

- suffix_B:

  Character string, suffix to append to `data_B` columns (excluding the
  URL column) in the output. Defaults to "\_B".

- name_A:

  Character string, the name of the output column holding the original
  `data_A` URLs. Defaults to `NULL`, in which case the name is derived
  from the `data_A` argument expression via `deparse(substitute())`.
  Supply an explicit value for stable output names when piping or
  passing anonymous inputs (e.g.
  `canonical_join(df[df$x > 1, ], get_b())`).

- name_B:

  Character string, the name of the output column holding the original
  `data_B` URLs. Defaults to `NULL`; behaves like `name_A` for `data_B`.

- join:

  Join type: `"inner"`, `"left"`, `"right"`, or `"full"`. Defaults to
  `"inner"`.

- collision:

  How to handle duplicate canonical keys within inputs. `"first"` keeps
  the first row per key, `"all"` keeps all rows (many-to-many), and
  `"error"` stops on duplicates. Defaults to `"first"`.

- on_parse_error:

  How to handle URLs that fail canonicalization. `"keep"` retains them
  as unmatched rows (for left/right/full joins), `"drop"` removes them
  before joining, and `"error"` stops. Defaults to `"keep"`.

- join_parse_status:

  Which parse statuses yield joinable canonical keys. `"ok"` (default)
  joins only rows whose `parse_status` begins with `"ok"` (`"ok"`,
  `"ok-ftp"`, `"ok-scheme-relative"`). `"ok_or_warning"` additionally
  treats parseable-but-suspicious `warning-*` statuses
  (`"warning-no-tld"`, `"warning-invalid-tld"`,
  `"warning-public-suffix"`) as joinable. Joining on warning statuses
  can increase false-positive matches between distinct hosts that both
  fail TLD derivation.

- ...:

  Additional arguments forwarded to
  [`safe_parse_urls`](https://bart-turczynski.github.io/rurl/reference/safe_parse_urls.md),
  controlling canonicalization (e.g., `protocol_handling`,
  `www_handling`, `trailing_slash_handling`, `index_page_handling`,
  `path_normalization`, `scheme_relative_handling`, `host_encoding`,
  `path_encoding`).

## Value

A data frame representing the join. The output includes:

- The original URL columns (named via `name_A` / `name_B`, or after the
  input expressions when those are `NULL`).

- `JoinKey`: the canonicalized URL used for matching.

- All other columns from `data_A` and `data_B` with suffixes applied.

Returns an empty data frame with the expected structure if no matches
are found or if inputs are invalid.

## Examples

``` r
A <- data.frame(
  URL = c("http://Example.com/Page", "http://example.com/Other"),
  ValA = 1:2, stringsAsFactors = FALSE
)
B <- data.frame(
  URL = c("https://www.example.com/Page/", "http://example.com/Miss"),
  ValB = c("x", "y"), stringsAsFactors = FALSE
)

canonical_join(
  A, B,
  protocol_handling = "strip",
  www_handling = "strip",
  case_handling = "lower_host",
  trailing_slash_handling = "strip"
)
#>                         A                             B          JoinKey ValA_A
#> 1 http://Example.com/Page https://www.example.com/Page/ example.com/Page      1
#>   ValB_B
#> 1      x
```
