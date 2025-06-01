rurl
================

# rurl

`rurl` is a small, pipe-friendly, and vectorized R package that helps
you construct, parse, and clean URLs from various components. It is
designed to make URL manipulation and HTTP endpoint generation readable,
composable, and easy to integrate into modern R workflows.

It includes helpers to: - Get cleaned URLs with fine-grained control
over protocols, ‘www’ prefixes, letter casing, and trailing slashes. -
Extract domains, paths, schemes, hosts, and top-level domains (TLDs). -
Normalize, strip, or enforce specific protocols. - Handle ‘www’ prefixes
in hostnames.

## Installation

``` r
# Install from GitHub
# devtools::install_github("bart-turczynski/rurl")
```

## Core Functionality

The primary workhorse of the package is `safe_parse_url()`. This
function comprehensively parses a URL and allows for various
transformations. It returns a detailed list of URL components and a
cleaned URL string.

``` r
library(rurl)

# Example of detailed parsing and transformation
parsed_details <- safe_parse_url(
  "Http://Www.Example.Com/Some/Path/?Query=1#Frag",
  protocol_handling = "https",      # Force https
  www_handling = "strip",         # Remove www
  case_handling = "lower",        # Convert to lowercase (new default)
  trailing_slash_handling = "strip" # Remove trailing slash
)

# The cleaned URL (scheme, host, path only by default)
print(parsed_details$clean_url)
#> [1] "https://example.com/some/path"

# Full list of parsed and derived components
# print(parsed_details)
# Output would include:
# $original_url: "Http://Www.Example.Com/Some/Path/?Query=1#Frag"
# $scheme: "https"
# $host: "example.com"
# $port: NULL (or parsed port if present and not stripped)
# $path: "/some/path" (after trailing slash handling)
# $query: "Query=1"
# $fragment: "Frag"
# $domain: "example.com"
# $tld: "com"
# $is_ip_host: FALSE
# $clean_url: "https://example.com/some/path"
# $parse_status: "ok"
```

Most other exported functions are convenient wrappers around
`safe_parse_url()` to extract specific parts of a URL or just the
`clean_url` string.

## Examples of Helper Functions

``` r
library(rurl)

# Get cleaned URL with specific handling
# Note: case_handling defaults to "lower", trailing_slash_handling to "none"

get_clean_url("Http://Example.Com/MyPath/")
#> [1] "http://example.com/mypath/"

get_clean_url("Http://Example.Com/MyPath/",
              case_handling = "keep",
              trailing_slash_handling = "strip")
#> [1] "Http://example.com/MyPath"

get_clean_url("ftp://Sub.Example.ORG/anotherPath",
              protocol_handling = "strip", # Removes ftp://
              www_handling = "keep",       # Ensures www.
              case_handling = "upper",     # Converts to uppercase
              trailing_slash_handling = "keep") # Ensures trailing slash
#> [1] "WWW.EXAMPLE.ORG/ANOTHERPATH/"

get_clean_url("example.com:8080/path", trailing_slash_handling = "keep")
#> [1] "http://example.com/path/" # Port is not part of clean_url by default

# Extracting specific components
get_domain("https://sub.example.co.uk/page")
#> [1] "example.co.uk"

get_scheme("Example.com/Test", protocol_handling = "https")
#> [1] "https"

get_scheme("Example.com/Test", protocol_handling = "none") # No scheme forced or kept
#> [1] NA

# Host is lowercased due to default case_handling in the underlying safe_parse_url call
get_host("Http://User:Pass@MyHost.Com:8080/SomeWhere")
#> [1] "myhost.com"

# Path is lowercased; trailing slash from input is preserved with trailing_slash_handling = "none" (default)
get_path("HTTP://EXAMPLE.NET/A/B/C/?p=1")
#> [1] "/a/b/c/"

get_tld("www.sub.example.co.uk")
#> [1] "co.uk"

get_parse_status("mailto:test@example.com")
#> [1] "error"
get_parse_status("http://example.com")
#> [1] "ok"

# Handling Subdomain Levels

Functions like `get_host()` and `get_clean_url()` also support the `subdomain_levels_to_keep` argument.
This provides control over how many subdomain levels are kept in the host *after* `www_handling`.

# `NULL` (default): keeps all subdomains (beyond www handling).
# `0`: strips all subdomains (e.g., `one.two.example.com` becomes `example.com`).
# `N > 0`: keeps N levels of subdomains (e.g., `one.two.example.com` with N=1 becomes `two.example.com`).

# With `www_handling = "strip"` and `subdomain_levels_to_keep = 1`:
get_host("http://www.three.two.one.example.com", www_handling = "strip", subdomain_levels_to_keep = 1)
#> [1] "one.example.com"

# With `subdomain_levels_to_keep = 0` (default `www_handling` is `"none"`):
get_clean_url("http://www.deep.sub.example.com/path", subdomain_levels_to_keep = 0)
#> [1] "http://www.example.com/path"
```

## Data Sources

This package includes a processed copy of the [Public Suffix List
(PSL)](https://publicsuffix.org/), used to extract registered domains
and top-level domains. It is updated manually via
`data-raw/update_psl.R`. The original list is maintained by Mozilla and
hosted at: <https://publicsuffix.org/list/public_suffix_list.dat> The
data is included in accordance with the [Mozilla Public License
2.0](https://github.com/publicsuffix/list/blob/main/LICENSE) and is
never downloaded at runtime. See `inst/LICENSE.psl` for full license
text.

To refresh it:

``` r
# From the root of the package:
source("data-raw/update_psl.R")
```

This regenerates the internal `sysdata.rda` file used for domain
parsing.

## License

MIT © 2025 Bart Turczynski
