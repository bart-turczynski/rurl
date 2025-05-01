rurl
================

# rurl

`rurl` is a lightweight R package for parsing and extracting parts of
URLs.

It includes helpers to: - Get cleaned URLs - Extract domains, paths,
schemes, and hosts - Normalize or strip protocols

## Installation

``` r
# Install from GitHub
# devtools::install_github("bart-turczynski/rurl")
```

## Example

``` r
library(rurl)

get_clean_url("example.com/path")
#> "http://example.com/path"

get_domain("https://sub.example.co.uk/page")
#> "example.co.uk"

get_scheme("example.com", protocol_handling = "none")
#> NA
```

## Data Sources

This package includes a processed copy of the [Public Suffix List
(PSL)](https://publicsuffix.org/), used to extract top-level domains.  
It is updated manually via `data-raw/update_psl.R`. The original list is
maintained by Mozilla and hosted at:
<https://publicsuffix.org/list/public_suffix_list.dat> The data is
included in accordance with the [Mozilla Public License
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
