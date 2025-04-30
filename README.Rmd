# rurl

`rurl` is a lightweight R package for parsing and extracting parts of URLs.

It includes helpers to:
- Get cleaned URLs
- Extract domains, paths, schemes, and hosts
- Normalize or strip protocols

## Installation

```r
# Install from GitHub
# devtools::install_github("bart-turczynski/rurl")
```

## Example

```r
library(rurl)

get_clean_url("example.com/path")
#> "http://example.com/path"

get_domain("https://sub.example.co.uk/page")
#> "example.co.uk"

get_scheme("example.com", protocol_handling = "none")
#> NA
```

## License

MIT © Bart Turczynski
